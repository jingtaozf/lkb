/*****************************************************************************\
|*        file: tsdb_engine.c
|*      module: 
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <memory.h>
#include <limits.h>
#include "globals.h"
#include "tsdb.h"
#include "errors.h"

int tsdb_shorten_tuple(Tsdb_tuple** tuples,Tsdb_tuple* fuck)
{
  int j=0, i;
  
  for (i=0;tuples[i]!=NULL;i++)
    {
      if (tuples[i]!=fuck)
        tuples[j++]=tuples[i];
    }
  tuples[j]=NULL;
  return(j);
}

/*****************************************************************************\
|*        file: 
|*      module: tsdb_clean_selection()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* deletes all tuples in a selection with first tuple==NULL;
|* 
\*****************************************************************************/

Tsdb_selection *tsdb_clean_selection(Tsdb_selection* selection,Tsdb_tuple* fuck)
{
  Tsdb_key_list *next, *first, *previous;
  int i,found;

  for (i=0;i<selection->n_key_lists;i++) {
    found = 0;
    if (selection->key_lists[i]){
      for (first=selection->key_lists[i], next=first->next;
           next!=NULL &&
           first->tuples[0] == (Tsdb_tuple*)NULL;
           free(first), first = NULL , first=next , next=next->next) {
        if (first->tuples[0] == (Tsdb_tuple*)NULL) {
          found++;
        }
        if (i==(selection->n_key_lists-1)) { 
          free(first->tuples);
          first->tuples=NULL;
          selection->length--;
        }
      } /* for */
      
      if (first->tuples[0] == (Tsdb_tuple*)NULL) {
        /* the whole list empty */
        if (i==selection->n_key_lists-1) {
          free(first->tuples);
          first->tuples=NULL;
          selection->length=0;
        }
        free(first);
        first=NULL;
        selection->key_lists[i]=NULL;
      } /* if */
      else {
        selection->key_lists[i] = first;
        for (previous=selection->key_lists[i],
             first=next=previous ? previous->next : NULL;
             next != (Tsdb_key_list*)NULL;
             first = next)  {
          
          if (next->tuples[0] == (Tsdb_tuple*)NULL) {
            found++;
            next = next->next;
            if (i==selection->n_key_lists-1) {
              free(first->tuples);
              first->tuples=NULL;
              selection->length--;
            }
            free(first);
            first = NULL;
          } /*if */
          else {
            if (fuck!=NULL) {
              previous->n_tuples = tsdb_shorten_tuple(previous->tuples,fuck);
            }
            previous->next = next ;
            previous = previous->next;
            next = previous->next;
          } /* else */
        } /* for */
        previous->next = (Tsdb_key_list*)NULL;
        if (fuck !=NULL)
          previous->n_tuples = tsdb_shorten_tuple(previous->tuples,fuck);
      } /* else */
    } /* if */
  } /* for */
  return(selection);
} /* tsdb_clean_selection() */

int* tsdb_double_relations(Tsdb_selection *selection_1, 
                           Tsdb_selection *selection_2,int* d){
  BOOL kaerb;
  Tsdb_tuple fuck;
  Tsdb_relation *relation_1, *relation_2, *foo;
  Tsdb_key_list *first, *next_1, *next_2, *bar, *baz;
  int  i, j, k, m,n, index_1, index_2 ;
  char *key_1, *key_2;
  int* delete;

  delete = (int*)malloc(sizeof(int)*
                        (1+selection_2->n_relations+selection_2->n_key_lists));
  memset(delete,'\0',sizeof(int)*(1+selection_2->n_relations));

  for(i = 0;
      i < selection_1->n_relations;
      i++) {
    for(j = 0;
        j < selection_2->n_relations;
        j++) {

      if(tsdb_relations_are_equal(selection_1->relations[i],
                                  selection_2->relations[j])) {

        relation_1 = tsdb_find_relation(selection_1->relations[i]->name);

        relation_2 = tsdb_find_relation(selection_2->relations[j]->name);
       
        /* relation_1 and relation_2 have same name */
        delete[j]=TRUE;
        (*d)++;
        /* one more to delete */
        if(!relation_1->n_keys || !relation_2->n_keys) {
          fprintf(tsdb_error_stream, "Bollox, no key in relation.\n");
          return((int *)NULL);
        } /* if */

        /* we chose the first key that exists to join?? */
        key_1 = relation_1->fields[relation_1->keys[0]];
        key_2 = relation_2->fields[relation_2->keys[0]];

        /* find corresponding key_list to key_1 in selection_1 
           (not necessarily in relation_1 */
        for(kaerb = FALSE, index_1 = 0, k = 0; !kaerb && k <= i; k++) {
          foo = selection_1->relations[k];
          for(m = 0; !kaerb && m < foo->n_keys; m++) {
            if(!strcmp(foo->fields[foo->keys[m]], key_1)) {
              index_1 += m;
              kaerb = TRUE;
            } /* if */
          } /* for */
          if(!kaerb) {
            index_1 += foo->n_keys;
          } /* if */
        } /* for */
        m--; /* for counts one too far */
       /* in relation k-1, list index_1-1  */

        /* find corresponding key_list to key_2 in selection_2 
           (not necessarily in relation_2 */
 
        for(kaerb = FALSE, index_2 = 0, k = 0; !kaerb && k <= j; k++) {
          foo = selection_2->relations[k];
          for(n = 0; !kaerb && n < foo->n_keys; n++) {
            if(!strcmp(foo->fields[foo->keys[n]], key_2)) {
              index_2 += n;
              kaerb = TRUE;
            } /* if */
          } /* for */
          if(!kaerb) {
            index_2 += foo->n_keys;
          } /* if */
        } /* for */
        n--;

        /* index_1 and index_2 are index to key_lists with same Attribute
           belonging to first key of relation_1 and first key of 
           relation_2 . This is only to find out which tuples are to
           be eliminated!! */

#if defined(DEBUG) && defined(TOM)

        fprintf(tsdb_debug_stream,
                "tsdb_join::\n common indices for elimination of tuples\n");
        fprintf(tsdb_debug_stream,
                " %s %s %d \n",selection_1->relations[i]->name, key_1,m);
        fprintf(tsdb_debug_stream,
                " %s %s %d \n",selection_2->relations[j]->name, key_2,n);
#endif       

        if(kaerb) { 
          next_1 = selection_1->key_lists[index_1];
          next_2 = selection_2->key_lists[index_2];

          /* we really should be NOT working with the original: so compare
             and copy!!*/

          if (tsdb_key_list_not_copied(relation_1,m,next_1)) {
             next_1 = selection_1->key_lists[index_1] =
              tsdb_copy_key_list(next_1);
            /* hey thats copy on demand! */
          } /* if */
          if (tsdb_key_list_not_copied(relation_2,n,next_2)) {
            next_2 = selection_2->key_lists[index_2] =
              tsdb_copy_key_list(next_2);
          }
          
          /* tuples in the common relation only stay there if
             they are contained in both ! */
          while(next_1 != NULL && next_2 != NULL) {
            for(; next_1 != NULL &&
                (tsdb_value_compare(next_1->key,
                                    next_2->key) == TSDB_LESS_THAN);
                next_1 = next_1->next)
              next_1->tuples[0] = (Tsdb_tuple *)NULL;

            if(next_1 != NULL) {
              for(; next_2 != NULL &&
                  (tsdb_value_compare(next_2->key,
                                      next_1->key) == TSDB_LESS_THAN);
                  next_2 = next_2->next)
                next_2->tuples[0] = (Tsdb_tuple *)NULL;
            } /* if */
            
            if(next_1 != NULL && next_2 != NULL &&
               (tsdb_value_compare(next_1->key, next_2->key) == TSDB_EQUAL)) {
              /* First : keys must be the same */
              
              for(bar = next_1;
                  bar != NULL &&
                  tsdb_value_compare(bar->key, next_1->key) == TSDB_EQUAL;
                  bar = bar->next) { /* look in relation_1 */
                for(kaerb = FALSE, baz = next_2;
                    !kaerb &&
                    baz != NULL &&
                    (tsdb_value_compare(bar->key, next_2->key) == TSDB_EQUAL);
                    baz = baz->next) {
                  if(bar->tuples[0] != NULL && baz->tuples[0] != NULL && 
                     bar->tuples[i] == baz->tuples[j]) {
                    kaerb = TRUE;
                  } /* if */
                } /* for */
                if(!kaerb) { /* all tuples with same key are different */
                  bar->tuples[0] = (Tsdb_tuple *)NULL;
                } /* if */
              } /* for */
              
              first = bar;
              for(baz = next_2;
                  baz != NULL &&
                  tsdb_value_compare(baz->key, next_2->key) == TSDB_EQUAL;
                  baz = baz->next) {
                for(kaerb = FALSE, bar = next_1;
                    !kaerb &&
                    bar != NULL &&
                    (tsdb_value_compare(bar->key, next_1->key) == TSDB_EQUAL);
                    bar = bar->next) {
                  if(bar->tuples[0] != NULL && baz->tuples[0] != NULL && 
                     bar->tuples[i] == baz->tuples[j]) {
                    baz->tuples[j] = &fuck;
                    /* it will stay but will be compressed */
                    kaerb = TRUE;
                  } /* if */
                } /* for */
                if(!kaerb) {
                  baz->tuples[0] = (Tsdb_tuple *)NULL;
                } /* if */
              } /* for */
              
              next_1 = first;
              next_2 = baz;
            } /* if */
          } /* while */

          if (next_1||next_2) { /* one list at the end but not the other*/
            if (next_1&&next_2) {
              printf("i'm sorry a bug\n"); 
              /* this should not be happening! */
            }
            else {
              if (next_1)
                for (;next_1;next_1->tuples[0]=NULL, next_1=next_1->next);
              if (next_2)
                for (;next_2;next_2->tuples[0]=NULL, next_2=next_2->next);
            } /* else */
          } /* if */
          
        } /* if */ /* if (kaerb) */

      } /* if */ /* relations i and j are equal */

    } /* for */
  } /* for */

  selection_1 = tsdb_clean_selection(selection_1,&fuck);
  selection_2 = tsdb_clean_selection(selection_2,&fuck);

  return delete;

} /* tsdb_double_relations() */

Tsdb_selection *tsdb_simple_join(Tsdb_selection *selection_1,
                                 Tsdb_selection *selection_2) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_simple_join()
|*     version: 
|*  written by: andrew p. white & oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_simple_join() computes the equijoin (on the first common key) of
|* .selection_1. and .selection_2. which are assumed to be compatible relation
|* instances (i.e. they contain the same relations in identical order).
|* tsdb_simple_join() doesn't free selection_[1,2].
|*****************************************************************************|
|* <known bugs>
|* At least for the key list we are currently looking at insert() clearly is 
|* overkill; since we look at the end of the list, a simple append should do
|* instead.  however, we would then somehow have to prevent insert() from
|* inserting into the very same key list once more ... |:-{.
\*****************************************************************************/

  Tsdb_selection *result;
  Tsdb_key_list *next_1, *next_2, *foo;
  Tsdb_tuple **tuples;
  int index_1, index_2, key_1, key_2, offset_1, offset_2;
  int  i, j, k, l,success;
  BOOL kaerb = FALSE;
#if defined(DEBUG) && defined(JOIN)
  float time;
  int m;
#endif

#if defined(DEBUG) && defined(JOIN)
  time = tsdb_timer(TSDB_START_TIMER);
#endif

  for(offset_1 = 0, i = 0; !kaerb && i < selection_1->n_relations; i++) {
    for(offset_2 = 0, j = 0; !kaerb && j < selection_2->n_relations; j++) {
      for(k = 0; !kaerb && k < selection_1->relations[i]->n_keys; k++) {
        for(l = 0; !kaerb && l < selection_2->relations[j]->n_keys; l++) {
          key_1 = selection_1->relations[i]->keys[k];
          key_2 = selection_2->relations[j]->keys[l];
          if(!strcmp(selection_1->relations[i]->fields[key_1],
                     selection_2->relations[j]->fields[key_2])) {
            index_1 = i;
            key_1 = k;
            index_2 = j;
            key_2 = l;
            kaerb = TRUE;
          } /* if */
        } /* for */
      } /* for */
      if(!kaerb) {
        offset_2 += l;
      } /* if */
    } /* for */
    if(!kaerb) {
      offset_1 += k;
    } /* if */
  } /* for */
 /* relation index_1 and key key_1 an relation index_2 and key key_2 
    are the join-keys */
#if defined(DEBUG) && defined(JOIN)
    fprintf(tsdb_debug_stream,
            "simple_join(): %s", selection_1->relations[0]->name);
    for(m = 1; m < selection_1->n_relations; m++) {
      fprintf(tsdb_debug_stream, ":%s", selection_1->relations[m]->name);
    } /* for */
    fprintf(tsdb_debug_stream,
            " (%d) # %s",
            selection_1->length, selection_2->relations[0]->name);
    for(m = 1; m < selection_2->n_relations; m++) {
      fprintf(tsdb_debug_stream, ":%s", selection_2->relations[m]->name);
    } /* for */
  if(kaerb) {
    fprintf(tsdb_debug_stream,
            " (%d) [%s].\n",
            selection_2->length,
            selection_1->relations[index_1]->fields[key_1]);
  } /* if */
  else {
    fprintf(tsdb_debug_stream,
            " (%d) have no join key.\n", selection_2->length);
  } /* else */
  fflush(tsdb_debug_stream);
#endif

  if(kaerb) {
#ifdef FAST_INSERT
    Tsdb_key_list*** lists;
    int last, n_lists,size;
#endif
    /* initialisation phase */
    (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                     (Tsdb_tuple **)NULL);

    result = (Tsdb_selection *)malloc(sizeof(Tsdb_selection));
    result->relations =
      (Tsdb_relation **)malloc((selection_1->n_relations +
                                selection_2->n_relations + 1) * 
                               sizeof(Tsdb_relation *));
    result->n_relations = selection_1->n_relations + selection_2->n_relations;

    for(i = 0; i < selection_1->n_relations; i++) {
      result->relations[i] = tsdb_copy_relation(selection_1->relations[i]);
    } /* for*/
    for(j = 0; j < selection_2->n_relations; j++) {
      result->relations[i + j] = tsdb_copy_relation(selection_2->relations[j]);
      if(j == index_2) {
        for(l = 0, k = 0; k < selection_2->relations[j]->n_keys; k++) {
          if(k != key_2) {
            result->relations[i + j]->keys[l] = 
              selection_2->relations[j]->keys[k];
            l++;
          } /* if */
        } /* for */
        result->relations[i + j]->n_keys--;
      } /* if */
    } /* for */

    result->relations[result->n_relations] = (Tsdb_relation *)NULL;
    result->key_lists =
      (Tsdb_key_list **)malloc((selection_1->n_key_lists + 
                                selection_2->n_key_lists - 1) * 
                               sizeof(Tsdb_key_list *));
    result->n_key_lists = 
      selection_1->n_key_lists + selection_2->n_key_lists - 1;
    for(i = 0; i < result->n_key_lists; i++) {
      result->key_lists[i] = (Tsdb_key_list *)NULL;
    } /* for */
    result->length = 0;
    /* result is prepared */
#ifdef FAST_INSERT
    if (result->n_key_lists>1) {
      lists = (Tsdb_key_list***)malloc((result->n_key_lists+1)*
                                        sizeof(Tsdb_key_list**));
      size = 4000;
      for (i=0;i<result->n_key_lists;i++) {
        lists[i] = (Tsdb_key_list**)malloc((size+1)*sizeof(Tsdb_key_list*));
      } /* for */
    }
    else 
      lists = NULL;
    last = 0;
#endif

    next_1 = selection_1->key_lists[offset_1 + key_1];
    next_2 = selection_2->key_lists[offset_2 + key_2];
    while(next_1 != NULL && next_2 != NULL) {
      for(;
          next_1 != NULL &&
          tsdb_value_compare(next_1->key,
                             next_2->key) == TSDB_LESS_THAN;
          next_1 = next_1->next);
      if(next_1 != NULL) {
        for(;
            next_2 != NULL &&
            tsdb_value_compare(next_2->key,
                               next_1->key) == TSDB_LESS_THAN;
            next_2 = next_2->next);
      } /* if */
      if(next_1 != NULL && next_2 != NULL &&
         tsdb_value_compare(next_1->key, next_2->key) == TSDB_EQUAL) {
        for(;
            next_1 != NULL &&
            tsdb_value_compare(next_1->key, next_2->key) == TSDB_EQUAL;
            next_1 = next_1->next) {
          for(foo = next_2;
              foo != NULL &&
              tsdb_value_compare(next_1->key, foo->key) == TSDB_EQUAL;
              foo = foo->next) {
            tuples =
              (Tsdb_tuple **)malloc((selection_1->n_relations +
                                     selection_2->n_relations + 1) *
                                    sizeof(Tsdb_tuple *));
            for(i = 0; i < next_1->n_tuples; i++) {
              tuples[i] = next_1->tuples[i];
            } /* for */
            for(; i < next_1->n_tuples + foo->n_tuples; i++) {
              tuples[i] = foo->tuples[i - next_1->n_tuples];
            } /* for */
            tuples[i] = (Tsdb_tuple *)NULL;
#ifdef FAST_INSERT
            if (lists) {
              success = tsdb_collect_tuples(result,tuples,lists,last,&size);
              if (success) 
                last++;
              else {
                free(tuples);
                /* free arrays!!! */
                return (Tsdb_selection*)NULL;
              } /* else */
            } /* if */
            else {
              if(tsdb_insert_into_selection(result, &tuples[0])) {
                result->length++;
              } /* if */
              else {
                free(tuples);
                return NULL;
              } /* else */
            }
#else
            /* in the first run we could count here! */
            if(tsdb_insert_into_selection(result, &tuples[0])) {
              result->length++;
            } /* if */
            else {
              free(tuples);
            } /* else */
#endif
          } /* for */
        } /* for */
        next_2 = foo;
      } /* if */
    } /* while */

#ifdef FAST_INSERT
    success = tsdb_sort_tuples(lists,last,result->n_key_lists);
    if (success) {
      success = tsdb_array_to_lists(result,lists,last,size);
    }
#endif
    
#if defined(DEBUG) && defined(JOIN)
    if((time = tsdb_timer(time)) != (float)-1) {
      fprintf(tsdb_debug_stream,
              "simple_join(): assembled %d tuples in %.1f seconds.\n",
              result->length, time);
      fflush(tsdb_debug_stream);
    } /* if */
#endif    

    return(result);
  } /* if */
  else {
    return((Tsdb_selection *)NULL);
  } /* else */

} /* tsdb_simple_join() */

Tsdb_selection *tsdb_join(Tsdb_selection *selection_1, 
                          Tsdb_selection *selection_2) {

  Tsdb_selection *selection_3=NULL,*temp, *result;
  Tsdb_relation **path, *relation_1, *foo;
  char *key_1, **names;
  int d=0,i, j, k, l, m;
  BOOL kaerb;
  int* delete;

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)

  tsdb_print_selection(selection_1,tsdb_debug_stream);
  tsdb_print_selection(selection_2,tsdb_debug_stream);

#endif

  delete =  tsdb_double_relations(selection_1,selection_2,&d);

#if defined(DEBUG) && defined(TOM)&& defined(CRAZY)
/*  output = tsdb_open_pager();*/
  fprintf(tsdb_debug_stream,"reduced selection 1\n");
  tsdb_print_selection(selection_1,tsdb_debug_stream);
  fprintf(tsdb_debug_stream,"reduced selection 2\n");
  tsdb_print_selection(selection_2,tsdb_debug_stream);
/*  pclose(output);*/
#endif

  /* Now begin with keylists */
  names = tsdb_key_names(selection_2);

  /* Lets make a new selection, the order of Lists may be changed */
  selection_3 = (Tsdb_selection*)malloc(sizeof(Tsdb_selection));

  selection_3->n_relations = selection_2->n_relations-d;
  selection_3->relations = (Tsdb_relation**)
    malloc(sizeof(Tsdb_relation*)*(1+selection_3->n_relations));
  selection_3->relations[selection_3->n_relations]=NULL;

  selection_3->n_key_lists = 0;

  selection_3->key_lists = (Tsdb_key_list**)
    malloc(sizeof(Tsdb_key_list*)*(selection_2->n_key_lists+1));
  selection_3->key_lists[selection_2->n_key_lists] = NULL;

  selection_3->length = selection_2->length ; 

  /* now for all remaining relations do:
     if key-name is key-list add this to relation and key_list to selection
     */
  key_1 = "";
  for (i=0,j=0,l=0;i<selection_2->n_relations;i++) {
    if (!delete[i]) {
      foo = tsdb_find_relation(selection_2->relations[i]->name);
      relation_1 = selection_3->relations[j++] = tsdb_copy_relation(foo);
      relation_1->n_keys = 0;
      for (k=0; k<foo->n_keys; k++){
        for (kaerb=FALSE, m=0;
             !kaerb && 
             m < selection_2->n_key_lists;
             m++) { 
          if (!strcmp(names[m],foo->fields[foo->keys[k]])) {
            kaerb = TRUE;
            relation_1->keys[relation_1->n_keys++]=foo->keys[k];
            selection_3->key_lists[l++] = selection_2->key_lists[m];
            selection_3->n_key_lists++;
            names[m]=key_1;
          } /* if */
        } /* for */
      }/* for */
    } /* if */
  } /* for */
  
  selection_3->key_lists[selection_3->n_key_lists] = NULL;
  free(names);
  free(delete);
  delete = NULL;

  if(d) {
    if(selection_3->n_relations) {
      /* there is a common relation -> there is a  direct common key */
      result = tsdb_simple_join(selection_1, selection_3);
    } /* if */
    else {
      /* selection_2 disappeared completely */
      /*result = tsdb_copy_selection(selection_1);*/
      return(selection_1);
    } /* else */
  } /* if */
  else {
    /* no common relation: look for common keys! */
    if((path = tsdb_join_path(selection_1->relations,
                              selection_3->relations)) != NULL) {
      /* proceed with fingers crossed */
      if (path[2]!=NULL)
        { /* there is no direct connection, path[0] is in selection_1
             the last relation in path too! */
          i=1;
          while (path[i+1]) {
            temp = tsdb_find_table(path[i++]);
            selection_1=tsdb_simple_join(selection_1,temp);
          }
        }
      selection_1=tsdb_simple_join(selection_1,selection_3);

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
      tsdb_print_selection(selection_1,tsdb_debug_stream);
#endif
      result = selection_1;
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "join(): no path from %s", selection_1->relations[0]->name);
      for(i = 1; i < selection_1->n_relations; i++) {
        fprintf(tsdb_error_stream, ":%s", selection_1->relations[i]->name);
      } /* for */
      fprintf(tsdb_error_stream, " to %s", selection_2->relations[0]->name);
      for(i = 1; i < selection_2->n_relations; i++) {
        fprintf(tsdb_error_stream, ":%s", selection_2->relations[i]->name);
      } /* for */
      fprintf(tsdb_error_stream, ".\n");
      return((Tsdb_selection *)NULL);
    } /* else */
  } /* else */
  
  return(result);

} /* tsdb_join() */


/*****************************************************************************\
|*        file: 
|*      module: tsdb_simple_merge()
|*     version: 
|*  written by: tom fettig
|* last update: 13.07.95
|*  updated by: tom, dfki saarbruecken
|*****************************************************************************|
|* tsdb_simple_merge() doesn't deallocate selection_[1,2]!
\*****************************************************************************/

Tsdb_selection *tsdb_simple_merge(Tsdb_selection *selection_1,
                                  Tsdb_selection *selection_2) {

/*  Tsdb_relation *relation_1, *relation_2;                        */
/*  Die Selectionen sind gleich (gleiche Relationen beteiligt)     */
/*  Bilde neue Tsdb_Selection, die alle gemeinsamen Tupel enthaelt */
/*  Bedingung: Die Relationen beider Selections sind gleich!!      */
/*  dh auch die Reihenfolge                                 */
/*  Wenn keys gleich sind-> Ueberpruefe alle anderen Felder */
/*  auf Gleichheit -> Wird in neue Selection uebernommen    */

 Tsdb_selection *result;
 Tsdb_key_list *next_1, *next_2, *foo,*last_1,*last_2,*bar;
 Tsdb_tuple **tuples, **ordered_tuple;
 int index_1, index_2, key_1, key_2, offset_1, offset_2, *order;
 int  i, j, k, l;
 BOOL kaerb = FALSE,empty_1;
 Tsdb_key_list*** lists=NULL;
 int last, n_lists,size,success;
 
 
 for(offset_1 = 0, i = 0; !kaerb && i < selection_1->n_relations; i++) {
   for(offset_2 = 0, j = 0; !kaerb && j < selection_2->n_relations; j++) {
     for(k = 0; !kaerb && k < selection_1->relations[i]->n_keys; k++) {
       for(l = 0; !kaerb && l < selection_2->relations[j]->n_keys; l++) {
         key_1 = selection_1->relations[i]->keys[k];
         key_2 = selection_2->relations[j]->keys[l];
         if(!strcmp(selection_1->relations[i]->fields[key_1],
                    selection_2->relations[j]->fields[key_2])) {
           index_1 = i;
           key_1 = k;
           index_2 = j;
           key_2 = l;
           kaerb = TRUE;
         } /* if */
       } /* for */
     } /* for */
     if(!kaerb) {
       offset_2 += l;
     } /* if */
   } /* for */
   if(!kaerb) {
     offset_1 += k;
   } /* if */
 } /* for */
 /* merge-key gefunden offset_2 und offset_1 indices der Key_lists */

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
 fprintf(tsdb_debug_stream,"vor create result\n");
 tsdb_print_selection(selection_1,tsdb_debug_stream); 
 tsdb_print_selection(selection_2,tsdb_debug_stream); 
#endif

 (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                  (Tsdb_tuple **)NULL);

 if(kaerb) {
   result = tsdb_create_selection(selection_1->n_relations,
                                  selection_1->n_key_lists);
   for(i = 0; i < selection_1->n_relations; i++) {
     result->relations[i] = tsdb_copy_relation(selection_1->relations[i]);
   } /* for*/
 } /* if */
 result->length = 0;

#ifdef FAST_INSERT
 if (result->n_key_lists>1) {
   lists = (Tsdb_key_list ***)malloc((result->n_key_lists+1)*
                                     sizeof(Tsdb_key_list*)); 
   size = 4000;
   for (i=0;i<result->n_key_lists;i++) {
     lists[i] = (Tsdb_key_list**)malloc((size+1)*sizeof(Tsdb_key_list*));
   } /* for */
 } /* if */
 else
   lists = NULL;
 last = 0;
#endif

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
 fprintf(tsdb_debug_stream,"vor match\n");
 tsdb_print_selection(selection_1,tsdb_debug_stream); 
 tsdb_print_selection(selection_2,tsdb_debug_stream); 
#endif

 order = tsdb_relation_match(selection_1, selection_2);

 next_1 = selection_1->key_lists[offset_1 + key_1];
 next_2 = selection_2->key_lists[offset_2 + key_2];

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
 fprintf(tsdb_debug_stream,"vor merge\n");
 tsdb_print_selection(selection_1,tsdb_debug_stream); 
 tsdb_print_selection(selection_2,tsdb_debug_stream); 
#endif

 while(next_1 != NULL && next_2 != NULL) {
   for(; next_1 != NULL &&
       tsdb_value_compare(next_1->key,
                          next_2->key) == TSDB_LESS_THAN;
       next_1 = next_1->next) {
     tuples = (Tsdb_tuple **)malloc((result->n_relations+1)*
                                    sizeof(Tsdb_tuple*));
     memcpy(tuples,next_1->tuples,
            (result->n_relations+1)*sizeof(Tsdb_tuple*));
     if (lists) {
       if(tsdb_collect_tuples(result,tuples,lists,last,&size)) {
         last ++;
       }           
       else {
         free(tuples);
         return (Tsdb_selection*)NULL;
       } /* else */
     } /* lists */
     else {
       if(tsdb_insert_into_selection(result, &tuples[0])) {
         result->length++;
       } /* if */
       else {
         free(tuples);
         return (Tsdb_selection*)NULL;
       } /* else */
     } /* else lists */
   } /* for */

   if(next_1 != NULL) {
     for(; next_2 != NULL &&
         tsdb_value_compare(next_2->key,
                            next_1->key) == TSDB_LESS_THAN;
         next_2 = next_2->next) {
       ordered_tuple = (Tsdb_tuple **)malloc((result->n_relations+1)*
                                              sizeof(Tsdb_tuple*));
       /* if the order of relations is different, reorder */
       for (i=0; i<selection_2->n_relations ; i++)
         ordered_tuple[order[i]] = next_2->tuples[i];
       ordered_tuple[i]=NULL;
       if (lists)
         if(tsdb_collect_tuples(result,ordered_tuple,lists,last,&size)) {
           last ++;
         }           
         else {
           free(ordered_tuple);
           return (Tsdb_selection*)NULL;
         } /* else */
       else
         if(tsdb_insert_into_selection(result, &ordered_tuple[0])) 
           result->length++;
         else {
           free(ordered_tuple);
           return (Tsdb_selection*)NULL;
         } /* else */
     } /* for */
   } /* if */

     /* if there are equal keys, then the tuple could be the same
        as well. Eliminate double tuples. */
   
   if(next_1 != NULL && next_2 != NULL &&
      tsdb_value_compare(next_1->key, next_2->key) == TSDB_EQUAL) {
     last_1 = tsdb_first_other_key(next_1);
     last_2 = tsdb_first_other_key(next_2);
     
     for (kaerb = FALSE,bar = next_2; 
          bar != last_2 ; 
          kaerb = FALSE,bar = bar->next) {
       for(foo = next_1; !kaerb && foo !=last_1; foo = foo->next) {
         /* keys are equal!! 
            now if bar is equal to some foo, don't insert it*/
         for ( kaerb = TRUE , i=0; kaerb && i< result->n_relations ; i++) 
           if (!tsdb_tuple_equal(bar->tuples[i], foo->tuples[order[i]])) {
             kaerb = FALSE;
           } /* if */
       } /* for foo */

       if (!kaerb) {
           /* unequal */
         ordered_tuple = (Tsdb_tuple **)malloc((result->n_relations + 1)*
                                               sizeof(Tsdb_tuple *));
         for (i=0;i<selection_2->n_relations; i++) {
           ordered_tuple[order[i]]=bar->tuples[i];
         } /* for */
         ordered_tuple[i]=NULL;
         if (lists) {
           if(tsdb_collect_tuples(result,ordered_tuple,lists,last,&size)) {
             last ++;
           }           
           else {
             free(ordered_tuple);
             return (Tsdb_selection*)NULL;
           } /* else */
         }
         else {
           if(tsdb_insert_into_selection(result, &ordered_tuple[0])) {
             result->length++;
           }
           else {
             free(ordered_tuple);
             return (Tsdb_selection*)NULL;
           } /* else */
         } /* else */
       } /* if */
     } /* for bar */
     
     for (foo = next_1; foo !=last_1; foo = foo->next) {
       tuples = (Tsdb_tuple **)malloc((result->n_relations + 1)*
                                      sizeof(Tsdb_tuple *));
       memcpy(tuples,foo->tuples,(result->n_relations + 1)*
              sizeof(Tsdb_tuple *)); 
       if (lists) {
         if(tsdb_collect_tuples(result,tuples,lists,last,&size)) {
           last ++;
         }           
         else {
           free(tuples);
           return (Tsdb_selection*)NULL;
         } /* else */
       } 
       else {
         if(tsdb_insert_into_selection(result, &tuples[0])) {
           result->length++;
         } /* if */
         else {
           free(tuples); 
           return (Tsdb_selection*)NULL;
         }
       }
     } /* for */
     next_1 = last_1;
     next_2 = last_2;
   } /* if */

 } /* while */    
 
 if ((next_1 != NULL) || (next_2 != NULL)) {
   if (next_1 !=NULL) {
     foo = next_1;
     empty_1 = FALSE;
   }
   else {
     foo = next_2;
     empty_1 = TRUE;
   }
   for (;foo!=NULL; foo=foo->next) {
     tuples = (Tsdb_tuple **)malloc((result->n_relations + 1)*
                                    sizeof(Tsdb_tuple *));
     if (empty_1) {
       for (i=0;i<selection_2->n_relations; i++) {
         tuples[order[i]]=foo->tuples[i];
       }
       tuples[i]=NULL;
     }
     else
       memcpy(tuples,foo->tuples,(result->n_relations + 1)*
              sizeof(Tsdb_tuple *));
     if (lists)
       if(tsdb_collect_tuples(result,tuples,lists,last,&size)) {
         last ++;
       }           
       else {
         free(tuples);
         return (Tsdb_selection*)NULL;
       } /* else */
     else
       if(tsdb_insert_into_selection(result, &tuples[0])) {
         result->length++;
       } /* if */
       else {
         free(tuples); 
         return (Tsdb_selection*)NULL;
       }
   } /* for */
 } /* if */

#ifdef FAST_INSERT
 if (lists) {
   success = tsdb_sort_tuples(lists,last,result->n_key_lists);
   if (success) {
     success = tsdb_array_to_lists(result,lists,last,size);
   }
 } /* if */
#endif
 
 free(order);
 return(result);
   
} /* tsdb_simple_merge() */
/*---------------------------------------------------------------------------*/


/*****************************************************************************\
|*        file: 
|*      module: tsdb_complex_merge()
|*     version: 
|*  written by: tom fettig 
|* last update: 13-jul-95
|*  updated by: tom, dfki saarbruecken
|*****************************************************************************|
|* doesn't touch selection_1 and selection_2, but shouldn't lose memory
|* if all tables are loaded!!
\*****************************************************************************/

Tsdb_selection* tsdb_complex_merge(Tsdb_selection *selection_1,
                                   Tsdb_selection *selection_2)
/* selection_1 und selection_2 may be any selections that are
   joinable in a way, ie with a pair of relations that may be joined.
   result is a selection which contains both the relations of
   selection_1 and the relations of selection_2. 
*/
{

 /* Plan: build selections new_1 and new_2 which can be simple_merged.
    First: Which relations lacks selection_1 and selection_2??
    Which relations are needed to make them joinable?
    First stage:
     find out about lacking relations

     none of them -> go to simple_merge

     some of them, but some relations are equal -> find them out, add them
     one after the other

     all of them, ie no relations are equal -> find join_path,
     join them both, 

     -> go to simple merge
     
     */
  
  Tsdb_selection *result;
  Tsdb_relation **path=NULL,**not_in_1,**not_in_2;
  Tsdb_selection *one, *two, *three,*temp;
  int offset_1, offset_2;
  int  i, j, l_1,l_2;
  BOOL joinable = FALSE,kaerb = FALSE;
   
  not_in_1 = (Tsdb_relation**)
    malloc(tsdb_n_relations()*sizeof(Tsdb_relation*));
  if (!not_in_1) 
    return(NULL);
  not_in_2 = (Tsdb_relation**)
    malloc(tsdb_n_relations()*sizeof(Tsdb_relation*));
  if (!not_in_2)
    return NULL;

  for(l_2 = 0, offset_1 = 0, i = 0, kaerb = FALSE; 
      !kaerb && i < selection_1->n_relations; 
      i++) {
    for(offset_2 = 0, j = 0; !kaerb && j < selection_2->n_relations; j++) {
      if (!strcmp(selection_1->relations[i]->name,
                  selection_2->relations[j]->name)) {
        kaerb=TRUE;
      } /* if */
      if (tsdb_are_joinable(selection_1->relations[i],
                            selection_2->relations[j])){
        joinable = TRUE;
      } /* if */
    } /* for */
    if (!kaerb) {
      /* selection_1->relations[i] is only in selection_1 */
      not_in_2[l_2++]=selection_1->relations[i];
    }
  } /* for */
  not_in_2[l_2]= (Tsdb_relation*) NULL;

  for(l_1 = 0, offset_1 = 0, i = 0, kaerb = FALSE; 
      !kaerb && i < selection_2->n_relations; 
      i++) {
    for(offset_2 = 0, j = 0; !kaerb && j < selection_1->n_relations; j++) {
      if (!strcmp(selection_2->relations[i]->name,
                  selection_1->relations[j]->name)) {
        kaerb=TRUE;
      } /* if */
    } /* for */
    if (!kaerb) {
      /* selection_2->relations[i] is only in selection_2 */
      not_in_1[l_1++] = selection_2->relations[i];
    }
  } /* for */
  not_in_1[l_1]= (Tsdb_relation*) NULL;
  
  /*
     if l equals number of relations of selection_2, then no relation
     of it is in selection_1, so there's no common relation ->
     build a path first!!
 */
      
  if (l_1==selection_2->n_relations &&
      l_2==selection_1->n_relations) {
    one = tsdb_find_tables(selection_1->relations);
    two = tsdb_find_tables(selection_2->relations);
    if (!joinable)
      path = tsdb_join_path(selection_1->relations,selection_2->relations);
      
    for (i=0;path && path[i];i++);
    if (i>2) {
      path[i-1] = (Tsdb_relation*) NULL;

      three = tsdb_find_tables(path+1);
      temp = one;
      one = tsdb_simple_join(one,three);
      tsdb_free_selection(temp);
      tsdb_free_selection(three);
      three = tsdb_find_tables(path+1);
      temp = two;
      two  = tsdb_simple_join(two,three);
      tsdb_free_selection(temp);
      tsdb_free_selection(three);
    }
    selection_2 = tsdb_simple_join(selection_2,one);
    tsdb_free_selection(one);
    selection_1 = tsdb_simple_join(selection_1,two);
    tsdb_free_selection(two);
#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
    tsdb_print_selection(selection_1,tsdb_debug_stream);
    tsdb_print_selection(selection_2,tsdb_debug_stream);
#endif
  } /* if */
  else {
    if (l_1) {
      /* l_1 relations are lacking from selection_1 */
      one = tsdb_find_tables(not_in_1);
      selection_1 = tsdb_simple_join(selection_1,one);
      tsdb_free_selection(one);
    } /* l_1 */
    if (l_2) {
      /* l_2 relations are lacking from selection_2 */
      two = tsdb_find_tables(not_in_2);
      selection_2 = tsdb_simple_join(selection_2,two);
      tsdb_free_selection(two);
    } /* l_2 */
  } /* else */
  /* now everything is set up that selection_1 and selection_2 have
     the same relations, not necessarily in the same order, but
     this is handled by simple_merge
     */
#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
  fprintf(tsdb_debug_stream,"vor tsdb_simple_merge\n");

  tsdb_print_selection(selection_1,tsdb_debug_stream);
  tsdb_print_selection(selection_2,tsdb_debug_stream);
#endif
  result = tsdb_simple_merge(selection_1,selection_2);
  free(not_in_1);
  free(not_in_2);
  return(result);
} /* tsdb_complex_merge() */
/*---------------------------------------------------------------------------*/
