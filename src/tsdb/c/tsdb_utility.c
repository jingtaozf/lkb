/*File: tsdb_utility.c                                                       *
 *Author: tom (fettig@dfki.uni-sb.de), former part of tsdb.c,                *
 *slaughtered on 17th August nineteen ninety four                            *
 *tsdb.c: Started by Andrew P. White (apwhite@unix1.tcd.ie), memory managment*
 *data_structures, chauffering services, volleyball coaching, marriage       *
 *guidance counselling & german styling by the lovely oe (oe@dfki.uni-sb.de) *
 *Completed: 13th September 1993 by oe :-)                                   *
 *Fixed and turned into somthing useful by tom (fettig@dfki.uni-sb.de)       *
 *Comment: Contains many smaller tsdb library functions                      *
 *Date (creation): 17th August 1994                                          */
 

#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <malloc.h>
#include <sys/timeb.h>

#include "globals.h"
#include "tsdb.h"
#include "errors.h"


/* ------------------------- Tsdb_value Operations ---------------------*/

BYTE tsdb_value_compare(Tsdb_value *foo, Tsdb_value *bar) {

  int teresa;

  if(foo->type == bar->type) {
    switch(foo->type) {
    case TSDB_INTEGER:
      if(foo->value.integer == bar->value.integer) {
        return(TSDB_EQUAL);
      } /* if */
      else {
        return((foo->value.integer < bar->value.integer ?
                TSDB_LESS_THAN :
                TSDB_GREATER_THAN));
      } /* else */
      break;
    case TSDB_STRING:
      if(!(teresa = strcmp(foo->value.string, bar->value.string))) {
        return(TSDB_EQUAL);
      } /* if */
      return(teresa < 0 ? TSDB_LESS_THAN : TSDB_GREATER_THAN);
      break;
    default:
      if(really_verbose_mode) 
        fprintf(stderr,
                "What are you at? I've never heard of such a tsdb type.\n");
      else 
        fprintf(stderr,
                "tsdb_value_compare: invalid value type in comparison.\n");
      return(TSDB_VALUE_INCOMPATIBLE);
    } /* switch */
  } /* if */
  else {
    if(really_verbose_mode) 
      fprintf(stderr,
              "Dickhead, you want me to compare strings & integers???.\n");
    else 
      fprintf(stderr,
              "tsdb_value_compare: incompatible types in comparison.\n");
    return(TSDB_VALUE_INCOMPATIBLE);
  } /* else */

} /* tsdb_value_compare() */


/*  True if bar is substring of foo */

BOOL tsdb_value_substring(Tsdb_value *foo, Tsdb_value *bar) {

  if(foo->type == TSDB_STRING && bar->type == TSDB_STRING) {
    return(strstr(foo->value.string, bar->value.string) != NULL);
  } /* if */
  else {
    return(FALSE);
  } /* else */

} /* tsdb_value_substring() */
/*-------------- end of Operations ---------------------*/

BOOL tsdb_tuple_equal(Tsdb_tuple* foo, Tsdb_tuple* bar)
{
  int i;
  
  for (i=0 ; i < foo->n_fields ; i++) {
    if (tsdb_value_compare(foo->fields[i],bar->fields[i]) !=
        TSDB_EQUAL)
      return (FALSE) ;
  }
  return(TRUE);
} /* tsdb_tuple_equal() */
/*-------------- array functions -----------------------*/

/* Append Tsdb_value value to array */

Tsdb_value **tsdb_value_array_append(Tsdb_value **array, Tsdb_value *value) {
  
  int n;
  
  for(n = 0; array[n] != NULL; n++);
  array = (Tsdb_value **)realloc(array, (n + 2) * sizeof(Tsdb_value *));
  array[n] = value;
  array[++n] = (Tsdb_value *)NULL;

  return(array);

} /* tsdb_value_array_append() */

/*---------------- Tsdb_field --------------------*/
/* Attribute are implemented as Tsdb_field array  */
/* create array with one value field */
Tsdb_field **tsdb_singleton_field_array(Tsdb_field *field) {

  Tsdb_field **foo, **bar;

  foo = bar = (Tsdb_field **)malloc(2 * sizeof(Tsdb_field *));
  *foo = field;
  *++foo = (Tsdb_field *)NULL;

  return(bar);

} /* tsdb_singleton_field_array() */


/* Append field to array */

Tsdb_field **tsdb_field_array_append(Tsdb_field **array, Tsdb_field *field) {
  
  int n;
  
  for(n = 0; array[n] != NULL; n++);
  array = (Tsdb_field **)realloc(array, (n + 2) * sizeof(Tsdb_field *));
  array[n] = field;
  array[++n] = (Tsdb_field *)NULL;

  return(array);

} /* tsdb_field_array_append() */
/*-------------------------end of array-Operations ------------------------*/


BOOL tsdb_contains_relation(Tsdb_selection* foo,Tsdb_relation* bar){
  int i;

  for (i=0; i< foo->n_relations;i++)
    if (!strcmp(foo->relations[i]->name,bar->name))
      return(TRUE);

  return(FALSE);
} /* tsdb_contains_relation() */


/* True if attribute_list contains attributes of relation */

BOOL tsdb_are_attributes(Tsdb_value **attribute_list, Tsdb_relation *relation){
  
  BOOL kaerb;
  int i, j;
  
  for(i = 0; attribute_list[i] != NULL; i++) {
    for(j = 0, kaerb = FALSE; j < relation->n_fields && !kaerb; j++) {
      if(!strcmp(attribute_list[i]->value.identifier, relation->fields[j]))
        kaerb = TRUE;
    } /* for */
    if(!kaerb) {
      if(really_verbose_mode)
        fprintf(stderr, "No such fucking attribute: %s\n",
                attribute_list[i]->value.identifier);
      else
        fprintf(stderr, "No such attribute: %s\n",
                attribute_list[i]->value.identifier);
      return(FALSE);
    } /* if */
  } /* for */
  
  return(TRUE);

} /* tsdb_are_attributes() */

BOOL tsdb_are_joinable(Tsdb_relation *source, Tsdb_relation *target) {
  
  int i, j, k, l;
  
  for(i = 0; i < source->n_keys; i++) {
    for(j = 0; j < target->n_keys; j++) {
      if(!strcmp(source->fields[source->keys[i]], 
                 target->fields[target->keys[j]])) {
        return(TRUE);
      } /* if */
    } /* for */
  } /* for */

  return(FALSE);

} /* tsdb_are_joinable() */


BOOL tsdb_joins_to(Tsdb_relation *rel,Tsdb_selection* sel) {

  int i;

  for (i=0; i<sel->n_relations; i++) {
    if (tsdb_are_joinable(rel,sel->relations[i]))
      return(TRUE);
  } /* for */
  return FALSE;

} /* tsdb_joins_to */

BOOL tsdb_is_attribute(Tsdb_value *value)
{
  int i,j;

  for(i = 0; tsdb_relations[i] != NULL; i++) {
    for (j = 0; j<tsdb_relations[i]->n_fields ; j++) {
      if (!strcmp(tsdb_relations[i]->fields[j],value->value.string))
        return(TRUE);
    } /* for */
  } /* for */
  return(FALSE);

} /* tsdb_is_attribute() */
/*--------------------------------------------------------------------------*/


int tsdb_n_attributes() {
  Tsdb_relation *relation,** all = tsdb_all_relations();
  int m=20,h,l,k,i,n = tsdb_n_relations();
  char** names;
  BOOL kaerb;

  names = (char**)malloc(sizeof(char**)*(m+1));
  for (k=0,i=0;i<n;i++) {
    relation=all[i];
    for (l=0;l<relation->n_fields;l++) {
      for(h=0,kaerb=FALSE;!kaerb && h<k;h++) {
        if (!strcmp(names[h],relation->fields[l])) {
          kaerb=TRUE;
        } /* if */
      } /* for h */
      if (!kaerb){
        if (!(k<m)){
          m += m;
          names = realloc(names,sizeof(char**)*(m+1));
        }
        names[k++]=relation->fields[l];
      } /* if */
    } /* for l */
  } /* for i */
  free(names);
  return(k);
  
} /* tsdb_n_attributes() */

char** tsdb_all_attribute_names()
{
  Tsdb_relation *relation,** all = tsdb_all_relations();
  int m=20,h,l,k,i,n = tsdb_n_relations();
  char** names;
  BOOL kaerb;

  names = (char**)malloc(sizeof(char**)*(m+1));
  for (k=0,i=0;i<n;i++) {
    relation=all[i];
    for (l=0; l<relation->n_fields ;l++) {
      for(h=0,kaerb=FALSE ; !kaerb && h<k ; h++) {
        if (!strcmp(names[h],relation->fields[l])) {
          kaerb=TRUE;
        } /* if */
      } /* for h */
      if (!kaerb){
        if (!(k<m)){
          m += m;
          names = realloc(names,sizeof(char**)*(m+1));
        }
        names[k++]=relation->fields[l];
      } /* if */
    } /* for l */
  } /* for i */
  names[k]=NULL;
  return(names);
  
}/* tsdb_all_attributes */


Tsdb_relation** tsdb_attribute_relations(Tsdb_value *value)
{
  int i,j,k;
  Tsdb_relation** attribute_relations = NULL;

  for(i = 0,k = 0; tsdb_relations[i] != NULL; i++) {
    for (j = 0; j<tsdb_relations[i]->n_fields ; j++) {
      if (!strcmp(tsdb_relations[i]->fields[j],value->value.string)){
        if (!attribute_relations) {
          attribute_relations = (Tsdb_relation**) 
            malloc((tsdb_n_relations()+1)*sizeof(Tsdb_relation*));
          memset(attribute_relations,0,
                 (tsdb_n_relations()+1)*sizeof(Tsdb_relation*));
        } /* if */
        attribute_relations[k++] = tsdb_relations[i];
      } /* if */
    } /* for */
  } /* for */
  if (attribute_relations) attribute_relations[k] = NULL;
  
  return(attribute_relations);
}


/*--------------------------------------------------------------------------*/

BOOL tsdb_is_relation(Tsdb_value *value) { /* True if value is the name of a relation */
  
  int i;

  for(i = 0; tsdb_relations[i] != NULL; i++)
    if(!strcmp(tsdb_relations[i]->name, value->value.identifier))
      return(TRUE);
  return(FALSE);
  
} /* tsdb_is_relation() */

/*---------------------------------------------------------------------------*/

BOOL tsdb_relations_are_equal(Tsdb_relation *relation_1,
                              Tsdb_relation *relation_2) {

  return(relation_1 == relation_2 ||
         !strcmp(relation_1->name, relation_2->name));
} /* tsdb_relations_are_equal() */

/*---------------------------------------------------------------------------*/

BOOL tsdb_key_list_not_copied(Tsdb_relation* relation,int key_list_key,
                              Tsdb_key_list* key_list)
{
  int i;
  
  for (i=0;i<relation->n_keys;i++)
    if (relation->keys[i]==key_list_key)
      if (tsdb_find_table(relation)->key_lists[i]==key_list)
        return(TRUE);
  return(FALSE);
} /* tsdb_key_list_not_copied() */

/*---------------------------------------------------------------------------*/


Tsdb_key_list* tsdb_first_other_key(Tsdb_key_list* key_list)
{
  Tsdb_key_list *last_1;

  for(last_1 = key_list;
      last_1 != NULL &&
      tsdb_value_compare(key_list->key,last_1->key) == TSDB_EQUAL;
      last_1 = last_1->next)  
    ;
  
   return(last_1);
 }



/*---------------------------------------------------------------------------*/

BOOL tsdb_satisfies_condition(Tsdb_tuple *tuple, Tsdb_node *condition,
                              Tsdb_relation *relation) {
  
  int i, integer;
  char *string, *attribute;
  BOOL number, answer;

  attribute = strdup(condition->left->node->value.identifier);
  if(condition->right->node->type == TSDB_STRING) {
    string = strdup(condition->right->node->value.string);
    number = FALSE;
  }
  else if(condition->right->node->type == TSDB_INTEGER) {
    integer = condition->right->node->value.integer;
    number = TRUE;
  }
  else {
    fprintf(stderr, "tsdb: bad tsdb_type.\n");
    return(FALSE);
  }

  for(i = 0; strcmp(attribute, relation->fields[i]) != 0; i++);
  if(number) 
    printf("comparing %d with %d with operator: %d.\n",
           integer, tuple->fields[i]->value.integer,
           condition->node->value.operator);
  else
    printf("comparing %s with %s with operator: %d.\n",
           string, tuple->fields[i]->value.string, 
           condition->node->value.operator);
  switch(condition->node->value.operator) {
  case TSDB_EQUAL :
    if(number) answer = (tuple->fields[i]->value.integer == integer);
    else answer = (!strcmp(string, tuple->fields[i]->value.string));
    break;
  case TSDB_NOT_EQUAL :
    if(number) answer = (tuple->fields[i]->value.integer != integer);
    else answer = (strcmp(string, tuple->fields[i]->value.string));
    break;
  case TSDB_LESS_THAN :
    if(number) answer = (tuple->fields[i]->value.integer < integer);
    else {
      fprintf(stderr, "Cannot compare strings with '<' operator.\n");
      answer = FALSE;
    }
    break;
  case TSDB_LESS_OR_EQUAL_THAN :
    if(number) answer = (tuple->fields[i]->value.integer <= integer);
    else {
      fprintf(stderr, "Cannot compare strings with '<=' operator.\n");
      answer = FALSE;
    }
    break;
  case TSDB_GREATER_THAN :
    if(number) answer = (tuple->fields[i]->value.integer > integer);
    else {
      fprintf(stderr, "Cannot compare strings with '>' operator.\n");
      answer = FALSE;
    }
    break;
  case TSDB_GREATER_OR_EQUAL_THAN :
    if(number) answer = (tuple->fields[i]->value.integer >= integer);
    else {
      fprintf(stderr, "Cannot compare strings with '>=' operator.\n");
      answer = FALSE;
    }
    break;
  case TSDB_SUBSTRING :
    if(number) {
      fprintf(stderr, "Cannot compare integers with '~' operator.\n");
      answer = FALSE;
    }
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) != NULL);
    }
    break; 
  case TSDB_NOT_SUBSTRING :
    if(number) {
      fprintf(stderr, "Cannot compare integers with '~' operator.\n");
      answer = TRUE;
    }
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) == NULL);
    }
    break; 
    default :
      fprintf(stderr, "no such operator.\n");
  } /* switch */
  
  printf("returning %d.\n", answer);
  return(answer);
    
} /* tsdb_satisfies_condition() */

char *tsdb_join_key(Tsdb_relation *relation_1, Tsdb_relation *relation_2) {

  int i, j;

  for(i = 0; i < relation_1->n_keys; i++) {
    for(j = 0; j < relation_2->n_keys; j++) {
      if(!strcmp(relation_1->fields[relation_1->keys[i]],
                 relation_2->fields[relation_2->keys[j]])) {
        return(strdup(relation_1->fields[relation_1->keys[i]]));
      } /* if */
    } /* for */
  } /* for */

  return((char *)NULL);

} /* tsdb_join_key() */

char **tsdb_common_keys(Tsdb_relation *relation_1, Tsdb_relation *relation_2) {

  char **result;
  int i, j, n_results;

  result = (char **)NULL;
  n_results = 0;

  for(i = 0; i < relation_1->n_keys; i++) {
    for(j = 0; j < relation_2->n_keys; j++) {
      if(!strcmp(relation_1->fields[relation_1->keys[i]],
                 relation_2->fields[relation_2->keys[j]])) {
        if(result == NULL) {
          result = (char **)malloc(2 * sizeof(char *));
        } /* if */
        else {
          result = (char **)realloc(result, (n_results + 2) * sizeof(char *));
        } /* else */
        result[n_results++] = strdup(relation_1->fields[relation_1->keys[i]]);
      } /* if */
    } /* for */
  } /* for */
  result[n_results] = (char *)NULL;
  return(result);

} /* tsdb_common_keys() */

/*---------------------------------------------------------------------------*/

/* -------------- functions on relations --------------- */
void tsdb_remove_relation(char *relation_name) {
  
  int i;
  
  for(i = 0; tsdb_relations[i] != NULL; i++) {
    if(!strcmp(tsdb_relations[i]->name, relation_name))
      break;
  } /* for */
  if(tsdb_relations[i] != NULL) {
    while((tsdb_relations[i] = tsdb_relations[++i]) != NULL);
  } /* if */

} /* tsdb_remove_relation() */

void tsdb_add_relation(Tsdb_relation *relation) {
  
  int i;
  
  for(i = 0; tsdb_relations[i] != NULL; i++);
  tsdb_relations = 
    (Tsdb_relation **)realloc(tsdb_relations, 
                              (i + 2) * sizeof(Tsdb_relation *));
  tsdb_relations[i] = relation;
  tsdb_relations[i + 1] = (Tsdb_relation *)NULL;
  
} /* tsdb_add_relation() */


Tsdb_relation *tsdb_field_2_relation(char *table, Tsdb_field **fields) {
  
  int i, n_keys = 0;
  Tsdb_relation *relation = (Tsdb_relation *)malloc(sizeof(Tsdb_relation));
  relation->fields = (char **)NULL;
  relation->types = (BYTE *)NULL;
  relation->keys = (int *)NULL;
  relation->n_fields = 0;
  relation->n_keys = 0;

  relation->name = strdup(table);
  for(i = 0; fields[i] != NULL; i++) {
    if(relation->fields == NULL) {
        relation->fields = (char **)malloc(sizeof(char *));
        relation->types = (BYTE *)malloc(sizeof(BYTE));
      } /* if */
    else {
      relation->fields = (char **)realloc(relation->fields,
                                          (i + 1) * sizeof(char *));
      relation->types = (BYTE *)realloc(relation->types,
                                        (i + 1) * sizeof(BYTE));
    } /* else */
    
    relation->fields[i] = strdup(fields[i]->name);
    
    if(relation->keys == NULL) 
      relation->keys = (int *)malloc(sizeof(int));
    else
      relation->keys = (int *)realloc(relation->keys, (i + 1) * sizeof(int));
    
    if(fields[i]->key)
      relation->keys[n_keys++] = i;
    relation->types[i] = fields[i]->type;
  } /* for */

  relation->n_fields = i;
  relation->n_keys = n_keys;
  
  return(relation);

} /* tsdb_field_2_relation() */

     
Tsdb_relation *tsdb_find_relation(char *name) {

  int i;

  if(tsdb_relations != NULL ||
     tsdb_all_relations() != NULL) {
    for(i = 0; tsdb_relations[i] != NULL; i++) {
      if(!strcmp(name, tsdb_relations[i]->name)) {
        return(tsdb_relations[i]);
      } /* if */
    } /* for */
  } /* if */
  return((Tsdb_relation *)NULL);

} /* tsdb_find_relation() */

Tsdb_relation **tsdb_all_relations() {

  Tsdb_relation *relation;
  FILE *input;
  int i;

  if(tsdb_relations == NULL) {
    if((input = tsdb_find_relations_file("r")) != NULL) {
      for(i = 0; (relation = tsdb_read_relation(input)) != NULL; i++) {
        if(tsdb_relations == NULL) {
          tsdb_relations =
            (Tsdb_relation **)malloc(2 * sizeof(Tsdb_relation *));
        } /* if */
        else {
          tsdb_relations =
            (Tsdb_relation **)realloc(tsdb_relations,
                                      (i + 2) * sizeof(Tsdb_relation *));
        } /* else */
        tsdb_relations[i] = relation;
        tsdb_relations[i + 1] = (Tsdb_relation *)NULL;
      } /* for */
      return(tsdb_relations);
    } /* if */
    else {
      return((Tsdb_relation **)NULL);
    } /* else */  
  } /* if */
  else {
    return(tsdb_relations);
  } /* else */

} /* tsdb_all_relations() */

static char** names = NULL;

char** tsdb_all_relation_names() {
  Tsdb_relation** all;
  int i,n= tsdb_n_relations();
  static char **names=NULL;

  all = tsdb_all_relations();
  if(names) 
    names = realloc(names,sizeof(char*)*(n+1));
  else
    names = malloc(sizeof(char*)*(n+1));
  
  for (i=0;i<n;i++) {
    names[i]=all[i]->name;
  }
  names[i]=NULL;
  return(names);
} /* tsdb_all_relation_names() */

int tsdb_n_relations() {

  Tsdb_relation **relations;
  int n;

  for(n = 0, relations = tsdb_all_relations();
      relations != NULL && relations[n];
      n++);
  return(n);

} /* tsdb_n_relations() */


/*---------------------------------------------------------------------------*/

Tsdb_relation *tsdb_copy_relation(Tsdb_relation *relation) {

  Tsdb_relation *new;
  char **foo;
  int i, j;

  new = (Tsdb_relation *)malloc(sizeof(Tsdb_relation));
  new->fields = (char **)malloc((relation->n_fields + 1) * sizeof(char *));
  new->types = (BYTE *)malloc((relation->n_fields + 1) * sizeof(BYTE));
  new->keys = (int *)malloc((relation->n_keys + 1) * sizeof(int));
  new->total = (BYTE *)malloc((relation->n_keys + 1) * sizeof(BYTE));

  new->name = strdup(relation->name);
  for(i = 0; i < relation->n_fields; i++) {
    new->fields[i] = strdup(relation->fields[i]);
    new->types[i] = relation->types[i];
  } /* for */
  new->n_fields = relation->n_fields;
  for(i = 0; i < relation->n_keys; i++) {
    new->keys[i] = relation->keys[i];
    new->total[i] = relation->total[i];
  } /* for */
  new->n_keys = relation->n_keys;
  new->keys[new->n_keys]=NULL;
  return(new);

} /* tsdb_copy_relation() */


/*---------------------------------------------------------------------------*/

Tsdb_key_list *tsdb_copy_key_list(Tsdb_key_list *key_list) {

  Tsdb_key_list *new;
  int i;

  new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
  new->key = key_list->key;
  new->n_tuples = key_list->n_tuples;
  new->tuples =
    (Tsdb_tuple **)malloc((key_list->n_tuples+1) * sizeof(Tsdb_tuple *));
  for(i = 0; i < key_list->n_tuples; i++) {
    new->tuples[i] = key_list->tuples[i];
  } /* for */
  new->tuples[i]=NULL;
  new->next = key_list->next;

  return(new);

} /* tsdb_copy_key_list() */

Tsdb_selection *tsdb_find_table(Tsdb_relation *relation) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_find_table()
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
|* last update: 30-jun-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

  int i;
  BOOL kaerb = FALSE;

  if(relation == NULL || relation->name == NULL || tsdb_relations == NULL) {
    fprintf(TSDB_ERROR_STREAM,
            "find_table(): invalid context or parameter call.\n");
    return((Tsdb_selection *)NULL);
  } /* if */
  for(i = 0;
      tsdb_relations[i] != NULL && strcmp(tsdb_relations[i]->name,
                                          relation->name);
      i++);
  if(tsdb_relations[i] == NULL) {
    fprintf(TSDB_ERROR_STREAM,
            "find_table(): unknown relation `%s'.\n",
            relation->name);
    return((Tsdb_selection *)NULL);
  } /* if */

  if(tsdb_data == NULL) {
    tsdb_data = (Tsdb_selection **)malloc(2 * sizeof(Tsdb_selection *));
    tsdb_data[0] = tsdb_read_table(relation, (Tsdb_node *)NULL);
    tsdb_data[1] = (Tsdb_selection *)NULL;
    return(tsdb_data[0]);
  } /* if */
  else {
    for(i = 0;
        tsdb_data[i] != NULL &&
        strcmp(tsdb_data[i]->relations[0]->name, relation->name);
        i++);
    if(tsdb_data[i] == NULL) {
      tsdb_data =
        (Tsdb_selection **)realloc(tsdb_data,
                                   (i + 2) * sizeof(Tsdb_selection *));
      tsdb_data[i] = tsdb_read_table(relation, (Tsdb_node *)NULL);
      tsdb_data[i + 1] = (Tsdb_selection *)NULL;
    } /* if */
    return(tsdb_data[i]);
  } /* else */

} /* tsdb_find_table() */
/*---------------------------------------------------------------------------*/



Tsdb_selection* tsdb_find_tables(Tsdb_relation** relations)
{
  int i,j,k;
  Tsdb_relation **real_relations;
  Tsdb_selection *select,*tmp;
  BOOL kaerb;

  real_relations = (Tsdb_relation**) 
    malloc(sizeof(Tsdb_relation*)*(tsdb_n_relations()+1));
  
  for ( i=0;relations[i];i++)
    real_relations[i] = tsdb_find_relation(relations[i]->name);

  real_relations[i] = (Tsdb_relation*)NULL;
  
  select = tsdb_find_table(real_relations[0]);
  
  j=i-1;
  while (j) {
    for (kaerb=FALSE,k=1;
         !kaerb && k<i;
         k++)
      if (real_relations[k]) {
        if (tsdb_joins_to(real_relations[k],select)) {
          tmp = tsdb_find_table(real_relations[k]);
          select = tsdb_simple_join(select,tmp);
          real_relations[k]= (Tsdb_relation*)NULL;
          j--;
          kaerb=TRUE;
        } /* if */
      } /* if */
  } /* while */

  return(select);

} /* tsdb_find_tables() */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
int tsdb_relation_in_selection(Tsdb_selection* selection,char* name)
{
  /* returns -1 if name isn't a relation in selection */
  int i;

  for (i=0;i<selection->n_relations;i++) {
    if (!strcmp(selection->relations[i]->name,name))
      return(i);
  } /* for */

  return(-1);
} /* tsdb_relation_in_selection */

/*---------------------------------------------------------------------------*/

BOOL tsdb_attribute_in_selection(Tsdb_selection *selection, char *name)
{
  Tsdb_relation *relation;
  int i,j;

  for (i=0;relation=selection->relations[i];i++) {
    for (j=0;j<relation->n_fields;j++) {
      if (!strcmp(relation->fields[j],name))
        return(TRUE);
    } /* for */
  } /* for */

  return(FALSE);
}
/*---------------------------------------------------------------------------*/


char** tsdb_key_names(Tsdb_selection* selection)
{
  char** names;
  Tsdb_relation* foo;
  int k,j,i;

  names = (char**)malloc(sizeof(char*)*(selection->n_key_lists+1));
  names[selection->n_key_lists]=NULL;

  for(k=0,j=0,i = 0;i<selection->n_key_lists ;)
    {
      foo = selection->relations[k++];
      /* First we do name->list ordering */
      for (j=0;i<selection->n_key_lists && j<foo->n_keys;j++,i++)
          names[i]=foo->fields[foo->keys[j]];
    } /* for */
  
  return(names);
} /* tsdb_key_names() */

/*---------------------------------------------------------------------------*/

int* tsdb_relation_match(Tsdb_selection *selection_1,
                         Tsdb_selection *selection_2) {
  int *order,i,j ;
  BOOL kaerb=FALSE;
  char * name;

  order = (int*)malloc(sizeof(int)*(selection_1->n_relations));
  
  for (i=0; i<selection_2->n_relations; i++) {
    kaerb=FALSE;
    name = selection_2->relations[i]->name;
    for (j=0; !kaerb && j<selection_1->n_relations; j++) {
      if (!strcmp(name, selection_1->relations[j]->name)) {
        order[i] = j;
        kaerb = TRUE;
      } /* if */
    } /* for */
  } /* for */

  return order;
  /* the i-th relation of selection_2 is the order[i]-th relation of
     selection_1 */
}/* tsdb_relation_match() */


/*---------------------------------------------------------------------------*/


BYTE tsdb_tuple_compare(Tsdb_tuple *t1, Tsdb_tuple *t2) {
  
  int i; 
  BYTE result;
  
  for(i = 0; i < t1->n_fields; i++) {
    if((result = tsdb_value_compare(t1->fields[i], 
                                    t2->fields[i])) != TSDB_EQUAL)
      return(result);
  } /* for */
  return(TSDB_EQUAL);

} /* tsdb_tuple_compare */
/*---------------------------------------------------------------------------*/

BOOL tsdb_children_leaf(Tsdb_node* node)
{
 
 if ((node->left)&&(node->right)
     &&(!node->left->left)&&(!node->left->right)
     &&(!node->right->left)&&(!node->right->right))
   return(TRUE);
 else
   return(FALSE);
     
}


/* ----------------------- create different Tsdb_value`s -----------------*/
Tsdb_value *tsdb_integer(int foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_INTEGER;
  bar->value.integer = foo;

  return(bar);

} /* tsdb_integer() */

Tsdb_value *tsdb_identifier(char *foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_IDENTIFIER;
  bar->value.identifier = foo;

  return(bar);

} /* tsdb_identifier() */
Tsdb_value *tsdb_string(char *foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_STRING;
  bar->value.string = foo;

  return(bar);

} /* tsdb_string() */

Tsdb_value *tsdb_connective(BYTE foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_CONNECTIVE;
  bar->value.connective = foo;

  return(bar);

} /* tsdb_connective() */


Tsdb_value *tsdb_operator(BYTE foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_OPERATOR;
  bar->value.operator = foo;

  return(bar);

} /* tsdb_operator() */

/* ------------------------- free Tsdb_value  ---------------------*/
void tsdb_free_tsdb_value(Tsdb_value* foo)
{
  switch (foo->type) {
  case TSDB_STRING: 
    if (foo->value.string) free(foo->value.string);
    break;
  case TSDB_IDENTIFIER: 
    if (foo->value.identifier) free(foo->value.identifier);
    break;
  };
  free(foo);
}

/*-------------- Tsdb_value ------------------*/

void tsdb_free_tsdb_values(Tsdb_value** bar)
{
  int i;

  for (i=0; (bar[i]!=NULL) ; i++)
    tsdb_free_tsdb_value(bar[i]);
  free(bar);

} /* tsdb_free_tsdb_values() */


/* create value array with one entry */

Tsdb_value **tsdb_singleton_value_array(Tsdb_value *value) {

  Tsdb_value **foo, **bar;

  foo = bar = (Tsdb_value **)malloc(2 * sizeof(Tsdb_value *));
  *foo = value;
  *++foo = (Tsdb_value *)NULL;

  return(bar);

} /* tsdb_singleton_value_array() */

Tsdb_relation *tsdb_create_relation() {

  Tsdb_relation *foo;

  foo = (Tsdb_relation *)malloc(sizeof(Tsdb_relation));
  foo->name = (char *)NULL;
  foo->n_fields = 0;
  foo->n_keys = 0;
  foo->fields = (char **)NULL;
  foo->types = (BYTE *)NULL;
  foo->keys = (int *)NULL;
  foo->total = (BYTE *)NULL;
  foo->status = TSDB_UNCHANGED;

  return(foo);

} /* tsdb_create_relation() */

void tsdb_free_relation(Tsdb_relation *relation) {

  int i;
  char **foo;

  if(relation->name != NULL) {
    free(relation->name);
  } /* if */
  if(relation->fields != NULL) {
    for(i = 0; i < relation->n_fields && relation->fields[0] != NULL; i++) {
      free(relation->fields[i]);
    } /* for */
    free(relation->fields);
  } /* if */
  if(relation->types != NULL) {
    free(relation->types);
  } /* if */
  if(relation->keys != NULL) {
    free(relation->keys);
  } /* if */
  if(relation->total != NULL) {
    free(relation->total);
  } /* if */
  free(relation);
} /* tsdb_free_relation() */

Tsdb_selection* tsdb_create_selection(int r, int k) {
  Tsdb_selection *foo;

  foo = (Tsdb_selection*)malloc(sizeof(Tsdb_selection));
  foo->n_relations = r;
  foo->n_key_lists = k;
  foo->relations = (Tsdb_relation **)malloc(sizeof(Tsdb_relation *) * (r + 1));
  foo->relations[r + 1]=(Tsdb_relation *)NULL;
  memset(foo->relations, '\0', sizeof(Tsdb_relation *) * (r + 1));
  foo->key_lists = (Tsdb_key_list **)malloc(sizeof(Tsdb_key_list *) * (k + 1));
  memset(foo->key_lists, '\0', sizeof(Tsdb_key_list *) * (k + 1));
  foo->key_lists[k + 1] = (Tsdb_key_list *)NULL;
  return(foo);
} /*   tsdb_create_selection() */

void tsdb_free_selection(Tsdb_selection* foo) {
  int i;
  free(foo->relations);
  for (i=0;i<foo->n_key_lists;i++)
    tsdb_free_key_list_chain(foo->key_lists[i],FALSE);
  free(foo->key_lists);
  free(foo);
} /* tsdb_free_selection() */

void tsdb_free_selections(Tsdb_selection** bar) {
  int i;
  for (i=0;bar[i];i++) {
    tsdb_free_selection(bar[i]);
  }
  free(bar);
} /* tsdb_free_selections() */


Tsdb_node *tsdb_leaf(Tsdb_value *value) {

  Tsdb_node *foo;

  foo = (Tsdb_node *)malloc(sizeof(Tsdb_node));
  foo->left = (Tsdb_node *)NULL;
  foo->node = value;
  foo->right = (Tsdb_node *)NULL;

  return(foo);

} /* tsdb_leaf() */



void tsdb_free_leaf(Tsdb_value *value) {
  if (value)
    free(value);
}


/*---------------------------------------------------------------------------*/

void tsdb_free_key_list_chain(Tsdb_key_list *foo,BOOL tuples) {
  Tsdb_key_list* n;

  while(foo) {
      n=foo->next;
      if (tuples) if (foo->tuples) free(foo->tuples);
      free(foo);
      foo=n;
    } /* while */

} /* tsdb_free_key_list_chain() */



/*---------------------------------------------------------------------------*/


void ignore() {
  int a;
  a=1;
}

BOOL tsdb_initialize() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_initialize()
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
|* last update: 30-jun-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
|*****************************************************************************|
|* <open questions>
|* Why install the ignore handler for SIGPIPE; this seems to suggest misuse of
|* popen(3) to me (30-jun-95 -- oe).
\*****************************************************************************/

  char *name;
  FILE* f;
#ifdef DEBUG
  int i;
#endif

#ifdef DBMALLOC
  union dbmalloptarg m;
#endif

#ifdef SYSV
  struct sigaction tsdb_sig = { SIG_IGN, (sigset_t)0, 0 };
  sigaction(SIGPIPE, &tsdb_sig);
#endif

#ifndef SYSV
  signal(SIGPIPE, SIG_IGN);
#endif

#ifdef DBMALLOC
  m.str = "malloc_tsdb";
  mallopt(MALLOC_ERRFILE,m);
#endif

  if((tsdb_debug_stream = tsdb_open_debug()) == NULL) {
    tsdb_debug_stream = TSDB_ERROR_STREAM;
  } /* if */
  if((name = getenv("TSDB_HOME")) != NULL || 
     (name = tsdb_pseudo_user()) != NULL) {
    
    tsdb_home = strdup(name);
    if(tsdb_home[strlen(tsdb_home) - 1] != '/') {
      tsdb_home = (char *)realloc(tsdb_home, strlen(tsdb_home) + 2);
      tsdb_home = strcat(tsdb_home, "/");
    } /* if */
    tsdb_relations_file =
      (char *)malloc(strlen(tsdb_home) + 
                     strlen(TSDB_RELATIONS_FILE) + 1);
    tsdb_relations_file = strcpy(tsdb_relations_file, tsdb_home);
    tsdb_relations_file = strcat(tsdb_relations_file, TSDB_RELATIONS_FILE);
    
    tsdb_data_path =
      (char *)malloc(strlen(tsdb_home) + 
                     strlen(TSDB_DATA_PATH) + 1);
    tsdb_data_path = strcpy(tsdb_data_path, tsdb_home);
    tsdb_data_path = strcat(tsdb_data_path, TSDB_DATA_PATH);
  } /* if */
  else {
    tsdb_home = strdup(TSDB_HOME);
  } /* else */
  
  if((name = getenv("TSDB_RELATIONS_FILE")) != NULL) {
    if(name[0] == '/') { /* absolute path */
      tsdb_relations_file = strdup(name);
      if(tsdb_relations_file[strlen(tsdb_relations_file) - 1] != '/') {
        tsdb_relations_file = 
          (char *)realloc(tsdb_relations_file, 
                          strlen(tsdb_relations_file) + 2);
        tsdb_relations_file = strcat(tsdb_relations_file, "/");
      } /* if */
    } /* if */
    else { /* relative path */
      tsdb_relations_file = strdup(name);
      tsdb_relations_file = strdup(tsdb_home);
      tsdb_relations_file = (char *)realloc(tsdb_relations_file, 
                                            strlen(tsdb_relations_file) + 
                                            strlen(name) + 1);
      tsdb_relations_file = strcat(tsdb_relations_file, name);
    } /* else */
  } /* if */
  else { /* use default relations_file */
    tsdb_relations_file = strdup(tsdb_home);
    tsdb_relations_file = (char *)realloc(tsdb_relations_file, 
                                          strlen(tsdb_relations_file) + 
                                          strlen(TSDB_RELATIONS_FILE) + 1);
    tsdb_relations_file = strcat(tsdb_relations_file, TSDB_RELATIONS_FILE);
  } /* else */
  if((name = getenv("TSDB_DATA_PATH")) != NULL) {
    if(name[0] == '/') { /* absolute path */
      tsdb_data_path = strdup(name);
      if(tsdb_data_path[strlen(tsdb_data_path) - 1] != '/') {
        tsdb_data_path = (char *)realloc(tsdb_data_path, 
                                         strlen(tsdb_data_path) + 2);
        tsdb_data_path = strcat(tsdb_data_path, "/");
      } /* if */
    } /* if */
    else { /* relative path */
      tsdb_data_path = strdup(name);
      tsdb_data_path = strdup(tsdb_home);
      tsdb_data_path = (char *)realloc(tsdb_data_path, 
                                       strlen(tsdb_data_path) + 
                                       strlen(name) + 1);
      tsdb_data_path = strcat(tsdb_data_path, name);
    } /* else */
  } /* if */
  else { /* use default data_path */
    tsdb_data_path = strdup(tsdb_home);
    tsdb_data_path = (char *)realloc(tsdb_data_path, 
                                     strlen(tsdb_data_path) + 
                                     strlen(TSDB_DATA_PATH) + 1);
    tsdb_data_path = strcat(tsdb_data_path, TSDB_DATA_PATH);
  } /* else */

  if (!(name=getenv("TSDB_LAST_RESULT"))) {
    name = tempnam(NULL,"TSDB_");
    tsdb_last_result = strdup(name);
  }
  else {
    tsdb_last_result = strdup(name);
  } /* else */

  name = getenv("PAGER");
  if (name && (f=popen(name,"w"))) {
    tsdb_pager = strdup(name);
    pclose(f);
  }/* if */
  else {
    if (f=popen("more","w")) {
      tsdb_pager = strdup("more");
      pclose(f);
    } /* if */
    else {
      if (f=popen("page","w")) {
        tsdb_pager = strdup("page");
        pclose(f);
      } /* if */
    }/* else */
  }/* else */
  
  (void)tsdb_all_relations();

#ifdef DEBUG
  if(tsdb_relations != NULL) {
    for(i = 0; tsdb_relations[i] != NULL; i++) {
      tsdb_print_relation(tsdb_relations[i], tsdb_debug_stream);
    } /* for */
    fflush(tsdb_debug_stream);
  } /* if */
#endif  
} /* tsdb_initialize */

char *tsdb_pseudo_user() {
  
  struct passwd *clare;
  char *fs, *name = strdup(TSDB_PSEUDO_USER);

  for(fs = strchr(name, TSDB_FS); 
      fs != NULL; 
      name = ++fs, fs = strchr(name, TSDB_FS)) {
    *fs = 0;
    if((clare = getpwnam(name)) != NULL) {
      printf("returning %s.\n", clare->pw_dir);
      return(clare->pw_dir);
    } /* if */
  } /* while */
  return((char *)NULL);
  
} /* tsdb_pseudo_user() */

float tsdb_timer(BYTE action) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_timer()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  static struct timeb start, stop;
  static BOOL running = FALSE;

  if(action == TSDB_START_TIMER) {
    if(ftime(&start) == -1) {
      perror("tsdb_timer()");
      return((float)-1);
    } /* if */
    running = TRUE;
    return((float)0);
  } /* if */
  else if(action == TSDB_STOP_TIMER) {
    if(!running) {
      fprintf(TSDB_ERROR_STREAM, "timer(): timer has to be started first.\n");
      return(-1);
    } /* if */
    if(ftime(&stop) == -1) {
      perror("tsdb_timer()");
      return((float)-1);
    } /* if */
    running = FALSE;
    return((stop.time - start.time) + ((stop.millitm - start.millitm) * 0.001));
  } /* if */
  else {
    fprintf(TSDB_ERROR_STREAM,
            "timer(): invalid action `%d'.\n", action);
    return((float)-1);
  } /* else */

} /* tsdb_timer() */

BOOL tsdb_insert_into_selection(Tsdb_selection *selection,
                                Tsdb_tuple **tuples) {

/* inserts tuple tuples in selection.
 * tuples isn't copied, so don't free it afterwards!
*/

  Tsdb_value *value, *comparison;
  Tsdb_key_list *new, *next;
  int i, j, offset, n_tuples;
  BOOL kaerb;

  for(n_tuples = 0; tuples != NULL && tuples[n_tuples] != NULL; n_tuples++);

  for(i = 0, offset = 0; i < selection->n_relations; i++, offset += j) {
    for (j = 0; j < selection->relations[i]->n_keys; j++) {
      kaerb = FALSE;
      value = tuples[i]->fields[selection->relations[i]->keys[j]];
      if(selection->key_lists[offset + j] != NULL) {
        comparison = selection->key_lists[offset + j]->key;
        switch(tsdb_value_compare(value, comparison)) {
          case TSDB_GREATER_THAN:
            next = selection->key_lists[offset + j];
            while(!kaerb && next->next != NULL) {
              comparison = next->next->key;
              switch(tsdb_value_compare(value, comparison)) {
              case TSDB_GREATER_THAN:
                next = next->next;
                break;
              case TSDB_EQUAL:
              case TSDB_LESS_THAN:
                kaerb = TRUE;
                break;
              default:
                fprintf(stderr, "tsdb: ignoring invalid data tuple.\n");
                return(FALSE);
              } /* switch */
            } /* while */

            new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
            new->key = value;
            new->tuples = tuples;
            new->n_tuples = n_tuples;
            new->next = next->next;
            next->next = new;
            kaerb = TRUE;
            break;
          case TSDB_EQUAL:
          case TSDB_LESS_THAN:
            break;
          default:
            fprintf(stderr, "tsdb: ignoring invalid data tuple.\n");
            return(FALSE);
          } /* switch */
      } /* if */
      if(!kaerb) {
        new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
        new->key = value;
        new->tuples = tuples;
        new->n_tuples = n_tuples;
        new->next = selection->key_lists[offset + j];
        selection->key_lists[offset + j] = new;
      } /* if */
    } /* for */
  } /* for */
  return(TRUE);

} /* tsdb_insert_into_selection() */



/*---------------------------------------------------------------------------*/

void tsdb_negate_node(Tsdb_node* node)
{
  if (node->node->type != TSDB_OPERATOR) {
    fprintf(stderr,"tsdb_negate_node: wrong type at leaf\n");
    return ;
  }
  switch (node->node->value.operator) {
  case TSDB_EQUAL:
    node->node->value.operator = TSDB_NOT_EQUAL;
    break;
  case TSDB_NOT_EQUAL:
    node->node->value.operator = TSDB_EQUAL; 
    break;
  case TSDB_LESS_THAN:
    node->node->value.operator = TSDB_GREATER_OR_EQUAL_THAN;
    break;
  case TSDB_LESS_OR_EQUAL_THAN: 
   node->node->value.operator = TSDB_GREATER_THAN;
   break;
  case TSDB_GREATER_THAN: 
    node->node->value.operator = TSDB_LESS_OR_EQUAL_THAN;
    break;
  case TSDB_GREATER_OR_EQUAL_THAN: 
    node->node->value.operator = TSDB_LESS_THAN;
    break;
  case TSDB_SUBSTRING:    
    node->node->value.operator = TSDB_NOT_SUBSTRING; 
    break;
  case TSDB_NOT_SUBSTRING: 
    node->node->value.operator = TSDB_SUBSTRING ; 
    break;
  default:
    fprintf(stderr," eh what??\n");
  }/* switch */

} /* tsdb_negate_node */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/

void tsdb_tree_negate(Tsdb_node* node)
{
  if (tsdb_children_leaf(node))
    tsdb_negate_node(node);
  else 
    if (node->node->value.connective == TSDB_BRACE) /* Klammer */
      tsdb_tree_negate(node->left);
    else
      if (node->node->value.connective == TSDB_NOT) {
        node->node->value.connective = TSDB_NOT_NOT ;
        tsdb_check_not(node->right);
        return;
      } /* if */
      else {
        if (node->node->value.connective == TSDB_AND)
          node->node->value.connective = TSDB_OR;
        else
          node->node->value.connective = TSDB_AND;
        tsdb_tree_negate(node->left);
        tsdb_tree_negate(node->right);
      } /* else */
 
} /* tsdb_tree_negate() */

/*---------------------------------------------------------------------------*/

/* call root of syntax-tree */
void tsdb_check_not(Tsdb_node* node)
/* this function doesn't do nothing */
{
  if (!node || (tsdb_children_leaf(node)))
    return; 
  

  if (node->node->value.connective == TSDB_NOT) {
    node->node->value.connective = TSDB_NOT_NOT;
    tsdb_tree_negate(node->right);
  }
  else {
    tsdb_check_not(node->left);
    tsdb_check_not(node->right);
  }


} /* tsdb_check_not() */

/*---------------------------------------------------------------------------*/

char** tsdb_condition_attributes(Tsdb_node *node,
                                 char **attributes,
                                 int* s_attributes)
{
  int i ;
  BOOL kaerb;

  if (!node) return attributes;
  
  if (node->node->type == TSDB_CONNECTIVE) {
    attributes = tsdb_condition_attributes(node->left,attributes,
                                             s_attributes);
    attributes = tsdb_condition_attributes(node->right,attributes,
                                             s_attributes);
  } /* if connective */
  else { /* leaf */
    if (node->node->type == TSDB_OPERATOR)
      attributes = tsdb_condition_attributes(node->left,attributes,
                                             s_attributes);
    else {
      
      for(i=0, kaerb=FALSE; !kaerb && attributes[i];i++)
        if (!strcmp(attributes[i],node->node->value.string))
          kaerb = TRUE;
      
      if (!kaerb) {
        if (!(i<*s_attributes)) {
          *s_attributes *= 2;
          attributes = (char**)realloc(attributes,*s_attributes*sizeof(char*));
        }
        attributes[i++] = strdup(node->node->value.string);
        attributes[i] = NULL;
      } /* if */
    } /* else */
  } /* else */
  return attributes;
} /* tsdb_condition_attributes() */
