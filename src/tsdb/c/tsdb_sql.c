/*****************************************************************************\
|*        file: tsdb_sql.c
|*      module: TSDB query language functionality
|*     version: 
|*  written by: andrew white, tom fettig & oe (dfki saarbruecken)
|* last update: 14-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <malloc.h>
#include <limits.h>
#include "globals.h"
#include "tsdb.h"
#include "errors.h"

int tsdb_drop_table(Tsdb_value *table) {
  
  FILE *infp, *outfp;
  char *path; 
  Tsdb_relation *relation;

  if((infp = tsdb_find_relations_file("r")) != NULL) {

    if((outfp = fopen(TSDB_TEMPORARY_FILE,"w")) == NULL) {
      fprintf(tsdb_error_stream,"Cannot create: %s\n", TSDB_TEMPORARY_FILE);
      return(TSDB_TMPFILE);
    } /* if */
    
    while((relation = tsdb_read_relation(infp)) != NULL) {
      if(strcmp(table->value.identifier, relation->name)) {      
        tsdb_print_relation(relation, outfp);
      } /* if */
    } /* while */
    
    fclose(infp);
    fclose(outfp);

    if(rename(TSDB_TEMPORARY_FILE, tsdb_relations_file)) {
      perror("tsdb: rename(): ");
    } /* if */

    path = strdup(tsdb_data_path);
    path = (char *)realloc(path,
                           strlen(tsdb_data_path) +
                           strlen(table->value.identifier) + 1);
    path = strcat(path, table->value.identifier);
    /*unlink(path);*/
    free(path);
    tsdb_remove_relation(table->value.identifier);
  } /* if */
  
} /* tsdb_drop_table() */

int tsdb_create_table(Tsdb_value *table, Tsdb_field **fields) {
  
  FILE *fp; 
  int n;
  char *path;
  Tsdb_relation *relation;
  
  if((fp = tsdb_find_relations_file("a+")) != NULL) {
    fprintf(fp, "%s:\n", table->value.identifier);
    for(n = 0; fields[n] != NULL; n++) {
      fprintf(fp, "\t%s %s", fields[n]->name,
              (fields[n]->type == TSDB_INTEGER ? ":integer" : ":string"));
      if(fields[n]->key) {
        fprintf(fp, " :key\n");
      } /* if */
      else {
        fprintf(fp, "\n");
      } /* else */
    } /* for */
    fprintf(fp, "\n");
    fclose(fp);
  
    path = strdup(tsdb_data_path);
    path = (char *)realloc(path,strlen(tsdb_data_path) + 
                           strlen(table->value.identifier) + 1);
    path = strcat(path, table->value.identifier);
    creat(path, 0644);
    free(path);
    relation = tsdb_field_2_relation(table->value.identifier, fields);
    tsdb_add_relation(relation);
  } /* if */

} /* tsdb_create_table() */

int tsdb_alter_table(Tsdb_value *table, Tsdb_field **fields) {
  
  FILE *input, *output;
  Tsdb_relation *relation;
  int i;

  if((input = tsdb_find_relations_file("r")) != NULL) {
  
    if((output = fopen(TSDB_TEMPORARY_FILE, "w")) == NULL) {
      fprintf(tsdb_error_stream,"Cannot create: %s\n", TSDB_TEMPORARY_FILE);
      return(TSDB_TMPFILE);
    } /* if */

    while((relation = tsdb_read_relation(input)) != NULL &&
          strcmp(table->value.identifier, relation->name)) {
      tsdb_print_relation(relation, output);
    } /* while */

    if(relation != NULL) {
      for(i = 0; fields[i] != NULL; i++) {
        relation->fields =
          (char **)realloc(relation->fields,
                           (relation->n_fields + 1) * sizeof(char *));
        relation->types =
          (BYTE *)realloc(relation->types,
                           (relation->n_fields + 1) * sizeof(BYTE));
        relation->fields[relation->n_fields] = strdup(fields[i]->name);
        relation->types[relation->n_fields] = fields[i]->type;
        if(fields[i]->key) {
          if(relation->n_keys == 0) {
            relation->keys = (int *)malloc(sizeof(int));
          } /* if */
          else {
            relation->keys =
              (int *)realloc(relation->keys,
                             (relation->n_keys + 1) * sizeof(int));
          } /* else */
          relation->keys[relation->n_keys] = relation->n_fields;
          relation->n_keys++;
        } /* if */
        relation->n_fields++;
        /* tsdb_free_field(fields[i]); */
      } /* for */
      free(fields);
      tsdb_print_relation(relation, output);

      while((i = fgetc(input)) != EOF) {
        fputc(i, output);
      } /* while */
      for(i = 0; strcmp(tsdb_relations[i]->name, relation->name); i++);
      free(tsdb_relations[i]);
      tsdb_relations[i] = relation;
    } /* if */
    
    fclose(input);
    fclose(output);

    if(rename(TSDB_TEMPORARY_FILE, tsdb_relations_file)) {
      perror("tsdb: rename(): ");
    } /* if */
  } /* if */

} /* tsdb_alter_table() */

int tsdb_insert(Tsdb_value *table, 
                Tsdb_value **attribute_list, 
                Tsdb_value **value_list) {
  
  FILE *data;
  Tsdb_relation *relation;
  Tsdb_selection *selection;
  Tsdb_tuple *tuple = (Tsdb_tuple *)malloc(sizeof(Tsdb_tuple)), **foo;
  int m, n, n_values, n_attributes;
  BOOL success;
  
  if((relation = tsdb_find_relation(table->value.identifier)) != NULL) {

    tuple->n_fields = relation->n_fields;
    tuple->fields = (Tsdb_value **)malloc(sizeof(Tsdb_value *));

    for(n_values = 0; value_list[n_values] != NULL; n_values++);

    if(attribute_list != NULL) {
      if(!tsdb_are_attributes(attribute_list, relation))
          return(TSDB_NO_SUCH_ATTRIBUTE);
      for(n_attributes = 0; attribute_list[n_attributes] != NULL;
          n_attributes++);
      if(n_attributes != n_values) {
        fprintf(tsdb_error_stream, "tsdb: incompatible list sizes.\n");
        return(TSDB_INCOMPAT_SIZES);
      } /* if */
    } /* if */
    
    if((data = tsdb_find_data_file(table->value.identifier, "a+")) != NULL) {
      (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                       (Tsdb_tuple **)NULL);
      for(n = 0; n < relation->n_fields; n++) {

        if(attribute_list != NULL) { /* attribute list was specified */ 
          success = FALSE;
          for(m = 0; m < n_values; m++) {
            if(!strcmp(relation->fields[n],
                       attribute_list[m]->value.identifier)) {
              tuple->fields[n] = value_list[m];
              success = TRUE;
              break;
            } /* if */
          } /* for */
          if(!success) {
            tuple->fields[n] = (Tsdb_value *)malloc(sizeof(Tsdb_value *));
            tuple->fields[n]->value.string = strdup(DEFAULT_VALUE);
            tuple->fields[n]->type = TSDB_STRING;
          } /* if */
        } /* if */

        else { /* attribute list wasn't specified */
          if(n < n_values) {
            tuple->fields[n] = value_list[n];
          } /* if */
          else {
            tuple->fields[n] = (Tsdb_value *)malloc(sizeof(Tsdb_value *));
            tuple->fields[n]->value.string = strdup(DEFAULT_VALUE);
            tuple->fields[n]->type = TSDB_STRING;
          } /* else */
        } /* else */
        tuple->fields = (Tsdb_value **)realloc(tuple->fields,
                                               (n + 2) * sizeof(Tsdb_value *));
      } /* for */
      tsdb_print_tuple(tuple, data);
      fprintf(data, "\n");
      fclose(data);
      if((selection = tsdb_find_table(relation)) != NULL) {
        foo = (Tsdb_tuple **)malloc(2 * sizeof(Tsdb_tuple *));
        foo[0] = tuple;
        foo[1] = (Tsdb_tuple *)NULL;
        tsdb_insert_into_selection(selection, &foo[0]);
      } /* if */
      else {
        fprintf(tsdb_error_stream,
                "segementation fault: core well and truly dumped.\n");
      } /* else */
    } /* if */
    else {
      return(TSDB_NO_DATA_FILE);
    } /* else */
  } /* if */
  else {
    fprintf(tsdb_error_stream,
            "insert(): unknown relation `%s'.\n", table->value.identifier);
    return(TSDB_NO_SUCH_RELATION);
  } /* else */

} /* tsdb_insert() */

int tsdb_clean_relation(Tsdb_relation* relation, Tsdb_value* fuck)
{
  Tsdb_selection* table;
  Tsdb_key_list* list,*first,*next,*last;
  int  j,i;
  
  table = tsdb_find_table(relation);
    
  for (last=NULL,first = list = table->key_lists[0]; list != NULL ;) {
    if (list->tuples[0]->fields[0]==fuck) {
      /* to be freed */
      next = list->next;
      if (first==list)
        first = next;
      /* here things get freed! */
      for (i=1;i<list->tuples[0]->n_fields;i++)
        tsdb_free_tsdb_value(list->tuples[0]->fields[i]);
      free(list->tuples[0]);
      free(list->tuples);
      free(list);

      /* now do the unlink */
      if (last)
        last->next = next;
      list = next;
      
      table->length--;
    } /* if */
    else {
      last = list;
      list = list->next;
    } /* else */
  } /* for */

  table->key_lists[0] = first;
  return(table->length);

} /* tsdb_clean_relation */

int tsdb_delete(Tsdb_value *table, Tsdb_node *condition) {
  
  Tsdb_selection *selection;
  Tsdb_relation *relation;
  Tsdb_relation* wanted[2]={NULL,NULL};
  Tsdb_value boo,*fuck=&boo;
  Tsdb_key_list* list;
  int anna;

/* there's only one value, this is the relation. All Attributes in node
   have to belong to it.
*/
  if((relation = tsdb_find_relation(table->value.identifier)) != NULL) {
    wanted[0] = relation;
    selection = tsdb_complex_select(condition,wanted);
    if ((selection->n_relations!=1) ||
        (strcmp(relation->name,selection->relations[0]->name))){
      fprintf(tsdb_error_stream,"Wrong Specification of attributes!\n");
      return(TSDB_NO_SUCH_RELATION);
    }
    /* now perform the hard deletion of tuples */
    /* mark them with fucked an delete in one pass */
    /* the keylist af a basic relation consist of node with tuple-arrays 
       size one. This one tuple consists of fields, which are 
       value-Pointers. So delete the first value, put something known 
       in it, then clean the original list. then you may delete the selection
       */
#if defined(DEBUG) && defined(TOM)
    fprintf(tsdb_debug_stream," Tuples to be deleted:\n");
    tsdb_print_selection(selection, tsdb_debug_stream);
#endif
    list = selection->key_lists[0];
    for (anna=0;anna<selection->length; anna++) {
      list->tuples[0]->fields[0]=fuck;
    } /* for */
    tsdb_clean_relation(relation,fuck);
    relation->status = TSDB_CHANGED;
    return(1);
  } /* if */
  else {
    return(TSDB_NO_SUCH_RELATION);
  } /* else */

} /* tsdb_delete() */

int tsdb_update(Tsdb_value *table, Tsdb_node *condition) {

  /* space to be filled soon(ish) */

} /* tsdb_update */

Tsdb_selection* tsdb_complex_select(Tsdb_node *node,Tsdb_relation ** wanted)
{
  Tsdb_selection *left,*right,*result;
  Tsdb_relation *node_relation,**all_relations;
  int i,j;
  BOOL kaerb;

  if (node->node->type==TSDB_CONNECTIVE) {
    switch (node->node->value.connective) {
    case TSDB_OR:
      left = tsdb_complex_select(node->left,wanted);
      right = tsdb_complex_select(node->right,wanted);
      result = tsdb_complex_merge(left,right);
      tsdb_free_selection(left);
      tsdb_free_selection(right);
      break;
    case TSDB_AND:
      left = tsdb_complex_select(node->left,wanted);
      right = tsdb_complex_select(node->right,wanted);
      result = tsdb_join(left,right);
      tsdb_free_selection(left);
      tsdb_free_selection(right);
      break;
    case TSDB_NOT:
    case TSDB_NOT_NOT:
      result = tsdb_complex_select(node->right,wanted);
      break;
    default:
      result = tsdb_complex_select(node->left,wanted);
      break;
    } /* switch */
  } /* if */
  else {
    if (node->node->type != TSDB_OPERATOR) {
      printf("error in condition\n");
      return(NULL);
    }
    all_relations = tsdb_attribute_relations(node->left->node);
    if (all_relations) {
      Tsdb_node* a[2];
      a[1]=NULL;
      a[0]=node;
      kaerb = FALSE;
      if (wanted) {
        for (i=0;!kaerb && wanted[i];i++)
          for (j=0;!kaerb && all_relations[j];j++) {
            if (!strcmp(wanted[i]->name,all_relations[j]->name)) {
              kaerb = TRUE;
            } /* if */
          } /* for */
      } /* if */
        
      if (kaerb)
        node_relation = wanted[i];
      else
        node_relation = all_relations[0];
      result = tsdb_find_table(node_relation);
      result = tsdb_select(result,&a[0],TSDB_AND);
      /* No free necessary: find_table returns global Data */
    } /* if */
  } /* else */
  return result;
} /* tsdb_complex_select() */

Tsdb_selection* tsdb_join_one_relation(Tsdb_selection* selection,
                                       Tsdb_relation** relations)
{
  /* Try to join on of the relations to selection */
  /* Does it have to be joinable???? */
  int i,smallest=-1,smallest_joinable=-1;
  int s=INT_MAX,sj=INT_MAX,j=INT_MAX;
  Tsdb_selection *table;

  for (i=0;relations[i];i++) {
    table = tsdb_find_table(relations[i]);
    if (tsdb_joins_to(relations[i],selection))
      if (table->length < sj) 
        smallest_joinable=i;
    else
      if (table->length < s)
        smallest = i;
  }
  
  if (smallest_joinable!=-1)
    selection = tsdb_join(selection,
                          tsdb_find_table(relations[smallest_joinable]));
  else
    if (smallest != -1 )
      selection = tsdb_join(selection,tsdb_find_table(relations[smallest]));
    else
      selection = tsdb_join(selection,tsdb_find_table(relations[0]));
  return(selection);
} /* tsdb_join_one_relation */

int tsdb_complex_retrieve(Tsdb_value **relation_list,
                          Tsdb_value **attribute_list,
                          Tsdb_node* conditions ) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_complex_retrieve()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tom said: ``final retrieve() function'' --- i suppose he is mistaken |:-}.
\*****************************************************************************/

  FILE *output;
  char **attributes = NULL;
  Tsdb_relation **a_relations, **relations, *rel;
  int s_attributes = 10, i, j, k,r,kaerb=0;
  Tsdb_selection *selection=NULL;
  BOOL from_find=FALSE;

  /* 
     1. find out about attributes in conditions, transform expression
     2. build a set of used relations
     3. give a relation to every attribute
     4. enter complex_select with empty selections
     5. print the resulting selection
     
     */

  if (conditions) {
#if defined(DEBUG) && defined(TOM)
    tsdb_tree_print(conditions, tsdb_debug_stream);
    tsdb_check_not(conditions);
    tsdb_tree_print(conditions, tsdb_debug_stream);
#endif
    attributes = (char **)malloc(s_attributes * sizeof(char *));
    if (!attributes) {
      return(-1);
    }
    attributes[0] = NULL;
    attributes
      = tsdb_condition_attributes(conditions, attributes, &s_attributes);
  } /* if conditions */
  
  if (relation_list)  /* check relations */ {
    kaerb = 0;
    for(r=0;relation_list[r];r++) {
      if (!tsdb_is_relation(relation_list[r])) {
        fprintf(tsdb_error_stream,"Unkown Relation %s\n",
                 relation_list[r]->value.string);
        kaerb = 1;
      } /* if */
    } /* for */
    if (kaerb) {
      if (attributes)
        free(attributes);
      return -1;
    } /* if */
  } /* if */
  else
    r = tsdb_n_relations();
 
 if (attribute_list) /* check attributes */{
   kaerb = 0;
   for(i = 0; attribute_list[i]; i++) {
     if (!tsdb_is_attribute(attribute_list[i])) {
       fprintf(tsdb_error_stream,"Unknown Attribute %s\n",
               attribute_list[i]->value.string);
       kaerb = 1;
     } /* if */
   } /* for */
   if (kaerb) {
     if (attributes)
       free(attributes);
     return -1;
   }
 } /* if */

  if (conditions)
    selection = tsdb_complex_select(conditions, NULL);
  /* now join with relations that aren't in */

#if defined(DEBUG) && defined(TOM)
  fprintf(tsdb_debug_stream,"printing selection\n");
  tsdb_print_selection(selection,tsdb_debug_stream);
#endif
  
  for(i = 0; relation_list && relation_list[i]; i++) ;
  for(j = 0; attribute_list && attribute_list[j]; j++) ;
  
  /* check from relations */
  for (i=0;attribute_list && attribute_list[i];i++) {
    if (!tsdb_attribute_in_selection(selection,
                                     attribute_list[i]->value.identifier)){
      a_relations = tsdb_attribute_relations(attribute_list[i]);
      for (j=0;a_relations[j];j++) {
        if (!tsdb_relation_in_selection(selection,a_relations[j]->name)!=-1) {
          if (selection==NULL) {
            from_find = TRUE;
            selection = tsdb_find_table(a_relations[0]);
          }
          else {
            Tsdb_selection *temp;
            temp = selection;
            selection = tsdb_join_one_relation(selection,a_relations);
            if (!from_find) {
              tsdb_free_selection(temp);
            }
            from_find = FALSE;
          }
          a_relations[i+1]=NULL;
        }
      } /* for */
      free(a_relations);
    } /* if */
  }/* for */

  if (attributes)
    free(attributes);

  /* now check the attributes for projections */
  if (attribute_list) {
    if((output = tsdb_open_pager()) != NULL) {
      tsdb_project(selection, attribute_list, output);
      pclose(output);
    } /* if */
    else {
      tsdb_project(selection,attribute_list,tsdb_default_stream);
    } /* else */
    if (!from_find)
      tsdb_free_selection(selection);
    return(selection->length);
  } /* if */
  
  if (!from_find)
    tsdb_free_selection(selection);
  return(0);

} /* tsdb_complex_retrieve */

void tsdb_project(Tsdb_selection *selection,Tsdb_value **attributes,FILE* stream)
{
  /* print attributes in order */
  int i, j, k, n, n_attributes, offset;
  Tsdb_relation *relation;
  Tsdb_key_list *list;
  char **fields;
  int *r, *f;
  BOOL kaerb;

  for (n_attributes = 0; attributes[n_attributes]; n_attributes++);
  r = (int *)malloc(n_attributes * sizeof(int));
  f = (int *)malloc(n_attributes * sizeof(int));
  
  for (k = 0; k < n_attributes; k++) {
    kaerb = FALSE; /* not found */
    r[k] = -1;
#if defined(DEBUG) && defined(TOM)
    fprintf(tsdb_debug_stream,"Looking for Attribute %s \n",attributes[k]->value.string);
#endif    
    for (i=0;!kaerb && i<selection->n_relations;i++) {
      relation = selection->relations[i];
#if defined(DEBUG) && defined(TOM)
      fprintf(tsdb_debug_stream,"in relation: %s\n",selection->relations[i]->name);
#endif
      fields = selection->relations[i]->fields;
      for (j=0; !kaerb && j<relation->n_fields; j++) {
        if (!strcmp(fields[j],attributes[k]->value.string)) {
          r[k] = i;
          f[k] = j;
          kaerb = TRUE;
        } /* if */
      } /* for */
    } /* for */
    if (!kaerb) {
      /* error! attribute not found */
      fprintf(stream,"can't find attribute %s in specified relations\n",
              attributes[k]->value.string);
      return;
      }      
  } /* for */ 
  /* attribute[k] will be printed from relation r[k] and field f[k] */

#if defined(DEBUG) && defined(TOM)
  for (i=0;i<selection->n_key_lists;i++) {
    fprintf(tsdb_debug_stream," Key list %ld\n",i);
  } /* for */
#endif

  relation = selection->relations[n=0];

/* #if defined(DEBUG) && defined(TOM)
  for (i=0,offset=0;i<selection->n_key_lists;i++,offset++) {
#else */
   for (i=0,offset=0;i<1;i++,offset++) {
    if (offset == relation->n_keys) {
      offset = 0;
      relation = selection->relations[++n];
    } /* if */

#if defined(DEBUG) && defined(TOM)
    if (!selection->key_lists[i]) {
      fprintf(tsdb_debug_stream," Key list %s is empty\n",
              relation->fields[relation->keys[offset]]);
    }
    else
      fprintf(tsdb_debug_stream,"Key list %s\n",relation->fields[relation->keys[offset]]);
#endif

    for (list = selection->key_lists[i]; list!=NULL; list=list->next) {
      for (k=0; k<n_attributes ; k++ ) {
        if (r[k]!=-1) {
        tsdb_print_value(list->tuples[r[k]]->fields[f[k]],stream);
        }
        if (k+1 < n_attributes) fprintf(stream,"%s", TSDB_FIELD_DELIMITER);
      } /* for */
      fprintf(stream,"\n");
    } /* for */
    fflush(stream);
  } /* for */

  free(r);
  free(f);
} /* tsdb_project() */


/*****************************************************************************\
|*        file: 
|*      module: tsdb_select()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* always returns a copy of selection!!
\*****************************************************************************/

Tsdb_selection *tsdb_select(Tsdb_selection *selection,
                            Tsdb_node **conditions,
                            BYTE type) {
  Tsdb_selection *result;
  Tsdb_relation  *relation_1;
  Tsdb_key_list  *list;
  Tsdb_tuple **new_tuples;
  int *relation, *field;
  int n_conditions,i,j,k;
  char *attribut;
  BOOL kaerb,match;
  BYTE *results;
  
  if (!selection->length) return(NULL);
  for(n_conditions = 0; conditions[n_conditions] != NULL; n_conditions++);

  relation = (int *)calloc(n_conditions,sizeof(int));
  field = (int *)calloc(n_conditions,sizeof(int));
  results = (BYTE *)calloc(n_conditions,sizeof(BYTE));
  
  for (i=0; i<n_conditions ; i++) {
    attribut = conditions[i]->left->node->value.identifier;
    relation[i]=-1;
    field[i]=-1;
    for (kaerb=FALSE, j=0; !kaerb && j<selection->n_relations ; j++) {
      relation_1 = selection->relations[j];
      for (k=0 ; !kaerb && k < relation_1->n_fields ; k++) {
        if (!strcmp(attribut,relation_1->fields[k])) {
          relation[i] = j;
          field[i] = k ;
          kaerb=TRUE;
        } /* if */
      } /* for */
    } /* for */
  } /* for */

  /* Found out relations and fields to compare to in condition i */

  (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                   (Tsdb_tuple **)NULL);

  result = (Tsdb_selection *)malloc(sizeof(Tsdb_selection));
  result->relations =
    (Tsdb_relation **)malloc((selection->n_relations + 1 ) * 
                             sizeof(Tsdb_relation *));
  result->n_relations = selection->n_relations;
  memcpy(result->relations,selection->relations,
         sizeof(Tsdb_relation*)*(selection->n_relations+1));
  result->n_key_lists = selection->n_key_lists;
  result->key_lists = (Tsdb_key_list **)
    malloc(result->n_key_lists*sizeof(Tsdb_key_list*));
  memset(result->key_lists,'\0',result->n_key_lists*sizeof(Tsdb_key_list*));
  result->length = 0;

  /* Prepared lists and anything elase */
  if (n_conditions==1) {
    type = TSDB_AND;
    match = 1;
  } /* if n_conditions */
  for ( match = (type==TSDB_AND), list = selection->key_lists[0];
       list != NULL ;
       list = list->next, match = (type==TSDB_AND)) {
    for ( i=0; i<n_conditions ; i++) { 
      /* check all conditions with tuple */
      if (relation[i]==-1) continue;
      results[i] = 
        tsdb_value_compare(list->tuples[relation[i]]->fields[field[i]],
                           conditions[i]->right->node);
      if (type==TSDB_OR) {
        switch(conditions[i]->node->value.operator) {
        case TSDB_EQUAL:
          match = match || (results[i] == TSDB_EQUAL);
          break;
        case TSDB_NOT_EQUAL:
          match = match || (results[i] != TSDB_EQUAL);
          break;
        case TSDB_LESS_THAN:
          match = match || (results[i] == TSDB_LESS_THAN);
          break;
        case TSDB_LESS_OR_EQUAL_THAN:
          match = match || (results[i] == TSDB_LESS_THAN ||
                            results[i] == TSDB_EQUAL);
          break;
        case TSDB_GREATER_THAN:
          match = match || (results[i] == TSDB_GREATER_THAN);
          break;
        case TSDB_GREATER_OR_EQUAL_THAN:
          match = match || (results[i] == TSDB_GREATER_THAN ||
                            results[i] == TSDB_EQUAL);
          break;
        case TSDB_SUBSTRING:
          match = match ||
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,NULL);
          break;
        case TSDB_NOT_SUBSTRING:
          match = match || 
            !(tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                                   conditions[i]->right->node,NULL));
          break;
        } /* switch */
      } /* if */
      if (type == TSDB_AND) {
        switch(conditions[i]->node->value.operator) {
        case TSDB_EQUAL:
          match = match && (results[i] == TSDB_EQUAL);
          break;
        case TSDB_NOT_EQUAL:
          match = match && (results[i] != TSDB_EQUAL);
          break;
        case TSDB_LESS_THAN:
          match = match && (results[i] == TSDB_LESS_THAN);
          break;
        case TSDB_LESS_OR_EQUAL_THAN:
          match = match && (results[i] == TSDB_LESS_THAN ||
                            results[i] == TSDB_EQUAL);
          break;
        case TSDB_GREATER_THAN:
          match = match && (results[i] == TSDB_GREATER_THAN);
          break;
        case TSDB_GREATER_OR_EQUAL_THAN:
          match = match && (results[i] == TSDB_GREATER_THAN ||
                            results[i] == TSDB_EQUAL);
          break;
        case TSDB_SUBSTRING:
          match = match &&
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,NULL);
          break;
        case TSDB_NOT_SUBSTRING:
          match = match && 
            !(tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                               conditions[i]->right->node,NULL));
          break;
        } /* switch */
      } /* if */
    } /* for all conditions */
    if (match) /* tuples matches all conditions */ {
      new_tuples = (Tsdb_tuple**)
        calloc(selection->n_relations+1,sizeof(Tsdb_tuple**));
      memcpy(new_tuples,list->tuples,
             ((selection->n_relations+1)*sizeof(Tsdb_tuple**)) );
      if (tsdb_insert_into_selection(result,new_tuples))
        result->length++;
    } /* if */
  } /* for */
  
  free(relation);
  free(field);
  free(results);

  return(result);
} /* tsdb_select() */

Tsdb_relation **tsdb_join_path(Tsdb_relation **sources,
                               Tsdb_relation **targets) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_join_path()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_join_path() determines the shortest path from one of the relations in
|* .sources. to one in .targets.
\*****************************************************************************/

  Tsdb_relation *source, *target, **guess, **path, ***paths;
  Tsdb_value *keys;
  unsigned int length, i, j, k, l;

  guess = (Tsdb_relation **)malloc(tsdb_n_relations() *
                                   sizeof(Tsdb_relation *));
  keys = (Tsdb_value *)malloc(tsdb_n_relations() *
                                   sizeof(Tsdb_value));
  length = tsdb_n_relations() + 1;
  path = (Tsdb_relation **)NULL;

#if defined(DEBUG) && defined(JOIN_PATH)
  fprintf(tsdb_debug_stream, "join_path(): sources: %s", sources[0]->name);
  for(i = 1; sources[i] != NULL; i++) {
    fprintf(tsdb_debug_stream, ":%s", sources[i]->name);
  } /* for */
  fprintf(tsdb_debug_stream, "\n");
  fprintf(tsdb_debug_stream, "join_path(): targets: %s", targets[0]->name);
  for(i = 1; targets[i] != NULL; i++) {
    fprintf(tsdb_debug_stream, ":%s", targets[i]->name);
  } /* for */
  fprintf(tsdb_debug_stream, "\n");
  fflush(tsdb_debug_stream);
#endif
  
  for(i = 0, source = sources[i]; source != NULL; source = sources[++i]) {
    for(j = 0, target = targets[j]; target != NULL; target = targets[++j]) {
      guess[0] = source;
      if((paths = tsdb_real_join_path(guess, 1, target, length)) != NULL) {
        for(k = 0; paths[k] != NULL; k++) {
          for(l = 0; paths[k][l] != NULL; l++);
          if(l < length) {
            length = l;
            if(path != NULL) {
              free(path);
            } /* if */
            path = paths[k];
#if defined(DEBUG) && defined(JOIN_PATH)
            fprintf(tsdb_debug_stream, "join_path(): path: ");
            tsdb_print_join_path(path, tsdb_debug_stream);
            fflush(tsdb_debug_stream);
#endif DEBUG
          } /* if */
          else {
            free(paths[k]);
          } /* else */
        } /* for */
        free(paths);
      } /* if */
    } /* for */
  } /* for */

  free(guess);
  return(path);

} /* tsdb_join_path() */

Tsdb_relation ***tsdb_real_join_path(Tsdb_relation **guess,
                                     int length,
                                     Tsdb_relation *target,
                                     int maximum) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_real_join_path()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_real_join_path() completes the path (of length .length.) in .guess. to
|* .target. 
|*****************************************************************************|
|* <known bugs>
|* .maximum. (length of shortest path found so far) is ignored; presumably, it
|* would be (even) more efficient if it was passed down by reference, thus
|* allowing to cut searches originating from higher recursion levels.
\*****************************************************************************/

  Tsdb_relation *source, **relations, **neighbours, ***paths, ***successes;
  int n_neighbours, n_successes, i, n;
  char *foo, *bar;

#if defined(DEBUG) && defined(JOIN_PATH)
  fprintf(tsdb_debug_stream, "real_join_path(): guess: %s",
          (length > 0 ? guess[0]->name : ""));
  for(i = 1; i < length; i++) {
    fprintf(tsdb_debug_stream, " -- %s", guess[i]->name);
  } /* for */
  fprintf(tsdb_debug_stream,
          "\nreal_join_path(): target: %s; maximum: %i\n",
          target->name, maximum);
  fflush(tsdb_debug_stream);
#endif  

  successes = (Tsdb_relation ***)NULL;
  n_neighbours = n_successes = n = 0;
  source = guess[length - 1];
  neighbours = (Tsdb_relation **)malloc((tsdb_n_relations() + 1) *
                                        sizeof(Tsdb_relation *));
  for(relations = tsdb_all_relations();
      relations != NULL && *relations != NULL;
      relations++) {
    for(i = 0;
        i < length && !tsdb_relations_are_equal(*relations, guess[i]);
        i++);
    if(i == length 
       /* relations is NOT on the path */
       && tsdb_are_joinable(*relations, source)) {
      if(length < 2 ||
         (foo = tsdb_join_key(guess[length - 2], guess[length - 1])) != NULL &&
         (bar = tsdb_join_key(guess[length - 1], *relations)) != NULL &&
         strcmp(foo, bar)) {
        neighbours[n_neighbours++] = *relations;
        if(length >= 2 && foo != NULL) {
          free(foo);
        } /* if */
        if(length >= 2 && bar != NULL) {
          free(bar);
        } /* if */        
      } /* if */
    } /* if */
  } /* for */

#if defined(DEBUG) && defined(JOIN_PATH)
  fprintf(tsdb_debug_stream,
          "real_join_path(): neighbours: %s",
          (n_neighbours ? neighbours[0]->name : ""));
  for(i = 1; i < n_neighbours; i++) {
    fprintf(tsdb_debug_stream, " | %s", neighbours[i]->name);
  } /* for */
  fprintf(tsdb_debug_stream, "\n");
  fflush(tsdb_debug_stream);
#endif

  for(i = 0; i < n_neighbours; i++) {

    if(tsdb_relations_are_equal(neighbours[i], target)) {
      if(!n_successes) {
        successes = (Tsdb_relation ***)malloc(2 * sizeof(Tsdb_relation **));
      } /* if */
      else {
        successes =
          (Tsdb_relation ***)realloc(successes,
                                     (n_successes + 2) * 
                                     sizeof(Tsdb_relation**));
      } /* else */
      successes[n_successes]
        = (Tsdb_relation **)malloc((length + 2) * 
                                   sizeof(Tsdb_relation *));
      memcpy(successes[n_successes], guess, length * sizeof(Tsdb_relation *));
      successes[n_successes][length] = target;
      successes[n_successes][length + 1] = (Tsdb_relation *)NULL;
      successes[++n_successes] = (Tsdb_relation **)NULL;
      maximum = length + 1;

#if defined(DEBUG) && defined(JOIN_PATH)
      fprintf(tsdb_debug_stream, "real_join_path(): successes: ");
      tsdb_print_join_path(successes[n_successes - 1], tsdb_debug_stream);
      fprintf(tsdb_debug_stream, "real_join_path(): new maximum: %i\n",
              maximum);
      fflush(tsdb_debug_stream);
#endif
    } /* if */
    else {
      guess[length] = neighbours[i];
      if((paths = tsdb_real_join_path(guess, length + 1, target, maximum))
         != NULL) {
        for(n = 0; paths[n]; n++);
        if(successes == NULL) {
          successes = (Tsdb_relation ***)malloc((n + 1) *
                                                sizeof(Tsdb_relation **));
        } /* if */
        else {
          successes =
            (Tsdb_relation ***)realloc(successes,
                                       (n_successes + n + 1) * 
                                       sizeof(Tsdb_relation));
        } /* else */
        memcpy(&successes[n_successes],
               &paths[0],
               sizeof(Tsdb_relation **) * (n + 1));
        n_successes += n;
        free(paths);
      } /* if */
    } /* else */
  } /* for */

  return(successes);
  
} /* tsdb_real_join_path() */
