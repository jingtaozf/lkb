/*****************************************************************************\
|*        file: tsdb_utility.c
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
#include <unistd.h>
#include <dirent.h>
#include <regex.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/param.h>
#include <errno.h>
#include <string.h>
extern int errno;

#include "globals.h"
#include "tsdb.h"
#include "errors.h"

#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX)
#  include <sys/time.h>
#else
#  include <sys/times.h>
#endif

#define REDIRECT " < /dev/null > /dev/null"


/* it's only hacked code but i like it */
int copy_key_list_flag = 0;


BYTE tsdb_value_compare(Tsdb_value *foo, Tsdb_value *bar) {
/* what about matching?*/ 

  int teresa, *date_1, *date_2, i;
  BYTE result;

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
    case TSDB_DATE:
      if((date_1 = tsdb_parse_date(foo->value.date)) == NULL) {
        return(TSDB_VALUE_INCOMPATIBLE);
      } /* if */
      if((date_2 = tsdb_parse_date(bar->value.date)) == NULL) {
        free(date_1);
        return(TSDB_VALUE_INCOMPATIBLE);
      } /* if */
      for(i = 0;
          i < 6
          && (date_1[i] == date_2[i] || date_1[i] == -1 || date_2[i] == -1);
          i++);
      if(i == 6) {
        free(date_1);
        free(date_2);
        return(TSDB_EQUAL);
      } /* if */
      result = TSDB_EQUAL;
      if(date_1[2] < date_2[2]) {
        result = TSDB_LESS_THAN;
      } /* if */
      else if(date_1[2] == date_2[2]) {
        if(date_1[1] < date_2[1]) {
          result = TSDB_LESS_THAN;
        } /* if */
        else if(date_1[1] == date_2[1]) {
          if(date_1[0] != -1 && date_2[0] != -1 && date_1[0] < date_2[0]) {
            result = TSDB_LESS_THAN;
          } /* if */
          else if(date_1[0] == date_2[0]) {
            if(date_1[3] < date_2[3]) {
              result = TSDB_LESS_THAN;
            } /* if */
            else if(date_1[3] == date_2[3]) {
              if(date_1[4] < date_2[4]) {
                result = TSDB_LESS_THAN;
              } /* if */
              else if(date_1[4] == date_2[4]) {
                if(date_1[5] < date_2[5]) {
                  result = TSDB_LESS_THAN;
                } /* if */
              } /* if */
            } /* if */
          } /* if */
        } /* if */
      } /* if */
      if(result == TSDB_EQUAL) {
        result = TSDB_GREATER_THAN;
      } /* if */
      free(date_1);
      free(date_2);
      return(result);
      break;
    default:
      fprintf(tsdb_error_stream,
              "tsdb_value_compare(): invalid value type in comparison.\n");
      fflush(tsdb_error_stream);
      return(TSDB_VALUE_INCOMPATIBLE);
    } /* switch */
  } /* if */
  else {
    fprintf(tsdb_error_stream,
            "tsdb_value_compare(): incompatible types in comparison.\n");
    fflush(tsdb_error_stream);
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

/*  True if the pattern bar matches foo: If given the compiled pattern
    bar_pat, it matches with it instead. */

BYTE tsdb_value_match(Tsdb_value *foo, Tsdb_value *bar,char mode,
                      void* bar_pat) {
  BOOL answer=FALSE;
  int result;
  regex_t baz,*pattern;

   if (foo->type == TSDB_STRING && bar->type == TSDB_STRING) {
     if (bar_pat!=NULL)
       pattern = (regex_t*) bar_pat;
     else { /* compile new pattern */
       pattern = &baz;
       if (mode==TSDB_INSENSITIVE_MATCH)
         result = regcomp(pattern,bar->value.string,REG_EXTENDED|REG_ICASE);
       else 
         result = regcomp(pattern,bar->value.string,REG_EXTENDED);
       if (result!=0) {
         int s = regerror(result,pattern,NULL,0);
         {
           char a[s+1];
           regerror(result,pattern,&a[0],s+1);
           fprintf(tsdb_error_stream,
                   "value_match(): invalid expression: %s \n",a);
           fflush(tsdb_error_stream);
         }
         return 3;
       } /* if */
     } /* else */
     answer = regexec(pattern,foo->value.string,0,NULL,0);
     if (answer!=0) {
       answer = FALSE;
     } /* if */
     else
       answer = TRUE;
   } /* if */
  if (!bar_pat) {
    regfree(pattern);
  }
  return(answer);

} /* tsdb_value_match() */

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
      fprintf(tsdb_error_stream,
              "are_attributes(): unknown attribute `%s'.\n",
              attribute_list[i]->value.identifier);
      fflush(tsdb_error_stream);
      return(FALSE);
    } /* if */
  } /* for */
  
  return(TRUE);

} /* tsdb_are_attributes() */

BOOL tsdb_are_joinable(Tsdb_relation *source, Tsdb_relation *target) {
  
  int i, j;
  
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

  if (sel==NULL) {
    return FALSE;
  }/* if */
    
  for (i=0; i<sel->n_relations; i++) {
    if (tsdb_are_joinable(rel,sel->relations[i]))
      return(TRUE);
  } /* for */
  return FALSE;

} /* tsdb_joins_to */

BOOL tsdb_is_attribute(Tsdb_value *value)
{
  int i,j;

  for(i = 0; tsdb.relations[i] != NULL; i++) {
    for (j = 0; j<tsdb.relations[i]->n_fields ; j++) {
      if (!strcmp(tsdb.relations[i]->fields[j],value->value.string))
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

  for(i = 0,k = 0; tsdb.relations[i] != NULL; i++) {
    for (j = 0; j<tsdb.relations[i]->n_fields ; j++) {
      if (!strcmp(tsdb.relations[i]->fields[j],value->value.string)){
        if (!attribute_relations) {
          attribute_relations = (Tsdb_relation**) 
            malloc((tsdb_n_relations()+1)*sizeof(Tsdb_relation*));
          memset(attribute_relations,'\0',
                 (tsdb_n_relations()+1)*sizeof(Tsdb_relation*));
        } /* if */
        attribute_relations[k++] = tsdb.relations[i];
      } /* if */
    } /* for */
  } /* for */
  if (attribute_relations) attribute_relations[k] = NULL;
  
  return(attribute_relations);
}


/*--------------------------------------------------------------------------*/

BOOL tsdb_is_relation(Tsdb_value *value) { /* True if value is the name of a relation */
  
  int i;

  for(i = 0; tsdb.relations[i] != NULL; i++)
    if(!strcmp(tsdb.relations[i]->name, value->value.identifier))
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



/*****************************************************************************\
|*        file: 
|*      module: tsdb_value_satisfies()
|*     version: 
|*  written by: tom fettig @ dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* evaluates (value1 operator value2) 
|*
\*****************************************************************************/

BOOL tsdb_value_satisfies(Tsdb_value* value1,Tsdb_value* operator,
                          Tsdb_value *value2) {
  BOOL answer;
  char* string=NULL;
  
  if (value1->type!=value2->type) {
    /* error! */
    return FALSE;
  } /* if */

  if (operator->value.operator==TSDB_MATCH) {
    return tsdb_value_match(value1,value2,0,NULL);
  }
  else
    if (operator->value.operator==TSDB_NOT_MATCH) {
      return !tsdb_value_match(value1,value2,0,NULL);
    }
  if (operator->value.operator==TSDB_INSENSITIVE_MATCH) {
     return tsdb_value_match(value1,value2,TSDB_INSENSITIVE_MATCH,NULL);
  }
   else
    if (operator->value.operator==TSDB_NOT_INSENSITIVE_MATCH) {
      return !tsdb_value_match(value1,value2,TSDB_INSENSITIVE_MATCH,NULL);
    }
  
  answer = tsdb_value_compare(value1,value2);
  if (answer == TSDB_VALUE_INCOMPATIBLE)
    return FALSE;
  
  switch (answer) {
  case TSDB_EQUAL:
    switch (operator->value.operator) {
    case TSDB_EQUAL:
    case TSDB_GREATER_OR_EQUAL_THAN:
    case TSDB_LESS_OR_EQUAL_THAN:
      return TRUE;
      break;
    default:
      return FALSE;
      break;
    }
    break;
  case TSDB_NOT_EQUAL:
    switch (operator->value.operator) {
    case TSDB_EQUAL:
      return FALSE;
      break;
    default:
      return TRUE;
      break;
    }
    break;
  case TSDB_LESS_THAN:
    if ((operator->value.operator == TSDB_LESS_THAN)||
        (operator->value.operator == TSDB_LESS_OR_EQUAL_THAN))
      return TRUE;
    else
      return FALSE;
    break;
  case TSDB_GREATER_THAN :
    if ((operator->value.operator == TSDB_GREATER_THAN)||
        (operator->value.operator == TSDB_GREATER_OR_EQUAL_THAN))
      return TRUE;
    else
      return FALSE;
    break;
  default:
    return FALSE;
    break;
  } /* switch */
  
  return FALSE;
} /* tsdb_value_satisfies() */

/*---------------------------------------------------------------------------*/

BOOL tsdb_satisfies_condition(Tsdb_tuple *tuple, Tsdb_node *condition,
                              Tsdb_relation *relation) {
  
  int i, integer;
  char *string = NULL, *attribute = NULL;
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
    fprintf(tsdb_error_stream,
            "satisfies_condition(): "
            "invalid righthand side comparison value.\n");
    fflush(tsdb_error_stream);
    if (attribute != NULL)
      free(attribute);
    return(FALSE);
  }

  for(i = 0; strcmp(attribute, relation->fields[i]) != 0; i++);

  switch(condition->node->value.operator) {
  case TSDB_EQUAL :
    answer = (number
              ? (tuple->fields[i]->value.integer == integer)
              : !strcmp(string, tuple->fields[i]->value.string));
    break;
  case TSDB_NOT_EQUAL :
    answer = (number 
              ? (tuple->fields[i]->value.integer != integer)
              : strcmp(string, tuple->fields[i]->value.string));
    break;
  case TSDB_LESS_THAN :
    if(number) {
      answer = (tuple->fields[i]->value.integer < integer);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '<' on strings.\n");
      answer = FALSE;
    } /* if */
    break;
  case TSDB_LESS_OR_EQUAL_THAN :
    if(number) {
      answer = (tuple->fields[i]->value.integer <= integer);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '<' on strings.\n");
      answer = FALSE;
    } /* else */
    break;
  case TSDB_GREATER_THAN :
    if(number) {
      answer = (tuple->fields[i]->value.integer > integer);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '>' on strings.\n");
      answer = FALSE;
    } /* else */
    break;
  case TSDB_GREATER_OR_EQUAL_THAN :
    if(number) {
      answer = (tuple->fields[i]->value.integer >= integer);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '>=' on strings.\n");
      answer = FALSE;
    } /* else */
    break;
  case TSDB_MATCH :
    if(number) {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '~' on integers.\n");
      answer = FALSE;
    } /* if */
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) != NULL);
    } /* else */
    break; 
  case TSDB_NOT_MATCH :
    if(number) {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '!~' on integers.\n");
      answer = TRUE;
    } /* if */
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) == NULL);
    } /* else */
    break; 
  case TSDB_INSENSITIVE_MATCH :
    if(number) {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '~~ on integers.\n");
      answer = FALSE;
    } /* if */
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) != NULL);
    } /* else */
    break; 
  case TSDB_NOT_INSENSITIVE_MATCH :
    if(number) {
      fprintf(tsdb_error_stream,
              "satisfies_condition(): undefined operator '!~~' on integers.\n");
      answer = TRUE;
    } /* if */
    else {
      answer = (strstr(string, tuple->fields[i]->value.string) == NULL);
    } /* else */
    break; 
  default :
      fprintf(tsdb_error_stream,
              "satisfies_condition(): unknown operator %d.\n",
              condition->node->value.operator);
  } /* switch */
  
  fflush(tsdb_error_stream);
  if(attribute != NULL) {
    free(attribute);
  } /* if */
  if(string != NULL) {
    free(string);
  } /* if */
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
  
  for(i = 0; tsdb.relations[i] != NULL; i++) {
    if(!strcmp(tsdb.relations[i]->name, relation_name))
      break;
  } /* for */
  if(tsdb.relations[i] != NULL) {
    while((tsdb.relations[i] = tsdb.relations[++i]) != NULL);
  } /* if */

} /* tsdb_remove_relation() */

void tsdb_add_relation(Tsdb_relation *relation) {
  
  int i;
  
  for(i = 0; tsdb.relations[i] != NULL; i++);
  tsdb.relations = 
    (Tsdb_relation **)realloc(tsdb.relations, 
                              (i + 2) * sizeof(Tsdb_relation *));
  tsdb.relations[i] = relation;
  tsdb.relations[i + 1] = (Tsdb_relation *)NULL;
  
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

  if(tsdb.relations != NULL ||
     tsdb_all_relations() != NULL) {
    for(i = 0; tsdb.relations[i] != NULL; i++) {
      if(!strcmp(name, tsdb.relations[i]->name)) {
        tsdb.errno = TSDB_OK;
        return(tsdb.relations[i]);
      } /* if */
    } /* for */
  } /* if */
  tsdb.errno = TSDB_UNKNOWN_RELATION_ERROR;
  return((Tsdb_relation *)NULL);

} /* tsdb_find_relation() */

Tsdb_relation **tsdb_all_relations() {

  Tsdb_relation *relation;
  FILE *input;
  int i;

  if(tsdb.relations == NULL) {
    if((input = tsdb_find_relations_file("r")) == NULL) {
      return((Tsdb_relation **)NULL);
    } /* if */
    if((relation = tsdb_read_relation(input)) != NULL) {
      for(i = 0;
          relation != NULL;
          relation = tsdb_read_relation(input), i++) {
        if(tsdb.relations == NULL) {
          tsdb.relations =
            (Tsdb_relation **)malloc(2 * sizeof(Tsdb_relation *));
        } /* if */
        else {
          tsdb.relations =
            (Tsdb_relation **)realloc(tsdb.relations,
                                      (i + 2) * sizeof(Tsdb_relation *));
        } /* else */
        tsdb.relations[i] = relation;
        tsdb.relations[i + 1] = (Tsdb_relation *)NULL;
      } /* for */
      return(tsdb.relations);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "all_relations(): invalid relations file `%s'.\n",
              tsdb.relations_file);
      fflush(tsdb_error_stream);
      return((Tsdb_relation **)NULL);
    } /* else */  
  } /* if */
  else {
    return(tsdb.relations);
  } /* else */

} /* tsdb_all_relations() */


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
  int i;

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
  new->keys[new->n_keys]=0L;
  return(new);

} /* tsdb_copy_relation() */


/*---------------------------------------------------------------------------*/

Tsdb_key_list *tsdb_copy_key_list(Tsdb_key_list *key_list) {

  Tsdb_key_list *new;
  int i;

  printf("*** --- copy_key_list considered harmful --- ***\n");
  copy_key_list_flag = 1;
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

/*---------------------------------------------------------------------------*/

Tsdb_key_list *tsdb_new_copy_key_list(Tsdb_key_list *key_list) {

  Tsdb_key_list *new;

  new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
  new->key = key_list->key;
  new->n_tuples = key_list->n_tuples;
  new->tuples = (Tsdb_tuple**) key_list->tuples[new->n_tuples];
  new->tuples[new->n_tuples]=NULL;
  new->next = key_list->next;

  return(new);

} /* tsdb_new_copy_key_list() */


Tsdb_selection *tsdb_find_table(Tsdb_relation *relation) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_find_table()
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
|* last update: 30-jun-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|* tsdb_find_table() returns the original table!
|* On demand it gets appended to tsdb.data.
|* so we won't free it!
\*****************************************************************************/

  Tsdb_selection *foo;
  int i;

  if(relation == NULL || relation->name == NULL || tsdb.relations == NULL) {
    fprintf(tsdb_error_stream,
            "find_table(): invalid context or parameter call.\n");
    fflush(tsdb_error_stream);
    tsdb.errno = TSDB_UNKNOWN_ERROR;
    return((Tsdb_selection *)NULL);
  } /* if */
  for(i = 0;
      tsdb.relations[i] != NULL && strcmp(tsdb.relations[i]->name,
                                          relation->name);
      i++);
  if(tsdb.relations[i] == NULL) {
    fprintf(tsdb_error_stream,
            "find_table(): unknown relation `%s'.\n",
            relation->name);
    fflush(tsdb_error_stream);
    tsdb.errno = TSDB_UNKNOWN_RELATION_ERROR;
    return((Tsdb_selection *)NULL);
  } /* if */

  if(tsdb.data == NULL) {
    if((foo = tsdb_read_table(relation, (Tsdb_node *)NULL)) != NULL) {
      tsdb.data = (Tsdb_selection **)malloc(2 * sizeof(Tsdb_selection *));
      tsdb.data[0] = foo;
      tsdb.data[1] = (Tsdb_selection *)NULL;
      tsdb.errno = TSDB_OK;
      return(tsdb.data[0]);
    } /* if */
    else {
      return((Tsdb_selection *)NULL);
    } /* else */
  } /* if */
  else {
    for(i = 0;
        tsdb.data[i] != NULL &&
        strcmp(tsdb.data[i]->relations[0]->name, relation->name);
        i++);
    if(tsdb.data[i] == NULL) {
      if((foo = tsdb_read_table(relation, (Tsdb_node *)NULL)) != NULL) {
        tsdb.data =
          (Tsdb_selection **)realloc(tsdb.data,
                                     (i + 2) * sizeof(Tsdb_selection *));
        tsdb.data[i] = foo;
        tsdb.data[i + 1] = (Tsdb_selection *)NULL;
      } /* if */
      else {
        return((Tsdb_selection *)NULL);
      } /* else */
    } /* if */
    return(tsdb.data[i]);
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
  /* we need a tsdb_copy_selection here !! */
  free(real_relations);
  if (j==0)
    select = tsdb_copy_selection(select);
  return(select);

} /* tsdb_find_tables() */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
int tsdb_relation_in_selection(Tsdb_selection* selection,char* name)
{
  /* returns -1 if name isn't a relation in selection */
  int i;

  if (!selection)
    return(-1);
  for (i=0;i<selection->n_relations;i++) {
    if (!strcmp(selection->relations[i]->name,name))
      return(i);
  } /* for */

  return(-1);
} /* tsdb_relation_in_selection */

/*---------------------------------------------------------------------------*/
BOOL tsdb_attribute_in_relation(Tsdb_relation *relation, char *name) {
  int j;

  for (j=0;j<relation->n_fields;j++) {
    if (!strcmp(relation->fields[j],name))
      return(TRUE);
  } /* for */

  return(FALSE);
} /* tsdb_attribute_in_relation() */

/*---------------------------------------------------------------------------*/

BOOL tsdb_attribute_in_selection(Tsdb_selection *selection, char *name)
{
  Tsdb_relation *relation;
  int i,j;
  
  if (!selection)
    return FALSE;
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

Tsdb_value *tsdb_date(char *foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_DATE;
  bar->value.date = foo;

  return(bar);

} /* tsdb_string() */

Tsdb_value *tsdb_position(char *foo) {

  Tsdb_value *bar;

  bar = (Tsdb_value *)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_POSITION;
  bar->value.position = foo;

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

Tsdb_value *tsdb_descriptor(int r,int f) {
  
  Tsdb_value *bar;
  int* d;
  
  bar = (Tsdb_value*)malloc(sizeof(Tsdb_value));
  bar->type = TSDB_DESCRIPTOR;
  d =  (int*)malloc(2*sizeof(int));
  d[0]= r;
  d[1]= f;
  bar->value.descriptor = d;
  return bar;
  
} /* tsdb_descriptor() */

void tsdb_free_tsdb_value(Tsdb_value* foo) {

  switch (foo->type) {
  case TSDB_STRING: 
    if (foo->value.string) free(foo->value.string);
    break;
  case TSDB_IDENTIFIER: 
    if (foo->value.identifier) free(foo->value.identifier);
    break;
  case TSDB_DESCRIPTOR:
    if (foo->value.descriptor) free(foo->value.descriptor);
    break;
  };
  free(foo);
}

void tsdb_free_tsdb_values(Tsdb_value** bar) {
  int i;

  for (i = 0; bar[i] != NULL; i++)
    tsdb_free_tsdb_value(bar[i]);
  free(bar);

} /* tsdb_free_tsdb_values() */

Tsdb_value **tsdb_singleton_value_array(Tsdb_value *value) {

  Tsdb_value **foo, **bar;

  foo = bar = (Tsdb_value **)malloc(2 * sizeof(Tsdb_value *));
  *foo = value;
  *++foo = (Tsdb_value *)NULL;

  return(bar);

} /* tsdb_singleton_value_array() */

Tsdb_tuple ** tsdb_tuples_dup(Tsdb_tuple ** foo,int n) {
  Tsdb_tuple** bar;
  
  bar = (Tsdb_tuple**)malloc((n+1)*sizeof(Tsdb_tuple*));
  memcpy(bar,foo,(n+1)*(sizeof(Tsdb_tuple*)));
  
  return bar;
} /* tsdb_tuples_dup() */


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
  foo->status = TSDB_CLEAN;

  return(foo);

} /* tsdb_create_relation() */

void tsdb_free_relation(Tsdb_relation *relation) {

  int i;

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

void tsdb_free_relations(Tsdb_relation **relations) {
  int i;
  
  for (i=0; relations[i]; i++)
    tsdb_free_relation(relations[i]);
  
} /* tsdb_free_relations() */

Tsdb_selection* tsdb_create_selection(int n_relations, int n_key_lists) {

  Tsdb_selection *foo;

  foo = (Tsdb_selection*)malloc(sizeof(Tsdb_selection));
  foo->n_relations = n_relations;
  foo->n_key_lists = n_key_lists;
  foo->relations = (Tsdb_relation **)malloc(sizeof(Tsdb_relation *)
                                            * (n_relations + 1));
  foo->relations[n_relations] = (Tsdb_relation *)NULL;
  memset(foo->relations, '\0', sizeof(Tsdb_relation *) * (n_relations + 1));
  foo->key_lists = (Tsdb_key_list **)malloc(sizeof(Tsdb_key_list *)
                                            * (n_key_lists + 1));
  memset(foo->key_lists, '\0', sizeof(Tsdb_key_list *) * (n_key_lists + 1));
  foo->key_lists[n_key_lists] = (Tsdb_key_list *)NULL;
  return(foo);
} /* tsdb_create_selection() */


/*****************************************************************************\
|*        file: 
|*      module: tsdb_copy_selection()
|*     version: 
|*  written by: tom, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* this one is a bit harder than it might look: the tuples-field in each
|* keylist is sharef, ie. it exists in any keylist. As we want to copy
|* them, we first patch the copy in them. They are copied by taking
|* the duplicate from the original!!
\*****************************************************************************/

Tsdb_selection* tsdb_copy_selection(Tsdb_selection* source) {
  Tsdb_selection* target;
  int i,j, n_tuples ;
  Tsdb_key_list* foo,*bar;
  Tsdb_tuple ** patch;

  target = tsdb_create_selection(source->n_relations,source->n_key_lists);
  for(i = 0; i < source->n_relations; i++) {
    target->relations[i] = tsdb_copy_relation(source->relations[i]);
  } /* for*/
  
  if (source->length==0)
    return target;
  
  foo = source->key_lists[0];
  n_tuples = foo->n_tuples;

  for (i=0;i<source->length;i++,foo=foo->next) {
    patch = tsdb_tuples_dup(foo->tuples,n_tuples);
    foo->tuples[n_tuples]= (Tsdb_tuple*)patch;
  } /* for */
  
  for(i = 0;i < source->n_key_lists; i++) {
    target->key_lists[i] = tsdb_new_copy_key_list(source->key_lists[i]);
    foo = target->key_lists[i];
    bar = source->key_lists[i]->next;
    for(j = 1; j < source->length; foo = foo->next, bar= bar->next, j++) {
      if (!bar) {
         fprintf(TSDB_ERROR_STREAM,"copy_selection: selection to short\n");
         fflush(TSDB_ERROR_STREAM);
         break;
       }
      else
        foo->next = tsdb_new_copy_key_list(bar);
    } /* for */
    foo->next = NULL;
  } /* for */
  target->length = source->length;
  
  foo = source->key_lists[0];
  
  for (i=0;i<source->length;i++,foo=foo->next) {
    foo->tuples[n_tuples]=(Tsdb_tuple*)NULL;
  } /* for */
  
  return(target);
} /* tsdb_copy_selection() */

void tsdb_free_selection(Tsdb_selection *foo) {
  int i;
/*  tsdb_free_relations(foo->relations);*/
  free(foo->relations);
  for(i = 0; i < foo->n_key_lists; i++)
    tsdb_free_key_list_chain(foo->key_lists[i], FALSE);
  free(foo->key_lists);
  free(foo);
} /* tsdb_free_selection() */

void tsdb_free_selections(Tsdb_selection** bar) {
  int i;
  for (i = 0; bar[i]; i++) {
    tsdb_free_selection(bar[i]);
  } /* for */
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
  if (value != NULL) {
    free(value);
  } /* if */
} /* tsdb_free_leaf() */


void tsdb_free_key_list_chain(Tsdb_key_list *foo, BOOL tuples) {

  Tsdb_key_list *node, *next;

  if(foo != NULL) {
    for(node = foo, next = node->next;
        next != NULL;
        node = next, next = node->next) {
      if(tuples && node->tuples != NULL) {
        free(node->tuples);
      } /* if */
      free(node);
    } /* for */
    if(tuples && node->tuples != NULL) {
      free(node->tuples);
    } /* if */
    free(node);
  } /* if */
} /* tsdb_free_key_list_chain() */

void ignore() {
  ;
}

BOOL tsdb_initialize() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_initialize()
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
|* last update: 14-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
|*****************************************************************************|
|* <open questions>
\*****************************************************************************/

  char *foo;
#ifdef COMPRESSED_DATA
  char *baz;
#endif
  FILE *bar;
  int len;
#ifdef DEBUG
  int i, j;
#endif

  tsdb_parse_environment();

  if(tsdb.port
     && !(tsdb.status & (TSDB_SERVER_MODE | TSDB_CLIENT_MODE))) {
    fprintf(tsdb_error_stream,
            "initialize(): `-port' option invalid in non-server mode.\n");
    fflush(tsdb_error_stream);
    tsdb.port = 0;
  } /* if */
  if(tsdb.status & (TSDB_SERVER_MODE | TSDB_CLIENT_MODE)) {
    if(!tsdb.port) {
      tsdb.port = TSDB_SERVER_PORT;
    } /* if */
    else if(tsdb.port < 1024) {
      fprintf(tsdb_error_stream,
              "initialize(): invalid (privileged) `-port' value (%d).\n",
              tsdb.port);
      fflush(tsdb_error_stream);
      tsdb.port = TSDB_SERVER_PORT;
    } /* if */
  } /* if */

#ifdef DEBUG
  if((tsdb_debug_stream = tsdb_open_debug()) == NULL) {
    tsdb_debug_stream = tsdb_error_stream;
  } /* if */
#endif

  if((tsdb.status & TSDB_SERVER_MODE) && tsdb.query != NULL) {
    fprintf(tsdb_error_stream,
            "initialize(): `-query' option invalid in server mode.\n");
    tsdb.query = (char *)NULL;
  } /* if */

  if((tsdb.status & TSDB_SERVER_MODE) && tsdb.max_results) {
    tsdb.max_results = 0;
  } /* if */
  else if(tsdb.result_path != NULL && tsdb.result_prefix != NULL
     && tsdb.max_results != -1 && tsdb.max_results
     && (strlen(tsdb.result_path) + strlen(tsdb.result_prefix)
         + tsdb.max_results / 10) > MAXNAMLEN) {
    fprintf(tsdb_error_stream,
            "initialize(): "
            "|result_path| + |result_prefix| values are too long.\n");
    free(tsdb.result_path);
    free(tsdb.result_prefix);
    tsdb.result_path = strdup(TSDB_RESULT_PATH);
    tsdb.result_prefix = strdup(TSDB_RESULT_PREFIX);
    if((foo = tsdb_user()) != NULL) {
      tsdb.result_prefix
        = (char *)realloc(tsdb.result_prefix,
                          strlen(tsdb.result_prefix + strlen(foo) + 2));
      tsdb.result_prefix = strcat(tsdb.result_prefix, foo);
      tsdb.result_prefix = strcat(tsdb.result_prefix, ".");
    } /* if */
    tsdb.max_results = TSDB_MAX_RESULTS;
  } /* if */

  if(tsdb.status & TSDB_CLIENT_MODE) {
    if(tsdb.server == NULL) {
      tsdb.server = strdup("localhost");
    } /* if */
  } /* if */
  if(tsdb.pager != NULL) {
    if(tsdb.status & TSDB_SERVER_MODE || tsdb.query != NULL) {
      tsdb.pager = (char *)NULL;
    } /* if */
    else {
      len = strlen(tsdb.pager) + 1 + strlen(REDIRECT);
      if(!strcmp(tsdb.pager, "nil") || !strcmp(tsdb.pager, "null")) {
        tsdb.pager = (char *)NULL;
      } /* if */
      else {
        char *pager = malloc(len);
        strcpy(pager, tsdb.pager);
        strcat(pager, REDIRECT);
        if((bar = popen(pager, "w")) == NULL) {
          fprintf(tsdb_error_stream,
                  "initialize(): unaple to popen(3) `%s'.\n", tsdb.pager);
          pager = realloc(pager, strlen(TSDB_PAGER + len));
          strcpy(pager, TSDB_PAGER);
          strcat(pager, REDIRECT);
          if((bar = popen(pager, "w")) != NULL) {
            free(pager);
            free(tsdb.pager);
            tsdb.pager = strdup("TSDB_PAGER");
            pclose(bar);
          } /* if */      
          else if((bar = popen("more >/dev/null </dev/null", "w")) != NULL) {
            free(tsdb.pager);
            tsdb.pager = strdup("more");
            pclose(bar);
          } /* if */
          else if((bar = popen("less >/dev/null </dev/null", "w")) != NULL) {
            free(tsdb.pager);
            tsdb.pager = strdup("less");
            pclose(bar);
          } /* if */
          else if((bar = popen("page >/dev/null </dev/null", "w")) != NULL) {
            free(tsdb.pager);
            tsdb.pager = strdup("page");
            pclose(bar);
          } /* if */
          else {
            fprintf(tsdb_error_stream,
                    "initialize(): "
                    "unable to locate pager; check your `PATH' variable.\n");
            free(tsdb.pager);
            tsdb.pager = (char *)NULL;
          } /* else */
        } /* if */
        else {
          pclose(bar);
        } /* else */
      } /* else */
    } /* else */
  } /* if */

#ifdef COMPRESSED_DATA
  if(tsdb.compress == NULL) {
    tsdb.compress = strdup(TSDB_COMPRESS);
    if(tsdb.uncompress == NULL) {
      tsdb.uncompress = strdup(TSDB_UNCOMPRESS);
    } /* if */
  } /* if */
  else {
    if(tsdb.uncompress == NULL) {
      tsdb.uncompress = (char *)malloc(strlen(tsdb.compress + 4));
      tsdb.uncompress = strcpy(tsdb.uncompress, tsdb.compress);
      tsdb.uncompress = strcat(tsdb.uncompress, " -d");
    } /* if */
  } /* if */
  if(tsdb.suffix == NULL) {
    tsdb.suffix = strdup(TSDB_SUFFIX);
  } /* if */
#endif

#ifdef ALEP
  if(!(tsdb.status & TSDB_CLIENT_MODE)) {
#endif
    if(tsdb_all_relations() == NULL) {
      return(TSDB_NO_RELATIONS_ERROR);
    } /* if */
#ifdef ALEP
  } /* if */
#endif

#ifdef ALEP
  if(!(tsdb.status & TSDB_CLIENT_MODE)) {
#endif
    foo = (char *)malloc(MAXNAMLEN + 1);
#ifdef COMPRESSED_DATA
    baz = (char *)malloc(MAXNAMLEN + 1);
#endif
    for(i = 0; tsdb.relations[i] != NULL; i++) {
      foo = strcpy(foo, tsdb.data_path);
      foo = strcat(foo, tsdb.relations[i]->name);
      if(access(foo, R_OK)) {
#ifdef COMPRESSED_DATA
        baz = strcpy(baz, foo);
        baz = strcat(baz, tsdb.suffix);
        if(access(baz, R_OK)) {
#endif      
          if((j = creat(foo, 0666)) == -1) {
            free(foo);
            fprintf(tsdb_error_stream,
                    "initialize(): unable to create data file for `%s'.\n",
                    tsdb.relations[i]->name);
            fflush(tsdb_error_stream);
            return(TSDB_NO_DATA_ERROR);
          } /* if */
          else {
            close(j);
            fprintf(tsdb_error_stream,
                    "initialize(): creating empty data file for `%s'.\n",
                    tsdb.relations[i]->name);
            fflush(tsdb_error_stream);
          } /* else */
#ifdef COMPRESSED_DATA
        } /* if */
#endif
      } /* if */
    } /* for */
    free(foo);
#ifdef COMPRESSED_DATA
    free(baz);
#endif
#ifdef ALEP
  } /* if */
#endif

#ifdef ALEP
  if(!(tsdb.status & TSDB_CLIENT_MODE)) {
#endif
    tsdb_init_history(&tsdb);
#ifdef ALEP
  } /* if */
#endif

#ifdef DEBUG
  if(tsdb.relations != NULL) {
    for(i = 0; tsdb.relations[i] != NULL; i++) {
      tsdb_print_relation(tsdb.relations[i], tsdb_debug_stream);
    } /* for */
  } /* if */
  if(tsdb.status & TSDB_SERVER_MODE) {
    fprintf(tsdb_debug_stream,
            "initialize(): going into server mode; port: %d;\n",
            tsdb.port);
  } /* if */
  if(tsdb.status & TSDB_CLIENT_MODE) {
    fprintf(tsdb_debug_stream,
            "initialize(): client mode; port: %d; server: `%s'\n",
            tsdb.port, tsdb.server);
  } /* if */
  fprintf(tsdb_debug_stream, "initialize(): home: `%s';\n", tsdb.home);
  fprintf(tsdb_debug_stream,
          "initialize(): relations: `%s';\n", tsdb.relations_file);
  fprintf(tsdb_debug_stream,
          "initialize(): data: `%s';\n", tsdb.data_path);
  if(tsdb.max_results) {
    fprintf(tsdb_debug_stream,
            "initialize(): result path `%s'; result prefix: `%s' (%d);\n",
            tsdb.result_path, tsdb.result_prefix, tsdb.max_results);
  } /* if */
  else {
    fprintf(tsdb_debug_stream,
            "initialize(): no query result storage;\n");
  } /* else */
  if(!(tsdb.status & TSDB_SERVER_MODE)) {
    fprintf(tsdb_debug_stream,
            "initialize(): output to `%s';\n",
            (tsdb.output != NULL ? tsdb.output : "stdio"));
  } /* if */
  fprintf(tsdb_debug_stream,
          "initialize(): pager: `%s'; debug: `%s'.\n",
          (tsdb.pager != NULL ? tsdb.pager : "null"), tsdb.debug_file);
  fprintf(tsdb_debug_stream,
          "initialize(): removal of duplicates from projections is %s;\n",
          (tsdb.status & TSDB_UNIQUELY_PROJECT ? "on" : "off"));
  fprintf(tsdb_debug_stream,
          "initialize(): implicit commit (and save) for new data %s;\n",
          (tsdb.status & TSDB_IMPLICIT_COMMIT ? "enabled" : "disabled"));
#ifdef ALEP
  fprintf(tsdb_debug_stream,
          "initialize(): ALEP tx() output mode is %s;\n",
          (tsdb.status & TSDB_TX_OUTPUT ? "on" : "off"));
#endif
#ifdef COMPRESSED_DATA
  fprintf(tsdb_debug_stream,
          "initialize(): compress: `%s'; uncompress: `%s';\n",
          (tsdb.compress != NULL ? tsdb.compress : "null"),
          (tsdb.uncompress != NULL ? tsdb.uncompress : "null"));
  fprintf(tsdb_debug_stream,
          "initialize(): compressed file suffix: `%s'; ", 
          (tsdb.suffix != NULL ? tsdb.suffix : "null"));
  fprintf(tsdb_debug_stream, "fs: `%c'; ofs: `%s'.\n",
          tsdb.fs,
          (tsdb.ofs != NULL ? tsdb.ofs : "null"));
#else
  fprintf(tsdb_debug_stream, 
          "initialize(): fs: `%c'; ofs: `%s'.\n",
          tsdb.fs,
          (tsdb.ofs != NULL ? tsdb.ofs : "null"));
#endif
  fflush(tsdb_debug_stream);
#endif
  return(TSDB_OK);
} /* tsdb_initialize */

void tsdb_parse_environment() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_parse_environment()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  char *name, *foo;

  if(tsdb.home == NULL) {
    if((name = getenv("TSDB_HOME")) != NULL || 
       (name = tsdb_pseudo_user()) != NULL) {
      tsdb.home = tsdb_expand_directory((char *)NULL, name);
    } /* if */
    else {
      tsdb.home = tsdb_expand_directory((char *)NULL, TSDB_HOME);
    } /* else */
  } /* if */
  
  if(tsdb.relations_file == NULL) {
    if((name = getenv("TSDB_RELATIONS_FILE")) != NULL) {
      if(name[0] == TSDB_DIRECTORY_DELIMITER[0]) {
        tsdb.relations_file = strdup(name);
      } /* if */
      else {
        tsdb.relations_file = strdup(tsdb.home);
        tsdb.relations_file = (char *)realloc(tsdb.relations_file, 
                                              strlen(tsdb.relations_file) + 
                                              strlen(name) + 1);
        tsdb.relations_file = strcat(tsdb.relations_file, name);
      } /* else */
    } /* if */
    else {
      tsdb.relations_file = strdup(tsdb.home);
      tsdb.relations_file = (char *)realloc(tsdb.relations_file, 
                                            strlen(tsdb.relations_file) + 
                                            strlen(TSDB_RELATIONS_FILE) + 1);
      tsdb.relations_file = strcat(tsdb.relations_file, TSDB_RELATIONS_FILE);
    } /* else */
  } /* if */

  if(tsdb.data_path == NULL) {
    if((name = getenv("TSDB_DATA_PATH")) != NULL) {
      if(name[0] == TSDB_DIRECTORY_DELIMITER[0]) {
        tsdb.data_path = tsdb_expand_directory(tsdb.home, name);
      } /* if */
      else if(name[0] == '.') {
        tsdb.data_path = tsdb_expand_directory((char *)NULL, name);
      } /* if */
      else {
        tsdb.data_path = strdup(tsdb.home);
        tsdb.data_path = (char *)realloc(tsdb.data_path, 
                                         strlen(tsdb.data_path) + 
                                         strlen(name) + 1);
        tsdb.data_path = strcat(tsdb.data_path, name);
      } /* else */
    } /* if */
    else {
      tsdb.data_path = strdup(tsdb.home);
      tsdb.data_path = (char *)realloc(tsdb.data_path, 
                                       strlen(tsdb.data_path) + 
                                       strlen(TSDB_DATA_PATH) + 1);
      tsdb.data_path = strcat(tsdb.data_path, TSDB_DATA_PATH);
    } /* else */
  } /* if */

  if(tsdb.result_path == NULL) {
    if((name = getenv("TSDB_RESULT_PATH")) != NULL) {
      tsdb.result_path = tsdb_expand_directory((char *)NULL, name);
      if(access(tsdb.result_path, W_OK | X_OK)) {
        fprintf(tsdb_error_stream,
                "initialize(): unable to write directory `%s'.\n",
                tsdb.result_path);
        tsdb.result_path
          = tsdb_expand_directory((char *)NULL, TSDB_RESULT_PATH);
      } /* if */
    } /* if */
    else {
      tsdb.result_path
        = tsdb_expand_directory((char *)NULL, TSDB_RESULT_PATH);
    } /* else */
  } /* if */

  if(tsdb.result_prefix == NULL) {
    if((name = getenv("TSDB_RESULT_PREFIX")) != NULL) {
      tsdb.result_prefix = strdup(name);
    } /* if */
    else {
      tsdb.result_prefix = strdup(TSDB_RESULT_PREFIX);
      if((foo = tsdb_user()) != NULL) {
        tsdb.result_prefix
          = (char *)realloc(tsdb.result_prefix,
                            strlen(tsdb.result_prefix) + strlen(foo) + 2);
        tsdb.result_prefix = strcat(tsdb.result_prefix, foo);
        tsdb.result_prefix = strcat(tsdb.result_prefix, ".");
      } /* if */
    } /* else */
  } /* if */

  
  if(tsdb.max_results == -1) {
    if((name = getenv("TSDB_MAX_RESULTS")) != NULL) {
      if(!(tsdb.max_results = (BYTE)strtol(name, &foo, 10)) &&
         name == foo) {
        fprintf(tsdb_error_stream,
                "initialize(): "
                "non-integer (`%s') for `TSDB_MAX_RESULTS'.\n", name);
        tsdb.max_results = TSDB_MAX_RESULTS;
      } /* if */
    } /* if */
    else {
      tsdb.max_results = TSDB_MAX_RESULTS;
    } /* else */
  } /* if */

  if(tsdb.pager == NULL) {
    if((name = getenv("TSDB_PAGER")) != NULL
       || (name = getenv("PAGER")) != NULL) {
      if(!strcmp(name, "null") || !strcmp(name, "nil")) {
        tsdb.pager = (char *)NULL;
      } /* if */
      else {
        tsdb.pager = strdup(name);
      } /* else */
    } /* if */
    else {
      tsdb.pager = strdup(TSDB_PAGER);
    } /* else */
  } /* if */

  if(tsdb.ofs == NULL) {
    if((name = getenv("TSDB_OFS")) != NULL) {
      tsdb.ofs = strdup(name);
    } /* if */
    else {
      tsdb.ofs = strdup(TSDB_OFS);
    } /* else */
  } /* if */

} /* tsdb_parse_environment() */

char *tsdb_pseudo_user() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_pseudo_user()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 14-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/
  
  struct passwd *clare;
  char *fs, *name = strdup(TSDB_PSEUDO_USER);

  for(fs = strchr(name, tsdb.fs); 
      fs != NULL; 
      name = ++fs, fs = strchr(name, tsdb.fs)) {
    *fs = 0;
    if((clare = getpwnam(name)) != NULL) {
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

#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX)
  static struct timeval start[TSDB_MAX_TIMERS], stop[TSDB_MAX_TIMERS];
  struct timezone foo;
#else
  static struct tms start[TSDB_MAX_TIMERS], stop[TSDB_MAX_TIMERS];
#endif
  static BYTE n_timers = 0;

  if(action == TSDB_START_TIMER) {
#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX)
    if(gettimeofday(&start[n_timers], &foo)) {
#else
    if(times(&start[n_timers]) == -1) {
#endif
      perror("timer()");
      return((float)-1);
    } /* if */
    n_timers++;
    return((float)n_timers);
  } /* if */
  else {
    if(!n_timers || action > n_timers) {
      fprintf(tsdb_error_stream,
              "timer(): timer # %d not running.\n", action);
      return((float)-1);
    } /* if */
#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX)
    if(gettimeofday(&stop[--n_timers], &foo)) {
#else
    if(times(&stop[--n_timers]) == -1) {
#endif
      perror("tsdb_timer()");
      return((float)-1);
    } /* if */
#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX)
    return((stop[n_timers].tv_sec - start[n_timers].tv_sec) +
           ((stop[n_timers].tv_usec - start[n_timers].tv_usec) * .000001));
#else
    return(((stop[n_timers].tms_utime - start[n_timers].tms_utime) +
            (stop[n_timers].tms_stime - start[n_timers].tms_stime)) / 60);
#endif
  } /* else */

} /* tsdb_timer() */


/*****************************************************************************\
|*        file: 
|*      module: tsdb_collect_tuples()
|*     version: 
|*  written by: tom, dfki saarbruecken
|* last update: 23-nov-95
|*  updated by: 
|*****************************************************************************|
|*
|*****************************************************************************|
|* <known bugs>
|* it ain't even ready yet!
\*****************************************************************************/

BOOL tsdb_collect_tuples(Tsdb_selection* selection,Tsdb_tuple** tuples,
                         Tsdb_key_list*** lists,int last,int* size) {
  int offset,i,j=0,n_tuples;
  BOOL kaerb,resized = 0;
  Tsdb_value* value;
  Tsdb_key_list* new,**temp;
  /* automatic resizing of lists-array! 
   */
  if (last==*size) 
    resized = TRUE;

  n_tuples = selection->n_relations;
  for(i = 0, offset = 0; i < selection->n_relations; i++, offset += j) {
    for (j = 0; j < selection->relations[i]->n_keys; j++) {
      kaerb = FALSE;
      value = tuples[i]->fields[selection->relations[i]->keys[j]];
      new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
      new->key = value;
      new->tuples = tuples;
      new->n_tuples = n_tuples;
      new->next = (Tsdb_key_list *)NULL;
      if (resized) {
        temp = (Tsdb_key_list**)
          realloc(lists[offset+j],(*size*2+1)*sizeof(Tsdb_key_list*));
        if (temp)
          lists[offset+j]=temp;
        else 
          return FALSE;
      } /* if */
      lists[offset+j][last] = new;
    } /* for j */
  } /* for i */
  if (resized) 
    *size*=2;

  last++;
  return TRUE;
} /* tsdb_collect_tuples */



BOOL tsdb_array_to_lists(Tsdb_selection* bar,Tsdb_key_list*** lists,
                        int last,int size) {
  int i,j;
  Tsdb_key_list* foo;

  if (last==0)
    return TRUE;
  for (i=0;i<bar->n_key_lists;i++) {
    foo = lists[i][0];
    bar->key_lists[i] = foo;
    for (j=1;j<last;j++) {
      foo->next = lists[i][j];
      foo = lists[i][j];
    }
    foo->next = NULL;
    free(lists[i]);
  } /* for */
  bar->length = size;
  free(lists);
  return TRUE;
} /* tsdb_array_2_lists() */


int tsdb_keylist_compare(Tsdb_key_list** foo,Tsdb_key_list** bar) {
  int i = tsdb_value_compare((*foo)->key,(*bar)->key);

  switch(i) {
    case 1: return 0;
    case 3: return -1;
    case 4: return 0;
    case 5: return 1;
    case 6: return 0;
  }
  return 0;
} /* tsdb_keylist_compare() */

int is_sorted(char* field,int num,int size, int(*compar)()) {
  int i,sorted = 1;
  char * obj_1,*obj_2;
  
  obj_2 = field;
  for (i=0;sorted && i<num-1;i++) {
    obj_1=obj_2;
    obj_2=obj_2+size;
    if (compar(obj_1,obj_2)>0)
      sorted = 0;
  } /* for */
  return sorted;
} /* is_sorted() */


BOOL tsdb_sort_tuples(Tsdb_key_list*** lists,int last,int n_lists) {
  int i;
  
  if (last==0)
    return TRUE;
  for (i=0;i<n_lists;i++ ) {
    if (!is_sorted((char*)lists[i],last,sizeof(Tsdb_key_list*),
                   tsdb_keylist_compare))
      qsort((char*)lists[i], last, sizeof(Tsdb_key_list*),
            (int (*)())tsdb_keylist_compare);
    else {
    }
  } /* for () */
  return TRUE;
} /* tsdb_sort_tuples () */


BOOL tsdb_init_insert(Tsdb_selection* selection) {
  
} /* tsdb_init_insert() */

BOOL tsdb_insert_into_selection(Tsdb_selection *selection,
                                Tsdb_tuple **tuples) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_insert_into_selection()
|*     version: 
|*  written by: andrew p. white & oe, dfki saarbruecken
|* last update: 16-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|* tsdb_insert_into_selection() inserts .tuples. into all key lists of
|* .selection.; note that .tuples. is not copied; a call with all arguments
|* NULL pointers resets the static memory (i.e. called in a series it is
|* assumed that .selection. is the same unless indicated by a reset call).
|*****************************************************************************|
|* <known bugs>
|* for several key lists and growing length the linear search turns out to be
|* too inefficient; hence, the plan is to add a second level index or similar.
|* --- which may now be unnecessary thanx to the static memory (16-jul-96 oe).
\*****************************************************************************/


  Tsdb_value *value, *comparison;
  Tsdb_key_list *new, *next;
  static Tsdb_key_list **last = (Tsdb_key_list **)NULL;
  int i, j, offset, n_tuples;
  static int n_key_lists = 0;
  BOOL kaerb;
#if defined(DEBUG) && defined(INSERT_INTO_SELECTION)
  float time = tsdb_timer(TSDB_START_TIMER);
  int n_keys = 0;
#endif

  if(selection == NULL && tuples == NULL) {
    if (last)
      free(last);
    last = (Tsdb_key_list **)NULL;
    n_key_lists = 0;
#if defined(DEBUG) && defined(INSERT_INTO_SELECTION)
    (void)tsdb_timer(time);
#endif
    return(FALSE);
  } /* if */
  else if(last == NULL && !n_key_lists) {
    n_key_lists = selection->n_key_lists;
    last = (Tsdb_key_list **)malloc(n_key_lists * sizeof(Tsdb_key_list *));
    for(i = 0; i < n_key_lists; i++) {
      last[i] = (Tsdb_key_list *)NULL;
    } /* for */
  } /* if */

  for(n_tuples = 0; tuples != NULL && tuples[n_tuples] != NULL; n_tuples++);
  for(i = 0, offset = 0; i < selection->n_relations; i++, offset += j) {
#if defined(DEBUG) && defined(INSERT_INTO_SELECTION)
    n_keys += selection->relations[i]->n_keys;
#endif    
    for (j = 0; j < selection->relations[i]->n_keys; j++) {
      kaerb = FALSE;
      value = tuples[i]->fields[selection->relations[i]->keys[j]];
      if(selection->key_lists[offset + j] != NULL) {

        if(last[offset + j] != NULL) {
          switch(tsdb_value_compare(value, last[offset + j]->key)) {
            case TSDB_GREATER_THAN:
            case TSDB_EQUAL:
              new = (Tsdb_key_list *)malloc(sizeof(Tsdb_key_list));
              new->key = value;
              new->tuples = tuples;
              new->n_tuples = n_tuples;
              new->next = (Tsdb_key_list *)NULL;
              last[offset + j]->next = new;
              last[offset + j] = new;
#if defined(DEBUG) && defined(INSERT_INTO_SELECTION)
              fprintf(tsdb_debug_stream,
                      "insert_into_selection(): "
                      "appended tuple at position %d.\n", selection->length);
              fflush(tsdb_debug_stream);
#endif
              continue;
          } /* switch */
        } /* if */

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
                fprintf(tsdb_error_stream,
                        "insert_into_selection(): ignoring invalid tuple.\n");
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
            fprintf(tsdb_error_stream,
                    "insert_into_selection(): ignoring invalid tuple.\n");
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
        if(last[offset + j] == NULL) {
          last[offset + j] = new;
        } /* if */
      } /* if */
    } /* for */
  } /* for */
#if defined(DEBUG) && defined(INSERT_INTO_SELECTION)
  if((time = tsdb_timer(time)) != (float)-1) {
    fprintf(tsdb_debug_stream,
            "insert_into_selection(): inserted %d tuple(s) (%d key lists) "
            "in %.3f seconds.\n",
            n_tuples, n_keys, time);
    fflush(tsdb_debug_stream);
  } /* if */
#endif
  return(TRUE);

} /* tsdb_insert_into_selection() */

int comp(char **a,char**b) {
  return(strcmp(*a,*b));
}

int tsdb_uniq_projection(char** projection,int n) {
  int i,j,d=0;
#if defined(TOM) && defined(DEBUG) && defined(CRAZY)
  fprintf(tsdb_debug_stream,"qsort\n");
  for (i=0;i<n;i++) {
    fprintf(tsdb_debug_stream,"%s\n",projection[i]);
  }
  fflush(tsdb_debug_stream);
#endif
  qsort(projection,n,sizeof(char*),(int(*)())comp);
#if defined(TOM) && defined(DEBUG) && defined(CRAZY)
  fprintf(tsdb_debug_stream,"qsort\n");
  for (i=0;i<n;i++) {
    fprintf(tsdb_debug_stream,"%s\n",projection[i]);
  }
  fflush(tsdb_debug_stream);
#endif

  for (i=0,j=1;j<n;j++) {
    if (!strcmp(projection[i],projection[j])) {
      free(projection[j]);
      projection[j]=NULL;
      d++;
    }
    else
      i=j;
  } /* for */

  return n-d ;
} /* tsdb_uniq_projection() */

void tsdb_free_char_array(char** array,int n) {
  int i;

  for (i=0;i<n;i++)
    if (array[i]) {
      free(array[i]);
      array[i]=NULL;
    } /* if */
  free(array);

} /* tsdb_free_char_array() */

void tsdb_negate_node(Tsdb_node* node)
{
  if (node->node->type != TSDB_OPERATOR) {
    fprintf(tsdb_error_stream,"tsdb_negate_node: wrong type at leaf\n");
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
  case TSDB_MATCH:    
    node->node->value.operator = TSDB_NOT_MATCH; 
    break;
  case TSDB_NOT_MATCH: 
    node->node->value.operator = TSDB_MATCH ; 
    break;
 case TSDB_INSENSITIVE_MATCH:    
    node->node->value.operator = TSDB_NOT_INSENSITIVE_MATCH; 
    break;
  case TSDB_NOT_INSENSITIVE_MATCH: 
    node->node->value.operator = TSDB_INSENSITIVE_MATCH ; 
    break;
  default:
    fprintf(tsdb_error_stream," eh what??\n");
  }/* switch */

} /* tsdb_negate_node */

void tsdb_tree_negate(Tsdb_node* node)
{
  if (tsdb_children_leaf(node)) {
    tsdb_negate_node(node);
  } /* if */
  else {
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
  } /* else */
} /* tsdb_tree_negate() */

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

char** tsdb_condition_attributes(Tsdb_node *node,
                                 char **attributes,
                                 int* s_attributes)
{
  int i ;
  BOOL kaerb;

  if (!node) return attributes;
  if (!attributes) return NULL;
  if (node->node->type == TSDB_CONNECTIVE) {
    attributes = tsdb_condition_attributes(node->left,attributes,
                                             s_attributes);
    attributes = tsdb_condition_attributes(node->right,attributes,
                                             s_attributes);
    if (!attributes)
      return NULL;
  } /* if connective */
  else { /* leaf */
    if (node->node->type == TSDB_OPERATOR) {
      attributes = tsdb_condition_attributes(node->left,attributes,
                                             s_attributes);
      if (!attributes) 
        return NULL;
    }
    else {
      
      for(i=0, kaerb=FALSE; !kaerb && attributes[i];i++)
        if (!strcmp(attributes[i],node->node->value.string))
          kaerb = TRUE;
      
      if (!kaerb) {
        attributes[i] = NULL;
        if (!tsdb_is_attribute(node->node)) {
          fprintf(tsdb_error_stream,
                  "condition_attributes(): %s is not a valid attribute.\n",
                  node->node->value.string);
          tsdb_free_char_array(attributes, *s_attributes);
          return NULL;
        }
        else {
          if (!(i<*s_attributes)) {
            *s_attributes *= 2;
            attributes
              = (char**)realloc(attributes,*s_attributes*sizeof(char*));
            memset(((char*)attributes)+(*s_attributes)/2,'\0',
                   (*s_attributes)/2*sizeof(char*));
          }
          attributes[i++] = strdup(node->node->value.string);
        } /* else */
      } /* if */
    } /* else */
  } /* else */
  return attributes;
} /* tsdb_condition_attributes() */

char *tsdb_rcs_strip(char *s1, char *s2) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_rcs_strip()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  char *foo, *bar;

  if(s1 != NULL && s2 != NULL) {
    if(*s1 && (foo = strchr(s1, '$')) != NULL) {
      for(foo++; *foo && *foo == *s2; foo++, s2++);
      if(!*foo || *foo++ != ':') {
        return(s1);
      } /* if */
      else {
        if((bar = strrchr(foo, '$')) != NULL) {
          *bar = 0;
          for(; *foo && isspace(*foo); foo++);
          for(bar--; bar > foo && isspace(*bar); bar--);
          *++bar = 0;
          return(foo);
        } /* if */
        else {
          return(s1);
        } /* else */
      } /* else */
    } /* if */
    else {
      return(s1);
    } /* else */
  } /* if */
  else {
    return((char *)NULL);
  } /* else */
} /* tsdb_rcs_strip() */

char *tsdb_expand_directory(char *base, char *name) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_expand_directory()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
|*****************************************************************************|
|* <known bugs>
|* Relative path names and `~' are not expanded.
\*****************************************************************************/

  char foo[MAXPATHLEN + 1];

  if(name == NULL) {
    return((char *)NULL);
  } /* if */
  if(name[0] == TSDB_DIRECTORY_DELIMITER[0]) {
    if(name[strlen(name) - 1] == TSDB_DIRECTORY_DELIMITER[0]) {
      return(strdup(name));
    } /* if */
    else {
      (void)strcpy(&foo[0], name);
      (void)strcat(&foo[0], TSDB_DIRECTORY_DELIMITER);
      return(strdup(foo));
    } /* else */
  } /* if */
  else if(name[0] && name[0] == '.' && !name[1]) {
#if defined(SUNOS)
    if(getwd(&foo[0]) == NULL) {
      fprintf(tsdb_error_stream,
              "expand_directory(): getpw(3) error; errno: %d.\n", errno);
#else
    if(getcwd(&foo[0], MAXPATHLEN + 1) == NULL) {
      fprintf(tsdb_error_stream,
              "expand_directory(): getcwd(3) error; errno: %d.\n", errno);
#endif
      return((char *)NULL);
    } /* if */
    return(tsdb_expand_directory((char *)NULL, &foo[0]));
  } /* if */
  else {
    if(name[0] && name[0] == '.' &&  name[1] == TSDB_DIRECTORY_DELIMITER[0]
       || base == NULL) {
#if defined(SUNOS)
      if(getwd(&foo[0]) == NULL) {
        fprintf(tsdb_error_stream,
                "expand_directory(): getpw(3) error; errno: %d.\n", errno);
#else
      if(getcwd(&foo[0], MAXPATHLEN + 1) == NULL) {
        fprintf(tsdb_error_stream,
                "expand_directory(): getcwd(3) error; errno: %d.\n", errno);
#endif
        return((char *)NULL);
      } /* if */
      if(foo[strlen(foo) - 1] != TSDB_DIRECTORY_DELIMITER[0]) {
        (void)strcat(&foo[0], TSDB_DIRECTORY_DELIMITER);
      } /* if */
      if(name[0] == '.' && name[1] == TSDB_DIRECTORY_DELIMITER[0]) {
        (void)strcat(&foo[0], &name[2]);
      } /* if */
      else {
        (void)strcat(&foo[0], &name[0]);
      } /* else */
        return(tsdb_expand_directory((char *)NULL, &foo[0]));
    } /* if */
    else {
      (void)strcpy(&foo[0], &base[0]);
      if(foo[strlen(foo) - 1] != TSDB_DIRECTORY_DELIMITER[0]) {
        (void)strcat(&foo[0], TSDB_DIRECTORY_DELIMITER);
      } /* if */
      else {
        (void)strcat(&foo[0], &name[0]);
      } /* else */
        return(tsdb_expand_directory((char *)NULL, &foo[0]));
    } /* else */
  } /* else */
  return((char *)NULL);
} /* tsdb_expand_directory() */

char *tsdb_expand_file(char *directory, char *name) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_expand_file()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  char bar[MAXNAMLEN], *foo;

  if(name != NULL && name[0] == TSDB_DIRECTORY_DELIMITER[0]) {
    return(strdup(name));
  } /* if */

  if(directory != NULL) {
    if((foo = tsdb_expand_directory(tsdb.home, directory)) == NULL) {
      return((char *)NULL);
    } /* if */
  } /* if */
  else {
    if((foo = tsdb_expand_directory(tsdb.home, ".")) == NULL) {
      return((char *)NULL);
    } /* if */
  } /* else */

  (void)strcpy(&bar[0], foo);
  free(foo);
  (void)strcat(&bar[0], name);

  return(strdup(&bar[0]));

} /* tsdb_expand_file() */

char *tsdb_user() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_user()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  struct passwd *user;

  if((user = getpwuid(getuid())) != NULL) {
    return(user->pw_name);
  } /* if */
  else {
    return((char *)NULL);
  } /* else */
} /* tsdb_user() */

void tsdb_quit(void) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_quit()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  tsdb.status |= TSDB_QUIT;

} /* tsdb_quit() */

char *tsdb_canonical_date(char *date) {

  int *numeric, position;
  char *result;

  if((numeric = tsdb_parse_date(date)) == NULL) {
    return((char *)NULL);
  } /* if */

  result = (char *)malloc(21);

  if(numeric[0] != -1) {
    (void)sprintf(result, "%d-", numeric[0]);
    position = (numeric[0] >= 10 ? 3 : 2);
  } /* if */
  else {
    position = 0;
  } /* else */
  switch(numeric[1]) {
    case 1:
      (void)sprintf(&result[position], "jan-");
      break;
    case 2:
      (void)sprintf(&result[position], "feb-");
      break;
    case 3:
      (void)sprintf(&result[position], "mar-");
      break;
    case 4:
      (void)sprintf(&result[position], "apr-");
      break;
    case 5:
      (void)sprintf(&result[position], "may-");
      break;
    case 6:
      (void)sprintf(&result[position], "jun-");
      break;
    case 7:
      (void)sprintf(&result[position], "jul-");
      break;
    case 8:
      (void)sprintf(&result[position], "aug-");
      break;
    case 9:
      (void)sprintf(&result[position], "sep-");
      break;
    case 10:
      (void)sprintf(&result[position], "oct-");
      break;
    case 11:
      (void)sprintf(&result[position], "nov-");
      break;
    case 12:
      (void)sprintf(&result[position], "dec-");
      break;
  } /* switch */
  position += 4;

  (void)sprintf(&result[position], "%.4d", numeric[2]);
  position += 4;

  if(numeric[3] != -1) {
    if(numeric[5] != -1) {
      (void)sprintf(&result[position]," %.2d:%.2d:%.2d",
                    numeric[3], numeric[4], numeric[5]);
      position += 9;
    } /* if */
    else {
      (void)sprintf(&result[position]," %.2d:%.2d",
                    numeric[3], numeric[4]);
      position += 6;
    } /* else */
  } /* if */

  result[position] = 0;
  
  free(numeric);
  return(result);
      
} /* tsdb_canonical_date() */

int *tsdb_parse_date(char *date) {

  char *copy, *foo, *bar;
  int *result, i, j;

  if(date == NULL) {
    return((int *)NULL);
  } /* if */

  for(foo = date, i = 1; (foo = strchr(foo, '-')) != NULL; i++, foo++);
  for(foo = date; (foo = strchr(foo, '/')) != NULL; i++, foo++);
  for(foo = date, j = 1; (foo = strchr(foo, ':')) != NULL; j++, foo++);
  if(i < 2 || i > 3 || j < 1 || j > 3) {
    fprintf(tsdb_error_stream,
            "parse_date(): invalid date expression `%s'.\n", date);
    fflush(tsdb_error_stream);
    return((int *)NULL);
  } /* if */

  copy = (char *)malloc(strlen(date) + 2);
  copy = strcpy(copy, date);
  copy = strcat(copy, "@");
  result = (int *)malloc(6 * sizeof(int));
  if((foo = strchr(copy, '(')) != NULL) {
    *foo = ' ';
  } /* if */
  if((foo = strchr(copy, ')')) != NULL) {
    *foo = ' ';
  } /* if */
  if(i == 3) {
    foo = strtok(copy, "-/");
    result[0] = (int)strtol(foo, &bar, 10);
    if(bar == foo || result[0] < 1 || result[0] > 31) {
      fprintf(tsdb_error_stream,
              "parse_date(): invalid date expression `%s'.\n", date);
      fflush(tsdb_error_stream);
      free(copy);
      free(result);
      return((int *)NULL);
    } /* if */
    foo = strtok((char *)NULL, "-/");
  } /* if */
  else {
    result[0] = -1;
    foo = strtok(copy, "-/");
  } /* else */

  if(!strncasecmp(foo, "jan", 3)) {
    result[1] = 1;
  } /* if */
  else if(!strncasecmp(foo, "feb", 3)) {
    result[1] = 2;
  } /* if */
  else if(!strncasecmp(foo, "mar", 3)) {
    result[1] = 3;
  } /* if */
  else if(!strncasecmp(foo, "apr", 3)) {
    result[1] = 4;
  } /* if */
  else if(!strncasecmp(foo, "may", 3)) {
    result[1] = 5;
  } /* if */
  else if(!strncasecmp(foo, "jun", 3)) {
    result[1] = 6;
  } /* if */
  else if(!strncasecmp(foo, "jul", 3)) {
    result[1] = 7;
  } /* if */
  else if(!strncasecmp(foo, "aug", 3)) {
    result[1] = 8;
  } /* if */
  else if(!strncasecmp(foo, "sep", 3)) {
    result[1] = 9;
  } /* if */
  else if(!strncasecmp(foo, "oct", 3)) {
    result[1] = 10;
  } /* if */
  else if(!strncasecmp(foo, "nov", 3)) {
    result[1] = 11;
  } /* if */
  else if(!strncasecmp(foo, "dec", 3)) {
    result[1] = 12;
  } /* if */
  else {
    result[1] = strtol(foo, &bar, 10);
    if(bar == foo || result[1] < 1 || result[1] > 12) {
      fprintf(tsdb_error_stream,
              "parse_date(): invalid date expression `%s'.\n", date);
      fflush(tsdb_error_stream);
      free(copy);
      free(result);
      return((int *)NULL);
    } /* if */
  } /* else */

  foo = strtok((char *)NULL, " \t([@");
  result[2] = strtol(foo, &bar, 10);
  if(bar != foo && result[2] >= 1 && result[2] <= 99) {
    result[2] += 1900;
  } /* if */
  if(bar == foo || result[2] < 1900 || result[2] > 1999) {
    fprintf(tsdb_error_stream,
            "parse_date(): invalid date expression `%s'.\n", date);
    fflush(tsdb_error_stream);
    free(copy);
    free(result);
    return((int *)NULL);
  } /* if */

  if(j == 1) {
    result[3] = result[4] = result[5] = -1;
    return(result);
  } /* if */

  foo = strtok((char *)NULL, ":");
  result[3] = strtol(foo, &bar, 10);
  if(bar == foo || result[3] < 0 || result[3] > 23) {
    fprintf(tsdb_error_stream,
            "parse_date(): invalid date expression `%s'.\n", date);
    fflush(tsdb_error_stream);
    free(copy);
    free(result);
    return((int *)NULL);
  } /* if */

  foo = strtok((char *)NULL, ":@");
  result[4] = strtol(foo, &bar, 10);
  if(bar == foo || result[4] < 0 || result[4] > 59) {
    fprintf(tsdb_error_stream,
            "parse_date(): invalid date expression `%s'.\n", date);
    fflush(tsdb_error_stream);
    free(copy);
    free(result);
    return((int *)NULL);
  } /* if */

  if(j == 3) {
    foo = strtok((char *)NULL, "@");
    result[5] = strtol(foo, &bar, 10);
    if(bar == foo || result[5] < 0 || result[5] > 59) {
      fprintf(tsdb_error_stream,
              "parse_date(): invalid date expression `%s'.\n", date);
      fflush(tsdb_error_stream);
      free(copy);
      free(result);
      return((int *)NULL);
    } /* if */
  } /* if */
  else {
    result[5] = -1;
  } /* else */

  free(copy);
  return(result);

} /* tsdb_parse_date() */

char *tsdb_normalize_string(char *string) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_normalize_string()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 6-aug-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* 
\*****************************************************************************/

  char *foo, *bar;
  int i, j;

  foo = (char *)strdup(string);
  for(i = j = 0; foo[i]; i++, j++) {
    if(foo[i] == '\\' && foo[i + 1] == '\'' || foo[i + 1] == '"') {
      i++;
    } /* if */
    foo[j] = foo[i];
  } /* for */
  foo[j] = 0;

  bar = strdup(foo);
  free(foo);
  return(bar);

} /* tsdb_normalize_string() */

char *tsdb_prolog_escape_string(char *string) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_prolog_escape_string()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 6-aug-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* 
\*****************************************************************************/

  char *foo;
  int i, j;

  for(i = 0, foo = string;
      foo != NULL && *foo && (foo = strchr(foo, '\'')) != NULL;
      i++, foo++);
  if(i) {
    foo = (char *)malloc(strlen(string) + i + 1);
    for(i = j = 0; string[i]; i++, j++) {
      if(string[i] == '\'') {
        foo[j++] = '\'';
      } /* if */
      foo[j] = string[i];
    } /* for */
    foo[j] = (char)0;
    return(foo);
  } /* if */
  else {
    return(strdup(string));
  } /* else */

} /* tsdb_prolog_escape_string() */
