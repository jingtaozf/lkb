/*****************************************************************************\
|*        file: tsdb_tools.c
|*      module: 
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_utility.c is just too fat
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
#include <sys/types.h>
#include <sys/param.h>
#include <errno.h>
extern int errno;

#include "globals.h"
#include "tsdb.h"
#include "errors.h"


BOOL tsdb_selection_tree(Tsdb_node *node,
                         Tsdb_selection* selection) {
  int s_attributes = 4, i;
  char**
    attributes = (char **)malloc(s_attributes * sizeof(char *));
  
  if (!attributes) {
    return FALSE ;
  }
  
  memset(attributes,'\0',s_attributes*sizeof(char*));
  attributes = tsdb_condition_attributes(node,attributes,&s_attributes);
  
  if (!attributes) 
    return FALSE;
  for (i=0;attributes[i];i++) {
    if (!tsdb_attribute_in_selection(selection,attributes[i])) {
      free(attributes);
      return FALSE;
    } /* if */
  } /* for */
  free(attributes);
  return TRUE;
} /* tsdb_selection_tree() */

BOOL tsdb_verify_tuple(Tsdb_node* node, Tsdb_tuple** tuple ) {
  BOOL left;
  int* desc;
 
  if (node==NULL) 
    return TRUE;
  if (node->node->type==TSDB_CONNECTIVE) {
    left = tsdb_verify_tuple(node->left,tuple);
    if ((node->node->value.connective==TSDB_AND)&&left) {
      return tsdb_verify_tuple(node->right,tuple);
    }
    else /* AND with FALSE */
      return FALSE;

    if ((!left)&&(node->node->value.connective==TSDB_OR)) {
      return tsdb_verify_tuple(node->right,tuple);
    }
    else /* OR with TRUE */
      return TRUE;
  } /* if connective */
  
  if (node->node->type==TSDB_OPERATOR) {
    desc = node->left->node->value.descriptor;
    if (tsdb_value_satisfies(tuple[desc[0]]->fields[desc[1]],
                             node->node,
                             node->right->node))
      return TRUE;
    else 
      return FALSE;
  } /* if operator */

  return FALSE; /* I'm irritated */
} /* tsdb_verify_tuple() */

void tsdb_field_to_descriptor(Tsdb_selection* selection,Tsdb_node* n) {
  int i,j;
  Tsdb_relation* relation;

  for (i=0;relation=selection->relations[i];i++) {
    for (j=0;j<relation->n_fields;j++) {
      if (!strcmp(relation->fields[j],n->node->value.string)) {
        tsdb_free_tsdb_value(n->node);
        n->node = tsdb_descriptor(i,j);
#ifdef DTOM 
        fprintf(TSDB_DEFAULT_STREAM,"rel %d attr %d\n",i,j);
#endif     
        return;
      } /* if */         
    } /* for */
  } /* for */
  
} /* tsdb_field_to_descriptor() */

Tsdb_node* tsdb_prepare_tree(Tsdb_node* node,Tsdb_selection* selection) {
  
  if (node==NULL) 
    return NULL;
#ifdef DTOM
    tsdb_print_node(node,TSDB_DEFAULT_STREAM);
    fprintf(TSDB_DEFAULT_STREAM,"\n");
#endif
  if (node->node->type==TSDB_CONNECTIVE) {
#ifdef DTOM
    tsdb_print_node(node->left,TSDB_DEFAULT_STREAM);
    fprintf(TSDB_DEFAULT_STREAM,"\n");
#endif
    tsdb_prepare_tree(node->left,selection);
#ifdef DTOM
    tsdb_print_node(node->right,TSDB_DEFAULT_STREAM);
    fprintf(TSDB_DEFAULT_STREAM,"\n");
#endif
    tsdb_prepare_tree(node->right,selection);
  }
  if (node->node->type== TSDB_OPERATOR) {
    tsdb_field_to_descriptor(selection,node->left);
  }

  return node;
} /* tsdb_prepare_tree() */

char* tsdb_translate_table() {
  unsigned i;
  char* translate = (char *) malloc (CHAR_SET_SIZE);
  
  /* Map uppercase characters to corresponding lowercase ones.  */
  for (i = 0; i < CHAR_SET_SIZE; i++) {
    translate[i] = ISUPPER (i) ? tolower (i) : i;
  }
  return translate;
} /* tsdb_translate_table() */
      
