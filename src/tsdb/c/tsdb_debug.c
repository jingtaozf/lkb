/*****************************************************************************\
|*        file: tsdb_debug.c
|*      module: debug routines for the TSDB project
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 30-jun-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

#include <signal.h>
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

BOOL tsdb_verify_selection(Tsdb_selection *selection) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_verify_selection()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_verify_selection() makes an attempt to check the integitry of
|* .selection.
\*****************************************************************************/

#ifdef DEBUG
  Tsdb_relation *foo;
  Tsdb_key_list *next;
  int i, j, k;

  if(selection != NULL) {
    fprintf(tsdb_debug_stream,
            "verify_selection(): %s", selection->relations[0]->name);
    for(i = 1; i < selection->n_relations; i++) {
      fprintf(tsdb_debug_stream, ":%s", selection->relations[i]->name);
    } /* for */
    fprintf(tsdb_debug_stream, " (%d).\n", selection->length);
    fflush(tsdb_debug_stream);

    /* verify that all key lists have equal length */
    for(i = 0; i < selection->n_key_lists; i++) {
      if(selection->key_lists[i] != NULL) {
        for(next = selection->key_lists[i], j = 0;
            next != NULL;
            next = next->next, j++);
        if(j != selection->length) {
          fprintf(tsdb_debug_stream,
                  "verify_selection(): key list # %d has invalid length %d.\n",
                  i, j);
          fflush(tsdb_debug_stream);
          return(FALSE);
        } /* if */
      } /* if */
      else {
        fprintf(tsdb_debug_stream,
                "verify_selection(): key list # %d is empty.\n", i);
        fflush(tsdb_debug_stream);
        return(FALSE);
      } /* else */
    } /* for */

    /* verify total number of key lists vs. the individual relations */
    for(i = 0, j = 0;
        i < selection->n_relations;
        j += selection->relations[i]->n_keys, i++);
    if(j != selection->n_key_lists) {
      fprintf(tsdb_debug_stream,
              "verify_selection(): invalid number of key lists.\n");
      fflush(tsdb_debug_stream);
      return(FALSE);
    } /* if */
  } /* if */
  else {
    fprintf(tsdb_debug_stream, "verify_selection(): empty selection.\n");
    fflush(tsdb_debug_stream);
  } /* else */
#endif
  return(TRUE);
} /* tsdb_verify_selection() */

void tsdb_debug_join_path(Tsdb_value **source_attribute_list,
                          Tsdb_value **target_attribute_list) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_debug_join_path()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_debug_join_path() determines the (shortest) join path from
|* .source_attribute_list. to .target_attribute_list. 
\*****************************************************************************/

  Tsdb_relation **sources, **targets;
  int i;
  Tsdb_relation **path;
  
  for(i = 0; source_attribute_list[i] != NULL; i++);
  sources = (Tsdb_relation **)malloc((i + 1) * sizeof(Tsdb_relation *));
  for(i = 0; source_attribute_list[i] != NULL; i++) {
    if(tsdb_is_relation(source_attribute_list[i])) {
      sources[i]
        = tsdb_find_relation(source_attribute_list[i]->value.identifier);
    } /* if */
    else {
      fprintf(TSDB_ERROR_STREAM,
              "debug_join_path(): unknown relation `%s'\n",
              source_attribute_list[i]->value.identifier);
      tsdb_free_tsdb_values(source_attribute_list);
      tsdb_free_tsdb_values(target_attribute_list);
      free(sources);
      return;
    } /* else */
  } /* for */
  sources[i] = (Tsdb_relation *)NULL;

  for(i = 0; target_attribute_list[i] != NULL; i++);
  targets = (Tsdb_relation **)malloc((i + 1) * sizeof(Tsdb_relation *));
  for(i = 0; target_attribute_list[i] != NULL; i++) {
    if(tsdb_is_relation(target_attribute_list[i])) {
      targets[i]
        = tsdb_find_relation(target_attribute_list[i]->value.identifier);
    } /* if */
    else {
      fprintf(stderr, "debug_join_path(): unknown relation `%s'\n",
              target_attribute_list[i]->value.identifier);
      tsdb_free_tsdb_values(source_attribute_list);
      tsdb_free_tsdb_values(target_attribute_list);
      free(sources);
      free(targets);
      return;
    } /* else */
  } /* for */
  targets[i] = (Tsdb_relation *)NULL;

  if((path = tsdb_join_path(sources, targets)) != NULL) {
    fprintf(TSDB_DEFAULT_STREAM, "%s", path[0]->name);
    for(i = 1; path[i] != NULL; i++) {
      fprintf(TSDB_DEFAULT_STREAM, " -- %s", path[i]->name);
    } /* for */
    fprintf(TSDB_DEFAULT_STREAM, ".\n");
  } /* if */
  else {
    fprintf(TSDB_DEFAULT_STREAM,
            "debug_join_path(): no path from here to there.\n");
  } /* else */
  tsdb_free_tsdb_values(source_attribute_list);
  tsdb_free_tsdb_values(target_attribute_list);
  free(sources);
  free(targets);

} /* tsdb_debug_join_path() */

void tsdb_debug_simple_join(Tsdb_value **attribute_list_1,
                            Tsdb_value **attribute_list_2) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_debug_simple_join()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  Tsdb_relation **relations_1, **relations_2;
  Tsdb_selection **selections_1, **selections_2;
  Tsdb_selection *selection_1, *selection_2, *result;
  FILE *output;
  int i;

  for(i = 0; attribute_list_1[i] != NULL; i++);
  relations_1 = (Tsdb_relation **)malloc((i + 1) * sizeof(Tsdb_relation *));
  for(i = 0; attribute_list_1[i] != NULL; i++) {
    if(tsdb_is_relation(attribute_list_1[i])) {
      relations_1[i]
        = tsdb_find_relation(attribute_list_1[i]->value.identifier);
    } /* if */
    else {
      fprintf(TSDB_ERROR_STREAM,
              "debug_simple_join(): unknown relation `%s'\n",
              attribute_list_1[i]->value.identifier);
      tsdb_free_tsdb_values(attribute_list_1);
      tsdb_free_tsdb_values(attribute_list_2);
      free(relations_1);
      return;
    } /* else */
  } /* for */
  relations_1[i] = (Tsdb_relation *)NULL;
  selections_1 = (Tsdb_selection **)malloc((i + 1) * sizeof(Tsdb_selection *));
  for(i = 0; relations_1[i] != NULL; i++) {
    if((selections_1[i] = tsdb_find_table(relations_1[i])) == NULL) {
      fprintf(TSDB_ERROR_STREAM,
              "debug_simple_join(): no table for relation `%s'\n",
              relations_1[i]->name);
      tsdb_free_tsdb_values(attribute_list_1);
      tsdb_free_tsdb_values(attribute_list_2);
      free(relations_1);
      free(selections_1);
      return;
    } /* if */
  } /* for */
  if(i >= 2) {
    for(i = 1; selections_1[i] != NULL; i++) {
      if((selection_1 = tsdb_simple_join(selections_1[i - 1],
                                         selections_1[i])) == NULL) {
        fprintf(TSDB_ERROR_STREAM,
                "debug_simple_join(): empty selection after join with `%s'.\n",
                relations_1[i]);
        tsdb_free_tsdb_values(attribute_list_1);
        tsdb_free_tsdb_values(attribute_list_2);
        free(relations_1);
        free(selections_1);
        return;
      } /* if */
    } /* for */
  } /* if */
  else {
    selection_1 = selections_1[0];
  } /* else */

  for(i = 0; attribute_list_2[i] != NULL; i++);
  relations_2 = (Tsdb_relation **)malloc((i + 1) * sizeof(Tsdb_relation *));
  for(i = 0; attribute_list_2[i] != NULL; i++) {
    if(tsdb_is_relation(attribute_list_2[i])) {
      relations_2[i]
        = tsdb_find_relation(attribute_list_2[i]->value.identifier);
    } /* if */
    else {
      fprintf(TSDB_ERROR_STREAM,
              "debug_simple_join(): unknown relation `%s'\n",
              attribute_list_2[i]->value.identifier);
      tsdb_free_tsdb_values(attribute_list_1);
      tsdb_free_tsdb_values(attribute_list_2);
      free(relations_1);
      free(relations_2);
      return;
    } /* else */
  } /* for */
  relations_2[i] = (Tsdb_relation *)NULL;
  selections_2 = (Tsdb_selection **)malloc((i + 1) * sizeof(Tsdb_selection *));
  for(i = 0; relations_2[i] != NULL; i++) {
    if((selections_2[i] = tsdb_find_table(relations_2[i])) == NULL) {
      fprintf(TSDB_ERROR_STREAM,
              "debug_simple_join(): no table for relation `%s'\n",
              relations_2[i]->name);
      tsdb_free_tsdb_values(attribute_list_1);
      tsdb_free_tsdb_values(attribute_list_2);
      free(relations_1);
      free(relations_2);
      free(selections_1);
      free(selections_2);
      return;
    } /* if */
  } /* for */
  if(i >= 2) {
    for(i = 1; selections_2[i] != NULL; i++) {
      if((selection_2 = tsdb_simple_join(selections_2[i - 1],
                                         selections_2[i])) == NULL) {
        fprintf(TSDB_ERROR_STREAM,
                "debug_simple_join(): empty selection after join with `%s'.\n",
                relations_2[i]);
        tsdb_free_tsdb_values(attribute_list_1);
        tsdb_free_tsdb_values(attribute_list_2);
        free(relations_1);
        free(relations_2);
        free(selections_1);
        free(selections_2);
        return;
      } /* if */
    } /* for */
  } /* if */
  else {
    selection_2 = selections_2[0];
  } /* else */

  if((result = tsdb_simple_join(selection_1, selection_2)) != NULL) {
    (void)tsdb_verify_selection(result);
    if((output = tsdb_open_result()) != NULL) {
      tsdb_print_selection(result, output);
    } /* if */
    else {
      tsdb_print_selection(result, TSDB_DEFAULT_STREAM);
    } /* else */
  } /* if */
} /* tsdb_debug_simple_join() */
