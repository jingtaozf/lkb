/*****************************************************************************\
|*        file: tsdb_sql.c
|*      module: TSDB query language functionality
|*     version: 
|*  written by: andrew white, tom fettig & oe (dfki saarbruecken)
|* last update: 30-jul-96
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
extern int errno;

#include "globals.h"
#include "tsdb.h"
#include "errors.h"

extern int copy_key_list_flag;

int tsdb_info(Tsdb_value **value_list, char *redirection) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_info()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  Tsdb_relation *foo;
  Tsdb_history *bar;
  FILE *output;
  int i, j, status;
  BOOL match;
  BYTE output_type;

  if(value_list != NULL) {
    if(redirection != NULL 
       && (output = tsdb_open_output(redirection)) != NULL) {
      output_type = 0;
    } /* if */
    else {
      if(tsdb.output == NULL 
         && (output = tsdb_open_pager()) != NULL) {
        output_type = 1;
      } /* if */
      else {
        output = tsdb_default_stream;
        output_type = 2;
      } /* else */
    } /* else */

    for(i = 0, match = FALSE, status = TSDB_UNKNOWN_VARIABLE_ERROR;
        value_list[i] != NULL;
        i++, status = (match ? TSDB_OK : status), match = FALSE) {
      switch(value_list[i]->type) {
        case TSDB_IDENTIFIER:
          if(!strcmp(value_list[i]->value.identifier, "relations")
             || !strcmp(value_list[i]->value.identifier, "all")) {
            if(tsdb.relations != NULL) {
              for(j = 0; tsdb.relations[j] != NULL; j++) {
                tsdb_print_relation(tsdb.relations[j], output);
              } /* for */
            } /* if */
            match = TRUE;
          } /* if */
          if(tsdb_is_relation(value_list[i])) {
            if((foo = tsdb_find_relation(value_list[i]->value.identifier))
               == NULL) {
              fprintf(tsdb_error_stream,
                      "info(): no cached table for relation `%s'.\n",
                      value_list[i]->value.identifier);
              fflush(tsdb_error_stream);
            } /* if */
            else {
              tsdb_print_relation(foo, output);
            } /* else */
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, "tsdb_home", 9)
             || !strncmp(value_list[i]->value.identifier, "home", 4)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb home: `%s'.\n", tsdb.home);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, 
                      "tsdb_relations_file", 19)
             || !strncmp(value_list[i]->value.identifier, "relations-file", 14)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb relations file: `%s'.\n", tsdb.relations_file);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, "tsdb_data_path", 14)
             || !strncmp(value_list[i]->value.identifier, "data-path", 9)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb data path: `%s'.\n", tsdb.data_path);
            match = TRUE;
          } /* if */
          if(!strcmp(value_list[i]->value.identifier, "all")
             && tsdb.output != NULL) {
            fprintf(output,
                    "tsdb output file: `%s'.\n", tsdb.output);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, "tsdb_result_path", 16)
             || !strncmp(value_list[i]->value.identifier, "result-path", 11)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb result path: `%s'.\n", tsdb.result_path);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, 
                      "tsdb_result_prefix", 18)
             || !strncmp(value_list[i]->value.identifier, "result-prefix", 13)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb result prefix: `%s'.\n", tsdb.result_prefix);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, "tsdb_max_results", 16)
             || !strncmp(value_list[i]->value.identifier, "max-results", 11)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb maximum number of results: %d.\n",
                    tsdb.max_results);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier, "tsdb_history_size", 17)
             || !strncmp(value_list[i]->value.identifier, "history-size", 12)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb size of query result storage: %d.\n",
                    tsdb.history_size);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier,
                      "tsdb_uniquely_project", 21)
             || !strncmp(value_list[i]->value.identifier,
                         "uniquely-project", 16)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb removal of duplicates from projections is %s.\n",
                    (tsdb.status & TSDB_UNIQUELY_PROJECT ? "on" : "off"));
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier,
                      "tsdb_implicit_commit", 20)
             || !strncmp(value_list[i]->value.identifier,
                         "implicit-commit", 15)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "implicit commit (and save) for new data is %s.\n",
                    (tsdb.status & TSDB_IMPLICIT_COMMIT 
                     ? "enabled" 
                     : "disabled"));
            match = TRUE;
          } /* if */
#ifdef ALEP
          if(!strncmp(value_list[i]->value.identifier,
                      "tsdb_tx_output", 14)
             || !strncmp(value_list[i]->value.identifier,
                         "tx-output", 9)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb ALEP tx() output mode is %s.\n",
                    (tsdb.status & TSDB_TX_OUTPUT ? "on" : "off"));
            match = TRUE;
          } /* if */
#endif
          if(!strncmp(value_list[i]->value.identifier,
                      "tsdb_status", 11)
             || !strncmp(value_list[i]->value.identifier,
                         "status", 6)) {
            fprintf(output, "current tsdb status: %d (0%o).\n",
                    tsdb.status, tsdb.status);
            match = TRUE;
          } /* if */
          if(!strncmp(value_list[i]->value.identifier,
                         "version", 7)
             || !strcmp(value_list[i]->value.identifier, "all")) {
            fprintf(output,
                    "tsdb(1) %s (%s) [%s] --- "
                    "(c) oe@coli.uni-sb.de.\n",
                    tsdb_version, &tsdb_revision[0], &tsdb_revision_date[0]);
            match = TRUE;
          } /* if */
          if(!match) {
            fprintf(tsdb_error_stream,
                    "info(): invalid argument `%s'.\n",
                    value_list[i]->value.identifier);
            fflush(tsdb_error_stream);
          } /* if */
          break;
        case TSDB_INTEGER:
          if((bar = tsdb_get_history(value_list[i]->value.integer)) 
             == NULL) {
            fprintf(tsdb_error_stream,
                    "info(): no history item %d.\n",
                    value_list[i]->value.integer);
            fflush(tsdb_error_stream);
          } /* if */
          else {
            fprintf(output, 
                    "[%d]", bar->command);
            if(bar->result && bar->result->relations[0]) {
              fprintf(output,
                      " `%s", bar->result->relations[0]->name);
              for(j = 1; bar->result->relations[j]; j++) {
                fprintf(output,
                        ":%s", bar->result->relations[j]->name);
              } /* for */
              fprintf(output,
                      "' (%d record(s)).", bar->result->length);
            } /* if */
            fprintf(output, "\n");
            fflush(output);
          } /* else */
          match = TRUE;
          break;
        default:
          fprintf(tsdb_error_stream, "info(): invalid argument entity.\n");
          fflush(tsdb_error_stream);
      } /* switch */
    } /* for */

    switch(output_type) {
      case 0:
        fclose(output);
        break;
      case 1:
        pclose(output);
        break;
    } /* switch */
  } /* if */
  return(status);
} /* tsdb_info() */

int tsdb_set(Tsdb_value *variable, Tsdb_value *value) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_set()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 2-aug-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

  char *foo;

  if(!strncmp(variable->value.identifier, "tsdb_result_path", 16)
     || !strncmp(variable->value.identifier, "result-path", 11)) {
    if(value->type != TSDB_STRING) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-string) type for `result-path'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      free(tsdb.result_path);
      tsdb.result_path = tsdb_expand_directory((char *)NULL,
                                               value->value.string);
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_result_prefix", 18)
          || !strncmp(variable->value.identifier, "result-prefix", 31)) {
    if(value->type != TSDB_STRING) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-string) type for `result-prefix'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      free(tsdb.result_prefix);
      tsdb.result_prefix = value->value.string;
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_max_results", 16)
          || !strncmp(variable->value.identifier, "max-results", 11)) {
    if(value->type != TSDB_INTEGER) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-integer) type for `max-results'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      if(value->value.integer < 0) {
        fprintf(tsdb_error_stream,
                "set(): invalid value (%d) for `max-results'.\n",
                value->value.integer);
        fflush(tsdb_error_stream);
      } /* if */
      else {
        tsdb.max_results = value->value.integer;
      } /* else */
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_history_size", 17)
          || !strncmp(variable->value.identifier, "history-size", 12)) {
    if(value->type != TSDB_INTEGER) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-integer) type for `history-size'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      if(value->value.integer < 0) {
        fprintf(tsdb_error_stream,
                "set(): invalid value (%d) for `history-size'.\n",
                value->value.integer);
        fflush(tsdb_error_stream);
      } /* if */
      else {
        tsdb_set_history_size(value->value.integer);
      } /* else */
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_status", 11)
          || !strncmp(variable->value.identifier, "status", 6)) {
    if(value->type != TSDB_INTEGER) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-integer) type for `status'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      if(value->value.integer < 0) {
        tsdb.status &= ~value->value.integer;
      } /* if */
      else {
        tsdb.status |= value->value.integer;
      } /* else */
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_lock", 9)
          || !strncmp(variable->value.identifier, "lock", 4)) {
    if(value->type != TSDB_STRING) {
      if(value->type != TSDB_IDENTIFIER) {
        fprintf(tsdb_error_stream, 
                "set(): invalid type for `lock'; "
                "should be `on' or `off'.\n");
        foo = (char *)NULL;
        fflush(tsdb_error_stream);
      } /* if */
      else {
        foo = value->value.identifier;
      } /* else */
    } /* if */
    else {
      foo = value->value.string;
    } /* else */
    
    if(foo != NULL) {
      if(!strcmp(foo, "on")
         && !tsdb_lock(TRUE)) {
        tsdb.status |=  TSDB_LOCK;
      } /* if */
      else if(!strcmp(foo, "off")
              && !tsdb_lock(FALSE)) {
        tsdb.status &= ~TSDB_LOCK;
      } /* if */
      else {
        fprintf(tsdb_error_stream, 
                "set(): invalid value for `lock': "
                "should be `on' or `off'.\n");
        fflush(tsdb_error_stream);
      } /* else */
    } /* if */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_implicit_commit", 20)
          || !strncmp(variable->value.identifier, "implicit-commit", 15)) {
    if(value->type != TSDB_STRING) {
      if(value->type != TSDB_IDENTIFIER) {
        fprintf(tsdb_error_stream, 
                "set(): invalid type for `implicit-commit'; "
                "should be `on' or `off'.\n");
        foo = (char *)NULL;
        fflush(tsdb_error_stream);
      } /* if */
      else {
        foo = value->value.identifier;
      } /* else */
    } /* if */
    else {
      foo = value->value.string;
    } /* else */
    
    if(foo != NULL) {
      if(!strcmp(foo, "on")) {
        tsdb.status |=  TSDB_IMPLICIT_COMMIT;
      } /* if */
      else if(!strcmp(foo, "off")) {
        tsdb.status &= ~TSDB_IMPLICIT_COMMIT;
      } /* if */
      else {
        fprintf(tsdb_error_stream, 
                "set(): invalid value for `implicit-commit': "
                "should be `on' or `off'.\n");
        fflush(tsdb_error_stream);
      } /* else */
    } /* if */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_uniquely_project", 21)
          || !strncmp(variable->value.identifier, "uniquely-project", 16)) {
    if(value->type != TSDB_STRING) {
      if(value->type != TSDB_IDENTIFIER) {
        fprintf(tsdb_error_stream, 
                "set(): invalid type for `uniquely-project'; "
                "should be `on' or `off'.\n");
        foo = (char *)NULL;
        fflush(tsdb_error_stream);
      } /* if */
      else {
        foo = value->value.identifier;
      } /* else */
    } /* if */
    else {
      foo = value->value.string;
    } /* else */
    
    if(foo != NULL) {
      if(!strcmp(foo, "on")) {
        tsdb.status |=  TSDB_UNIQUELY_PROJECT;
      } /* if */
      else if(!strcmp(foo, "off")) {
        tsdb.status &= ~TSDB_UNIQUELY_PROJECT;
      } /* if */
      else {
        fprintf(tsdb_error_stream, 
                "set(): invalid value for `uniquely-project': "
                "should be `on' or `off'.\n");
        fflush(tsdb_error_stream);
      } /* else */
    } /* if */
  } /* if */
  else if(!strncmp(variable->value.identifier, "tsdb_ofs", 8)
          || !strncmp(variable->value.identifier, "ofs", 3)) {
    if(value->type != TSDB_STRING) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-string) type for `ofs'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      free(tsdb.ofs);
      tsdb.ofs = tsdb_denormalize_string(value->value.string);
    } /* else */
  } /* if */
  else if(!strncmp(variable->value.identifier, "fs", 2)) {
    if(value->type != TSDB_STRING
       || (foo = tsdb_denormalize_string(value->value.string)) == NULL
       || strlen(foo) != 1) {
      fprintf(tsdb_error_stream,
              "set(): invalid (non-character) type for `fs'.\n");
      fflush(tsdb_error_stream);
    } /* if */
    else {
      tsdb.fs = foo[0];
    } /* else */
    if(foo != NULL) {
      tsdb_free(foo);
    } /* if */
  } /* if */
#ifdef ALEP
  else if(!strncmp(variable->value.identifier, "tsdb_tx_output", 14)
          || !strncmp(variable->value.identifier, "tx-output", 9)) {
    if(value->type != TSDB_STRING) {
      if(value->type != TSDB_IDENTIFIER) {
        fprintf(tsdb_error_stream, 
                "set(): invalid type for `tx-output'; "
                "should be `on' or `off'.\n");
        foo = (char *)NULL;
        fflush(tsdb_error_stream);
      } /* if */
      else {
        foo = value->value.identifier;
      } /* else */
    } /* if */
    else {
      foo = value->value.string;
    } /* else */
    
    if(foo != NULL) {
      if(!strcmp(foo, "on")) {
        tsdb.status |=  TSDB_TX_OUTPUT;
      } /* if */
      else if(!strcmp(foo, "off")) {
        tsdb.status &= ~TSDB_TX_OUTPUT;
      } /* if */
      else {
        fprintf(tsdb_error_stream, 
                "set(): invalid value for `tx-output': "
                "should be `on' or `off'.\n");
        fflush(tsdb_error_stream);
      } /* else */
    } /* if */
  } /* if */
#endif
  else {
    fprintf(tsdb_error_stream,
            "set(): unknown variable `%s'.\n", variable->value.identifier);
    fflush(tsdb_error_stream);
    return(TSDB_UNKNOWN_VARIABLE_ERROR);
  } /* else */
  return(TSDB_OK);
} /* tsdb_set() */

int tsdb_drop_table(Tsdb_value *table) {
  
  FILE *infp, *outfp;
  char *path; 
  Tsdb_relation *relation;

  if((infp = tsdb_find_relations_file("r")) != NULL) {

    if((outfp = fopen(TSDB_TEMPORARY_FILE,"w")) == NULL) {
      fprintf(tsdb_error_stream,
              "drop_table(): unable to create temporary file `%s' [%d].\n",
              TSDB_TEMPORARY_FILE, errno);
      return(TSDB_FILE_CREATION_ERROR);
    } /* if */
    
    while((relation = tsdb_read_relation(infp)) != NULL) {
      if(strcmp(table->value.identifier, relation->name)) {      
        tsdb_print_relation(relation, outfp);
      } /* if */
    } /* while */
    
    fclose(infp);
    fclose(outfp);

    if(rename(TSDB_TEMPORARY_FILE, tsdb.relations_file)) {
      perror("tsdb: rename(): ");
    } /* if */

    path = strdup(tsdb.data_path);
    path = (char *)realloc(path,
                           strlen(tsdb.data_path) +
                           strlen(table->value.identifier) + 1);
    path = strcat(path, table->value.identifier);
    /*unlink(path);*/
    free(path);
    tsdb_remove_relation(table->value.identifier);
  } /* if */
  return(0);
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
  
    path = strdup(tsdb.data_path);
    path = (char *)realloc(path,strlen(tsdb.data_path) + 
                           strlen(table->value.identifier) + 1);
    path = strcat(path, table->value.identifier);
    creat(path, 0644);
    free(path);
    relation = tsdb_field_2_relation(table->value.identifier, fields);
    tsdb_add_relation(relation);
  } /* if */
  return 0;
} /* tsdb_create_table() */

int tsdb_alter_table(Tsdb_value *table, Tsdb_field **fields) {
  
  FILE *input, *output;
  Tsdb_relation *relation;
  int i;

  if((input = tsdb_find_relations_file("r")) != NULL) {
  
    if((output = fopen(TSDB_TEMPORARY_FILE, "w")) == NULL) {
      fprintf(tsdb_error_stream,
              "alter_table(): unable to create temporary file `%s' [%d].\n",
              TSDB_TEMPORARY_FILE, errno);
      return(TSDB_FILE_CREATION_ERROR);
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
      for(i = 0; strcmp(tsdb.relations[i]->name, relation->name); i++);
      free(tsdb.relations[i]);
      tsdb.relations[i] = relation;
    } /* if */
    
    fclose(input);
    fclose(output);

    if(rename(TSDB_TEMPORARY_FILE, tsdb.relations_file)) {
      perror("tsdb: rename(): ");
    } /* if */
  } /* if */
  return(TSDB_OK);
} /* tsdb_alter_table() */

int tsdb_insert(Tsdb_value *table, 
                Tsdb_value **attribute_list, 
                Tsdb_value **value_list) {
  
  FILE *data;
  Tsdb_relation *relation;
  Tsdb_selection *selection;
  Tsdb_tuple *tuple, **foo;
  Tsdb_value *value, **values;
  int m, n, n_values, n_attributes;
  char *bar;
  BOOL success;
  
  if(tsdb.status & TSDB_READ_ONLY) {
    fprintf(tsdb_error_stream,
            "insert(): `%s' is read-only.\n",
            tsdb.data_path);
    fflush(tsdb_error_stream);
    return(TSDB_READ_ONLY_ERROR);
  } /* if */

  if(!tsdb_is_relation(table)) {
    fprintf(tsdb_error_stream,
            "insert(): unknown relation `%s'.\n",
            table->value.identifier);
    fflush(tsdb_error_stream);
    return(TSDB_UNKNOWN_RELATION_ERROR);
  } /* if */

  if((relation = tsdb_find_relation(table->value.identifier)) == NULL) {
    fprintf(tsdb_error_stream,
            "insert(): unable to identify relation `%s'.\n",
            table->value.identifier);
    fflush(tsdb_error_stream);
    return(TSDB_UNKNOWN_RELATION_ERROR);
  } /* if */

  tuple = (Tsdb_tuple *)malloc(sizeof(Tsdb_tuple));
  tuple->n_fields = relation->n_fields;
  
  for(n_values = 0; value_list[n_values] != NULL; n_values++);
  
  if(attribute_list != NULL) {
    for(n_attributes = 0; attribute_list[n_attributes] != NULL;
        n_attributes++) {
      bar = attribute_list[n_attributes]->value.identifier;
      if(!tsdb_attribute_in_relation(relation, bar)) {
        fprintf(tsdb_error_stream,
                "insert(): unknown attribute `%s' for `%s'.\n",
                bar, relation->name);
        fflush(tsdb_error_stream);
        return(TSDB_UNKNOWN_ATTRIBUTE_ERROR);
      } /* if */
    } /* for */
    if(n_attributes != n_values) {
      fprintf(tsdb_error_stream,
              "insert(): "
              "numbers of attributes (%d) and values (%d) mismatch.\n",
              n_attributes, n_values);
      fflush(tsdb_error_stream);
      return(TSDB_INCOMPATIBLE_SIZE_ERROR);
    } /* if */
  } /* if */

  if((data = tsdb_find_data_file(relation->name, "a+")) == NULL) {
    fprintf(tsdb_error_stream,
              "insert(): no data file for relation `%s'.\n",
            relation->name);
    fflush(tsdb_error_stream);
    fclose(data);
    return(TSDB_NO_DATA_ERROR);
  } /* if */
  fclose(data);

  (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                   (Tsdb_tuple **)NULL);
  
  tuple->fields = (Tsdb_value **)malloc(relation->n_fields 
                                        * sizeof(Tsdb_value *));
  for(n = 0; n < relation->n_fields; n++) {
    if(attribute_list != NULL) {
      for(m = 0; m < n_attributes; m++) {
        if(!strcmp(relation->fields[n],
                   attribute_list[m]->value.identifier)) {
          tuple->fields[n] = value_list[m];
          break;
        } /* if */
      } /* for */
      if(m == n_attributes) {
        if((tuple->fields[n] = tsdb_generate_default_value(relation, n)) 
           == NULL) {
          fprintf(tsdb_error_stream,
                  "insert(): "
                  "no (default) value for field `%s' in `%s'.\n",
                  relation->fields[n], relation->name);
          fflush(tsdb_error_stream);
          tsdb_free(tuple->fields);
          tsdb_free(tuple);
          return(TSDB_MISSING_VALUE_ERROR);
        } /* if */
      } /* if */
    } /* if */
    else {
      if(n < n_values) {
        tuple->fields[n] = value_list[n];
      } /* if */
      else {
        if((tuple->fields[n] = tsdb_generate_default_value(relation, n)) 
           == NULL) {
          fprintf(tsdb_error_stream,
                  "insert(): "
                  "no (default) value for field `%s' in `%s'.\n",
                  relation->fields[n], relation->name);
          fflush(tsdb_error_stream);
          tsdb_free(tuple->fields);
          tsdb_free(tuple);
          return(TSDB_MISSING_VALUE_ERROR);
        } /* if */
      } /* else */
    } /* else */
  } /* for */

  if((selection = tsdb_find_table(relation)) != NULL) {
    foo = (Tsdb_tuple **)malloc(2 * sizeof(Tsdb_tuple *));
    foo[0] = tuple;
    foo[1] = (Tsdb_tuple *)NULL;
    if(tsdb_insert_into_selection(selection, &foo[0])) {
      selection->length++;
      relation->status = TSDB_CHANGED;
    } /* if */
  } /* if */
  else {
    fprintf(tsdb_error_stream,
            "insert(): no cached table for relation `%s'.\n",
            relation->name);
    fflush(tsdb_error_stream);
  } /* else */

  if(tsdb.status & TSDB_IMPLICIT_COMMIT) {
    tsdb_commit(tsdb_singleton_value_array(table));
  } /* if */

  return(TSDB_OK);
} /* tsdb_insert() */

int tsdb_commit(Tsdb_value **relations) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_commit()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* 
\*****************************************************************************/

  int i, status;
  Tsdb_relation *relation;
  Tsdb_selection *selection;

  if(tsdb.status & TSDB_READ_ONLY) {
    fprintf(tsdb_error_stream,
            "commit(): `%s' is read-only.\n",
            tsdb.data_path);
    fflush(tsdb_error_stream);
    return(TSDB_READ_ONLY_ERROR);

    return(TSDB_READ_ONLY_ERROR);
  } /* if */

  if(relations == NULL) {
    return(tsdb_save_changes(TRUE));
  } /* if */

  for(i = 0, status = TSDB_OK; relations[i] != NULL; i++) {
    if((relation 
        = tsdb_find_relation(relations[i]->value.identifier)) == NULL) {
      fprintf(tsdb_error_stream,
              "commit(): unknown relation `%s'.\n",
              relations[i]->value.identifier);
      fflush(tsdb_error_stream);
      status = tsdb.errno;
      continue;
    } /* if */
    if((selection = tsdb_find_table(relation)) == NULL) {
      status = tsdb.errno;
      continue;
    } /* if */
    if(relation->status != TSDB_CLEAN) {
      if(tsdb_write_table(selection, TRUE) == TSDB_OK) {
        relation->status = TSDB_CLEAN;
#if defined(DEBUG) && defined(COMMIT)
        fprintf(tsdb_debug_stream,
                "commit(): written %d record(s) for `%s'.\n",
                selection->length, relation->name);
        fflush(tsdb_debug_stream);
#endif
      } /* if */
    } /* if */
#if defined(DEBUG) && defined(COMMIT)
    else {
      fprintf(tsdb_debug_stream,
              "commit(): relation `%s' is clean (%d record(s)).\n",
              relation->name, selection->length);
      fflush(tsdb_debug_stream);
    } /* else */
#endif
  } /* for */

  return(status);

} /* tsdb_commit() */

int tsdb_clean_relation(Tsdb_relation* relation, Tsdb_value* fuck)
{
  Tsdb_selection *table;
  Tsdb_key_list *list, *first, *next, *last;
  int  i;
  
  if ((table = tsdb_find_table(relation)) == NULL) {
    return(0);
  } /* if */
  for (last = NULL, first = list = table->key_lists[0]; list != NULL;) {
    if (list->tuples[0]->fields[0] == fuck) {
      /* to be freed */
      next = list->next;
      if (first == list) {
        first = next;
      } /* if */
      /* here things get freed! */
      for (i = 1; i < list->tuples[0]->n_fields; i++) {
        tsdb_free_tsdb_value(list->tuples[0]->fields[i]);
      } /* for */
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
    selection = tsdb_complex_select(condition,NULL,NULL);
    if ((selection->n_relations!=1) ||
        (strcmp(relation->name,selection->relations[0]->name))){
      fprintf(tsdb_error_stream,"Wrong Specification of attributes!\n");
      return(TSDB_UNKNOWN_RELATION_ERROR);
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
    return(TSDB_UNKNOWN_RELATION_ERROR);
  } /* else */

} /* tsdb_delete() */

int tsdb_update(Tsdb_value *table, Tsdb_node *condition) {

  /* space to be filled soon(ish) */
  return(TSDB_OK);
} /* tsdb_update */


/*****************************************************************************\
|*        file: 
|*      module: tsdb_tree_select()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* reduces selection as specified by the syntax-tree rooted with node
\*****************************************************************************/

Tsdb_selection* tsdb_tree_select(Tsdb_node* node,
                                 Tsdb_selection* selection) {
  /*
     for every tuple:
     tsdb_verify_tuple -
      Algorithmus:
      dekoriere Baum mit Wahrheitswerten
      laufe bottom-up den Baum ab und kombiniere Werte
     */

  Tsdb_key_list* list;
  int i,j=0;


  tsdb_prepare_tree(node,selection);
  
  list = selection->key_lists[0];
  
  for (i=0;i<selection->length && list;i++) {
#ifdef DTOM
    tsdb_print_tuples(list->tuples,TSDB_DEFAULT_STREAM);
#endif
    if (!tsdb_verify_tuple(node,list->tuples)) {
      list->tuples[0]=NULL;
      j++;
#ifdef DTOM
      fprintf(TSDB_DEFAULT_STREAM," NOT!!!");
#endif
    } /* if */
#ifdef DTOM
    fprintf(TSDB_DEFAULT_STREAM,"\n");
#endif
   list=list->next;
  } /* for */

#ifdef TOM
    fprintf(TSDB_DEFAULT_STREAM,"to delete: %d\n",j);
#endif
  selection = tsdb_clean_selection(selection,NULL);
#ifdef TOM
/*  tsdb_print_selection(selection,TSDB_DEFAULT_STREAM);*/
#endif
  return selection;
} /* tsdb_tree_select() */


Tsdb_selection* tsdb_complex_select(Tsdb_node *node,
                                    Tsdb_value ** wanted_relations,
                                    Tsdb_selection *history)
{
  Tsdb_selection *left,*right,*result;
  Tsdb_relation *node_relation,**all_relations;
  int i,j;
  BOOL kaerb;

  
  if (history && (tsdb_selection_tree(node,history))) {
    result = tsdb_copy_selection(history);
    tsdb_tree_select(node,result);
    return result;
  } /* if */

  if (node->node->type==TSDB_CONNECTIVE) {
    switch (node->node->value.connective) {
    case TSDB_OR:
      left = tsdb_complex_select(node->left,wanted_relations,history);
      right = tsdb_complex_select(node->right,wanted_relations,history);
      if (!left || !right) 
        return NULL;
      result = tsdb_complex_merge(left,right);
      if (!result)
        return NULL;
      if (result!=left)
        tsdb_free_selection(left);
      tsdb_free_selection(right);
      break;
    case TSDB_AND:
      left = tsdb_complex_select(node->left,wanted_relations,history);
      right = tsdb_complex_select(node->right,wanted_relations,history);
      if (!left || !right) 
        return NULL;
      result = tsdb_join(left,right);
      if (!result)
        return NULL;
      if (result != left)
        tsdb_free_selection(left);
      tsdb_free_selection(right);
      break;
    case TSDB_NOT:
    case TSDB_NOT_NOT:
      result = tsdb_complex_select(node->right,wanted_relations,history);
      break;
    default:
      result = tsdb_complex_select(node->left,wanted_relations,history);
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
      if (wanted_relations) {
        for (i=0;!kaerb && wanted_relations[i];i++)
          for (j=0;!kaerb && all_relations[j];j++) {
            if (!strcmp(wanted_relations[i]->value.string,
                        all_relations[j]->name)) {
              kaerb = TRUE;
            } /* if */
          } /* for */
      } /* if */
        
      if (kaerb)
        node_relation = 
          tsdb_find_relation(wanted_relations[i-1]->value.string);
      else
        node_relation = all_relations[0];
      result = tsdb_find_table(node_relation);
      if (!result) 
        return NULL;
      result = tsdb_select(result,&a[0],TSDB_AND);
      /* No free necessary: find_table returns global Data */
    } /* if */
  } /* else */
  return result;
} /* tsdb_complex_select() */

/*****************************************************************************\
|*        file: 
|*      module: tsdb_add_relations()
|*     version: 
|*  written by: tom, dfki saarbruecken, 19 Jul 95
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* selection may NOT be a selection given by tsdb_find_table:
|* It might be freed!!
|* 
\*****************************************************************************/

Tsdb_selection* tsdb_add_relations(Tsdb_selection* selection,
                                   Tsdb_relation** relations) 
{
  int * joined,r,kaerb,to_join,i,j;
  Tsdb_selection* table,*temp;

  for (r=0;relations[r];r++);
  to_join = r;
  joined = (int *)malloc((r+1)*sizeof(int));
  memset(joined,'\0',(r+1)*(sizeof(int)));
  if (!selection) {
    selection = tsdb_find_table(relations[0]);
    if (!selection)
      return NULL;
    selection = tsdb_copy_selection(selection);;
    joined[0] = 1;
    to_join--;
  } /* if */

  while (to_join) {
    for (i=0,kaerb=0; i<r ;i++) {
      if (!joined[i]) {
        if (tsdb_contains_relation(selection,relations[i])) {
          joined[i] = 1;
          to_join--;
          kaerb = 1;
        }
        else
          if (tsdb_joins_to(relations[i],selection)) {
            table = tsdb_find_table(relations[i]);
            if (!table)
              return(NULL);
            temp = tsdb_join(selection,table);
            if (temp!=selection) {
              tsdb_free_selection(selection);
              selection = temp;
            } /* if */
            joined[i] = 1;
            to_join--;
            kaerb = 1;
          } /* if */
      } /* if !joined */
    } /* for */
    if (!kaerb && to_join !=0) {
      for (j=0;j<r;j++)
        if (!joined[j]) {
          table = tsdb_find_table(relations[j]);
          if (!table) 
            return NULL;
          temp = tsdb_join(selection,table);
          if (temp!=selection) {
              tsdb_free_selection(selection);
              selection = temp;
            } /* if */
          joined[j] = 1;
          to_join--;
        } /* if */
    } /* if */
  } /* while */
  free(joined);

  return(selection);

} /* tsdb_add_relations() */

Tsdb_selection* tsdb_join_one_relation(Tsdb_selection* selection,
                                       Tsdb_relation** relations)
{
  /* Try to join on of the relations to selection */
  /* Does it have to be joinable???? */
  int i,smallest=-1,smallest_joinable=-1,found=0;
  int s=INT_MAX,sj=INT_MAX;
  Tsdb_selection *table;

  for (i=0;relations[i];i++) {
    table = tsdb_find_table(relations[i]);
    if (!table)
      return((Tsdb_selection *)NULL);
    if (tsdb_joins_to(relations[i],selection)) {
      found = 1;
      if (table->length < sj) {
        smallest_joinable=i;
        sj = table->length;
      }
    } /* if */
    else
      if (!found&&(table->length < s)) {
        smallest = i;
        s = table->length;
      } /* if */
  } /* for */
  
  if (found) {
    table = tsdb_find_table(relations[smallest_joinable]);
    if (!table)
      return((Tsdb_selection *)NULL);
    selection = tsdb_join(selection,table);
  } /* if */
  else {
    if (smallest==-1)
      smallest = 0;
    table = tsdb_find_table(relations[smallest]);
    if (!table)
      return((Tsdb_selection *)NULL);
    selection = tsdb_join(selection,table);
  } /* else */

  return(selection);
} /* tsdb_join_one_relation */

int tsdb_do(char *file, char *redirection) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_do()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  char *path, *buffer, *command;
  char *foo, *bar, *baz;
  FILE *input, *output, *default_stream;
  int status;
#ifdef DEBUG
  int n_commands = 0;
#endif

  command = (char *)NULL;
  status = 0;

  if((path = tsdb_expand_file((char *)NULL, file)) == NULL) {
    fprintf(tsdb_error_stream,
            "do(): unable to expand file name `%s'.\n", 
            file);
    fflush(tsdb_error_stream);
    return(TSDB_UNKNOWN_ERROR);
  } /* if */
  
  if((input = fopen(path, "r")) == NULL) {
    fprintf(tsdb_error_stream,
            "do(): unable to open file name `%s' [%d].\n", 
            path, errno);
    fflush(tsdb_error_stream);
    return(TSDB_FILE_NOT_FOUND_ERROR);
  } /* if */

#ifdef DEBUG
  fprintf(tsdb_debug_stream,
          "do(): reading `%s'.\n", path);
  fflush(tsdb_debug_stream);
#endif

  if(redirection != NULL 
     && (output = tsdb_open_output(redirection)) != NULL) {
    default_stream = tsdb_default_stream;
    tsdb_default_stream = output;
    baz = tsdb.output;
    tsdb.output = strdup(&redirection[1]);
  } /* if */
  else {
    default_stream = (FILE *)NULL;
  } /* else */
  
  buffer = (char *)malloc(4096);
  while(!(tsdb.status & TSDB_QUIT) 
        && !status 
        && fgets(&buffer[0], 4096, input) != NULL) {
#ifdef MUELL
    for(foo = &buffer[0]; *foo && isspace(*foo); foo++);
    if(*foo) {
      for(bar = &foo[strlen(foo) - 1];
          bar >= foo && isspace(*bar);
          *bar = 0, bar--);
      if(command == NULL) {
        command = strdup(foo);
      } /* if */
      else {
        command
          = (char *)realloc(command, strlen(command) + strlen(foo) + 2);
        command = strcat(command, " ");
        command = strcat(command, foo);
      } /* else */
      if(command != NULL && *command && command[strlen(command) - 1] == '.') {
        status = tsdb_parse(command, input);
        tsdb_free(command);
        command = (char *)NULL;
#ifdef DEBUG
        n_commands++;
#endif     
      } /* if */
    } /* if */
#else
    if(command == NULL) {
      command = strdup(&buffer[0]);
    } /* if */
    else {
      command
        = (char *)realloc(command, strlen(command) + strlen(&buffer[0]) + 1);
      command = strcat(command, &buffer[0]);
    } /* else */
    if(command != NULL) {
      for(foo = &command[strlen(command) - 1];
          foo >= &command[0] && isspace(*foo);
          foo--);
      if(foo > &command[0] 
         && tsdb_quotes_are_balanced(&command[0]) == -1
         && *foo == '.') {
        for(foo++; *foo; foo++) {
          *foo = (char)0;
        } /* for */
        status = tsdb_parse(command, input);
        tsdb_free(command);
        command = (char *)NULL;    
#ifdef DEBUG
        n_commands++;
#endif     
      } /* if */
    } /* if */
#endif
  } /* while */

  if(command != NULL) {
    fprintf(tsdb_error_stream,
            "do(): ignoring incomplete last command from `%s'.\n",
            path);
    fflush(tsdb_error_stream);
  } /* if */

  tsdb.status &= ~TSDB_QUIT;
  fclose(input);

#ifdef DEBUG
  fprintf(tsdb_debug_stream,
          "do(): read %d command(s) from `%s'.\n", n_commands, path);
  fflush(tsdb_debug_stream);
#endif

  if(default_stream != NULL) {
    fclose(tsdb_default_stream);
    tsdb_default_stream = default_stream;
    free(tsdb.output);
    tsdb.output = baz;
  } /* if */

  tsdb_free(path);
  return(status);

} /* tsdb_do() */

void tsdb_shutdown(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_shutdown()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_shutdown() attempts to shutdown a running tsdb(1) server; this will
|* only be allowed for servers that have no more than one active client.  yet,
|* as many calls to tsdb_shutdown() as there are servers will ultimately
|* succeed.
\*****************************************************************************/

  if(tsdb.status & TSDB_SERVER_MODE) {
    tsdb_server_shutdown(signal);
  } /* if */
  else if(tsdb.status & TSDB_CLIENT_MODE) {
    
  } /* if */
  else {
    tsdb_quit();
  } /* else */

} /* tsdb_shutdown() */

int tsdb_retrieve(Tsdb_value **relation_list,
                  Tsdb_value **attribute_list,
                  Tsdb_node *conditions,
                  char *report,
                  char *redirection) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_retrieve()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 29-jul-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* retrieve function to be called by the parser.
\*****************************************************************************/

  Tsdb_selection* result ;

  tsdb.errno = TSDB_OK;

  if((result = tsdb_complex_retrieve(relation_list,
                                     attribute_list, conditions,
                                     report, redirection)) != NULL) {
    tsdb_add_to_history(result);
  } /* if */

#if defined(DEBUG) && defined(TOM)
  if (copy_key_list_flag) {
    printf("tsdb_copy_keylist was just called... strange\n");
    copy_key_list_flag=0;
  } /* if */
#endif

  return((result == NULL ? tsdb.errno : TSDB_OK));

} /* tsdb_retrieve() */

Tsdb_selection *tsdb_complex_retrieve(Tsdb_value **relation_list,
                                      Tsdb_value **attribute_list,
                                      Tsdb_node* conditions,
                                      char *report,
                                      char *redirection) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_complex_retrieve()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 2-aug-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* tom said: ``final retrieve() function'' --- i suppose he is mistaken |:-}.
\*****************************************************************************/

  char **attributes = NULL;
  Tsdb_relation **a_relations;
  int s_attributes = 10, i, j, r, kaerb = 0;
  Tsdb_selection *selection = NULL, *temp, *history = NULL;
  Tsdb_history *foo;
  FILE *output;
  BOOL from_find = FALSE, history_retrieve = FALSE, extend_history = FALSE;

  /* 
     1. find out about attributes in conditions, transform expression
     2. build a set of used relations
     3. give a relation to every attribute
     4. enter complex_select with empty selections
     5. print the resulting selection
     
     */

#if defined(ALEP)
  if(tsdb.status & TSDB_TX_OUTPUT) {
    if(attribute_list != NULL) {
      tsdb_free_tsdb_values(attribute_list);
    } /* if */
    attribute_list = (Tsdb_value **)malloc(4 * sizeof(Tsdb_value *));
    attribute_list[0] = tsdb_identifier("i-id");
    attribute_list[1] = tsdb_identifier("i-input");
    attribute_list[2] = tsdb_identifier("i-comment");
    attribute_list[3] = (Tsdb_value *)NULL;
    tsdb_free(report);
    report = strdup(TSDB_TX_FORMAT);
  } /* if */
#endif

  if ((relation_list) && (relation_list[0]->type==TSDB_INTEGER)) {
    foo = tsdb_get_history(relation_list[0]->value.integer);
    if (foo==NULL) {
      fprintf(tsdb_error_stream,"retrieve(): no history item %d.\n",
              relation_list[0]->value.integer);
      tsdb.errno = TSDB_NO_HISTORY_ITEM_ERROR;
      return((Tsdb_selection *)NULL);
    } /* if */
    history = foo->result;
    history_retrieve = TRUE;
  } /* if */
  
  if (conditions) {
#if defined(DEBUG) && defined(TOM)
    tsdb_tree_print(conditions, tsdb_debug_stream);
#endif
    tsdb_check_not(conditions);
#if defined(DEBUG) && defined(TOM)
    tsdb_tree_print(conditions, tsdb_debug_stream);
#endif
    attributes = (char **)malloc(s_attributes * sizeof(char *));
    if (!attributes) {
      tsdb.errno = TSDB_OS_ERROR;
      return((Tsdb_selection *)NULL);
    }
    memset(attributes,'\0',s_attributes*sizeof(char*));
    attributes
      = tsdb_condition_attributes(conditions, attributes, &s_attributes);
    /* attribute check is done in tsdb_condition_attributes!! */
    if (!attributes) {
      tsdb.errno = TSDB_OS_ERROR;
      return((Tsdb_selection *)NULL);
    } /* if */
  } /* if conditions */
  
  if(attribute_list) /* check explicit attribute names! */{
    kaerb = 0;
    for(i = 0; attribute_list[i]; i++) {
      if (!tsdb_is_attribute(attribute_list[i])) {
        fprintf(tsdb_error_stream,
                "complex_retrieve(): unknown attribute `%s'.\n",
                attribute_list[i]->value.string);
        kaerb = 1;
      } /* if */
      else if(history_retrieve && 
              (!tsdb_attribute_in_selection(history,
                                            attribute_list[i]->value.string))) {
        extend_history = TRUE;
      } /* if */
    } /* for */
    if (kaerb) {
      if (attributes) {
        free(attributes);
      } /* if */
      tsdb.errno = TSDB_UNKNOWN_ATTRIBUTE_ERROR;
      return((Tsdb_selection *)NULL);
    } /* if */
  } /* if */
  
  if(relation_list && !history) {
    kaerb = 0;
    for(r=0;relation_list[r];r++) {
      if (!tsdb_is_relation(relation_list[r])) {
        fprintf(tsdb_error_stream,
                "complex_retrieve(): unkown relation `%s'.\n",
                relation_list[r]->value.string);
        kaerb = 1;
      } /* if */
    } /* for */
    if (kaerb) {
      if (attributes) {
        free(attributes);
      } /* if */
      tsdb.errno = TSDB_UNKNOWN_RELATION_ERROR;
      return((Tsdb_selection *)NULL);
    } /* if */
  } /* else */ 
  else
    r = tsdb_n_relations();
  

  if (conditions) {
    if (history_retrieve) {
      history = tsdb_copy_selection(history);
      selection = tsdb_complex_select(conditions,NULL,history);
      if (tsdb_relation_in_selection(selection,
                                     history->relations[0]->name)==-1) {
        selection = tsdb_join(selection,history);
      } /* if */
    }
    else
      selection = tsdb_complex_select(conditions, relation_list,NULL);
    if (!selection) {
      return((Tsdb_selection *)NULL);
    }
  } /* if */
  else  /* no conditions!! */
    if (history_retrieve) {
      selection = tsdb_copy_selection(history);
    } /* if */
  
  /* selection is our result!! */
  
  if (relation_list && !history_retrieve) {
    for(r = 0; relation_list && relation_list[r]; r++) ;
    a_relations = (Tsdb_relation **)malloc((r+1)*sizeof(Tsdb_relation*));
    for (i=0;i<r;i++) {
      a_relations[i] = tsdb_find_relation(relation_list[i]->value.string);
    } /* for */
    a_relations[i] = NULL;
    temp = tsdb_add_relations(selection,a_relations);
    if (!temp) {
      return((Tsdb_selection *)NULL);
    }
    if (temp!=selection) {
      if (selection) 
        tsdb_free_selection(selection);
      selection = temp;
    } /* if */
    free(a_relations);
    a_relations = NULL; 
  } /* if */

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
  fprintf(tsdb_debug_stream,"printing selection\n");
  tsdb_print_selection(selection,tsdb_debug_stream);
#endif
  
  for(i = 0; relation_list && relation_list[i]; i++) ;

  if (attribute_list) { /* else */
    j=0;
    for(;attribute_list && attribute_list[j]; j++) ;
    /* check from relations */
    for (i=0;attribute_list && attribute_list[i];i++) {
      if (!tsdb_attribute_in_selection(selection,
                                       attribute_list[i]->value.identifier)) {
        a_relations = tsdb_attribute_relations(attribute_list[i]);
        for (j=0;a_relations[j];j++) {
          if (tsdb_relation_in_selection(selection,a_relations[j]->name) 
              ==-1) {
            if (selection==NULL) {
              from_find = TRUE;
              selection = tsdb_find_table(a_relations[0]);
              if (!selection) {
                return((Tsdb_selection *)NULL);
              } /* if */
            } /* if */
            else {
              Tsdb_selection *temp;
              temp = selection;
              selection = tsdb_join_one_relation(selection,a_relations);
              if (!selection) {
                return((Tsdb_selection *)NULL);
              } /* if */
              if (!from_find) {
                tsdb_free_selection(temp);
              } /* if */
              from_find = FALSE;
            } /* else */
            a_relations[i+1]=NULL;
          } /* if */
        } /* for */
        free(a_relations);
      } /* if */
    }/* for */
  /* in case of history: join the history with the selection?? */

  } /* else */


  if (attributes)
    free(attributes);

  /* now check the attributes for projections */

  if(redirection != NULL 
     && (output = tsdb_open_output(redirection)) != NULL) {
    tsdb_project(selection, attribute_list, report, output);
    fclose(output);
  } /* if */
  else {
    if(tsdb.output == NULL 
       && (output = tsdb_open_pager()) != NULL) {
      tsdb_project(selection, attribute_list, report, output);
      pclose(output);
    } /* if */
    else {
      tsdb_project(selection, attribute_list, report, tsdb_default_stream);
      fflush(tsdb_default_stream);
    } /* else */
  } /* else */
  return(selection);

} /* tsdb_complex_retrieve */

void tsdb_project(Tsdb_selection *selection,
                  Tsdb_value **attributes, 
                  char* format,
                  FILE* stream) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_project()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  /* print attributes in order */
  int i, j, k,h,l,m, n, n_attributes,sum_attr=0;
  Tsdb_relation *relation;
  Tsdb_key_list *list;
  char **fields,**projection;

  int *r, *f;
  BOOL kaerb;
  FILE* output;

  if (attributes) {
    for (n_attributes = 0; attributes[n_attributes]; n_attributes++);
    r = (int *)malloc(n_attributes * sizeof(int));
    f = (int *)malloc(n_attributes * sizeof(int));
  } /* if */
  else {
    for (l=0;l<selection->n_relations;l++) {
      sum_attr += selection->relations[l]->n_fields;
    } /* for */
    r = (int *)malloc(++sum_attr * sizeof(int));
    f = (int *)malloc(sum_attr * sizeof(int));
    n_attributes = 0;
    for (l=0;l<selection->n_relations;l++) {
      relation = selection->relations[l];
      for (h=0;h<relation->n_fields;h++) {
        for (kaerb=FALSE,m=0; (m<l) && (!kaerb) ; m++) {
          if (tsdb_attribute_in_relation(selection->relations[m],
                                          relation->fields[h]))
            kaerb = TRUE;
        } /* for m */
        if (!kaerb) {
          r[n_attributes] = l;
          f[n_attributes] = h;
          n_attributes++;
        } /* if */
        else 
          r[n_attributes] = -1;
      } /* for h */
    } /* for */
  } /* else */
  
  if (attributes) {
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
        fprintf(tsdb_error_stream,
                "project(): no attribute `%s' in relation %s",
                attributes[k]->value.string, selection->relations[0]->name);
        for(i = 1; i < selection->n_relations; i++) {
          fprintf(tsdb_error_stream, ":%s", selection->relations[i]->name);
        } /* for */
        fprintf(tsdb_error_stream, ".\n");
        return;
      }      
    } /* for */ 
    /* attribute[k] will be printed from relation r[k] and field f[k] */
  } /* if */
#if defined(DEBUG) && defined(TOM)
  for (i=0;i<selection->n_key_lists;i++) {
    fprintf(tsdb_debug_stream," Key list %d\n",i);
  } /* for */
#endif

  relation = selection->relations[n=0];
  
#if defined(DEBUG) && defined(TOM)
  if (!selection->key_lists[0]) {
    fprintf(tsdb_debug_stream," Key list %s is empty\n",
            relation->fields[relation->keys[0]]);
  }
  else
    fprintf(tsdb_debug_stream,"Key list %s\n",
            relation->fields[relation->keys[0]]);
#endif

  projection = (char**)malloc((selection->length+1)*sizeof(char*));
  if (projection) {
    memset(projection,'\0',(selection->length+1)*sizeof(char*));
    
    for (n=0,list = selection->key_lists[0]; 
         list!=NULL; list=list->next,n++) {
      projection[n]=tsdb_sprint_key_list(list,r,f,n_attributes);
      if (!projection[n])
        return;
      
    } /* for key_list */
  } /* if */

#if defined(DEBUG) && defined(TOM) && defined(CRAZY)
  fprintf(tsdb_debug_stream,"project:\n");
  for (k=0;k<n;k++) {
    fprintf(tsdb_debug_stream,"%s\n",projection[k]);
  } /* for */
#endif    


  /* _hack_
   * frob report string (if not specified in query) to incorporate .ofs.
   *                                                 (2-aug-96  -  oe)
   */
  if(format == NULL
     && n_attributes) {
    format = (char *)malloc((2 * n_attributes)
                            + (strlen(tsdb.ofs) * (n_attributes - 1))
                            + 1);
    format = strcpy(format, "%s");
    for(i = 1; i < n_attributes; i++) {
      format = strcat(format, tsdb.ofs);
      format = strcat(format, "%s");
    } /* for */
  } /* if */

  if(tsdb.status & TSDB_UNIQUELY_PROJECT) {
    n = tsdb_uniq_projection(projection,n);
  } /* if */
  tsdb_print_projection(projection, n, format, stream);
  if (output=tsdb_open_result()) {
    tsdb_print_projection(projection, n, format, output);
    fclose(output);
  } /* if */
  tsdb_free_char_array(projection,n);
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
  Tsdb_relation *relation_1;
  Tsdb_key_list *list;
  Tsdb_tuple **new_tuples;
  int *relation, *field;
  int n_conditions, i, j, k, vm_result;
  char *attribut;
  BOOL kaerb, match;
  BYTE *results;
  
  if (!selection->length) return(NULL);
  for(n_conditions = 0; conditions[n_conditions] != NULL; n_conditions++);

  relation = (int *)calloc(n_conditions,sizeof(int));
  field = (int *)calloc(n_conditions,sizeof(int));
  results = (BYTE *)calloc(n_conditions,sizeof(BYTE));
  
  for (i=0; i<n_conditions ; i++) {
    attribut = conditions[i]->left->node->value.identifier;
    relation[i] = -1;
    field[i] = -1;
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
      if (results[i] == TSDB_VALUE_INCOMPATIBLE) 
        return NULL;
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
        case TSDB_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,0,NULL);
          if (vm_result==3) {
            return(NULL);
          }
          match = match || vm_result;
          break;
        case TSDB_NOT_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                               conditions[i]->right->node,0,NULL);
          if (vm_result==3) {
            return NULL;
          } 
          match = match || !(vm_result); 
          break;
        case TSDB_INSENSITIVE_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,
                             TSDB_INSENSITIVE_MATCH,NULL);
          if (vm_result==3) {
            return(NULL);
          }
          match = match || vm_result;
          break;
        case TSDB_NOT_INSENSITIVE_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                               conditions[i]->right->node,
                             TSDB_INSENSITIVE_MATCH,NULL);
          if (vm_result==3) {
            return NULL;
          } 
          match = match || !(vm_result); 
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
        case TSDB_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,0,NULL);
          if (vm_result==3)
            return NULL;
          match = match && vm_result;
          break;
        case TSDB_NOT_MATCH:
           vm_result = 
             tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                              conditions[i]->right->node,0,NULL);
          if (vm_result==3)
            return NULL;
          match = match && !(vm_result);
          break;
        case TSDB_INSENSITIVE_MATCH:
          vm_result = 
            tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                             conditions[i]->right->node,
                             TSDB_INSENSITIVE_MATCH,NULL);
          if (vm_result==3)
            return NULL;
          match = match && vm_result;
          break;
        case TSDB_NOT_INSENSITIVE_MATCH:
           vm_result = 
             tsdb_value_match(list->tuples[relation[i]]->fields[field[i]],
                              conditions[i]->right->node,
                              TSDB_INSENSITIVE_MATCH,NULL);
          if (vm_result==3)
            return NULL;
          match = match && !vm_result;
          break;
        } /* switch */
      } /* if */
    } /* for all conditions */
    if (match) /* tuples matches all conditions */ {
      new_tuples = (Tsdb_tuple**)
        calloc(selection->n_relations+1,sizeof(Tsdb_tuple**));
      memcpy(new_tuples,list->tuples,
             ((selection->n_relations+1)*sizeof(Tsdb_tuple**)) );
      if (tsdb_insert_into_selection(result,new_tuples)) {
        result->length++;
      } /* if */
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
  if(n_neighbours) {
    fprintf(tsdb_debug_stream,
            "real_join_path(): neighbours: %s",
            (n_neighbours ? neighbours[0]->name : ""));
    for(i = 1; i < n_neighbours; i++) {
      fprintf(tsdb_debug_stream, " | %s", neighbours[i]->name);
    } /* for */
    fprintf(tsdb_debug_stream, "\n");
    fflush(tsdb_debug_stream);
  } /* if */
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
