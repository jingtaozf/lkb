/*****************************************************************************\
|*        file: tsdb_io.c
|*      module: 
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
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
#include <malloc.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/time.h>

#include "globals.h"
#include "tsdb.h"
#include "errors.h"

int tsdb_parse(char *command) {

  int foo;

  tsdb_input = command;
  foo = yyparse();
  yyrestart(stdin);

  return(foo);

} /* tsdb_parse() */

int tsdb_getchar(void) {

  return((*tsdb_input ? *tsdb_input++ : EOF));

} /* tsdb_getchar */

FILE* tsdb_open_pager()
{

  FILE* tsdb_stream=NULL;

  if (tsdb_pager=getenv("PAGER"))
      tsdb_stream = popen(tsdb_pager,"w");
  if (tsdb_stream) return(tsdb_stream);

  tsdb_stream = popen("more","w");
  if (tsdb_stream) return(tsdb_stream);
  
  tsdb_stream = popen("page","w");
  if (tsdb_stream) 
    return(tsdb_stream);
  
} /* tsdb_open_pager */

FILE *tsdb_open_debug() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_open_debug()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_open_debug() opens (and creates, if necessary) the TSDB debug trace
|* file and resets the file pointer to its beginning.
\*****************************************************************************/

#ifdef DEBUG
  FILE *output;
  char *name, *file, *date, *user;
  time_t clock;

  if(time(&clock) != (time_t)-1) {
    if((date = ctime(&clock)) != NULL) {
      date[strlen(date) - 1] = 0;
    } /* if */
    user = getenv("USER");
  } /* if */
     
  if((name = getenv("TSDB_DEBUG_FILE")) != NULL) {
    file = (char *)malloc(strlen(name)
                          + (user != NULL ? 1 + strlen(user) : 0) + 1);
    file = strdup(name);
  } /* if */
  else {
    file = (char *)malloc(strlen(TSDB_DEBUG_FILE)
                          + (user != NULL ? 1 + strlen(user) : 0) + 1);
    file = strdup(TSDB_DEBUG_FILE);
  } /* else */
  if(user != NULL) {
    file = strcat(file, ".");
    file = strcat(file, user);
  } /* if */

  if((output = fopen(file, "w")) != NULL) {
    fprintf(output,
            "TSDB debug opened by %s on %s.\n\n",
            (user != NULL ? user : ""),
            (date != NULL ? date : ""));
    fflush(output);
    free(file);
    return(output);
  } /* if */

  fprintf(TSDB_ERROR_STREAM,
          "open_debug(): unable to open file `%s'.\n",
          (file != NULL ? file : TSDB_DEBUG_FILE));
  if(file != NULL) {
    free(file);
  } /* if */
#endif    
  return((FILE *)NULL);
} /* tsdb_open_debug() */

void tsdb_close_debug(FILE *stream) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_close_debug()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 12-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

#ifdef DEBUG
  char *date;
  time_t clock;

  if(time(&clock) != (time_t)-1) {
    if((date = ctime(&clock)) != NULL) {
      date[strlen(date) - 1] = 0;
    } /* if */
  } /* if */
     
  fprintf(stream, "\nTSDB debug closed on %s.\n", (date != NULL ? date : ""));
  fclose(stream);
#endif
} /* tsdb_close_debug() */

/*--------------------------- print functions ------------------------------*/
BOOL tsdb_print_value(Tsdb_value *value, FILE *stream) {
  int r;

  switch(value->type) {
    case TSDB_INTEGER:
      r=fprintf(stream, "%d", value->value.integer);
      break;
    case TSDB_IDENTIFIER:
      r=fprintf(stream, "%s", value->value.identifier);
      break;
    case TSDB_STRING:
      r=fprintf(stream, "%s", value->value.string);
      break;
    case TSDB_CONNECTIVE:
      switch(value->value.connective) {
        case TSDB_AND:
          r=fprintf(stream, "&&");
          break;
        case TSDB_OR:
          r=fprintf(stream, "||");
          break;
        case TSDB_NOT:
          r=fprintf(stream, "!");
          break;
        case TSDB_NOT_NOT:
          r=fprintf(stream, " ");
          break;
        case TSDB_BRACE:
          r=fprintf(stream, "() ");
          break;
  } /* switch */
      break;
    case TSDB_OPERATOR:
      switch(value->value.operator) {
        case TSDB_EQUAL:
          r=fprintf(stream, "==");
          break;
        case TSDB_NOT_EQUAL:
          r=fprintf(stream, "!=");
          break;
        case TSDB_LESS_THAN:
          r=fprintf(stream, "<");
          break;
        case TSDB_LESS_OR_EQUAL_THAN:
          r=fprintf(stream, "<=");
          break;
        case TSDB_GREATER_THAN:
          r=fprintf(stream, ">");
          break;
        case TSDB_GREATER_OR_EQUAL_THAN:
          r=fprintf(stream, ">=");
          break;
        case TSDB_SUBSTRING:
          r=fprintf(stream, "~");
          break;
        case TSDB_NOT_SUBSTRING:
          r=fprintf(stream, "!~");
          break;
      } /* switch */
      printf("%c", value->value.operator);
      break;
    default:
      if(really_verbose_mode) 
        fprintf(TSDB_ERROR_STREAM, "Never fucking heard of tsdb_value: %d\n",value->type);
      else
        fprintf(TSDB_ERROR_STREAM, "tsdb: unknown tsdb_value type: %d\n", value->type);
    } /* switch */
  if (r==EOF)
    return FALSE;
  else 
    return TRUE;
      
} /* tsdb_print_value() */

void tsdb_print_array(Tsdb_value **array, FILE *stream) {

  tsdb_print_value(*array, stream);
  for(array++; *array != NULL; array++) {
    fprintf(stream, ", ");
    tsdb_print_value(*array, stream);
  } /* for */
  fprintf(stream, " \n");

} /* tsdb_print_array() */

void tsdb_print_selection(Tsdb_selection *selection, FILE *stream) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_print_selection()
|*     version: 
|*  written by: tom fettig & oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_print_selection() behaves rather different in DEBUG than in regular
|* mode: without the DEBUG option it simply prints out all tuples from
|* .selection. in the order of the first key list; yet, in DEBUG mode it prints
|* additional information about .selction. to the debug stream.
\*****************************************************************************/

#ifdef DEBUG
  Tsdb_relation *foo;
  int i, j, k;
#endif

  if(selection != NULL
     && selection->length && selection->key_lists[0] != NULL) {
    tsdb_print_key_list(selection->key_lists[0], stream);
  } /* if */
  
#if defined(DEBUG) && defined(PRINT_SELECTION) && !defined(TOM)
  if(selection != NULL) {
    fprintf(tsdb_debug_stream,
            "print_selection(): %s", selection->relations[0]->name);
    for(i = 1; i < selection->n_relations; i++) {
      fprintf(tsdb_debug_stream, ":%s", selection->relations[i]->name);
    } /* for */
    fprintf(tsdb_debug_stream, " (%d);\n", selection->length);
    foo = selection->relations[0];
    fprintf(tsdb_debug_stream,
            "print_selection(): keys: %s (0)", foo->fields[foo->keys[0]]);
    for(j = 1; j < foo->n_keys; j++) {
      fprintf(tsdb_debug_stream, ", %s (%d)",
              foo->fields[foo->keys[j]], i);
    } /* for */
    for(i = 1; i < selection->n_relations; i++) {
      foo = selection->relations[i];
      for(j = 1; j < foo->n_keys; j++) {
        fprintf(tsdb_debug_stream, ", %s (%d)",
                foo->fields[foo->keys[j]], i);
      } /* for */
    } /* for */
    fprintf(tsdb_debug_stream, ".\n");
    fflush(tsdb_debug_stream);
  } /* if */
  else {
    fprintf(tsdb_debug_stream, "print_selection(): null pointer.\n");
  } /* else */
#endif

#if defined(DEBUG) && defined(TOM)
  for ( i=0;i<selection->n_relations;i++){
    fprintf(stream,"selection relation %ld\n",i);
    tsdb_print_relation(selection->relations[i],stream);
  }
  for (i=0;i<selection->n_key_lists;i++) {
    fprintf(tsdb_debug_stream,"key list %ld\n",i);
    tsdb_print_key_list(selection->key_lists[i],stream);
  }
#endif

} /* tsdb_print_selection */

void tsdb_print_relation(Tsdb_relation *relation, FILE *stream) {

  int i, j;

  if(relation != NULL) {
    fprintf(stream, "%s:\n", relation->name);
    for(i = 0; i < relation->n_fields; i++) {
      fprintf(stream, "\t%s %s",
              relation->fields[i],
              (relation->types[i] == TSDB_STRING ? ":string" : ":integer"));
      for(j = 0; j < relation->n_keys && i != relation->keys[j] ; j++);
      if(j != relation->n_keys) {
        fprintf(stream, " :key");
        if(!relation->total[j]) {
          fprintf(stream, " :partial");
        } /* if */
      } /* if */
      fprintf(stream, "\n");
    } /* for */
    fprintf(stream, "\n");
  } /* if */
  
} /* tsdb_print_relation() */

void tsdb_print_node(Tsdb_node *node, FILE *stream) {

  if(node->left != NULL) {
    fprintf(stream, "(");
    tsdb_print_node(node->left, stream);
    fprintf(stream, " ");
  } /* if */
  tsdb_print_value(node->node, stream);
  if(node->right != NULL) {
    fprintf(stream, " ");
    tsdb_print_node(node->right, stream);
    fprintf(stream, (node->left != NULL ? ")" : ""));
  } /* if */

} /* tsdb_print_node() */

void tsdb_print_tuple(Tsdb_tuple *tuple, FILE *stream) {

  int i;

  if(tuple != NULL) {
    for(i = 0; i < tuple->n_fields - 1; i++) {
      tsdb_print_value(tuple->fields[i], stream);
      fprintf(stream, "%c", TSDB_FS);
    } /* for */
    tsdb_print_value(tuple->fields[i], stream);
  } /* if */
  else {
    if(really_verbose_mode)
      fprintf(TSDB_ERROR_STREAM, "Ignoring a shite tuple, you have a crap data file.\n");
    else
      fprintf(TSDB_ERROR_STREAM, "tsdb_print_tuple is ignoring invalid tuple.\n");
  } /* else */

} /* tsdb_print_tuple() */

void tsdb_print_tuples(Tsdb_tuple **tuples,FILE* stream){
  int i;
  for (i=0;tuples[i];i++) {
    tsdb_print_tuple(tuples[i],stream);
    if(tuples[i + 1] != NULL) {
        fprintf(stream,"|");
    } /* if */
  }
} /* tsdb_print_tuples() */

void tsdb_print_key_list(Tsdb_key_list *list, FILE *stream) {

  Tsdb_key_list *next;
  int i;

  for(next = list; next != NULL; next = next->next) {
    for(i = 0; i < next->n_tuples; i++) {
      tsdb_print_tuple(next->tuples[i], stream);
      if(i + 1 < next->n_tuples) {
        fprintf(stream, "%c", '|');
      } /* if */
    } /* for */
    fprintf(stream, "\n");
  } /* for */
 
} /* tsdb_print_key_list() */

void tsdb_print_join_path(Tsdb_relation **path, FILE *stream) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_print_join_path()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int i;

  fprintf(stream, "%s", path[0]->name);
  for(i = 1; path[i] != NULL; i++) {
    fprintf(stream, " -- %s", path[i]->name);
  } /* for */
  fprintf(stream, ".\n");
  fflush(stream);

} /* tsdb_print_join_path() */

FILE *tsdb_find_relations_file(char *mode) {

  FILE *file;

  if((file = fopen(tsdb_relations_file, mode )) == NULL) {
    if(really_verbose_mode)
      fprintf(TSDB_ERROR_STREAM, "I can't find the fucking relations file.\n");
    else 
      fprintf(TSDB_ERROR_STREAM, "tsdb: unable to open relations file %s.\n",
            tsdb_relations_file);
    return((FILE *)NULL);
  } /* if */
  return(file);

} /* tsdb_find_relations_file() */

FILE *tsdb_find_data_file(char *name, char *mode) {

  char *path;
  FILE *file;

  path = strdup(tsdb_data_path);
  path = (char *)realloc(path, strlen(tsdb_data_path) + strlen(name) + 1);
  path = strcat(path, name);

  if((file = fopen(path, mode)) == NULL) {
    fprintf(TSDB_ERROR_STREAM, "tsdb: unable to open data file %s.\n", path);
    return((FILE *)NULL);
  } /* if */
  else {
    return(file);
  } /* else */

} /* tsdb_find_data_file() */

char *tsdb_data_backup_file(char* name) {
 char* path;
 char* tmpnam;
 char* buf;
 FILE* file;
 int r,l;

 l = strlen(tsdb_data_path)+strlen(name)+2;
 path = malloc(l-1);
 tmpnam = malloc(l);
 strcpy(path,tsdb_data_path);
 path = (char*)realloc(path,strlen(tsdb_data_path) + strlen(name) + 2);
 path = strcat(path,name);
 tmpnam = strdup(path);
 strcat(tmpnam,TSDB_BACKUP_SUFFIX);

 r = rename(path,tmpnam);
 if (r==-1)
   return(NULL);
 else
   return(tmpnam);
}

int tsdb_restore_data_file(char*temp, char *name) {
  int r;
  char* path;

  path = strdup(tsdb_data_path);
  path = (char*)realloc(path,strlen(tsdb_data_path) + strlen(name) + 1);
  path = strcat(path,name);

  r=rename(temp,name);
  if (r==-1) return 0 ; else return(1);
}

BOOL tsdb_write_tuple(Tsdb_key_list* bar,FILE *output)
{
  int  r,i;
  
  for (i=0;i<bar->tuples[0]->n_fields-1;i++) {
    r=tsdb_print_value(bar->tuples[0]->fields[i],output);
    if (!r) return FALSE;
    r=fputc(TSDB_FS,output);
    if (!r) return FALSE;
  }
  r=tsdb_print_value(bar->tuples[0]->fields[i],output);
  if (!r) return FALSE;

  r=fputc('\n',output);
  if (!r) return FALSE;
  
  return(TRUE);
} /* tsdb_write_tuple() */

Tsdb_tuple *tsdb_read_tuple(Tsdb_relation *relation, FILE *input) {

  Tsdb_tuple *tuple;
  Tsdb_value *value;
  char buf[1024+1];
  int n, foo;
  char *field, *fs, *bar,*baz;


  baz = fgets(&buf[0],1024, input);
  if(baz != NULL) {
    tuple = (Tsdb_tuple *)malloc(sizeof(Tsdb_tuple));
    tuple->n_fields = 0;
    tuple->fields = (Tsdb_value **)NULL;

    if(buf[0] != '\n') {
      *(char *)strchr(&buf[0], '\n') = TSDB_FS;
      /* tom : changed strrchr -> strchr */
      for(field = &buf[0], n = 0, fs = strchr(field, TSDB_FS);
          n < relation->n_fields && fs != NULL;
          field = ++fs, n++, fs = strchr(field, TSDB_FS)) {
        value = (Tsdb_value *)malloc(sizeof(Tsdb_value));
        *fs = 0;
        if(relation->types[n] == TSDB_INTEGER) {
          if((foo = (int)strtol(field, &bar, 10)) != 0 || field != bar) {
            value->type = TSDB_INTEGER;
            value->value.integer = foo;
          } /* if */
          else {
            fprintf(TSDB_ERROR_STREAM, "tsdb_read_tuple(): non-integer ");
            fprintf(TSDB_ERROR_STREAM, "`%s' in field `%s' of `%s'.\n",
                    field, relation->fields[n], relation->name);
            return(Tsdb_tuple *)NULL;
          } /* else */
        } /* if */
        else {
          value->type = TSDB_STRING;
          value->value.string = strdup(field);
        } /* else */
        if(tuple->fields == NULL) {
          tuple->fields = (Tsdb_value **)malloc(sizeof(Tsdb_value *));
        } /* if */
        else {
          tuple->fields =
            (Tsdb_value **)realloc(tuple->fields,
                                 (n + 1) * sizeof(Tsdb_value *));
        } /* else */
        tuple->fields[n] = value;
      } /* for */
      tuple->n_fields = n;
    } /* if */
    return(tuple);
  } /* if */
  else {
    return((Tsdb_tuple *)NULL);
  } /* else */

} /* tsdb_read_tuple() */

int tsdb_write_relations() {
  char* tsdb_relfile_dir;
  char* last,*temp_name;
  Tsdb_relation* relation;
  static char buf[1024];
  FILE* file;
  int i,r,j,k,l;

  last = strrchr(tsdb_relations_file, '/');
  strncpy(buf, tsdb_relations_file, (last - tsdb_relations_file));
  buf[last - tsdb_relations_file] = '\0';
  temp_name = tempnam(buf, NULL);
  file = fopen(temp_name, "w");

  if (!file)
    return 0;

  for (i=0;i<tsdb_n_relations();i++) {
    relation= tsdb_relations[i];
    r = fprintf(file,"%s:\n",relation->name);
    if (r==EOF) return 0;
    for (j=0,k=0;j<relation->n_fields;j++) {
      r=fprintf(file,"\t%s ",relation->fields[j]);
      switch (relation->types[j]) {
      case TSDB_INTEGER:
        r=fputs(file,":integer ");  
        if (r==EOF) return 0;
        break;
      case TSDB_STRING:
        r=fputs(file,":string ");
        if (r==EOF) return 0;
        break;
      } /* switch */
      if (j==relation->keys[k]) {
        r=fputs(file,":key");
        if (r==EOF) return 0;
        k++;
      } /* if */
      r=fputs(file,"\n");
      if (r==EOF) return 0;
    }/* for */
    r=fputs(file,"\n");
    if (r==EOF) return 0;
  } /* for */
  
  r = rename(temp_name,tsdb_relations_file);
  if (r==-1) {
    perror();
    return 0;
  }
  return(1);
}

Tsdb_relation *tsdb_read_relation(FILE *input) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_read_relation()
|*     version: 
|*  written by: andrew p. white, oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  Tsdb_relation *relation;
  char buf[256 + 1], *foo, *bar, *baz;
  int c, i;

  relation = tsdb_create_relation();

  if(feof(input)) {
    return((Tsdb_relation *)NULL);
  } /* if */

  while(isspace(c = fgetc(input)));
  buf[0] = (char)c;

  if(fgets(&buf[1], 256, input) != NULL) {
    if((foo = strrchr(&buf[0], ':')) == NULL) {
      if((foo = strchr(&buf[0], '\n')) != NULL) {
        *foo = 0;
      } /* if */
      fprintf(TSDB_ERROR_STREAM,
              "read_relation(): invalid relation `%s'.\n",
              &buf[0]);
      for(foo = fgets(&buf[0], 256, input);
          foo != NULL && buf[0] != '\n';
          foo = fgets(&buf[0], 256, input));
      tsdb_free_relation(relation);
      if(!feof(input) && (relation = tsdb_read_relation(input)) != NULL) {
        return(relation);
      } /* if */
      else {
        return((Tsdb_relation *)NULL);
      } /* else */
    } /* if */
    *foo = 0;
    relation->name = strdup(&buf[0]);
    
    while(fgets(&buf[0], 256, input) != NULL && buf[0] != '\n') {
      for(foo = &buf[0]; isspace(*foo); foo++);
      
      if(relation->fields == NULL) {
        relation->fields = (char **)malloc(sizeof(char *));
        relation->types = (BYTE *)malloc(sizeof(BYTE));
      } /* if */
      else {
        relation->fields = 
          (char **)realloc(relation->fields,
                           (relation->n_fields + 1) * sizeof(char *));
        relation->types = 
          (BYTE *)realloc(relation->types,
                           (relation->n_fields + 1) * sizeof(BYTE));
       } /* else */
      if((bar = strchr(foo , '\n')) != NULL) {
        *bar = 0;
      } /* if */
      for(bar = foo; *bar && !isspace(*bar) && *bar != ':'; bar++);
      *bar++ = 0;
      if(strstr(bar, "integer") != NULL) {
        relation->types[relation->n_fields] = TSDB_INTEGER;
      } /* if */
      else {
        if(strstr(bar, "string") != NULL) { 
          relation->types[relation->n_fields] = TSDB_STRING;
        } /* if */
        else {
          fprintf(TSDB_ERROR_STREAM,
                  "read_relation(): `%s' has no type in relation `%s'.\n",
                  foo, relation->name);
          for(foo = fgets(&buf[0], 256, input);
              foo != NULL && buf[0] != '\n';
              foo = fgets(&buf[0], 256, input));
          tsdb_free_relation(relation);
          if(!feof(input) && (relation = tsdb_read_relation(input)) != NULL) {
            return(relation);
          } /* if */
          else {
            return((Tsdb_relation *)NULL);
          } /* else */
        } /* else */
      } /* else */
      if(strstr(bar, "key") != NULL) { 
        if(relation->keys == NULL) {
          relation->keys = (int *)malloc(sizeof(int));
          relation->total = (BYTE *)malloc(sizeof(BYTE));
        } /* if */
        else {
          relation->keys
            = (int *)realloc(relation->keys, 
                             (relation->n_keys + 1) * sizeof(int));
          relation->total
            = (BYTE *)realloc(relation->total, 
                              (relation->n_keys + 1) * sizeof(BYTE));
        } /* else */
        relation->keys[relation->n_keys] = relation->n_fields;
        if(strstr(bar, "partial") != NULL) {
          relation->total[relation->n_keys] = FALSE;
        } /* if */
        else {
          relation->total[relation->n_keys] = TRUE;
        } /* else */
        relation->n_keys++;
      } /* if */
      relation->fields[relation->n_fields] = strdup(foo);
      relation->n_fields++;
    } /* while */

    return(relation);
  } /* if */
  else {
    return((Tsdb_relation *)NULL);
  } /* else */

} /* tsdb_read_relation() */

BOOL tsdb_write_table(Tsdb_selection* selection) {
  FILE *output;
  Tsdb_tuple** foo;
  char *temp;
  Tsdb_key_list *bar;
  int i;
  BOOL f;
  
  bar = selection->key_lists[0];
  if (((output = tsdb_find_data_file(selection->relations[0]->name,"r"))
        == NULL) ||
      ((temp = tsdb_data_backup_file(selection->relations[0]->name)) 
        == NULL) ){
    printf("can't open the fucking data or backup-file");
    return FALSE;
  }
  fclose(output);
  output = tsdb_find_data_file(selection->relations[0]->name,"w");
  if (!output){
    printf("the fucking data-file may not be written\n");
    return FALSE;
  }
    
  for (i=0, f=TRUE ;i<selection->length && f==TRUE ; i++){
    f = tsdb_write_tuple(bar,output);
    bar = bar->next;
  } /* for */

  if (!f) {
    perror(NULL);
    tsdb_restore_data_file(selection->relations[0]->name,temp);
    return FALSE;
  }
  else {
    remove(temp);
    return TRUE;
  }
} /* tsdb_write_table() */

Tsdb_selection *tsdb_read_table(Tsdb_relation *relation,
                                Tsdb_node *condition) {

/*****************************************************************************\
|*        file: 
|*      module: 
|*     version: 
|*  written by: tom fettig & oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  Tsdb_tuple *tuple, **foo;
  Tsdb_selection *selection;
  FILE *input;
  int i;
#if defined(DEBUG) && defined(READ_TABLE)
  float time = tsdb_timer(TSDB_START_TIMER);
#endif

  if(((input = tsdb_find_data_file(relation->name, "r")) != NULL) &&
     ((tuple = tsdb_read_tuple(relation, input)) != NULL)) {

    selection = (Tsdb_selection *)malloc(sizeof(Tsdb_selection));
    selection->relations =
      (Tsdb_relation **)malloc(2 * sizeof(Tsdb_relation *));
    selection->relations[0] = relation;
    selection->relations[1] = (Tsdb_relation *)NULL;
    selection->n_relations = 1;
    selection->n_key_lists = relation->n_keys;
    selection->key_lists =
      (Tsdb_key_list **)malloc((relation->n_keys+1) * sizeof(Tsdb_key_list *));
    for(i = 0; i < relation->n_keys+1; i++) {
      selection->key_lists[i] = (Tsdb_key_list *)NULL;
    } /* for */
    selection->length = 0;

    i = 0;
    do {
      i++;
      if(condition == NULL ||
         tuple->n_fields &&
         tsdb_satisfies_condition(tuple, condition, relation)) {
        foo = (Tsdb_tuple **)malloc(2 * sizeof(Tsdb_tuple *));
        foo[0] = tuple;
        foo[1] = (Tsdb_tuple *)NULL;
        if(tsdb_insert_into_selection(selection, &foo[0])) {
          selection->length++;
        } /* if */
        else {
          free(tuple);
          free(foo);
        } /* else */
      } /* if */
    } while((tuple = tsdb_read_tuple(relation, input)) != NULL);

    fclose(input);
#if defined(DEBUG) && defined(READ_TABLE)
  if((time = tsdb_timer(time)) != (float)-1) {
    fprintf(tsdb_debug_stream,
            "read_table(): read %d tuples for `%s' in %.1f seconds.\n",
            selection->length, relation->name, time);
    fflush(tsdb_debug_stream);
  } /* if */
#endif
    return(selection);
  } /* if */
  else {
    return((Tsdb_selection *)NULL);
  } /* else */

} /* tsdb_read_table() */

FILE* tsdb_open_result() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_open_result()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  FILE* output;
  int i;
  char old[MAXNAMLEN], new[MAXNAMLEN];

  for(i = tsdb_max_results; i > 1; i--) {
    (void)sprintf(&new[0],
                  "%s%s%d", tsdb_result_path, tsdb_result_prefix, i);
    (void)sprintf(&old[0],
                  "%s%s%d", tsdb_result_path, tsdb_result_prefix, i - 1);
    (void)rename(old, new);
  } /* for */
  

  if((output = fopen(&old[0], "w")) == NULL) {
    fprintf(TSDB_ERROR_STREAM,
            "open_result(): unable to create \%s'.\n", &old[0]);
    return((FILE *)NULL);
  } /* if */
  
  return(output);

} /* tsdb_open_result() */

void tsdb_tree_print(Tsdb_node* node, FILE* stream)
{
 static int insert = 0;
 int i;

 insert++;
 for (i=0;i<insert;i++)
   putc(' ',stream);
 tsdb_print_value(node->node,stream);
 fprintf(stream,"\n");
 if (node->left)
   tsdb_tree_print(node->left,stream);
 if (node->right)
   tsdb_tree_print(node->right,stream);
 insert--;

}

/*---------------------------------------------------------------------------*/
void tsdb_save_changes() {
  /* save all tables that have been changed */
  int i=0;
  Tsdb_selection* select;

  for (i=0;tsdb_relations[i];i++) {
    if (tsdb_relations[i]->status!=TSDB_UNCHANGED) {
      select = tsdb_find_table(tsdb_relations[i]);
      tsdb_write_table(select);
      tsdb_relations[i]->status==TSDB_UNCHANGED;
    }
  }
}
/*---------------------------------------------------------------------------*/
