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
#include <unistd.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
extern int errno;

#include "globals.h"
#include "tsdb.h"
#include "errors.h"

int tsdb_parse(char *command, FILE *stream) {

  int foo;

#ifdef DEBUG
  if(!(tsdb.status & TSDB_SERVER_MODE)) {
    fprintf(tsdb_debug_stream,
            "yyparse(): `%s'.\n", command);
    fflush(tsdb_debug_stream);
  } /* if */
#endif

  if(tsdb.query != NULL) {
    free(tsdb.query);
  } /* if */
  tsdb.query = strdup(command);

  tsdb.input = command;
  foo = yyparse();
  if(stream != NULL && !(tsdb.status & TSDB_SERVER_MODE)) {
    yyrestart(stream);
  } /* if */

  return(foo);

} /* tsdb_parse() */

int tsdb_getchar(void) {

  return((*tsdb.input ? *tsdb.input++ : EOF));

} /* tsdb_getchar */

FILE *tsdb_open_output(char *path) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_open_output()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  char *foo;
  FILE *output;

  if((foo = tsdb_expand_file((char *)NULL, &path[1])) == NULL) {
    fprintf(tsdb_error_stream,
            "open_output(): unable to expand file name `%s'.\n", 
            &path[1]);
    fflush(tsdb_error_stream);
    return((FILE *)NULL);
  } /* if */

  if((output = fopen(foo, (path[0] == TSDB_REDIRECTION_OVERWRITE ?
                            "w" : "a"))) == NULL) {
    fprintf(tsdb_error_stream,
            "open_output(): unable to open output file `%s' [%d].\n", 
            foo, errno);
    fflush(tsdb_error_stream);
    return((FILE *)NULL);
  } /* if */

#ifdef DEBUG
  fprintf(tsdb_debug_stream,
          "open_output(): %s `%s'.\n",
          (path[0] == TSDB_REDIRECTION_OVERWRITE ?
           "overwriting" : "appending to"),
          foo);
  fflush(tsdb_debug_stream);
#endif

  free(path);
  return(output);

} /* tsdb_open_output() */

FILE *tsdb_open_pager() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_open_pager()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 16-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

  FILE* stream = (FILE *)NULL;

  if(tsdb.pager != NULL && (stream = popen(tsdb.pager, "w")) == NULL) {
    fprintf(tsdb_error_stream,
            "open_pager(): unable to popen(3) `%s'.\n", tsdb.pager);
  } /* if */
  return(stream);

} /* tsdb_open_pager */

#ifdef DEBUG
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

  FILE *output;
  char *name, *date, *user;
  time_t clock;

  if(tsdb.status & TSDB_SERVER_MODE) {
   /* 
    * _fix_me_
    * determine appropriate length for .port.
    *                                            (1-apr-95  -  oe)
    */
    user = (char *)malloc(42);
    (void)sprintf(user, "%d", tsdb.port);
  } /* if */
  else {
    user = tsdb_user();
  } /* else */

  if(tsdb.debug_file == NULL) {
    if((name = getenv("TSDB_DEBUG_FILE")) != NULL
       && strcmp(name, "null") && strcmp(name, "nil")) {
      tsdb.debug_file
        = (char *)malloc(strlen(name)
                         + (user != NULL ? 1 + strlen(user) : 0) + 1);
      tsdb.debug_file = strcpy(tsdb.debug_file, name);
    } /* if */
    else if(strcmp(TSDB_DEBUG_FILE, "null")
            && strcmp(TSDB_DEBUG_FILE, "nil")) {
      tsdb.debug_file
        = (char *)malloc(strlen(TSDB_DEBUG_FILE)
                         + (user != NULL ? 1 + strlen(user) : 0) + 1);
      tsdb.debug_file = strcpy(tsdb.debug_file, TSDB_DEBUG_FILE);
    } /* else */

    if(tsdb.debug_file != NULL && user != NULL) {
      tsdb.debug_file = strcat(tsdb.debug_file, ".");
      tsdb.debug_file = strcat(tsdb.debug_file, user);
    } /* if */
  } /* if */
  
  if(tsdb.debug_file == NULL) {
    return((FILE *)NULL);
  } /* if */

  if(time(&clock) != (time_t)-1) {
    if((date = ctime(&clock)) != NULL) {
      date[strlen(date) - 1] = 0;
    } /* if */
  } /* if */

  if((output = fopen(tsdb.debug_file, "w")) != NULL) {
    if(tsdb.status & TSDB_SERVER_MODE) {
      fprintf(output,
              "TSDB debug file opened for port %s on %s.\n\n",
              (user != NULL ? user : ""),
              (date != NULL ? date : ""));
    } /* if */
    else {
      fprintf(output,
              "TSDB debug file opened by %s on %s.\n\n",
              (user != NULL ? user : ""),
              (date != NULL ? date : ""));
    } /* else */
    fprintf(output,
            "tsdb(1) %s (%s) [%s] --- (c) oe@coli.uni-sb.de.\n\n",
            tsdb_version,
            tsdb_rcs_strip(&tsdb_revision[0], "Revision"),
            tsdb_rcs_strip(&tsdb_revision_date[0], "Date"));
    fflush(output);
    return(output);
  } /* if */

  fprintf(tsdb_error_stream,
          "open_debug(): unable to open `%s' [%d].\n",
          tsdb.debug_file, errno);
  return((FILE *)NULL);

} /* tsdb_open_debug() */
#endif    

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
char* tsdb_sprint_value(Tsdb_value *value, BOOL raw) {
  char *result, *foo;
  
  switch(value->type) {
    case TSDB_INTEGER:
      result = malloc(20);
      sprintf(result, "%d", value->value.integer);
      break;
    case TSDB_IDENTIFIER:
      result = strdup(value->value.identifier);
      break;
    case TSDB_STRING:
      if(!raw) {
        foo = tsdb_denormalize_string(value->value.string);
        if(tsdb.status & TSDB_PROLOG_ESCAPE_OUTPUT) {
          result = tsdb_prolog_escape_string(foo);
        } /* if */
        else {
          if(tsdb.status & TSDB_LISP_ESCAPE_OUTPUT) {
            result = tsdb_lisp_escape_string(foo);
          } /* if */
          else {
            result = strdup(foo);
          } /* else */
        } /* else */
        tsdb_free(foo);
      } /* if */
      else {
        result = strdup(value->value.string);
      } /* else */
      break;
    case TSDB_DATE:
      result = tsdb_canonical_date(value->value.date);
      break;
    case TSDB_POSITION:
      result = strdup(value->value.position);
      break;
    case TSDB_CONNECTIVE:
      switch(value->value.connective) {
        case TSDB_AND:
          result = strdup("&&");
          break;
        case TSDB_OR:
          result=strdup("||");
          break;
        case TSDB_NOT:
          result=strdup("!");
          break;
        case TSDB_NOT_NOT:
          result = strdup(" ");
          break;
      } /* switch */
      break;
    case TSDB_OPERATOR:
      switch(value->value.operator) {
        case TSDB_EQUAL:
          result = strdup("==");
          break;
        case TSDB_NOT_EQUAL:
          result = strdup("!=");
          break;
        case TSDB_LESS_THAN:
          result = strdup("<");
          break;
        case TSDB_LESS_OR_EQUAL_THAN:
          result = strdup("<=");
          break;
        case TSDB_GREATER_THAN:
          result = strdup(">");
          break;
        case TSDB_GREATER_OR_EQUAL_THAN:
          result = strdup(">=");
          break;
        case TSDB_MATCH:
          result = strdup("~");
          break;
        case TSDB_NOT_MATCH:
          result = strdup("!~");
          break;
        case TSDB_INSENSITIVE_MATCH:
          result = strdup("~~");
          break;
        case TSDB_NOT_INSENSITIVE_MATCH:
          result = strdup("!~~");
          break; 
      } /* switch */
      break;
    default:
      fprintf(tsdb_error_stream,
              "print_value(): unknown type: %d.\n", value->type);
      fflush(tsdb_error_stream);
  } /* switch */

  return result;

} /* tsdb_sprint_value() */


char* tsdb_sprint_key_list(Tsdb_key_list* list,int* r,int* f,
                           int n_attributes) {
  static char* buf=NULL;
  static int blen=1024;
  int k,len;
  char *cat;
  
  if (!buf) {
    buf = malloc(blen);
    if (!buf) 
      return NULL;
  }/* if */
  buf[0]='\0';
  len = 0;

  for (k=0; k<n_attributes ; k++ ) {
    if (r[k]!=-1
        && ((cat = tsdb_sprint_value(list->tuples[r[k]]->fields[f[k]], FALSE))
            != NULL)) {
      if ((len+strlen(cat)+2)<blen) {
        strcat(&buf[len],cat);
      } /* if */
      else {
        blen+=blen;
        buf = realloc(buf,blen);
        strcat(&buf[len],cat);
      } /* else */
      len+=strlen(cat);
      free(cat);
    } /* if */
    if (k+1 < n_attributes) {
      buf[len++]=tsdb.fs;
      buf[len]='\0';
    } /* if */
  } /* for  n_attributes */

  return(strdup(buf));
} /* tsdb_sprint_key_list() */


/*--------------------------- print functions ------------------------------*/
BOOL tsdb_print_value(Tsdb_value *value, FILE *stream, BOOL raw) {

  char *foo;

  if((foo = tsdb_sprint_value(value, raw)) != NULL) {
    fprintf(stream, foo);
    return((fflush(stream) != EOF));
  } /* if */
  return(FALSE);

} /* tsdb_print_value() */

void tsdb_print_array(Tsdb_value **array, FILE *stream) {

  tsdb_print_value(*array, stream, FALSE);
  for(array++; *array != NULL; array++) {
    fprintf(stream, ", ");
    tsdb_print_value(*array, stream, FALSE);
  } /* for */
  fprintf(stream, " \n");
  fflush(stream);

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
  int i, j;
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
              foo->fields[foo->keys[j]], 1);
    } /* for */
    for(i = 1; i < selection->n_relations; i++) {
      foo = selection->relations[i];
      for(j = 0; j < foo->n_keys; j++) {
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
  if (selection) {
    for ( i=0;i<selection->n_relations;i++){
      fprintf(stream,"selection relation %d\n",i);
      tsdb_print_relation(selection->relations[i],stream);
    }
    for (i=0;i<selection->n_key_lists;i++) {
      fprintf(tsdb_debug_stream,"key list %d\n",i);
      tsdb_print_key_list(selection->key_lists[i],stream);
    }
  } /* if */
  
#endif
  fflush(stream);

} /* tsdb_print_selection */


int find_next_format(char* format,int *pos) {
  char* tmp;
  int typ;

  tmp = strchr(format,'%');
  if (!tmp) {
    typ = -1;
    *pos = -1;
    return typ;
  }
  
  switch (*(tmp+1)) {
  case '%':
    strcpy(tmp,tmp+1);
    typ = find_next_format(tmp+1,pos);
    break;
  case 's':; case'd':; case'i':
    typ = 1;
    *pos = tmp-format;
    break;
  case '*':
    typ = 2;
    *pos = tmp-format;
    break;
  default:
    typ = -1;
    *pos = -1;
    break;
  } /* switch */
  
  return typ;
  
}  /* find_next_format() */

void tsdb_print_projection(char** projection,int n,char* format,FILE *stream) {
  int k,i=0,j,l,size,where=0,typ,result;
  char **bar=NULL,*foo,*tmp,*praefix=NULL,*suffix=NULL;
  /* report: 
     %s \
     %i | - %s here output
     %d /
     %* rest of projection
     %% %
     */

  if (n==0) 
    return;
  if (format) {
    size = strlen(format)/2;
    typ = find_next_format(format,&where);
    tmp = format+where+2;
    if (where<0) {
      /* wahrscheinlich falscher Format-string */
    } /* if */
    else {
      bar = (char**)malloc(sizeof(char*)*(size+1));
      if (where >=0) {
        praefix = strdup(format);
        strncpy(praefix,format,where);
        praefix[where]='\0';
        if (typ==1) { /* s,i,d */
          foo = format+where+2;
          tmp = foo;
          typ = find_next_format(tmp,&where);
        }
        else
          if (typ==2) { /* * */
            tmp = format+strlen(format);
            foo = format+where+2;
            where = -1;
          } /* if */
      } /* where > 0 */
      else {
        char c = format[2];
        format[2] = '\0';
        bar[i++]=strdup(format);
        format[2] = c;
        typ = find_next_format(tmp,&where);
        foo = format +2;
      } /* where = 0 */
      for (; (where > -1) ;typ = find_next_format(tmp,&where)) {
        bar[i]=strdup(tmp);
        bar[i][where]='\0';
        i++;
        foo = tmp+where+2;
        tmp = foo;
      } /* for */
      bar[i]=NULL;
      if ((format+strlen(format))>foo) /* suffix! */ {
        suffix = strdup(foo);
      } /* if */
    } /* else */
  } /* if format */
  
  l=0;
  tmp = projection[0];
  while  ( tmp= strchr(tmp,tsdb.fs)) {
    tmp++;l++;
  } /* while */
  
    
  if (i>(l+1)) {
    fprintf(tsdb_error_stream," Wrong report string: %s \n",format);
    fprintf(tsdb_error_stream," too many format specifications\n");
    return;
  }
  if (!suffix)
    suffix=strdup("");
  if (!praefix)
    praefix=strdup("");
  

  /* main output loop: bar[i-1] contains string to be printed
     between component [i] and [i+1]. praefix is the  string 
     before the first and suffix the string after the last format 
     string.
     */
  for (j=0,l=0;j<n;l++) {
    if (projection[l]) {
      foo = projection[l];
      result = fputs(praefix,stream);
      if (result==EOF) 
        break;
      for (k=0;k<i;k++) {
        tmp = strchr(foo,tsdb.fs);
        if (!tmp) {
          k=i;
        }
        else {
          *tmp='\0';
          result = fputs(foo,stream);
          if (result==EOF)
            break;
          *tmp=tsdb.fs;
          foo = tmp+1;
          fputs(bar[k],stream);
        }
      } /* for */
      result = fputs(foo,stream);
      if (result==EOF)
        break;
      fputs(suffix,stream);
      fputc('\n',stream);
      j++;
    } /* if */
  } /* for */
  
  fflush(stream);
  free(suffix);
  free(praefix);
  for (i=0;bar && bar[i];i++)
    free(bar[i]);
  if (bar)
    free(bar);
  
} /* tsdb_print_projection() */


void tsdb_print_relation(Tsdb_relation *relation, FILE *stream) {

  int i, j;

  if(relation != NULL) {
    fprintf(stream, "%s:\n", relation->name);
    for(i = 0; i < relation->n_fields; i++) {
      fprintf(stream, "  %s", relation->fields[i]);
      switch(relation->types[i]) {
        case TSDB_INTEGER:
          fprintf(stream, " :integer");
          break;
        case TSDB_STRING:
          fprintf(stream, " :string");
          break;
        case TSDB_DATE:
          fprintf(stream, " :date");
          break;
        case TSDB_POSITION:
          fprintf(stream, " :position");
          break;
      } /* switch */
      for(j = 0; j < relation->n_keys && i != relation->keys[j] ; j++);
      if(j != relation->n_keys) {
        fprintf(stream, " :key");
        if(relation->types[i] & TSDB_PARTIAL) {
          fprintf(stream, " :partial");
        } /* if */
      } /* if */
      if(relation->types[i] & TSDB_UNIQUE) {
        fprintf(stream, " :unique");
      } /* if */
      fprintf(stream, "\n");
    } /* for */
    fprintf(stream, "\n");
  } /* if */
  fflush(stream);

} /* tsdb_print_relation() */

void tsdb_print_node(Tsdb_node *node, FILE *stream) {

  if(node->left != NULL) {
    fprintf(stream, "(");
    tsdb_print_node(node->left, stream);
    fprintf(stream, " ");
  } /* if */
  tsdb_print_value(node->node, stream, FALSE);
  if(node->right != NULL) {
    fprintf(stream, " ");
    tsdb_print_node(node->right, stream);
    fprintf(stream, (node->left != NULL ? ")" : ""));
  } /* if */
  fflush(stream);

} /* tsdb_print_node() */

void tsdb_print_tuple(Tsdb_tuple *tuple, FILE *stream) {

  int i;

  if(tuple != NULL) {
    for(i = 0; i < tuple->n_fields - 1; i++) {
      tsdb_print_value(tuple->fields[i], stream, FALSE);
      fprintf(stream, "%c", tsdb.fs);
    } /* for */
    tsdb_print_value(tuple->fields[i], stream, FALSE);
  } /* if */
  else {
    fprintf(tsdb_error_stream,
            "tsdb_print_tuple is ignoring invalid tuple.\n");
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
  fflush(stream);
} /* tsdb_print_tuples() */

void tsdb_print_key_list(Tsdb_key_list *list, FILE *stream) {

  Tsdb_key_list *next;
  int i,j;

  for(j=0,next = list; next != NULL; next = next->next,j++) {
    for(i = 0; i < next->n_tuples; i++) {
      tsdb_print_tuple(next->tuples[i], stream);
      if(i + 1 < next->n_tuples) {
        fprintf(stream, "%c", '|');
      } /* if */
    } /* for */
    fprintf(stream, "\n");
  } /* for */
  fflush(stream);
  
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

  if((file = fopen(tsdb.relations_file, mode )) == NULL) {
    if(!(tsdb.status & TSDB_QUIET)) {
      fprintf(tsdb_error_stream,
              "find_relations_file(): unable to open `%s' [%d].\n",
              tsdb.relations_file, errno);
      fflush(tsdb_error_stream);
    } /* if */
    return((FILE *)NULL);
  } /* if */
  return(file);

} /* tsdb_find_relations_file() */

FILE *tsdb_find_data_file(char *name, char *mode) {

  char *path;
  FILE *file;
#ifdef COMPRESSED_DATA
  char *command, *foo;
  FILE *stream;
#endif

#ifdef COMPRESSED_DATA
  path = (char *)malloc(strlen(tsdb.data_path) + strlen(name) 
                        + strlen(tsdb.suffix) + 1);
#else
  path = (char *)malloc(strlen(tsdb.data_path) + strlen(name) + 1);
#endif

  (void)strcpy(path, tsdb.data_path);
  (void)strcat(path, name);

  if((file = fopen(path, mode)) != NULL) {
    free(path);
    tsdb.error = TSDB_OK;
    return(file);
  } /* if */
  else {
#ifdef COMPRESSED_DATA
    foo = strdup(path);
    path = strcat(path, tsdb.suffix);
    if(!access(path, R_OK)) {
      command = (char *)malloc(strlen(tsdb.uncompress) + strlen(path) + 2);
      command = strcpy(command, tsdb.uncompress);
      command = strcat(command, " ");
      command = strcat(command, path);
      if((stream = popen(command, "r")) != NULL) {
        free(path);
        free(command);
        tsdb.status |= TSDB_READ_ONLY;
        tsdb.error = TSDB_OK;
        return(stream);
      } /* if */
      free(command);
    } /* if */
    if(!(tsdb.status & TSDB_QUIET)) {
      fprintf(tsdb_error_stream,
              "find_data_file(): unable to %s `%s[%s]' [%d].\n",
              (mode[0] == 'r' ? "read" : "write"), foo, tsdb.suffix, errno);
    } /* if */
    tsdb_free(foo);
#else
    if(!(tsdb.status & TSDB_QUIET)) {
      fprintf(tsdb_error_stream,
              "find_data_file(): unable to %s `%s' [%d].\n",
              (mode[0] == 'r' ? "read" : "write"), path, errno);
    } /* if */
#endif
    tsdb_free(path);
    tsdb.error = TSDB_NO_DATA_ERROR;
    return((FILE *)NULL);
  } /* else */

} /* tsdb_find_data_file() */

char *tsdb_data_backup_file(char* name) {
  char* path;
  char* tmpnam;
  int r,l;
  
  l = strlen(tsdb.data_path)+strlen(name)+2;
  path = malloc(l-1);
  tmpnam = malloc(l);
  strcpy(path,tsdb.data_path);
  path = (char*)realloc(path,strlen(tsdb.data_path) + strlen(name) + 2);
  path = strcat(path,name);
  tmpnam = strdup(path);
  strcat(tmpnam,TSDB_BACKUP_SUFFIX);
  
  r = rename(path,tmpnam);
  if (r==-1) {
    tsdb.error = TSDB_FILE_CREATION_ERROR;
    return(NULL);
  } /* if */
  else {
    tsdb.error = TSDB_OK;
    return(tmpnam);
  } /* else */
}

int tsdb_restore_data_file(char*temp, char *name) {
  int r;
  char* path;

  path = strdup(tsdb.data_path);
  path = (char*)realloc(path,strlen(tsdb.data_path) + strlen(name) + 1);
  path = strcat(path,name);

  r=rename(temp,name);
  if (r==-1) return 0 ; else return(1);
}

BOOL tsdb_write_tuple(Tsdb_key_list *bar, FILE *output, BOOL raw) {

  int r, i;
  
  for (i = 0; i < bar->tuples[0]->n_fields - 1; i++) {
    r = tsdb_print_value(bar->tuples[0]->fields[i], output, raw);
    if (!r) return FALSE;
    r = fputc(tsdb.fs,output);
    if (!r) return FALSE;
  }
  r = tsdb_print_value(bar->tuples[0]->fields[i], output, raw);
  if (!r) return FALSE;

  r=fputc('\n',output);
  if (!r) return FALSE;
  
  return(TRUE);
} /* tsdb_write_tuple() */

Tsdb_tuple *tsdb_read_tuple(Tsdb_relation *relation, FILE *input) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_read_tuple()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 31-jul-95
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
|*****************************************************************************|
|* <known bugs>
|* There is a (small) memory leak for invalid tuples because `value' and
|* `tuple' are not free(2)d.
\*****************************************************************************/

  Tsdb_tuple *tuple;
  Tsdb_value *value;
  static char *buf = (char *)NULL;
  static int buf_size = 1024;
  int i, n;
  long int foo;
  char *field, *fs, *bar;

  if (buf == NULL) {
    if((buf = (char *)malloc(buf_size)) == NULL) {
      fprintf(tsdb_error_stream,"read_tuple(): out of memory.\n");
      fflush(tsdb_error_stream);
      return((Tsdb_tuple *)NULL);
    } /* if */
  } /* if */

  if(fgets(buf, buf_size, input) == NULL) {
    return((Tsdb_tuple *)NULL);
  } /* if */

  n = strlen(buf);
  while(buf[n - 1] != '\n') {
    buf_size += buf_size;
    if((buf = (char *)realloc(buf, buf_size)) == NULL) {
      fprintf(tsdb_error_stream, "read_tuple(): out of memory.\n");
      fflush(tsdb_error_stream);
      return((Tsdb_tuple *)NULL);
    } /* if */
    if(fgets(&buf[n], buf_size / 2, input) == NULL) {
      return((Tsdb_tuple *)NULL);
    } /* if */
    n += strlen(&buf[n]);
  } /* while() */

  for(bar = strchr(buf ,tsdb.fs), n = 0;
      bar != NULL;
      bar = strchr(bar + 1, tsdb.fs), n++);
  if(n + 1 != relation->n_fields) {
    fprintf(tsdb_error_stream,
            "read_tuple(): arity mismatch (%d vs. %d) for `%s'",
            n + 1, relation->n_fields, relation->name);
    if((bar = strchr(buf, tsdb.fs)) != NULL) {
      *bar = 0;
    } /* if */
    fprintf(tsdb_error_stream, " (%s).\n", buf);
    fflush(tsdb_error_stream);
    return((Tsdb_tuple *)NULL);
  } /* if */
  
  if((tuple = (Tsdb_tuple *)malloc(sizeof(Tsdb_tuple))) == NULL) {
    fprintf(tsdb_error_stream,"read_tuple(): out of memory.\n");
    fflush(tsdb_error_stream);
    return((Tsdb_tuple *)NULL);
  } /* if */
  tuple->n_fields = n + 1;
  if((tuple->fields =
      (Tsdb_value **)malloc(sizeof(Tsdb_value *) * (n + 2))) == NULL) {
    fprintf(tsdb_error_stream,"read_tuple(): out of memory.\n");
    fflush(tsdb_error_stream);
    return((Tsdb_tuple *)NULL);
  } /* if */
  
  if(buf[0] != '\n') {
    *(char *)strchr(&buf[0], '\n') = tsdb.fs;
    for(field = &buf[0], n = 0, fs = strchr(field, tsdb.fs);
        n < relation->n_fields && fs != NULL;
        field = ++fs, n++, fs = strchr(field, tsdb.fs)) {
      value = (Tsdb_value *)malloc(sizeof(Tsdb_value));
      *fs = 0;
      if(!*field) {
        for(i = 0; i < relation->n_keys; i++) {
          if(!strcmp(relation->fields[relation->keys[i]],
                     relation->fields[n])) {
            fprintf(tsdb_error_stream,
                    "read_tuple(): empty key `%s' in `%s'",
                    relation->fields[n], relation->name);
            if(n) {
              if(tuple->fields[0]->type == TSDB_INTEGER) {
                fprintf(tsdb_error_stream,
                        " (%d)", tuple->fields[0]->value.integer);
              } /* if */
              else {
                fprintf(tsdb_error_stream,
                        " (%s)", tuple->fields[0]->value.string);
              } /* else */
            } /* if */
            fprintf(tsdb_error_stream, ".\n");
            fflush(tsdb_error_stream);
            return((Tsdb_tuple *)NULL);
          } /* if */
        } /* if */
      } /* if */
      if((relation->types[n] & TSDB_TYPE_MASK) == TSDB_INTEGER) {
        if((foo = strtol(field, &bar, 10)) != 0 || field != bar) {
          value->type = TSDB_INTEGER;
          value->value.integer = foo;
        } /* if */
        else {
          fprintf(tsdb_error_stream,
                  "read_tuple(): non-integer `%s' as `%s' in `%s'",
                  field, relation->fields[n], relation->name);
          if(n) {
            if(tuple->fields[0]->type == TSDB_INTEGER) {
              fprintf(tsdb_error_stream,
                      " (%d)", tuple->fields[0]->value.integer);
            } /* if */
            else {
              fprintf(tsdb_error_stream,
                      " (%s)", tuple->fields[0]->value.string);
            } /* else */
          } /* if */
          fprintf(tsdb_error_stream, ".\n");
          fflush(tsdb_error_stream);
          return((Tsdb_tuple *)NULL);
        } /* else */
      } /* if */
      else {
        switch(relation->types[n] & TSDB_TYPE_MASK) {
          case TSDB_STRING:
            value->type = relation->types[n];
            value->value.string = strdup(field);
            break;
          case TSDB_DATE:
            value->type = TSDB_DATE;
            value->value.date = strdup(field);
            break;
          case TSDB_POSITION:
            value->type = TSDB_POSITION;
            value->value.position = strdup(field);
            break;
        } /* switch */
      } /* else */
      tuple->fields[n] = value;
    } /* for */
  } /* if */
  
  return(tuple);
  
} /* tsdb_read_tuple() */

int tsdb_write_relations() {
  char* last,*temp_name;
  Tsdb_relation* relation;
  static char buf[1024];
  FILE* file;
  int i,r,j,k;

  last = strrchr(tsdb.relations_file, '/');
  strncpy(buf, tsdb.relations_file, (last - tsdb.relations_file));
  buf[last - tsdb.relations_file] = '\0';
  temp_name = tempnam(buf, NULL);
  file = fopen(temp_name, "w");

  if (!file)
    return 0;

  for (i=0;i<tsdb_n_relations();i++) {
    relation= tsdb.relations[i];
    r = fprintf(file,"%s:\n",relation->name);
    if (r==EOF) return 0;
    for (j=0,k=0;j<relation->n_fields;j++) {
      r=fprintf(file,"  %s ",relation->fields[j]);
      switch (relation->types[j] & TSDB_TYPE_MASK) {
      case TSDB_INTEGER:
        r=fputs(":integer ",file);  
        if (r==EOF) return 0;
        break;
      case TSDB_STRING:
        r=fputs(":string ",file);
        if (r==EOF) return 0;
        break;
      case TSDB_DATE:
        r=fputs(":date ",file);
        if (r==EOF) return 0;
        break;
      case TSDB_POSITION:
        r=fputs(":position ",file);
        if (r==EOF) return 0;
        break;
      } /* switch */
      if (j==relation->keys[k]) {
        r=fputs(":key",file);
        if (r==EOF) return 0;
        k++;
      } /* if */
      r=fputs("\n",file);
      if (r==EOF) return 0;
    }/* for */
    r=fputs("\n",file);
    if (r==EOF) return 0;
  } /* for */
  
  r = rename(temp_name,tsdb.relations_file);
  fclose(file);
  if (r==-1) {
    perror("rename()");
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
  char buf[2048 + 1], *foo, *bar;
  int c;

  relation = tsdb_create_relation();

  if(feof(input)) {
    return((Tsdb_relation *)NULL);
  } /* if */

  while(isspace(c = fgetc(input)));
  buf[0] = (char)c;

  if(fgets(&buf[1], 2047, input) != NULL) {
    if(tsdb_check_potential_command(&buf[0])) {
      if(tsdb.error) {
         fprintf(tsdb_error_stream,
                 "read_relation(): ignoring invalid tsdb(1) command.\n");
      } /* if */
      tsdb_free_relation(relation);
      if(!feof(input) && (relation = tsdb_read_relation(input)) != NULL) {
        return(relation);
      } /* if */
    } /* if */
    else if(buf[0] == TSDB_COMMENT_CHRARACTER) {
      tsdb_free_relation(relation);
      if(!feof(input) && (relation = tsdb_read_relation(input)) != NULL) {
        return(relation);
      } /* if */
    } /* if */
    else if((foo = strchr(&buf[0], ':')) == NULL) {
      if((foo = strchr(&buf[0], '\n')) != NULL) {
        *foo = 0;
      } /* if */
      fprintf(tsdb_error_stream,
              "read_relation(): invalid relation `%s'.\n",
              &buf[0]);
      for(foo = fgets(&buf[0], 2048, input);
          foo != NULL && buf[0] != '\n';
          foo = fgets(&buf[0], 2048, input));
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
    
    while(fgets(&buf[0], 2048, input) != NULL && buf[0] != '\n') {
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
      if((bar = strchr(foo , TSDB_COMMENT_CHRARACTER)) != NULL) {
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
        else if(strstr(bar, "date") != NULL) { 
          relation->types[relation->n_fields] = TSDB_DATE;
        } /* if */
        else if(strstr(bar, "position") != NULL) { 
          relation->types[relation->n_fields] = TSDB_POSITION;
        } /* if */
        else {
          fprintf(tsdb_error_stream,
                  "read_relation(): `%s' has no type in relation `%s'.\n",
                  foo, relation->name);
          for(foo = fgets(&buf[0], 2048, input);
              foo != NULL && buf[0] != '\n';
              foo = fgets(&buf[0], 2048, input));
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
        } /* if */
        else {
          relation->keys
            = (int *)realloc(relation->keys, 
                             (relation->n_keys + 1) * sizeof(int));
        } /* else */
        relation->keys[relation->n_keys] = relation->n_fields;
        if(strstr(bar, "partial") != NULL) {
          relation->types[relation->n_keys] |= TSDB_PARTIAL;
        } /* if */
        relation->n_keys++;
      } /* if */
      if(strstr(bar, "unique") != NULL) { 
        if(relation->types[relation->n_fields] != TSDB_INTEGER) {
          fprintf(tsdb_error_stream,
                  "read_relation(): illegal attribute (`unique') for "
                  "non-integer field `%s'.\n",
                  foo);
          fflush(tsdb_error_stream);
        } /* if */
        else {
          relation->types[relation->n_fields] |= TSDB_UNIQUE;
        } /* else */
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

int tsdb_count(Tsdb_value *relation) {

  FILE *data;
  int c, i, n;

  if((data = tsdb_find_data_file(relation->value.string, "r")) == NULL) {
    return(tsdb.error);
  } /* if */

  for(i = n = 0; (c = getc(data)) != EOF; i++) {
    if(c == '\n') ++n;
  } /* for */
  if(pclose(data) == -1) fclose(data);
  fprintf(tsdb_default_stream,
          "%s: %d record(s) (%d byte(s)).\n",
          relation->value.string, n, i);
  fflush(tsdb_default_stream);
  return(TSDB_OK);

} /* tsdb_count() */

int tsdb_write_table(Tsdb_selection *selection, BOOL raw) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_write_table()
|*     version: 
|*  written by: tom fettig, dfki saarbruecken
|* last update: 15-apr-97
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

  FILE *output;
  char *foo;
  Tsdb_key_list *bar;
  int i, n;
  
  bar = selection->key_lists[0];
  if((output = tsdb_find_data_file(selection->relations[0]->name, "r"))
     == NULL) {
    return(tsdb.error);
  } /* if */
  
  if((foo = tsdb_data_backup_file(selection->relations[0]->name)) 
     == NULL) {
    return(tsdb.error);
  } /* if */ 

  fclose(output);
  if((output = tsdb_find_data_file(selection->relations[0]->name, "w"))
     == NULL) {
    return(tsdb.error);
  } /* if */
    
  for(i = 0, n = 1;
      i < selection->length && n == 1;
      i++) {
    n = tsdb_write_tuple(bar, output, raw);
    bar = bar->next;
  } /* for */
  fclose(output);
  if (n != 1) {
    tsdb_restore_data_file(selection->relations[0]->name, foo);
#ifdef DEBUG
    fprintf(tsdb_debug_stream,
            "write_table(): reverted data file for `%s' from backup.\n",
            selection->relations[0]->name);
    fflush(tsdb_debug_stream);
#endif
    return(TSDB_WRITE_FILE_ERROR);
  } /* if */
  else {
    if(!(tsdb.status & TSDB_BACKUP_DATA_FILES)) {
      remove(foo);
    } /* if */
    return(TSDB_OK);
  } /* else */

} /* tsdb_write_table() */

Tsdb_selection *tsdb_read_table(Tsdb_relation *relation,
                                Tsdb_node *condition) {

/*****************************************************************************\
|*        file: 
|*      module: 
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 29-jul-96
|*  updated by: oe, coli saarbruecken
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

  if(((input = tsdb_find_data_file(relation->name, "r")) != NULL)) {

    (void)tsdb_insert_into_selection((Tsdb_selection *)NULL,
                                     (Tsdb_tuple **)NULL);

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
    while((tuple = tsdb_read_tuple(relation, input)) != NULL) {
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
    } /* while */

    if(pclose(input) == -1) {
      fclose(input);
    } /* if */
#if defined(DEBUG) && defined(READ_TABLE)
  if((time = tsdb_timer(time)) != (float)-1) {
    fprintf(tsdb_debug_stream,
            "read_table(): read %d tuples for `%s' in %.1f seconds.\n",
            selection->length, relation->name, time);
    fflush(tsdb_debug_stream);
  } /* if */
#endif
    tsdb.error = TSDB_OK;
    return(selection);
  } /* if */
  else {
    fprintf(tsdb_error_stream,
            "read_table(): no data file for `%s'.\n", relation->name);
    fflush(tsdb_error_stream);
    tsdb.error = TSDB_NO_DATA_ERROR;
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

  if(!tsdb.max_results) {
    return((FILE *)NULL);
  } /* if */
  if(tsdb.max_results < 0) {
    fprintf(tsdb_error_stream,
            "open_result(): invalid value (%d) for `max_results'.\n",
            tsdb.max_results);
    return((FILE *)NULL);
  } /* if */

  (void)sprintf(&old[0], "%s%s", tsdb.result_path, tsdb.result_prefix);
  for(i = tsdb.max_results; i > 1; i--) {
    (void)sprintf(&new[0],
                  "%s%s%d", tsdb.result_path, tsdb.result_prefix, i);
    (void)sprintf(&old[0],
                  "%s%s%d", tsdb.result_path, tsdb.result_prefix, i - 1);
    (void)rename(old, new);
  } /* for */
  
  if((output = fopen(&old[0], "w")) == NULL) {
    fprintf(tsdb_error_stream,
            "open_result(): unable to create \%s' [%d].\n", 
            &old[0], errno);
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
 tsdb_print_value(node->node,stream, FALSE);
 fprintf(stream,"\n");
 if (node->left)
   tsdb_tree_print(node->left,stream);
 if (node->right)
   tsdb_tree_print(node->right,stream);
 insert--;

}

int tsdb_save_changes(BOOL implicit) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_save_changes()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int i = 0, status;
  Tsdb_selection *selection;

  if(tsdb.status & TSDB_READ_ONLY) {
    fprintf(tsdb_error_stream,
            "save_changes(): `%s' is read-only.\n",
            tsdb.data_path);
    fflush(tsdb_error_stream);
    return(TSDB_READ_ONLY_ERROR);
  } /* if */

  for (i = 0, status = TSDB_OK; tsdb.relations[i] != NULL; i++) {
    if (tsdb.relations[i]->status != TSDB_CLEAN) {
      selection = tsdb_find_table(tsdb.relations[i]);
      if(implicit || (tsdb.status & TSDB_IMPLICIT_COMMIT)) {
        if(tsdb_write_table(selection, TRUE) == TSDB_OK) {
          tsdb.relations[i]->status = TSDB_CLEAN;
#if defined(DEBUG) && defined(COMMIT)
          fprintf(tsdb_debug_stream,
                  "save_changes(): written %d record(s) for `%s'.\n",
                  selection->length, tsdb.relations[i]->name);
          fflush(tsdb_debug_stream);
#endif
        } /* if */
        else {
          status = tsdb.error;
        } /* else */
      } /* if */
      else {
      /*
       * _fix_me_
       * make an attempt not to loose data: in interactive mode (at least)
       * query user first and allow save of individual relations.
       *                                              (29-jul-96  -  oe)
       */
        fprintf(tsdb_error_stream,
                "save_changes(): ignoring modified data for `%s' "
                "(%d record(s)).\n",
                tsdb.relations[i]->name, selection->length);
        fflush(tsdb_error_stream);
        status = TSDB_WRITE_FILE_ERROR;
      } /* else */
    } /* if */
  } /* for */

  return(status);

} /* tsdb_save_changes() */

int tsdb_lock(BOOL flag) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_lock()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/


  char *user;

  user = tsdb_user();

  /*
   * _fix_me_
   * actually create or release lock by creating a lock file containing our
   * getpid() in the database directory.
   *                                                 (31-jul-96  -  oe)
   */
  return(TSDB_OK);

} /* tsdb_lock() */
