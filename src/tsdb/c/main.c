/*****************************************************************************\
|*        file: main.c
|*      module: TSDB standalone version
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include "globals.h"
#include "tsdb.h"
#include <readline/readline.h>
#include <readline/history.h>

#define TSDB_HISTORY_FILE ".tsdb_history"

char tsdb_version[] = TSDB_VERSION;
char tsdb_revision[] = "$Revision$";
char tsdb_date[] = "$Date$";

char *tsdb_commands[] = {
  "create table",
  "drop table",
  "alter table",
  "retrieve",
  "select",
  "delete",
  "update",
  "quit",
  (char *)NULL 
};

char *rev_complete[] = {
  ":",
  "add",
  "table",
  "create",
  "alter",
  "drop",
  "where",
  "from",
  "retrieve",
  "select"
  "values",
  "into",
  "insert",
  "info",
  "set",
  (char*)NULL
};

char *tsdb_keywords[] = {
  "from",
  "where",
  "into",
  "add",
  "table",
  "values",
  "relations",
  ":integer",
  ":string",
  ":key",
  (char *)NULL
};

char *tsdb_rest_generate(char *, int) ;
char **tsdb_completion(char *, int, int);
char *tsdb_command_generate(char *, int);
char *tsdb_others_generate(char *, int);
int initialize_readline(void);
BOOL tsdb_command(char *);

int main(int argc, char **argv) {

/*****************************************************************************\
|*        file: 
|*      module: main()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* main() is the root of all evil.
\*****************************************************************************/

  char *input = NULL;
  char host[512 + 1], prompt[80 + 1], *foo, *bar;
  int n_commands = 0;

  if((foo = strdup(argv[0])) != NULL) {
    if((bar = strrchr(foo, TSDB_DIRECTORY_DELIMITER[0])) != NULL) {
      *bar = 0;
      bar++;
    } /* if */
    else {
      bar = foo;
    } /* else */
    if(!strcmp(bar, "tsdbd")) {
      tsdb.status |= TSDB_SERVER_MODE;
    } /* if */
    free(foo);
  } /* if */

  tsdb_parse_options(argc, argv);
  if(!tsdb_initialize()) {
#ifdef DEBUG
    tsdb_close_debug(tsdb_debug_stream);
#endif
    exit(1);
  } /* if */

  if(tsdb.status & TSDB_SERVER_MODE) {
    if(tsdb_server_initialize()) {
      exit(-1);
    } /* if */
    tsdb_server();
  } /* if */

#ifdef SYSV
  struct sigaction tsdb_sig = { SIG_IGN, (sigset_t)0, 0 };
  sigaction(SIGPIPE, &tsdb_sig);
#else
  signal(SIGPIPE, SIG_IGN);
#endif

  if(tsdb.query != NULL) {
    (void)tsdb_parse(tsdb.query);
  } /* if */
  else {
    using_history();
    if(read_history(TSDB_HISTORY_FILE)) {
      fprintf(tsdb_error_stream,
              "main(): no history file `%s'.\n",
              TSDB_HISTORY_FILE);
    } /* if */
    
    initialize_readline();
    if(gethostname(&host[0], 512)) {
      perror("main(): gethostname(): ");
      host[0] = 0;
    } /* if */
    else {
      if((foo = strchr(host, '.')) != NULL) {
        *foo = 0;
      } /* if */
    } /* else */
    
    sprintf(prompt, "tsdb@%s (%d) # ", host, n_commands);
    
    while(!(tsdb.status & TSDB_QUIT) && ((foo = readline(prompt)) != NULL)) {
      if(*foo) {
        if(input == NULL) {
          input = strdup(foo);
        } /* if */
        else {
          input = (char *)realloc(input, strlen(input) + strlen(foo) + 2);
          input = strcat(input, " ");
          input = strcat(input, foo);
        } /* else */
        free(foo);
        if(input != NULL && strchr(input, '.')) {
          tsdb_parse(input);
          add_history(input);
          free(input);
          input = (char *)NULL;
          sprintf(prompt, "tsdb@%s (%d) # ", host, ++n_commands);
        } /* if */
        else {
          sprintf(prompt, "> ", host, n_commands);
        } /* else */
      } /* if */
    } /* while */
    
    if(write_history(TSDB_HISTORY_FILE)) {
      fprintf(tsdb_error_stream,
              "main(): unable to write to history file `%s'.\n",
              TSDB_HISTORY_FILE);
    } /* if */
  } /* else */

  tsdb_save_changes();

#ifdef DEBUG
  tsdb_close_debug(tsdb_debug_stream);
#endif

  exit(0);
} /* main() */

void tsdb_parse_options(int argc, char **argv) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_parse_options()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int c, foo;
  char *bar;
  struct option options[] = {
    {"server", optional_argument, 0, TSDB_SERVER_OPTION},
    {"port", required_argument, 0, TSDB_PORT_OPTION},
    {"home", required_argument, 0, TSDB_HOME_OPTION},
    {"relations-file", required_argument, 0, TSDB_RELATIONS_FILE_OPTION},
    {"data-path", required_argument, 0, TSDB_DATA_PATH_OPTION},
    {"result-path", required_argument, 0, TSDB_RESULT_PATH_OPTION},
    {"result-prefix", required_argument, 0, TSDB_RESULT_PREFIX_OPTION},
    {"max-results", optional_argument, 0, TSDB_MAX_RESULTS_OPTION},
    {"debug-file", required_argument, 0, TSDB_DEBUG_FILE_OPTION},
    {"pager", optional_argument, 0, TSDB_PAGER_OPTION},
    {"query", required_argument, 0, TSDB_QUERY_OPTION},
    {"usage", no_argument, 0, TSDB_USAGE_OPTION},
    {"help", no_argument, 0, TSDB_USAGE_OPTION},
    {"version", no_argument, 0, TSDB_VERSION_OPTION},
    {0, 0, 0, 0}
  }; /* struct option */

  optind = 0;
  opterr = 1;

  while((c = getopt_long_only(argc, argv, "", options, &foo)) != EOF) {
    switch(c) {
      case '?':
        tsdb_usage();
        exit(-1);
        break;
      case TSDB_SERVER_OPTION:
        if(optarg != NULL) {
          tsdb.server = strdup(optarg);
        } /* if */
        else {
          tsdb.status |= TSDB_SERVER_MODE;
        } /* else */
        break;
      case TSDB_PORT_OPTION:
        if(optarg != NULL) {
          if((tsdb.port = strtol(optarg, &bar, 10)) == 0
             && optarg == bar) {
            fprintf(tsdb_error_stream,
                    "parse_options(): "
                    "non-integer (`%s') argument to `-port'.\n",
                    optarg);
            tsdb.port = 0;
          } /* if */
        } /* if */
        break;
      case TSDB_HOME_OPTION:
        if(optarg != NULL) {
          tsdb.home = tsdb_expand_directory((char *)NULL, optarg);
        } /* if */
        break;
      case TSDB_RELATIONS_FILE_OPTION:
        if(optarg != NULL) {
          tsdb.relations_file = strdup(optarg);
        } /* if */
        break;
      case TSDB_DATA_PATH_OPTION:
        if(optarg != NULL) {
          tsdb.data_path = tsdb_expand_directory(tsdb.home, optarg);
        } /* if */
        break;
      case TSDB_RESULT_PATH_OPTION:
        if(optarg != NULL) {
          tsdb.result_path = tsdb_expand_directory((char *)NULL, optarg);
        } /* if */
        break;
      case TSDB_RESULT_PREFIX_OPTION:
        if(optarg != NULL) {
          tsdb.result_prefix = strdup(optarg);
        } /* if */
        break;
      case TSDB_MAX_RESULTS_OPTION:
        if(optarg != NULL) {
          if((tsdb.max_results = strtol(optarg, &bar, 10)) == 0
             && optarg == bar) {
            fprintf(tsdb_error_stream,
                    "parse_options(): "
                    "non-integer (`%s') argument to `-max-results'.\n",
                    optarg);
            tsdb.max_results = -1;
          } /* if */
        } /* if */
        else {
          tsdb.max_results = 0;
        } /* else */
        break;
#ifdef DEBUG
      case TSDB_DEBUG_FILE_OPTION:
        if(optarg != NULL) {
          tsdb.debug_file = strdup(optarg);
        } /* if */
        break;
#endif
      case TSDB_PAGER_OPTION:
        if(optarg != NULL) {
          tsdb.pager = strdup(optarg);
        } /* if */
        else {
          tsdb.pager = (char *)NULL;
        } /* else */
        break;
      case TSDB_QUERY_OPTION:
        if(optarg != NULL) {
          tsdb.query = strdup(optarg);
          if(strchr(tsdb.query, '.') == NULL) {
            tsdb.query = (char *)realloc(tsdb.query, strlen(tsdb.query) + 2);
            tsdb.query = strcat(tsdb.query, ".");
          } /* if */
        } /* if */
        break;
       case TSDB_USAGE_OPTION:
        tsdb_usage();
        exit(0);
        break;
      case TSDB_VERSION_OPTION:
        fprintf(tsdb_error_stream,
                "tsdb(1) %s (%s) [%s] --- (c) oe@tsnlp.dfki.uni-sb.de.\n",
                tsdb_version,
                tsdb_rcs_strip(tsdb_revision, "Revision"),
                tsdb_rcs_strip(tsdb_date, "Date"));
        exit(0);
        break;
      } /* switch */
  } /* while */
} /* tsdb_parse_options() */

void tsdb_usage() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_usage()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  fprintf(tsdb_error_stream,
          "usage: `tsdb [options]'; "
          "valid options are (default values underlined):\n\n");
  fprintf(tsdb_error_stream,
          "  `-server' --- go into server (daemon) mode;\n");
  fprintf(tsdb_error_stream,
          "  `-server=host' --- "
          "go into client mode connecting to server `host';\n");
  fprintf(tsdb_error_stream,
          "  `-client' --- go into client mode;\n");
  fprintf(tsdb_error_stream,
          "  `-port=n' --- server TCP port address;\n");
  fprintf(tsdb_error_stream,
          "  `-home=directory' --- root directory for database;\n");
  fprintf(tsdb_error_stream,
          "  `-relations-file=file' --- relations file for database;\n");
  fprintf(tsdb_error_stream,
          "  `-data-path=directory' --- data directory for database;\n");
  fprintf(tsdb_error_stream,
          "  `-result-path=directory' "
          "--- directory to store query results;\n");
  fprintf(tsdb_error_stream,
          "  `-result-prefix=string' --- file prefix for query results;\n");
  fprintf(tsdb_error_stream,
          "  `-max-results[={_0_ | 1 | ...}]' "
          "--- maximum of stored query results;\n");
#ifdef DEBUG
  fprintf(tsdb_error_stream,
          "  `-debug-file=file' --- output file for debug information;\n");
#endif
  fprintf(tsdb_error_stream,
          "  `-pager[={command | _off_}' --- pager command to use;\n");
  fprintf(tsdb_error_stream,
          "  `-query=string' --- query to be processed in batch mode;\n");
  fprintf(tsdb_error_stream,
          "  `-usage' or `-help' --- this message (give it a try |:-);\n");
  fprintf(tsdb_error_stream,
          "  `-version' --- current TSDB version.\n");
  fflush(tsdb_error_stream);
} /* tsdb_usage() */

int initialize_readline(void) {

  rl_attempted_completion_function = (CPPFunction *)tsdb_completion;

} /* initialize_readline */

char* skip_whitespace(char* text) {
  while ((*text!='\0')&&isspace(*text))
    text++;
  return(text);
}

char* skip_non_whitespace(char *text) {
  while ((*text!='\0')&&!isspace(*text))
    text++;
  return(text);
}

char** string_to_words(char* text) {
  int n,k=0,nw=10;
  char **words,*start,*end,c;
  
  words = (char**)malloc(sizeof(char*)*nw);
  n = strlen(text);
  
  start = text;
  while (strlen(start)){
    start = skip_whitespace(start);
    if (*start) {
      end = skip_non_whitespace(start);
      c = *end;
      *end='\0';
      if (k==nw-2) {
        nw*=2;
        words = (char**)realloc(words,sizeof(char*)*nw);
      }
      words[k++]= strdup(start);
      *end = c;
      start = end;
    } /* if */
  } /* while */

  words[k]=NULL;
  return(words);
      
} /* string_to_words */

int key_word_num(char* word) {
  
  return 0;
}

int keyword(char* word) {
  int i;

  for (i=0;rev_complete[i];i++) {
    if (!strcmp(rev_complete[i],word))
      return i;
  } /* for */
  return(-1);
}

int last_keyword(char** words,int n) {
  int i,k;
  BOOL kaerb;

  for(i=n-1,kaerb=FALSE;!kaerb&&i>-1;i--){
    if ((k=keyword(words[i]))>-1)
      kaerb = TRUE;
  }
  if (kaerb)
    return(k);
  else
    return -1;

}/* last_keyword */

char* zero[10] = { "integer","string","key",NULL};
char* tvf[10] = {"table",NULL};
char* add[2] = {"add",NULL};
char* where[2] = {"where",NULL};
char* values[2] = {"values",NULL};
char* into[2] = {"into",NULL};
char* fr_wh[3] = {"from","where",NULL};

char** matches_rest(char* text, int start, int end) {
  char** words = string_to_words(text);
  static char** matches=NULL;
  static int num=50; 
  char **names_r, **names_a;
  int n,last,rel_num,attr_num;

  if (!matches) {
    matches = (char**)malloc((num+1)*sizeof(char*));
    memset(matches,'\0',(num+1)*sizeof(char*));
  }
  
  names_r = tsdb_all_relation_names();
  names_a = tsdb_all_attribute_names();
  rel_num = tsdb_n_relations();
  attr_num = tsdb_n_attributes();
  if (attr_num+rel_num+10 > num) {
    num+=num;
    matches = (char**)realloc(matches,(num+1)*sizeof(char*));
  }
  for (n=0;words[n];n++);
  
  last = last_keyword(words,n);
  
  switch (last) {
  case 0: matches = zero;
    break;
  case 1:
    /* attributnamen */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    break;
  case 2: /* create/alter/drop table*/
    /* relationennamen, oder wenn schon existiert add oder attributliste */
#ifdef EXACT
    v->name = words[2];
    if (n==2 ) {
      if (!strcmp(word[0],"drop")) 
        if (tsdb_is_relation(v))
          matches[0]=NULL;
        else
          memcpy(matches,names_r,sizeof(char*)*(1+rel_num));
      if (!strcmp(word[0],"alter"))
        if (tsdb_is_relation(v))
          memcpy(matches,add,2*sizeof(char*));
        else
          memcpy(matches,names_r,sizeof(char*)*(1+rel_num));
    }
    else {
      if (!strcmp(word[0],"create")) /* create */
        memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    }
#endif
    if ((n==3) || (n==2))
      memcpy(matches,names_r,sizeof(char*)*(1+rel_num));
    else {
      memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
      if (!strcmp(words[0],"alter")) {
        memcpy(matches+attr_num,add,2*sizeof(char*));
      }
    }
  case 3:
  case 4:
  case 5:matches = tvf;
    break;
  case 6: /*where */
    /* attributnamen */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    break;
  case 7: /* from */
    /* relationsnamen oder where */
    memcpy(matches,names_r,sizeof(char*)*(1+rel_num));
    memcpy(matches+rel_num,where,2*sizeof(char*));
    break;
  case 8: 
  case 9: /* select,retrieve */
    /* attributnamen oder from oder where */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    memcpy(matches+attr_num,fr_wh,3*sizeof(char*));
    break;
  case 10: /*values */
    matches[0] = NULL;
    /*nix*/
    break;
  case 11: /* into */
    /* relname oder values oder attrlist */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    memcpy(matches+attr_num,values,1*sizeof(char*));
    memcpy(matches+attr_num+1,names_r,(rel_num+1)*sizeof(char*));
    break;
  case 12: /* insert */
    /* into */
    memcpy(matches,into,2*sizeof(char*));
    break;
  default: 
    matches[0]=NULL;
  } /* switch */

  return(matches);
}

char **tsdb_completion(char *text, int start, int end) {

  char **matches = (char **)NULL;

  if(start == 0) {
    matches = completion_matches(text, tsdb_command_generate);
  }
  else {
    matches = completion_matches(text, tsdb_rest_generate);
  } 
  return(matches);

} /* tsdb_completion() */


char * tsdb_rest_generate(char* text, int state) {
  static char** matches;
  static int i,len;

  if (!state){
    matches = matches_rest(rl_line_buffer,0,rl_point);
    i = 0;
    len = strlen(text);
  }
   while(matches[i]) {
     if (strncmp(matches[i++],text,len) == 0)
       return(strdup(matches[i-1]));
  }
  return(NULL);
}

char *tsdb_command_generate(char *text, int state) {

  char *name;
  static int list_index, len;
  
  if(!state) {
    list_index = 0;
    len = strlen(text);
  }
  while(name = tsdb_commands[list_index]) {
    list_index++;
    if(strncmp(name, text, len) == 0)
      return(name);
  }
  return((char *)NULL);

} /* tsdb_command_generate() */

char *tsdb_others_generate(char *text, int state) {

  char *name;
  static int list_index, len;
  static int list2_index;
  Tsdb_relation **relations;
  
  if(!state) {
    relations = tsdb_all_relations();
    list_index = 0;
    list2_index = 0;
    len = strlen(text);
  } /* if */

  while(name = tsdb_keywords[list_index]) {
    list_index++;
    if(strncmp(name, text, len) == 0)
      return(name);
  } /* while */
  
 /* while((relations[list2_index]) != NULL) {
    name = relations[list2_index]->name;
    list2_index++;
    if(strncmp(name, text, len) == 0)  
      return(name);
  }  */
  
  return((char *)NULL);

} /* tsdb_others_generate() */

BOOL tsdb_command(char *command) {

  int num_quotes = 0;
  
  while(*command) {
    if(*command == '"')
      num_quotes++;
  } /* while */
  
  if((num_quotes % 2) != 0) {
    fprintf(stderr, "\ttsdb: quotes mismatch.\n");
    return(FALSE);
  } /* if */
  else {
    if(command[strlen(command) - 1] == '.')
      return(TRUE);
  } /* else */
    
} /* tsdb_command() */
