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
#include <memory.h>
#include "globals.h"
#include "tsdb.h"
#include <readline/readline.h>
#include <readline/history.h>

#define TSDB_HISTORY_FILE ".tsdb_history"

char *tsdb_commands[] = {
  "create table",
  "drop table",
  "alter table",
  "retrieve",
  "select",
  "insert",
  "delete",
  "update",
  "info",
  "set",
  "quit",
  "commit",
  "close",
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
  "select",
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
  "on",
  "off",
  (char *)NULL
};

char *tsdb_constants[] = {
  "home",
  "tsdb_home",
  "relations-file",
  "tsdb_relations_file",
  "data-path",
  "tsdb_data_path",
  (char *)NULL
};

#define TSDB_CONST_NUM 6

char *tsdb_variables[] = {
  "result-path",
  "tsdb_result_path",
  "result-prefix",
  "tsdb_result_prefix",
  "max-results",
  "tsdb_max_results",
  "history-size",
  "tsdb_history_size",
  "uniquely-project",
  "tsdb_uniquely_project",
#ifdef ALEP
  "tsdb_tx_output",
  "tx-output",
#endif
  (char *)NULL
};

#ifndef ALEP
#  define TSDB_VAR_NUM 10
#else
#  define TSDB_VAR_NUM 12
#endif

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
  int status;

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
    tsdb_free(foo);
  } /* if */

  tsdb_parse_options(argc, argv);
  if((status = tsdb_initialize())) {
#ifdef DEBUG
    tsdb_close_debug(tsdb_debug_stream);
#endif
    exit(status);
  } /* if */

  if(tsdb.status & TSDB_SERVER_MODE) {
    if(tsdb_server_initialize()) {
#ifdef DEBUG
      tsdb_close_debug(tsdb_debug_stream);
#endif
      exit(1);
    } /* if */
    tsdb_server();
  } /* if */

  if(tsdb.status & TSDB_CLIENT_MODE) {
#ifdef ALEP
    exit(tsdb_alep_client(tsdb.query));
#else
    fprintf(tsdb_error_stream,
            "main(): tsdb(1) client mode not yet implemented.\n");
    fflush(tsdb_error_stream);
    exit(1);
#endif    
  } /* if */

  signal(SIGPIPE, SIG_IGN);

  if(tsdb.query != NULL) {
    status = tsdb_parse(strdup(tsdb.query), (FILE *)NULL);
  } /* if */
  else {
    status = 0;
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

    tsdb.command = 0;
    if(tsdb.status & TSDB_TSDB_CLIENT) {
      sprintf(prompt, "%c", TSDB_CLIENT_CONNECT_OK);
    } /* if */
    else {
      sprintf(prompt, "tsdb@%s (%d) # ", host, tsdb.command);
    } /* else */

    while(!(tsdb.status & TSDB_QUIT) && ((foo = readline(prompt)) != NULL)) {
      for(; *foo && isspace(*foo); foo++);
      if(*foo) {
        for(bar = &foo[strlen(foo) - 1];
            bar >= foo && isspace(*bar);
            *bar = (char)0, bar--);
        if(input == NULL) {
          input = strdup(foo);
        } /* if */
        else {
          input = (char *)realloc(input, strlen(input) + strlen(foo) + 2);
          input = strcat(input, " ");
          input = strcat(input, foo);
        } /* else */
        tsdb_free(foo);
        if(input != NULL && *input && input[strlen(input) - 1] == '.') {
          tsdb_parse(input, stdin);
          add_history(input);
          input = (char *)NULL;
          if(tsdb.status & TSDB_TSDB_CLIENT) {
            sprintf(prompt, "%c", TSDB_CLIENT_CONNECT_OK);
          } /* if */
          else {
            sprintf(prompt, "tsdb@%s (%d) # ", host, tsdb.command);
          } /* else */
          tsdb.command++;
        } /* if */
        else {
          if(!(tsdb.status & TSDB_TSDB_CLIENT)) {
            sprintf(prompt, "> ");
          } /* if */
          else {
            *prompt = (char)0;
          } /* else */
        } /* else */
      } /* if */
    } /* while */
    
    if(write_history(TSDB_HISTORY_FILE)) {
      fprintf(tsdb_error_stream,
              "main(): unable to write to history file `%s'.\n",
              TSDB_HISTORY_FILE);
    } /* if */
  } /* else */

  (void)tsdb_save_changes(FALSE);

#ifdef DEBUG
  tsdb_close_debug(tsdb_debug_stream);
#endif

  exit(status);
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
    {"shutdown", optional_argument, 0, TSDB_SHUTDOWN_OPTION},
    {"hangup", optional_argument, 0, TSDB_HANGUP_OPTION},
    {"client", no_argument, 0, TSDB_CLIENT_OPTION},
    {"port", required_argument, 0, TSDB_PORT_OPTION},
    {"home", required_argument, 0, TSDB_HOME_OPTION},
    {"relations-file", required_argument, 0, TSDB_RELATIONS_FILE_OPTION},
    {"data-path", required_argument, 0, TSDB_DATA_PATH_OPTION},
    {"result-path", required_argument, 0, TSDB_RESULT_PATH_OPTION},
    {"result-prefix", required_argument, 0, TSDB_RESULT_PREFIX_OPTION},
    {"max-results", optional_argument, 0, TSDB_MAX_RESULTS_OPTION},
    {"implicit-commit", optional_argument, 0, TSDB_IMPLICIT_COMMIT_OPTION},
    {"debug-file", required_argument, 0, TSDB_DEBUG_FILE_OPTION},
    {"history-size", required_argument, 0, TSDB_HISTORY_OPTION},
    {"uniquely-project", optional_argument, 0, TSDB_UNIQUELY_PROJECT_OPTION},
    {"pager", optional_argument, 0, TSDB_PAGER_OPTION},
#ifdef COMPRESSED_DATA
    {"compress", optional_argument, 0, TSDB_COMPRESS_OPTION},
    {"uncompress", required_argument, 0, TSDB_UNCOMPRESS_OPTION},
    {"suffix", required_argument, 0, TSDB_SUFFIX_OPTION},
#endif
    {"fs", required_argument, 0, TSDB_FS_OPTION},
    {"ofs", required_argument, 0, TSDB_OFS_OPTION},
    {"output", required_argument, 0, TSDB_OUTPUT_OPTION},
    {"query", required_argument, 0, TSDB_QUERY_OPTION},
    {"usage", no_argument, 0, TSDB_USAGE_OPTION},
    {"help", no_argument, 0, TSDB_USAGE_OPTION},
    {"version", no_argument, 0, TSDB_VERSION_OPTION},
#ifdef ALEP
    {"tx", no_argument, 0, TSDB_TX_OPTION},
#endif
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
          tsdb.status |= TSDB_CLIENT_MODE;
          tsdb.status &= ~TSDB_SERVER_MODE;
        } /* if */
        else {
          tsdb.status |= TSDB_SERVER_MODE;
        } /* else */
        break;
      case TSDB_SHUTDOWN_OPTION:
        if(optarg != NULL) {
          tsdb.server = strdup(optarg);
          tsdb.status |= TSDB_CLIENT_MODE;
          tsdb.status |= TSDB_QUIT;
          tsdb.status &= ~TSDB_SERVER_MODE;
        } /* if */
        else {
          tsdb.status |= TSDB_CLIENT_MODE;
          tsdb.status |= TSDB_QUIT;
          tsdb.status &= ~TSDB_SERVER_MODE;
        } /* else */
        break;
      case TSDB_HANGUP_OPTION:
        if(optarg != NULL) {
          tsdb.server = strdup(optarg);
          tsdb.status |= TSDB_CLIENT_MODE;
          tsdb.status |= TSDB_HANGUP;
          tsdb.status &= ~TSDB_SERVER_MODE;
        } /* if */
        else {
          tsdb.status |= TSDB_CLIENT_MODE;
          tsdb.status |= TSDB_HANGUP;
          tsdb.status &= ~TSDB_SERVER_MODE;
        } /* else */
        break;
      case TSDB_CLIENT_OPTION:
        tsdb.status |= TSDB_CLIENT_MODE;
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
      case TSDB_IMPLICIT_COMMIT_OPTION:
        if(optarg == NULL) {
          tsdb.status |= TSDB_IMPLICIT_COMMIT;
        } /* if */
        else {
          if(!strcmp(optarg, "on")) {
            tsdb.status |= TSDB_IMPLICIT_COMMIT;
          } /* if */
          else {
            tsdb.status &= ~TSDB_IMPLICIT_COMMIT;
          } /* else */
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
      case TSDB_UNIQUELY_PROJECT_OPTION:
        if(optarg == NULL) {
          tsdb.status &= ~TSDB_UNIQUELY_PROJECT;
        } /* if */
        else {
          if(!strcmp(optarg, "on")) {
            tsdb.status |= TSDB_UNIQUELY_PROJECT;
          } /* if */
          else {
            tsdb.status &= ~TSDB_UNIQUELY_PROJECT;
          } /* else */
        } /* else */
        break;
      case TSDB_QUERY_OPTION:
        if(optarg != NULL) {
          tsdb.query = strdup(optarg);
          if((bar = strrchr(tsdb.query, '.')) != NULL) {
            for(++bar; *bar && isspace(*bar); bar++);
          } /* if */
          if(bar == NULL || *bar) {
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
                tsdb_rcs_strip(tsdb_revision_date, "Date"));
        exit(0);
        break;
      case TSDB_HISTORY_OPTION:
        if (optarg != NULL) {
          int l = (int)strtol(optarg,&bar,10);
          if ( l == 0 
              && optarg == bar ) {
            fprintf(tsdb_error_stream,
                    "parse_options(): "
                    "non-integer (`%s') argument to `-history-size'.\n",
                    optarg);            
            tsdb.history_size = -1;
          } /* if */
          else
            tsdb.history_size = l;
          /* further things are done in tsdb_initialize!! */
        } 
        else {
          tsdb_usage();
          exit(0);
        }
        break;
      case TSDB_FS_OPTION:
        if(optarg != NULL) {
          tsdb.fs = optarg[0];
        } /* if */
        break;
      case TSDB_OFS_OPTION:
        if(optarg != NULL) {
          tsdb.ofs = strdup(optarg);
        } /* if */
        break;
      case TSDB_OUTPUT_OPTION:
        if(optarg != NULL) {

          char *path;
          FILE *output;
          
          if((path = tsdb_expand_file((char *)NULL, optarg)) == NULL) {
            fprintf(tsdb_error_stream,
                    "parse_options(): unable to expand file name `%s'.\n", 
                    optarg);
            fflush(tsdb_error_stream);
            break;
          } /* if */

          if((output = fopen(path, "w")) == NULL) {
            fprintf(tsdb_error_stream,
                    "parse_options(): unable to open output file `%s'.\n", 
                    path);
            fflush(tsdb_error_stream);
            break;
          } /* if */
          tsdb.output = path;
          tsdb_default_stream = output;
        } /* if */
        break;
#ifdef ALEP
      case TSDB_TX_OPTION:
        tsdb.status |= TSDB_TX_OUTPUT;
        break;
#endif
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
          "go into client mode connecting to server on `host';\n");
  fprintf(tsdb_error_stream,
          "  `-hangup=host' --- "
          "(attemp to) shut down tsdb(1) server on `host';\n");
  fprintf(tsdb_error_stream,
          "  `-shutdown=host' --- "
          "(really) shut down tsdb(1) server on `host';\n");
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
  fprintf(tsdb_error_stream,
          "  `-history-size[={_0_ | 1 | ...}]' --- size of query storage;\n");
  fprintf(tsdb_error_stream,
          "  `-implicit-commit[={_on_ | off}]' --- "
          "always commit (and save) changes;\n");
  fprintf(tsdb_error_stream,
          "  `-uniquely-project[={on | _off_}]' --- "
          "remove duplicates from projections;\n");
#ifdef DEBUG
  fprintf(tsdb_error_stream,
          "  `-debug-file=file' --- output file for debug information;\n");
#endif
  fprintf(tsdb_error_stream,
          "  `-pager[={command | _off_}' --- pager command to use;\n");
  fprintf(tsdb_error_stream,
          "  `-fs=character' --- field separator character;\n");
  fprintf(tsdb_error_stream,
          "  `-ofs=string' --- output field separator character;\n");
  fprintf(tsdb_error_stream,
          "  `-output=file' --- output file;\n");
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
  if (attr_num+rel_num+TSDB_VAR_NUM+TSDB_CONST_NUM+10 > num) {
    num+=num;
    matches = (char**)realloc(matches,(num+1)*sizeof(char*));
  }
  for (n=0;words[n];n++);
  
  last = last_keyword(words,n);
  
  switch (last) {
  case 0: matches = zero;
    break;
  case 1: /* add */
    /* attributnamen */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    break;
  case 2: /* create/alter/drop >table< */
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
  case 3: /* >create< */
  case 4: /* >alter< */
  case 5:matches = tvf; /* >drop< */
    break;
  case 6: /* >where< */
    /* attributnamen */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    break;
  case 7: /* >from< */
    /* relationsnamen oder where */
    memcpy(matches,names_r,sizeof(char*)*(1+rel_num));
    memcpy(matches+rel_num,where,2*sizeof(char*));
    break;
  case 8: 
  case 9: /* >select,retrieve< */
    /* attributnamen oder from oder where */
    memcpy(matches,names_a,(attr_num+1)*sizeof(char*));
    memcpy(matches+attr_num,fr_wh,3*sizeof(char*));
    break;
  case 10: /* >values< */
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
  case 13: /* info */
    memcpy(matches,names_r,(rel_num+1)*sizeof(char*));
    memcpy(matches+rel_num,tsdb_variables,(TSDB_VAR_NUM+1)*sizeof(char*));
    memcpy(matches+rel_num+TSDB_VAR_NUM,tsdb_constants,
           (TSDB_CONST_NUM+1)*sizeof(char*));
    break;
  case 14: /* set */
    memcpy(matches,tsdb_variables,(TSDB_VAR_NUM+1)*sizeof(char*));
    break;
  default: 
    matches[0]=NULL;
  } /* switch */

  return(matches);
}

char **tsdb_completion(char *text, int start, int end) {

  char **matches = (char **)NULL;
  
  int i = 0;

  while ((text[i]!='\0') && isspace(text[i])) 
    i++;
  if ((start == 0)&& (start<=i)) {
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
      return(strdup(name));
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
