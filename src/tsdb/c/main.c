#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "globals.h"
#include "tsdb.h"
#include <readline/readline.h>
#include <readline/history.h>

#define TSDB_HISTORY_FILE ".tsdb_history"

BOOL quit = FALSE;

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
  (char*)NULL
};

extern char** relation_names;
extern char** attribute_names;

char *tsdb_keywords[] = {
  "from",
  "where",
  "into",
  "add",
  "table",
  "values",
  "set",
  ":integer",
  ":string",
  ":key",
  (char *)NULL
};

extern char** tsdb_relation_names;
extern char** tsdb_attribute_names;

char *tsdb_rest_generate(char* , int ) ;
char **tsdb_completion(char *, int, int);
char *tsdb_command_generate(char *, int);
char *tsdb_others_generate(char *, int);
char *cannon_whitespace(char *);
int initialize_readline(void);
BOOL tsdb_command(char *);

int main(void) {

  char *input = NULL;
  char host[512 + 1], prompt[80 + 1], *foo;
  int n_commands = 0;

  using_history();
  if(read_history(TSDB_HISTORY_FILE) != NULL) {
    fprintf(TSDB_ERROR_STREAM,
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

  tsdb_initialize();
  sprintf(prompt, "tsdb@%s (%d) # ", host, n_commands);

  while((!quit) && ((foo = readline(prompt)) != NULL)) {
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

  if(write_history(TSDB_HISTORY_FILE) != NULL) {
    fprintf(TSDB_ERROR_STREAM,
            "main(): unable to write to history file `%s'.\n",
            TSDB_HISTORY_FILE);
  } /* if */
  tsdb_save_changes();

#ifdef DEBUG
  tsdb_close_debug(tsdb_debug_stream);
#endif

} /* main() */

void main_quit(void) {

  quit = TRUE;

} /* main_quit() */

int initialize_readline(void) {

  rl_attempted_completion_function = (Function *)tsdb_completion;

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
  int n,i=0,k=0,nw=10;
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
  int i=0;
  
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
  Tsdb_value v;
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

  char **matches = (char **)NULL,**words,*line;
  int i,n;
  BOOL kaerb= FALSE;

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

char *cannon_whitespace(char *command) {

  char *result = (char *)malloc(sizeof(char));
  BOOL instring = FALSE;

  printf("command = %s\n", command);
  while(*command != NULL) {
    if(*command == '"') {
      if(!instring)
        instring = TRUE;
      else 
        instring = FALSE;
    } /* if */
    printf("%c.", *command);
    *result = *command;
    (*result)++;
    if(!instring) 
      while((*command)++ == ' ');
    else
      *command++;
  } /* while */
        
  printf("result = %s\n", result);
  return(result);

} /* cannon_whitespace() */

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
