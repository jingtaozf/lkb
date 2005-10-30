/*****************************************************************************\
|*        file: tsdb.h
|*      module: TSDB global definitions and prototypes
|*     version: 
|*  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#if defined(NOFREE)
#  define tsdb_free(foo) { ; }
#else
#  define tsdb_free(foo) free(foo)
#endif

#if defined(unix)
#  if defined(sun) && defined(__svr4__)
#    define SOLARIS
#  elif defined(sun) && !defined(__svr4__)
#    define SUNOS
#  elif defined(linux)
#    define LINUX
#  elif defined(_OSF_SOURCE)
#    define OSF
#  endif
#elif defined(__APPLE__)
#  define MACOS
#endif

#if defined(DEBUG) && defined(MALLOC) && defined(LINUX)
#  include <dbmalloc.h>
#elif !defined(MACOS)
#  include <malloc.h>
#endif

#define TSDB_VERSION "0.8"
#define TSDB_REVISION "$Revision$"
#define TSDB_REVISION_DATE "$Date$"

#define TSDB_DEFAULT_STREAM stdout
#define TSDB_ERROR_STREAM stderr

#define TSDB_SERVER_MODE 1
#define TSDB_CLIENT_MODE 2
#define TSDB_QUIT 4
#define TSDB_HANGUP 8
#define TSDB_UNIQUELY_PROJECT 16
#define TSDB_IMPLICIT_COMMIT 32
#define TSDB_TSDB_CLIENT 64
#define TSDB_STATUS 128
#define TSDB_LOCK 256
#define TSDB_BACKUP_DATA_FILES 512
#define TSDB_PROLOG_ESCAPE_OUTPUT 1024
#define TSDB_LISP_ESCAPE_OUTPUT 2048
#define TSDB_QUIET 4096
#define TSDB_READ_ONLY 8192
#define TSDB_VERIFY 16384
#define TSDB_COMMIT_ON_EXIT 32768
#ifdef ALEP
#  define TSDB_TX_OUTPUT 65536
#endif

#ifdef ALEP
#  define TSDB_INITIAL_STATUS \
     (TSDB_UNIQUELY_PROJECT \
      | TSDB_IMPLICIT_COMMIT \
      | TSDB_BACKUP_DATA_FILES)
#else
#  define TSDB_INITIAL_STATUS \
     (TSDB_UNIQUELY_PROJECT \
      | TSDB_IMPLICIT_COMMIT \
      | TSDB_BACKUP_DATA_FILES)
#endif

#ifdef ALEP
#  define TSDB_TX_FORMAT "tx(%s, '%s', '%s')."
#endif

#define TSDB_UNKNOWN_TYPE 0
#define TSDB_INTEGER 1
#define TSDB_IDENTIFIER 2
#define TSDB_STRING 3
#define TSDB_DATE 4
#define TSDB_POSITION 5
#define TSDB_CONNECTIVE 6
#define TSDB_OPERATOR 7
#define TSDB_DESCRIPTOR 8

#define TSDB_TYPE_MASK 0x0f
#define TSDB_PARTIAL 16
#define TSDB_UNIQUE 32

#define TSDB_VALUE_INCOMPATIBLE 0 
#define TSDB_EQUAL 1
#define TSDB_NOT_EQUAL 2
#define TSDB_LESS_THAN 3
#define TSDB_LESS_OR_EQUAL_THAN 4
#define TSDB_GREATER_THAN 5
#define TSDB_GREATER_OR_EQUAL_THAN 6
#define TSDB_MATCH 7
#define TSDB_NOT_MATCH 8
#define TSDB_INSENSITIVE_MATCH 9
#define TSDB_NOT_INSENSITIVE_MATCH 10

#define TSDB_NOT 0
#define TSDB_AND 1
#define TSDB_OR 2
#define TSDB_NOT_NOT 3
#define TSDB_NONE 4

#define TSDB_NEW 0
#define TSDB_CHANGED 1
#define TSDB_CLEAN 2

#define TSDB_START_TIMER 0
#define TSDB_MAX_TIMERS 20

#define TSDB_SERVER_OPTION 0
#define TSDB_CLIENT_OPTION 1
#define TSDB_PORT_OPTION 2
#define TSDB_HOME_OPTION 3
#define TSDB_RELATIONS_FILE_OPTION 4
#define TSDB_DATA_PATH_OPTION 5
#define TSDB_RESULT_PATH_OPTION 6
#define TSDB_RESULT_PREFIX_OPTION 7
#define TSDB_MAX_RESULTS_OPTION 8
#define TSDB_DEBUG_FILE_OPTION 9
#define TSDB_PAGER_OPTION 10
#define TSDB_QUERY_OPTION 11
#define TSDB_USAGE_OPTION 12
#define TSDB_VERSION_OPTION 13
#define TSDB_HISTORY_OPTION 14
#define TSDB_UNIQUELY_PROJECT_OPTION 15
#define TSDB_COMPRESS_OPTION 16
#define TSDB_UNCOMPRESS_OPTION 17
#define TSDB_SUFFIX_OPTION 18
#define TSDB_FS_OPTION 19
#define TSDB_OFS_OPTION 20
#define TSDB_OUTPUT_OPTION 21
#define TSDB_SHUTDOWN_OPTION 22
#define TSDB_HANGUP_OPTION 23
#define TSDB_IMPLICIT_COMMIT_OPTION 24
#define TSDB_STATUS_OPTION 25
#define TSDB_STANDALONE_OPTION 26
#define TSDB_STRING_ESCAPE_OPTION 27
#define TSDB_QUIET_OPTION 28
#define TSDB_READ_ONLY_OPTION 29
#define TSDB_VERIFY_OPTION 30
#define TSDB_EOF_OPTION 31
#ifdef ALEP
#  define TSDB_TX_OPTION 255
#endif

#define TSDB_REDIRECTION_OVERWRITE 1
#define TSDB_REDIRECTION_APPEND 2
#define TSDB_REDIRECTION_PIPE 3

#define TSDB_CLIENT_CONNECT_OK '\001'
#define TSDB_CLIENT_CONNECT_ERROR '\002'

#ifndef TSDB_PSEUDO_USER
#  define TSDB_PSEUDO_USER "TSDB@tsdb"
#endif

#ifndef TSDB_DIRECTORY_DELIMITER
#  define TSDB_DIRECTORY_DELIMITER "/"
#endif

#ifndef TSDB_HOME
#  define TSDB_HOME "."
#endif

#ifndef TSDB_RELATIONS_FILE
#  if 1
#    define TSDB_RELATIONS_FILE "relations"
#  else
#    define TSDB_RELATIONS_FILE "etc/relations"
#  endif
#endif

#ifndef TSDB_DATA_PATH
#  if 1
#    define TSDB_DATA_PATH ""
#  else
#    define TSDB_DATA_PATH "german/"
#  endif
#endif

#ifndef TSDB_RESULT_PATH
#  define TSDB_RESULT_PATH "/tmp/"
#endif

#ifndef TSDB_RESULT_PREFIX
#  define TSDB_RESULT_PREFIX "tsdb.query."
#endif

#ifndef TSDB_MAX_RESULTS
#  define TSDB_MAX_RESULTS 20
#endif

#ifndef TSDB_HISTORY_SIZE
#  define TSDB_HISTORY_SIZE 20
#endif

#ifndef TSDB_TEMPORARY_FILE
#  define TSDB_TEMPORARY_FILE "tsdb.tmp"
#endif

#ifndef TSDB_PAGER
#  define TSDB_PAGER "more"
#endif

#ifdef DEBUG
#  ifndef TSDB_DEBUG_FILE
#    define TSDB_DEBUG_FILE "/tmp/tsdb.debug"
#  endif
#endif

#ifndef TSDB_FS
#  define TSDB_FS '@'
#endif

#ifndef TSDB_OFS
#  define TSDB_OFS " | "
#endif

#ifndef TSDB_BACKUP_SUFFIX
#  define TSDB_BACKUP_SUFFIX "~"
#endif


#ifndef TSDB_DEFAULT_STRING_VALUE
#  define TSDB_DEFAULT_STRING_VALUE ""
#endif

#ifndef TSDB_DEFAULT_INTEGER_VALUE
#  define TSDB_DEFAULT_INTEGER_VALUE -1
#endif

#ifndef TSDB_COMPRESS
#  define TSDB_COMPRESS "gzip -c -f"
#endif

#ifndef TSDB_UNCOMPRESS
#  define TSDB_UNCOMPRESS "gzip -c -f -d"
#endif

#ifndef TSDB_SUFFIX
#  define TSDB_SUFFIX ".gz"
#endif

#ifndef TSDB_SERVER_PORT
#  define TSDB_SERVER_PORT 4711
#endif

#ifndef TSDB_SERVER_CONNECT_TIMEOUT
#  define TSDB_SERVER_CONNECT_TIMEOUT 10
#endif

#ifndef TSDB_SERVER_QUEUE_LENGTH
#  ifdef ALEP
#    define TSDB_SERVER_QUEUE_LENGTH 0
#  else
#    define TSDB_SERVER_QUEUE_LENGTH 5
#  endif
#endif

#ifndef CHAR_SET_SIZE
#  define CHAR_SET_SIZE 256
#endif

#ifndef TSDB_COMMENT_CHRARACTER
#  define TSDB_COMMENT_CHRARACTER '#'
#endif

#ifndef ISUPPER
#  define ISUPPER(c) (isascii (c) && isupper (c))
#endif

typedef struct tsdb_field {
  char *name;
  BYTE type;
  BOOL key;
} Tsdb_field;

typedef struct tsdb_value {
  BYTE type;
  union {
    long int integer;
    char *identifier;
    char *string;
    char *date;
    char *position;
    BYTE connective;
    BYTE operator;
    int *descriptor;
  } value;
} Tsdb_value;

typedef struct tsdb_node {
  struct tsdb_node *left;
  Tsdb_value *node;
  struct tsdb_node *right;
} Tsdb_node;

typedef struct tsdb_relation {
  char *name;
  char *path;
  int n_fields;
  char **fields;
  BYTE *types;
  int n_keys;
  int *keys;
  BYTE status;
} Tsdb_relation;

typedef struct tsdb_tuple {
  int n_fields;
  Tsdb_value **fields;
} Tsdb_tuple;

typedef struct tsdb_key_list {
  struct tsdb_value *key;
  int n_tuples;
  struct tsdb_tuple **tuples;
  struct tsdb_key_list *next;
} Tsdb_key_list;

typedef struct tsdb_selection {
  int n_relations; 
  struct tsdb_relation **relations;
  int n_key_lists;
  struct tsdb_key_list **key_lists;
  int length;
} Tsdb_selection;

typedef struct tsdb_history {
  int command;
  char *query;
  struct tsdb_selection *result;
  int unique_tuples;
} Tsdb_history;

typedef struct tsdb {
  int status;

  Tsdb_relation **relations;
  Tsdb_selection **data;

  char *input;

  char *home;
  char *relations_file;
  char *data_path;
  char *result_path;
  char *result_prefix;
  int max_results;

  char fs;
  char *ofs;

  char *eof;

  char *server;
  int port;
  char *pager;
  char *query;
#ifdef DEBUG
  char *debug_file;
#endif
  char *output;
  char *error_output;

  int error;

#ifdef COMPRESSED_DATA
  char *compress;
  char *uncompress;
  char *suffix;
#endif

  int command;
  Tsdb_history **history;
  int history_size;

} Tsdb;

#if !defined(TSDB_C)
  extern Tsdb tsdb;

  extern FILE *tsdb_default_stream;
  extern FILE *tsdb_error_stream;
#ifdef DEBUG
  extern FILE *tsdb_debug_stream;
#endif

  extern char tsdb_version[];
  extern char *tsdb_revision;
  extern char *tsdb_revision_date;
#endif

void tsdb_parse_options(int, char **);
void tsdb_usage(void);
char *tsdb_readline(char *);
int tsdb_parse(char *, FILE *);
int tsdb_getchar(void);
BOOL tsdb_verify_selection(Tsdb_selection *);
BOOL tsdb_key_list_not_copied(Tsdb_relation *, int, Tsdb_key_list *);
void tsdb_debug_join_path(Tsdb_value **, Tsdb_value **);
void tsdb_debug_simple_join(Tsdb_value **, Tsdb_value **);
void tsdb_debug_canonical_date(Tsdb_value **);
char *tsdb_pseudo_user();
float tsdb_timer(BYTE);

Tsdb_value *tsdb_integer(int);
Tsdb_value *tsdb_identifier(char *);
Tsdb_value *tsdb_string(char *);
Tsdb_value *tsdb_date(char *);
Tsdb_value *tsdb_position(char *);
Tsdb_value *tsdb_connective(BYTE);
Tsdb_value *tsdb_operator(BYTE);
Tsdb_value *tsdb_descriptor(int r, int f);
Tsdb_value **tsdb_singleton_value_array(Tsdb_value *);
Tsdb_value **tsdb_value_array_append(Tsdb_value **, Tsdb_value *);
Tsdb_field **tsdb_singleton_field_array(Tsdb_field *);
Tsdb_field **tsdb_field_array_append(Tsdb_field **, Tsdb_field *);

BYTE tsdb_value_compare(Tsdb_value *, Tsdb_value *);
BYTE tsdb_tuple_compare(Tsdb_tuple *, Tsdb_tuple *);
BOOL tsdb_verify_tuple(Tsdb_node *, Tsdb_tuple **);

BOOL tsdb_tuple_equal(Tsdb_tuple *, Tsdb_tuple *);
BYTE tsdb_value_match(Tsdb_value *, Tsdb_value *, char, void *);

FILE* tsdb_open_pager();
FILE* tsdb_open_debug();
FILE* tsdb_open_output(char *);
void tsdb_close_debug(FILE *);

void tsdb_tree_print(Tsdb_node *, FILE *);
BOOL tsdb_print_value(Tsdb_value *, FILE *, BOOL);
char* tsdb_sprint_value(Tsdb_value *, BOOL);
char* tsdb_sprint_key_list(Tsdb_key_list *, int *, int *, int);
void tsdb_print_array(Tsdb_value **, FILE *);
void tsdb_print_relation(Tsdb_relation *, FILE *);
void tsdb_print_node(Tsdb_node *, FILE *);
void tsdb_print_tuple(Tsdb_tuple *, FILE *);
void tsdb_print_key_list(Tsdb_key_list *, FILE *);
void tsdb_print_join_path(Tsdb_relation **, FILE *);
void tsdb_print_projection(char **, int, char *, FILE *);
void tsdb_print_selection(Tsdb_selection *, FILE *);
void tsdb_test_negation(Tsdb_value **, Tsdb_node *);
int tsdb_uniq_projection(char **, int);
Tsdb_node *tsdb_leaf(Tsdb_value *);
Tsdb_node* tsdb_prepare_tree(Tsdb_node *, Tsdb_selection *);
int tsdb_lock(BOOL);
int tsdb_save_changes(BOOL);

BOOL tsdb_children_leaf(Tsdb_node *);
void tsdb_negate_node(Tsdb_node *);
void tsdb_tree_negate(Tsdb_node *);
void tsdb_check_not(Tsdb_node *);

BOOL tsdb_contains_relation(Tsdb_selection *, Tsdb_relation *);
BOOL tsdb_joins_to(Tsdb_relation *, Tsdb_selection *);
BOOL tsdb_are_joinable(Tsdb_relation *, Tsdb_relation *);
BOOL tsdb_is_attribute(Tsdb_value *);
BOOL tsdb_are_attributes(Tsdb_value **, Tsdb_relation *);
BOOL tsdb_is_relation(Tsdb_value *value);
BOOL tsdb_relations_are_equal(Tsdb_relation *, Tsdb_relation *);
BYTE tsdb_initialize(void);
void tsdb_parse_environment(void);
BOOL tsdb_satisfies_condition(Tsdb_tuple *, Tsdb_node *, Tsdb_relation *);

char *tsdb_join_key(Tsdb_relation *, Tsdb_relation *);
char **tsdb_common_keys(Tsdb_relation *, Tsdb_relation *);
char** tsdb_key_names(Tsdb_selection *);
char** tsdb_all_attribute_names();
char** tsdb_all_relation_names();
BOOL tsdb_attribute_in_relation(Tsdb_relation *, char *);
BOOL tsdb_attribute_in_selection(Tsdb_selection *, char *);
int tsdb_relation_in_selection(Tsdb_selection *, char* );
int tsdb_info(Tsdb_value **, char *);
int tsdb_set(Tsdb_value *, Tsdb_value *);
int tsdb_drop_table(Tsdb_value *);
int tsdb_create_table(Tsdb_value *, Tsdb_field **);
int tsdb_alter_table(Tsdb_value *, Tsdb_field **);
int tsdb_insert(Tsdb_value *, Tsdb_value **, Tsdb_value **);
int tsdb_delete(Tsdb_value *, Tsdb_node *);
int tsdb_update(Tsdb_value *, Tsdb_node *);
int tsdb_count(Tsdb_value *);


int tsdb_project(Tsdb_selection*,Tsdb_value **,char*,FILE* );
FILE *tsdb_find_relations_file(char *);
FILE *tsdb_find_data_file(char *, char *);
FILE* tsdb_open_result();
char *tsdb_rcs_strip(char *, char *);
char *tsdb_expand_directory(char *, char *);
char *tsdb_expand_file(char *, char *);
char *tsdb_user(void);
char *tsdb_canonical_date(char *);
int *tsdb_parse_date(char *);
char *tsdb_normalize_string(char *);
char *tsdb_denormalize_string(char *);
int tsdb_quotes_are_balanced(char *);
char *tsdb_prolog_escape_string(char *);
char *tsdb_lisp_escape_string(char *);
BOOL tsdb_check_potential_command(char *);
Tsdb_value *tsdb_generate_default_value(Tsdb_relation *, int);

Tsdb_node **tsdb_linearize_conditions(Tsdb_node *);

BOOL  tsdb_selection_tree(Tsdb_node *,Tsdb_selection*);

Tsdb_tuple *tsdb_read_tuple(Tsdb_relation *, FILE *);

Tsdb_relation *tsdb_field_2_relation(char *, Tsdb_field **);
Tsdb_relation *tsdb_read_relation(FILE *);
int tsdb_write_table(Tsdb_selection *, BOOL);
Tsdb_relation *tsdb_find_relation(char *);
void tsdb_add_relation(Tsdb_relation *) ;
Tsdb_relation **tsdb_all_relations(void);
Tsdb_relation** tsdb_attribute_relations(Tsdb_value *);
int tsdb_n_relations(void);
int tsdb_n_attributes();
Tsdb_relation *tsdb_copy_relation(Tsdb_relation *);

Tsdb_key_list *tsdb_copy_key_list(Tsdb_key_list *);
Tsdb_key_list* tsdb_first_other_key(Tsdb_key_list*);
char* tsdb_translate_table() ;

BOOL tsdb_insert_into_selection(Tsdb_selection *, Tsdb_tuple **);
char** tsdb_condition_attributes(Tsdb_node *, char **, int *);
int* tsdb_relation_match(Tsdb_selection *, Tsdb_selection *);
Tsdb_relation *tsdb_create_relation(void);
void tsdb_free_relation(Tsdb_relation *);
void tsdb_remove_relation(char *);

void tsdb_free_tsdb_value(Tsdb_value *);
void tsdb_free_tsdb_values(Tsdb_value **);

Tsdb_selection *tsdb_create_selection(int, int);
void tsdb_free_selection(Tsdb_selection*);
void tsdb_free_selections(Tsdb_selection** );
Tsdb_selection* tsdb_clean_selection(Tsdb_selection *, Tsdb_tuple *);
Tsdb_selection *tsdb_copy_selection(Tsdb_selection *);
Tsdb_selection *tsdb_find_table(Tsdb_relation *);
Tsdb_selection *tsdb_find_tables(Tsdb_relation **);
Tsdb_selection *tsdb_read_table(Tsdb_relation *, Tsdb_node *);
void tsdb_free_key_list(Tsdb_key_list *);
void tsdb_free_key_list_chain(Tsdb_key_list *,BOOL);
void tsdb_free_char_array(char** ,int);

Tsdb_selection *tsdb_add_relations(Tsdb_selection *, Tsdb_relation **);
Tsdb_selection *tsdb_join(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_select(Tsdb_selection *, Tsdb_node **, BYTE);
Tsdb_selection *tsdb_complex_select(Tsdb_node *, 
                                    Tsdb_value **, Tsdb_selection *);
Tsdb_selection *tsdb_complex_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_simple_join(Tsdb_selection *, Tsdb_selection *);
Tsdb_relation **tsdb_join_path(Tsdb_relation **, Tsdb_relation **);
Tsdb_relation ***tsdb_real_join_path(Tsdb_relation **, int,
                                     Tsdb_relation *, int);
Tsdb_selection *tsdb_simple_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_complex_retrieve(Tsdb_value **, Tsdb_value **,
                                      Tsdb_node *,
                                      char *, char*, int*);
int tsdb_retrieve(Tsdb_value **, Tsdb_value **, Tsdb_node *, char *, char *);
int tsdb_commit(Tsdb_value **);
void tsdb_quit(void);
void tsdb_shutdown(int);
int tsdb_do(char *, char *);

Tsdb_history *tsdb_get_history(int);
void tsdb_add_to_history(Tsdb_selection*, int);
void tsdb_set_history_size(int);
int tsdb_init_history(Tsdb*);

int tsdb_server_initialize(void);
void tsdb_server(void);
int tsdb_server_child(int);
void tsdb_server_shutdown(int);
int tsdb_socket_write(int, char *);
int tsdb_socket_readline(int, char *, int);
int tsdb_obtain_server_status(int);
char *tsdb_obtain_server_home(int);
int tsdb_client_clear_stream(int, BOOL);
int tsdb_client_close(int);
#ifdef ALEP
int tsdb_alep_client(char *);
#endif

