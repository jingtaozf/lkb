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

#ifdef DEMO
#  define free(x) x
#endif

#define TSDB_VERSION "0.0"

#define TSDB_DEFAULT_STREAM stdout
#define TSDB_ERROR_STREAM stderr

#define TSDB_SERVER_MODE 1

#define TSDB_UNKNOWN_TYPE 0
#define TSDB_INTEGER 1
#define TSDB_IDENTIFIER 2
#define TSDB_STRING 3
#define TSDB_CONNECTIVE 4
#define TSDB_OPERATOR 5

#define TSDB_VALUE_INCOMPATIBLE 0 
#define TSDB_EQUAL 1
#define TSDB_NOT_EQUAL 2
#define TSDB_LESS_THAN 3
#define TSDB_LESS_OR_EQUAL_THAN 4
#define TSDB_GREATER_THAN 5
#define TSDB_GREATER_OR_EQUAL_THAN 6
#define TSDB_SUBSTRING 7
#define TSDB_NOT_SUBSTRING 8

#define TSDB_NOT 0
#define TSDB_AND 1
#define TSDB_OR 2
#define TSDB_NOT_NOT 3
#define TSDB_BRACE 4
#define TSDB_NONE 5

#define TSDB_NEW 'n'
#define TSDB_CHANGED 'c'
#define TSDB_UNCHANGED 'u'

#define TSDB_START_TIMER 0
#define TSDB_MAX_TIMERS 20

#define TSDB_SERVER_OPTION 0
#define TSDB_PORT_OPTION 1
#define TSDB_HOME_OPTION 2
#define TSDB_RELATIONS_FILE_OPTION 3
#define TSDB_DATA_PATH_OPTION 4
#define TSDB_RESULT_PATH_OPTION 5
#define TSDB_RESULT_PREFIX_OPTION 6
#define TSDB_MAX_RESULTS_OPTION 7
#define TSDB_DEBUG_FILE_OPTION 8
#define TSDB_PAGER_OPTION 9
#define TSDB_USAGE_OPTION 10
#define TSDB_VERSION_OPTION 11

#ifndef TSDB_PSEUDO_USER
#  define TSDB_PSEUDO_USER "TSDB@tsdb"
#endif

#ifndef TSDB_DIRECTORY_DELIMITER
#  define TSDB_DIRECTORY_DELIMITER "/"
#endif

#ifndef TSDB_HOME
#  define TSDB_HOME "/home/cl-home/fettig/work/tsdb/"
#endif

#ifndef TSDB_RELATIONS_FILE
#  define TSDB_RELATIONS_FILE "etc/relations"
#endif

#ifndef TSDB_DATA_PATH
#  define TSDB_DATA_PATH "lib/"
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

#ifndef TSDB_BACKUP_SUFFIX
#  define TSDB_BACKUP_SUFFIX "~"
#endif

#ifndef DEFAULT_VALUE
#  define DEFAULT_VALUE "null"
#endif

#ifndef TSDB_SERVER_PORT
#  define TSDB_SERVER_PORT 4711
#endif

#ifndef TSDB_SERVER_QUEUE_LENGTH
#  define TSDB_SERVER_QUEUE_LENGTH 5
#endif

typedef struct tsdb {
  BYTE status;
  int port;
} Tsdb;

typedef struct tsdb_field {
  char *name;
  BYTE type;
  BOOL key;
} Tsdb_field;

typedef struct tsdb_value {
  BYTE type;
  union {
    int integer;
    char *identifier;
    char *string;
    BYTE connective;
    BYTE operator;
  } value;
} Tsdb_value;

typedef struct tsdb_node {
  struct tsdb_node *left;
  Tsdb_value *node;
  struct tsdb_node *right;
} Tsdb_node;

typedef struct tsdb_relation {
  char *name;
  int n_fields;
  char **fields;
  BYTE *types;
  int n_keys;
  int *keys;
  BYTE *total;
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

#if !defined(TSDB_DOT_C)
  Tsdb tsdb;

  extern FILE *tsdb_default_stream;
  extern FILE *tsdb_error_stream;

#ifdef DEBUG
  extern FILE *tsdb_debug_stream;
  extern char *tsdb_debug_file;
#endif

  extern char *tsdb_home;
  extern char *tsdb_relations_file;
  extern char *tsdb_data_path;
  extern char *tsdb_result_path;
  extern char *tsdb_result_prefix;
  extern int tsdb_max_results;
  extern char *tsdb_pager;

  extern char *tsdb_input;
  extern Tsdb_relation **tsdb_relations;
  extern Tsdb_selection **tsdb_data;

  extern int really_verbose_mode;
#endif

void tsdb_parse_options(int, char **);
void tsdb_usage(void);
int tsdb_parse(char *);
int tsdb_getchar(void);
BOOL tsdb_verify_selection(Tsdb_selection *);
void tsdb_debug_join_path(Tsdb_value **, Tsdb_value**);
void tsdb_debug_simple_join(Tsdb_value **, Tsdb_value**);
char *tsdb_pseudo_user();
float tsdb_timer(BYTE);

Tsdb_value *tsdb_integer(int);
Tsdb_value *tsdb_identifier(char *);
Tsdb_value *tsdb_string(char *);
Tsdb_value *tsdb_connective(BYTE);
Tsdb_value *tsdb_operator(BYTE);
Tsdb_value **tsdb_singleton_value_array(Tsdb_value *);
Tsdb_value **tsdb_value_array_append(Tsdb_value **, Tsdb_value *);
Tsdb_field **tsdb_singleton_field_array(Tsdb_field *);
Tsdb_field **tsdb_field_array_append(Tsdb_field **, Tsdb_field *);

BYTE tsdb_value_compare(Tsdb_value *, Tsdb_value *);
BYTE tsdb_tuple_compare(Tsdb_tuple *, Tsdb_tuple *);
BOOL tsdb_value_match(Tsdb_value *, Tsdb_value *,void *);

FILE* tsdb_open_pager();
FILE* tsdb_open_debug();
void tsdb_close_debug(FILE *);
BOOL tsdb_print_value(Tsdb_value *, FILE *);
void tsdb_print_array(Tsdb_value **, FILE *);
void tsdb_print_relation(Tsdb_relation *, FILE *);
void tsdb_print_node(Tsdb_node *, FILE *);
void tsdb_print_tuple(Tsdb_tuple *, FILE *);
void tsdb_print_key_list(Tsdb_key_list *, FILE *);
void tsdb_print_join_path(Tsdb_relation **, FILE *);
void tsdb_test_negation(Tsdb_value ** attribute_list,Tsdb_node* conditions);
Tsdb_node *tsdb_leaf(Tsdb_value *);

extern void tsdb_save_changes(void) ;

BOOL tsdb_children_leaf(Tsdb_node* node);
void tsdb_negate_node(Tsdb_node* node);
void tsdb_tree_negate(Tsdb_node* node);
void tsdb_check_not(Tsdb_node* node);

BOOL tsdb_contains_relation(Tsdb_selection*,Tsdb_relation*);
BOOL tsdb_joins_to(Tsdb_relation *,Tsdb_selection*);
BOOL tsdb_are_joinable(Tsdb_relation *, Tsdb_relation *);
BOOL tsdb_are_attributes(Tsdb_value **, Tsdb_relation *);
BOOL tsdb_is_relation(Tsdb_value *value);
BOOL tsdb_relations_are_equal(Tsdb_relation *, Tsdb_relation *);
BOOL tsdb_initialize(void);
void tsdb_parse_environment(void);
BOOL tsdb_satisfies_condition(Tsdb_tuple *, Tsdb_node *, Tsdb_relation *);

char *tsdb_join_key(Tsdb_relation *, Tsdb_relation *);
char **tsdb_common_keys(Tsdb_relation *, Tsdb_relation *);
char** tsdb_key_names(Tsdb_selection* );
char** tsdb_all_attribute_names();
char** tsdb_all_relation_names();
int tsdb_drop_table(Tsdb_value *);
int tsdb_create_table(Tsdb_value *, Tsdb_field **);
int tsdb_alter_table(Tsdb_value *, Tsdb_field **);
int tsdb_insert(Tsdb_value *, Tsdb_value **, Tsdb_value **);
int tsdb_delete(Tsdb_value *, Tsdb_node *);
int tsdb_update(Tsdb_value *, Tsdb_node *);
int tsdb_retrieve(Tsdb_value **, Tsdb_node *);


void tsdb_project(Tsdb_selection*,Tsdb_value **,FILE* );
FILE *tsdb_find_relations_file(char *);
FILE *tsdb_find_data_file(char *, char *);
FILE* tsdb_open_result();
char *tsdb_rcs_strip(char *, char *);

Tsdb_node **tsdb_linearize_conditions(Tsdb_node *);

Tsdb_tuple *tsdb_read_tuple(Tsdb_relation *, FILE *);

Tsdb_relation *tsdb_field_2_relation(char *, Tsdb_field **);
Tsdb_relation *tsdb_read_relation(FILE *);
Tsdb_relation *tsdb_find_relation(char *);
Tsdb_relation **tsdb_all_relations(void);
Tsdb_relation** tsdb_attribute_relations(Tsdb_value *);
int tsdb_n_relations(void);
int tsdb_n_attributes();
Tsdb_relation *tsdb_copy_relation(Tsdb_relation *);

Tsdb_key_list *tsdb_copy_key_list(Tsdb_key_list *);
Tsdb_key_list* tsdb_first_other_key(Tsdb_key_list*);

BOOL tsdb_insert_into_selection(Tsdb_selection *, Tsdb_tuple **);
char** tsdb_condition_attributes(Tsdb_node* , char** , int* );
int* tsdb_relation_match(Tsdb_selection *, Tsdb_selection *);
Tsdb_relation *tsdb_create_relation(void);
void tsdb_free_relation(Tsdb_relation*);
Tsdb_selection *tsdb_create_selection(int, int);
Tsdb_selection *tsdb_copy_selection(Tsdb_selection*);
Tsdb_selection *tsdb_find_table(Tsdb_relation *);
Tsdb_selection *tsdb_find_tables(Tsdb_relation**);
Tsdb_selection *tsdb_read_table(Tsdb_relation *, Tsdb_node *);
void tsdb_free_key_list(Tsdb_key_list *);
void tsdb_free_key_list_chain(Tsdb_key_list* ,BOOL);

Tsdb_selection *tsdb_join(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_select(Tsdb_selection *, Tsdb_node **,BYTE);
Tsdb_selection *tsdb_complex_select(Tsdb_node *node,Tsdb_relation ** wanted);
Tsdb_selection *tsdb_complex_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection *tsdb_simple_join(Tsdb_selection *, Tsdb_selection *);
Tsdb_relation **tsdb_join_path(Tsdb_relation **, Tsdb_relation **);
Tsdb_relation ***tsdb_real_join_path(Tsdb_relation **, int,
                                     Tsdb_relation *, int);
Tsdb_selection *tsdb_simple_merge(Tsdb_selection *, Tsdb_selection *);
Tsdb_selection* tsdb_conditional_retrieve(Tsdb_value **, Tsdb_value **,
                                          Tsdb_node *);

int tsdb_server_initialize(void);
void tsdb_server_toplevel();
void tsdb_server_instance(int);
