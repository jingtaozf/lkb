/*****************************************************************************\
|*        file: parser.y
|*      module: tsdb command language parser
|*     version: 
|*  written by: andrew p. white & oe, dfki saarbruecken
|* last update: 23-jul-96
|*  updated by: oe, dfki saarbruecken
|*****************************************************************************|
|*
\*****************************************************************************/

%{

#if defined(DEBUG) && defined(PARSER)
# define YYDEBUG 1
  int yydebug = 1;
#endif

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include "globals.h"
#include "tsdb.h"
#include "errors.h"

%}

%pure_parser

%union {
  int integer;
  char *string;
  Tsdb_value *tsdb_value;
  Tsdb_value **tsdb_value_array;
  Tsdb_node *tsdb_node;
  Tsdb_field *tsdb_field;
  Tsdb_field **tsdb_field_array;
}

%token Y_MATCH
       Y_NOT_TILDA
       Y_TILDA
       Y_INSENSITIVE_TILDA
       Y_NOT_INSENSITIVE_TILDA
       Y_LESS
       Y_GREATER
       Y_NOT_EQUAL
       Y_EQUAL 
       Y_LESS_OR_EQUAL
       Y_GREATER_OR_EQUAL
       Y_DO
       Y_COMMIT
       Y_LOCK
       Y_UNLOCK
       Y_SHUTDOWN
       Y_HANGUP
       Y_RETRIEVE
       Y_COUNT
       Y_UPDATE
       Y_INSERT
       Y_DELETE
       Y_DROP
       Y_CREATE
       Y_ALTER 
       Y_INFO
       Y_SET
       Y_RELATIONS
       Y_TABLE
       Y_WHERE
       Y_REPORT
       Y_ADD
       Y_FROM
       Y_INTO
       Y_VALUES
       Y_KEY
       Y_ON
       Y_OFF
       Y_STRING_TYPE
       Y_INTEGER_TYPE
       Y_DATE_TYPE
       Y_QUIT
       Y_TEST

%left Y_OR
%left Y_AND
%nonassoc Y_NOT

%token <string> Y_IDENTIFIER
                Y_STRING
                Y_DATE

%token <integer> Y_INTEGER

%type <integer> y_do
                y_commit
                y_shutdown
                y_hangup
                y_retrieval
                y_counting
                y_dropping
                y_deletion
                y_insertion
                y_creation
                y_altering
                y_info
                y_set
                y_exit

%type <string> y_special
               y_redirection
               y_retrieve_report

%type <tsdb_value_array> y_retrieve_projection
                         y_retrieve_from
                         y_attribute_list
                         y_value_list
                         y_table_list
                         y_attributes
                         y_values
                         y_tables 

%type <tsdb_value> y_attribute
                   y_value
                   y_table_name
                   y_operator

%type <tsdb_field> y_create_attribute

%type <tsdb_field_array> y_create_attribute_list
                         y_create_attributes

%type <tsdb_node> y_condition

%start y_query

%%

y_query :
  y_do {
    return($1);
  }
|
  y_commit {
    return($1);
  }
|
  y_shutdown {
    return($1);
  }
|
  y_hangup {
    return($1);
  }
|
  y_retrieval {
    return($1);
  }
|
  y_dropping {
    return($1);
  }
|
  y_counting {
    return($1);
  }
|
  y_deletion {
    return($1);
  }
|
  y_insertion {
    return($1);
  }
|
  y_creation {
    return($1);
  }
| 
  y_altering {
    return($1);
  }
|
  y_info {
    return($1);
  }
|
  y_set {
    return($1);
  }
|
  y_exit {
    return($1);
  }
| 
  y_test {
    return(TSDB_OK);
  }
;

y_redirection : 
  Y_GREATER Y_STRING {
    $$ = (char *)malloc(strlen($2) + 2);
    $$[0] = (char)TSDB_REDIRECTION_OVERWRITE;
    $$[1] = 0;
    $$ = strcat($$, $2);
  }
|
  Y_GREATER Y_GREATER Y_STRING {
    $$ = (char *)malloc(strlen($3) + 2);
    $$[0] = (char)TSDB_REDIRECTION_APPEND;
    $$[1] = 0;
    $$ = strcat($$, $3);
  }
;

y_do :
  Y_DO Y_STRING '.' {
    $$ = tsdb_do($2, (char *)NULL);
  }
|
  Y_DO Y_STRING y_redirection '.' {
    $$ = tsdb_do($2, $3);
  }
;

y_commit :
  Y_COMMIT '.' {
    $$ = tsdb_commit((Tsdb_value **)NULL);
  }
|
  Y_COMMIT y_attribute_list '.' {
    $$ = tsdb_commit($2);
  }
;

y_shutdown :
  Y_SHUTDOWN '.' {
    tsdb_shutdown(SIGTERM);
    $$ = TSDB_OK;
  }
|
  Y_SHUTDOWN y_redirection '.' {
    tsdb_shutdown(SIGTERM);
    $$ = TSDB_OK;
  }
;

y_hangup :
  Y_HANGUP '.' {
    tsdb_shutdown(SIGHUP);
    $$ = TSDB_OK;
  }
|
  Y_HANGUP y_redirection '.' {
    tsdb_shutdown(SIGHUP);
    $$ = TSDB_OK;
  }
;

y_creation : 
  Y_CREATE Y_TABLE y_table_name y_create_attribute_list '.' { 
    $$ = tsdb_create_table($3, $4); 
  }
;

y_dropping : 
  Y_DROP Y_TABLE y_table_name '.' { 
    $$ = tsdb_drop_table($3); 
  }
;

y_altering : 
  Y_ALTER Y_TABLE y_table_name Y_ADD y_create_attribute_list '.' { 
    $$ = tsdb_alter_table($3, $5);
  }
;

y_insertion :
  Y_INSERT Y_INTO y_table_name y_attribute_list Y_VALUES y_value_list '.' {
    $$ = tsdb_insert($3, $4, $6);
  }
| 
  Y_INSERT Y_INTO y_table_name Y_VALUES y_value_list '.' {
    $$ = tsdb_insert($3, NULL, $5);
  }
;

y_deletion :
  Y_DELETE Y_FROM y_table_name '.' {
    $$ = tsdb_delete($3, (Tsdb_node *)NULL);
  }
|
  Y_DELETE Y_FROM y_table_name Y_WHERE y_condition '.' {
    $$ = tsdb_delete($3, $5);
  }
;

y_counting :
  Y_COUNT y_table_name {
    $$ = tsdb_count($2);
  }
;

y_retrieval :
  Y_RETRIEVE y_attribute_list '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, (Tsdb_node *)NULL,
                       (char *)NULL, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection Y_WHERE y_condition '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, $4,
                       (char *)NULL, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from '.' {
    $$ = tsdb_retrieve($3, $2, (Tsdb_node *)NULL,
                       (char *)NULL, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from Y_WHERE y_condition '.' {
    $$ = tsdb_retrieve($3, $2, $5,
                       (char *)NULL, (char *)NULL);
  }
|
  Y_RETRIEVE y_attribute_list
    y_retrieve_report '.' {
      $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, (Tsdb_node *)NULL,
                         $3, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection Y_WHERE y_condition
    y_retrieve_report '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, $4,
                       $5, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from
    y_retrieve_report '.' {
    $$ = tsdb_retrieve($3, $2, (Tsdb_node *)NULL,
                       $4, (char *)NULL);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from Y_WHERE y_condition
    y_retrieve_report '.' {
    $$ = tsdb_retrieve($3, $2, $5,
                       $6, (char *)NULL);
  }
|
  Y_RETRIEVE y_attribute_list 
    y_redirection '.' {
    $$= tsdb_retrieve((Tsdb_value **)NULL, $2, (Tsdb_node *)NULL,
                      (char *)NULL, $3);
  }
|
  Y_RETRIEVE y_retrieve_projection Y_WHERE y_condition 
    y_redirection '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, $4,
                       (char *)NULL, $5);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from 
    y_redirection '.' {
    $$ = tsdb_retrieve($3, $2, (Tsdb_node *)NULL,
                       (char *)NULL, $4);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from Y_WHERE y_condition 
    y_redirection '.' {
    $$ = tsdb_retrieve($3, $2, $5,
                       (char *)NULL, $6);
  }
|
  Y_RETRIEVE y_attribute_list
    y_retrieve_report y_redirection '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, (Tsdb_node *)NULL,
                       $3, $4);
  }
|
  Y_RETRIEVE y_retrieve_projection Y_WHERE y_condition
    y_retrieve_report y_redirection '.' {
    $$ = tsdb_retrieve((Tsdb_value **)NULL, $2, $4,
                       $5, $6);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from
    y_retrieve_report  y_redirection '.' {
    $$ = tsdb_retrieve($3, $2, (Tsdb_node *)NULL,
                       $4, $5);
  }
|
  Y_RETRIEVE y_retrieve_projection y_retrieve_from Y_WHERE y_condition
    y_retrieve_report y_redirection '.' {
    $$ = tsdb_retrieve($3, $2, $5,
                       $6, $7);
  }
;

y_retrieve_projection :
  y_attribute_list {
    $$ = $1;
  }
|
  '*' {
    $$ = (Tsdb_value **)NULL;
  }
;

y_retrieve_from :
  Y_FROM y_attribute_list {
    $$ = $2;
  }
|
  Y_FROM Y_INTEGER {
    $$ = (Tsdb_value **)malloc(2 * sizeof(Tsdb_value *));
    $$[0] = tsdb_integer($2);
    $$[1] = (Tsdb_value *)NULL;
  }
;

y_retrieve_report :
  Y_REPORT Y_STRING {
    $$ = $2;
  }
;

y_info :
  Y_INFO y_attribute_list '.' {
    $$ = tsdb_info($2, (char *)NULL);
  }
|
  Y_INFO y_value_list '.' {
    $$ = tsdb_info($2, (char *)NULL);
  }
|
  Y_INFO y_attribute_list y_redirection '.' {
    $$ = tsdb_info($2, $3);
  }
|
  Y_INFO y_value_list y_redirection '.' {
    $$ = tsdb_info($2, $3);
  }
;

y_table_list :
  '(' y_tables ')' {
    $$ = $2;
  }
|
  y_tables
;

y_tables :
  y_table_name {
    $$ = tsdb_singleton_value_array($1);
  }
|
  y_tables y_table_name {
    $$ = tsdb_value_array_append($1, $2);
  }
;
  
y_table_name :
  Y_IDENTIFIER {
    $$ = tsdb_identifier($1); 
  }
|
  y_special {
    $$ = tsdb_identifier($1);
  } 
;

y_attribute_list :
  '(' y_attributes ')' {
    $$ = $2;
  }
|
  y_attributes
;

y_attributes :
  y_attribute {
    $$ = tsdb_singleton_value_array($1);
  }
|
  y_attributes y_attribute {
    $$ = tsdb_value_array_append($1, $2);
  }
;

y_attribute :
  Y_IDENTIFIER {
    $$ = tsdb_identifier($1);
  }
|
  y_special {
    $$ = tsdb_identifier($1);
  } 
;

y_create_attribute_list :
  '(' y_create_attributes ')' {
    $$ = $2;
  }
|
  y_create_attributes
;

y_create_attributes :
  y_create_attribute {
    $$ = tsdb_singleton_field_array($1);
  }
|
  y_create_attributes y_create_attribute {
    $$ = tsdb_field_array_append($1, $2);
  }
;

y_create_attribute :
  Y_IDENTIFIER Y_INTEGER_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_INTEGER;
    $$->key = FALSE;
  }
|
  Y_IDENTIFIER Y_INTEGER_TYPE Y_KEY {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_INTEGER;
    $$->key = TRUE;
  }
|
  Y_IDENTIFIER Y_KEY Y_INTEGER_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_INTEGER;
    $$->key = TRUE;
  }
|
  Y_IDENTIFIER Y_STRING_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_STRING;
    $$->key = FALSE;
  }
|
  Y_IDENTIFIER Y_STRING_TYPE Y_KEY {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_STRING;
    $$->key = TRUE;
  }
|
  Y_IDENTIFIER Y_KEY Y_STRING_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_STRING;
    $$->key = TRUE;
  }
|
  Y_IDENTIFIER Y_DATE_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_DATE;
    $$->key = FALSE;
  }
|
  Y_IDENTIFIER Y_DATE_TYPE Y_KEY {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_DATE;
    $$->key = TRUE;
  }
|
  Y_IDENTIFIER Y_KEY Y_STRING_TYPE {
    $$ = (Tsdb_field *)malloc(sizeof(Tsdb_field));
    $$->name = $1;
    $$->type = TSDB_DATE;
    $$->key = TRUE;
  }
;

y_condition : 
  y_attribute y_operator y_value {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = tsdb_leaf($1);
    $$->node = $2;
    $$->right = tsdb_leaf($3);
   }
|
  Y_MATCH '(' y_attribute ',' y_value ')' {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = tsdb_leaf($3);
    $$->node = tsdb_operator(TSDB_MATCH);
    $$->right = tsdb_leaf($5);
  }
|
  '(' y_condition ')' {
    $$ = $2;
  }
|
  y_condition Y_AND y_condition {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = $1;
    $$->node = tsdb_connective(TSDB_AND);
    $$->right = $3;
  }
|
  y_condition Y_OR y_condition {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = $1;
    $$->node = tsdb_connective(TSDB_OR);
    $$->right = $3;
  }
|
  Y_NOT y_condition {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = (Tsdb_node *)NULL;
    $$->node = tsdb_connective(TSDB_NOT);
    $$->right = $2;
  }
;

y_operator :
  Y_EQUAL {
    $$ = tsdb_operator(TSDB_EQUAL);
  }
|
  Y_NOT_EQUAL {
    $$ = tsdb_operator(TSDB_NOT_EQUAL);
  }
|
  Y_LESS {
    $$ = tsdb_operator(TSDB_LESS_THAN);
  }
|
  Y_LESS_OR_EQUAL {
    $$ = tsdb_operator(TSDB_LESS_OR_EQUAL_THAN);
  }
|
  Y_GREATER {
    $$ = tsdb_operator(TSDB_GREATER_THAN);
  }
|
  Y_GREATER_OR_EQUAL {
    $$ = tsdb_operator(TSDB_GREATER_OR_EQUAL_THAN);
  }
|
  Y_TILDA {
    $$ = tsdb_operator(TSDB_MATCH);
  }
|
  Y_NOT_TILDA {
    $$ = tsdb_operator(TSDB_NOT_MATCH);
  }
 |
  Y_INSENSITIVE_TILDA {
    $$ = tsdb_operator(TSDB_INSENSITIVE_MATCH);
  }
|
  Y_NOT_INSENSITIVE_TILDA {
    $$ = tsdb_operator(TSDB_NOT_INSENSITIVE_MATCH);
  }
 
;

y_value_list :
  '(' y_values ')' {
    $$ = $2;
  }
|
  y_values
;

y_values :
  y_value {
    $$ = tsdb_singleton_value_array($1);
  }
|
   y_values y_value {
    $$ = tsdb_value_array_append($1, $2);
  }
;

y_value :
  Y_STRING {
    $$ = tsdb_string($1);
  }
|
  y_special {
    $$ = tsdb_string($1);
  } 
|
  Y_INTEGER {
    $$ = tsdb_integer($1);
  }
|
  Y_DATE {
    $$ = tsdb_date($1);
  }
;

y_special :
  Y_SET {
    $$ = "set";
  }
|
  Y_ON {
    $$ = "on";
  }
|
  Y_OFF {
    $$ = "off";
  }
|
  Y_QUIT {
    $$ = "exit";
  }
|
  Y_SHUTDOWN {
    $$ = "shutdown";
  }
|
  Y_HANGUP {
    $$ = "hangup";
  }
;

y_set :
  Y_SET y_attribute y_value '.' {
    $$ = tsdb_set($2, $3);
  }
;

y_test :
  Y_TEST y_attribute_list Y_GREATER y_attribute_list '.' {
    tsdb_debug_join_path($2, $4);
  }
|
  Y_TEST y_attribute_list '#' y_attribute_list '.' {
    tsdb_debug_simple_join($2, $4);
  }
|
  Y_TEST y_value_list '.' {
    tsdb_debug_canonical_date($2);
  }
;

y_exit : 
  Y_QUIT '.' {
    tsdb_quit();
    $$ = TSDB_OK;
  }
;
                                
%%

int yywrap() {

  return(1);

} /* yywrap() */

int yyerror(char *s) {
 
  fprintf(tsdb_error_stream,
          "yyparse(): parse error; check the tsdb(1) syntax.\n");
  fflush(tsdb_error_stream);
  tsdb.error = TSDB_PARSE_ERROR;

  YYABORT;

} /* yyerror() */
