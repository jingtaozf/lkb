/*****************************************************************************\
|*        file: parser.y
|*      module: tsdb command language parser
|*     version: 
|*  written by: andrew p. white & oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

%{

#if defined(DEBUG) && defined(PARSER)
# define YYDEBUG 1
  int yydebug = 1;
#endif

#include <stdio.h>
#include "globals.h"
#include "tsdb.h"

%}

%union {
  int integer;
  char *string;
  Tsdb_value *tsdb_value;
  Tsdb_value **tsdb_value_array;
  Tsdb_node *tsdb_node;
  Tsdb_field *tsdb_field;
  Tsdb_field **tsdb_field_array;
}

%token 
       Y_MATCH
       Y_TILDA
       Y_LESS
       Y_GREATER
       Y_NOT_EQUAL
       Y_EQUAL 
       Y_LESS_OR_EQUAL
       Y_GREATER_OR_EQUAL
       Y_RETRIEVE
       Y_UPDATE
       Y_INSERT
       Y_DELETE
       Y_DROP
       Y_CREATE
       Y_ALTER 
       Y_TABLE
       Y_WHERE
       Y_ADD
       Y_FROM
       Y_TO
       Y_INTO
       Y_VALUES
       Y_KEY
       Y_STRING_TYPE
       Y_INTEGER_TYPE
       Y_QUIT
       Y_TEST

%left Y_OR
%left Y_AND
%nonassoc Y_NOT

%token <string> Y_IDENTIFIER
                Y_STRING

%token <integer> Y_INTEGER

%type <tsdb_value_array> y_attribute_list
                         y_value_list
                         y_attribute_value_list
                         y_table_list
                         y_attributes
                         y_values
                         y_attribute_value_pairs
                         y_tables 

%type <tsdb_value> y_attribute
                   y_tsdb_value
                   y_table_name
                   y_operator

%type <tsdb_field> y_create_attribute

%type <tsdb_field_array> y_create_attribute_list
                         y_create_attributes

%type <tsdb_node> y_condition

%start y_query

%%

y_query :
  y_retrieval '.'
|
  y_dropping '.'
|
  y_deletion '.'
|
  y_insertion '.'
|
  y_creation '.'
| 
  y_altering '.'
|
  y_exit '.'
| 
  y_test '.'
;

y_creation : 
  Y_CREATE Y_TABLE y_table_name y_create_attribute_list { 
    if(!tsdb_is_relation($3)) {
      tsdb_create_table($3, $4); 
    } /* if */
    else {
      fprintf(TSDB_ERROR_STREAM, "\ttsdb: Table already exists.\n");
    } /* else */
  }
;

y_dropping : 
  Y_DROP Y_TABLE y_table_name { 
    if(tsdb_is_relation($3)) 
      tsdb_drop_table($3); 
    else
      fprintf(TSDB_ERROR_STREAM, "\ttsdb: Table does not exist.\n");
  }
;

y_altering : 
  Y_ALTER Y_TABLE y_table_name Y_ADD y_create_attribute_list { 
    if(tsdb_is_relation($3)) 
      tsdb_alter_table($3, $5);
    else 
      fprintf(TSDB_ERROR_STREAM, "\ttsdb: Table does not exist.\n");
  }
;

y_insertion :
  Y_INSERT Y_INTO y_table_name y_attribute_list Y_VALUES y_value_list {
    tsdb_insert($3, $4, $6);
  }
| 
  Y_INSERT Y_INTO y_table_name Y_VALUES y_value_list {
    tsdb_insert($3, NULL, $5);
  }
;

y_deletion :
  Y_DELETE Y_FROM y_table_name {
    tsdb_delete($3, (Tsdb_node *)NULL);
  }
|
  Y_DELETE Y_FROM y_table_name Y_WHERE y_condition {
    tsdb_delete($3, $5);
  }
;

y_retrieval :
  Y_RETRIEVE y_attribute_list Y_WHERE y_condition {
    tsdb_complex_retrieve(NULL, $2, $4);
  }
|
  Y_RETRIEVE y_attribute_list {
    tsdb_complex_retrieve(NULL, $2, NULL);
  }
|
  Y_RETRIEVE y_attribute_list Y_FROM y_attribute_list Y_WHERE y_condition {
    tsdb_complex_retrieve($4, $2, $6);
  }
| Y_RETRIEVE y_attribute_list Y_FROM y_attribute_list {
    tsdb_complex_retrieve($4, $2, NULL);
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
;

y_condition : 
  y_attribute y_operator y_tsdb_value {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = tsdb_leaf($1);
    $$->node = $2;
    $$->right = tsdb_leaf($3);
   }
|
  Y_MATCH '(' y_attribute ',' y_tsdb_value ')' {
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node));
    $$->left = tsdb_leaf($3);
    $$->node = tsdb_operator(TSDB_SUBSTRING);
    $$->right = tsdb_leaf($5);
  }
|
  '(' y_condition ')' {
    $$ = $2;
/*
    $$ = (Tsdb_node *)malloc(sizeof(Tsdb_node*));
    $$->left = $2;
    $$->node = tsdb_connective(TSDB_BRACE);
    $$->right = NULL;*/
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
    $$ = tsdb_operator(TSDB_SUBSTRING);
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
  y_tsdb_value {
    $$ = tsdb_singleton_value_array($1);
  }
|
   y_values y_tsdb_value {
    $$ = tsdb_value_array_append($1, $2);
  }
;

y_tsdb_value :
  Y_STRING {
    $$ = tsdb_string($1);
  }
|
  Y_INTEGER {
    $$ = tsdb_integer($1);
  }
;

y_attribute_value_list :
  '(' y_attribute_value_pairs ')' {
    printf("y_attribute_value_list\n");
  }
|
  y_attribute_value_pairs {
    printf("y_attribute_value_list\n");
  }
;

y_attribute_value_pairs :
  y_attribute_value_pair {
    printf("singleton a_v_p\n");
  }
|
  y_attribute_value_pairs y_attribute_value_pair {
    printf("array of  a_v_p's\n");
  }
;

y_attribute_value_pair :
  y_attribute Y_EQUAL y_tsdb_value {
    printf("y_attribute_value_pair\n");
  }
;

y_test :
  Y_TEST Y_FROM y_attribute_list Y_TO y_attribute_list {
    tsdb_debug_join_path($3, $5);
  }
|
  Y_TEST y_attribute_list '#' y_attribute_list {
    tsdb_debug_simple_join($2, $4);
  }
;

y_exit : 
  Y_QUIT {
    tsdb_quit();
  }
;
                                
%%

int yywrap() {

  return(1);

} /* yywrap() */

yyerror(char *s) {
 
  fprintf(TSDB_ERROR_STREAM,
          "parser(): parse error; check the tsdb(1) syntax.\n");

} /* yyerror() */
