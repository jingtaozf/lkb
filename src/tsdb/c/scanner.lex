/*****************************************************************************\
|*        file: scanner.lex
|*      module: TSDB lexical analyzer
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

%{

#include <string.h>
#include "globals.h"
#include "tsdb.h"
#include "parser.h"

#undef YY_INPUT
#define YY_INPUT(string, n, max) { \
  int c = tsdb_getchar(); \
  n = (c == EOF ? YY_NULL : (string[0] = c, 1)); \
} /* YY_INPUT() */

int verbose_mode = FALSE;
#if defined(DEBUG) && defined(SCANNER)
  verbose_mode = TRUE;
#endif

%}

a [aA]
b [bB]
c [cC]
d [dD]
e [eE]
f [fF]
g [gG]
h [hH]
i [iI]
j [jJ]
k [kK]
l [lL]
m [mM]
n [nN]
o [oO]
p [pP]
q [qQ]
r [rR]
s [sS]
t [tT]
u [uU]
v [vV]
w [wW]
x [xX]
y [yY]
z [zZ]

DIGIT [0-9]
NUMBER [-+]?{DIGIT}+
DELIMITER [ \t\n]
WHITESPACE {DELIMITER}+
QUOTE (\")
LETTER [a-zA-Z]
SPECIAL [-_]
IDENTIFIER {LETTER}({LETTER}|{DIGIT}|{SPECIAL})*
STRING ({QUOTE}[^"]*{QUOTE})|`[^']*'

%%

{r}{e}{t}{r}{i}{e}{v}{e} {
  if(verbose_mode) {
    fprintf(stderr, "RETRIEVE\n");
  } /* if */
  return(Y_RETRIEVE);
}

{s}{e}{l}{e}{c}{t} {
  if(verbose_mode) {
    fprintf(stderr, "SELECT\n");
  } /* if */
  return(Y_RETRIEVE);
}

{u}{p}{d}{a}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "UPDATE\n");
  } /* if */
  return(Y_UPDATE);
}

{i}{n}{s}{e}{r}{t} {
  if(verbose_mode) {
    fprintf(stderr, "INSERT\n");
  } /* if */
  return(Y_INSERT);
}

{d}{e}{l}{e}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "DELETE\n");
  } /* if */
  return(Y_DELETE);
}

{d}{r}{o}{p} {
  if(verbose_mode) {
    fprintf(stderr, "DROP\n");
  } /* if */
  return(Y_DROP);
}

{c}{r}{e}{a}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "CREATE\n");
  } /* if */
  return(Y_CREATE);
}

{a}{l}{t}{e}{r} {
  if(verbose_mode) {
    fprintf(stderr, "ALTER\n");
  } /* if */
  return(Y_ALTER);
}

{i}{n}{f}{o} {
  if(verbose_mode) {
    fprintf(stderr, "INFO\n");
  } /* if */
  return(Y_INFO);
}

{s}{e}{t} {
  if(verbose_mode) {
    fprintf(stderr, "SET\n");
  } /* if */
  return(Y_SET);
}

{t}{a}{b}{l}{e} {
  if(verbose_mode) {
    fprintf(stderr, "TABLE\n");
  } /* if */
  return(Y_TABLE);
}

{w}{h}{e}{r}{e} {
  if(verbose_mode) {
    fprintf(stderr, "WHERE\n");
  } /* if */
  return(Y_WHERE);
}

{r}{e}{p}{o}{r}{t} {
  if(verbose_mode) {
    fprintf(stderr, "REPORT\n");
  } /* if */
  return(Y_REPORT);
}

{a}{d}{d} {
  if(verbose_mode) {
    fprintf(stderr, "ADD\n");
  } /* if */
  return(Y_ADD);
}

{f}{r}{o}{m} {
  if(verbose_mode) {
    fprintf(stderr, "FROM\n");
  } /* if */
  return(Y_FROM);
}

{i}{n}{t}{o} {
  if(verbose_mode) {
    fprintf(stderr, "INTO\n");
  } /* if */
  return(Y_INTO);
}

{v}{a}{l}{u}{e}{s} {
  if(verbose_mode) {
    fprintf(stderr, "VALUES\n");
  } /* if */
  return(Y_VALUES);
}

{r}{e}{l}{a}{t}{i}{o}{n}{s} {
  if(verbose_mode) {
    fprintf(stderr, "RELATIONS\n");
  } /* if */
  return(Y_RELATIONS);
}

{t}{e}{s}{t} {
  if(verbose_mode) {
    fprintf(stderr, "TEST\n");
  } /* if */
  return(Y_TEST);
}

:{i}{n}{t}{e}{g}{e}{r} {
  if(verbose_mode) {
    fprintf(stderr, "INTEGER TYPE\n");
  } /* if */
  return(Y_INTEGER_TYPE);
}

:{s}{t}{r}{i}{n}{g} {
  if(verbose_mode) {
    fprintf(stderr, "STRING TYPE\n");
  } /* if */
  return(Y_STRING_TYPE);
}

:{k}{e}{y} {
  if(verbose_mode) {
    fprintf(stderr, "KEY\n");
  } /* if */
  return(Y_KEY);
}

{m}{a}{t}{c}{h} {
  if(verbose_mode) {
    fprintf(stderr, "MATCH\n");
  } /* if */
  return(Y_MATCH);
}

({q}{u}{i}{t})|({e}{x}{i}{t})|({c}{i}{a}{o})|({b}{y}{e}) {
  if(verbose_mode) {
    fprintf(stderr, "QUIT\n");
  } /* if */
  return(Y_QUIT);
}


"<" {
  if(verbose_mode) {
    fprintf(stderr, "LESS\n");
  } /* if */
  return(Y_LESS);
}

">" {
  if(verbose_mode) {
    fprintf(stderr, "GREATER\n");
  } /* if */
  return(Y_GREATER);
}
        
"!=" {
  if(verbose_mode) {
    fprintf(stderr, "NOT_EQUAL\n");
  } /* if */
  return(Y_NOT_EQUAL);
}

"="|"==" {
  if(verbose_mode) {
    fprintf(stderr, "EQUAL\n");
  } /* if */
  return(Y_EQUAL);
}

"<="|"=<" {
  if(verbose_mode) {
    fprintf(stderr, "LESS_OR_EQUAL\n");
  } /* if */
  return(Y_LESS_OR_EQUAL);
}

">="|"=>" {
  if(verbose_mode) {
    fprintf(stderr, "GREATER_OR_EQUAL\n");
  } /* if */
  return(Y_GREATER_OR_EQUAL);
}

"~" {
  if(verbose_mode) {
    fprintf(stderr, "TILDA\n");
  } /* if */
  return(Y_TILDA);
}
        
"!~" {
  if(verbose_mode) {
    fprintf(stderr, "NOT_TILDA\n");
  } /* if */
  return(Y_NOT_TILDA);
}
        
({a}{n}{d})|"&"|"&&" {
  if(verbose_mode) {
    fprintf(stderr, "AND\n");
  } /* if */
  return(Y_AND);
}

({n}{o}{t})|"!" {
  if(verbose_mode) {
    fprintf(stderr, "NOT\n");
  } /* if */
  return(Y_NOT);
}

({o}{r})|"|"|"||" {
  if(verbose_mode) {
    fprintf(stderr, "OR\n");
  } /* if */
  return(Y_OR);
}

{WHITESPACE}

{NUMBER} {
  if(verbose_mode) {
    fprintf(stderr, "INTEGER\n");
  } /* if */
  yylval.integer = atoi(&yytext[0]);
  return(Y_INTEGER);
}

{IDENTIFIER} {
  if(verbose_mode) {
    fprintf(stderr, "IDENTIFIER\n");
  } /* if */
  yylval.string = (char *)strdup(&yytext[0]);
  return(Y_IDENTIFIER);
}

{STRING} {
  if(verbose_mode) {
    fprintf(stderr, "STRING\n");
  } /* if */
  yytext[strlen(&yytext[0]) - 1] = 0;
  yylval.string = (char *)strdup(&yytext[1]);
  return(Y_STRING);
}

. {
  return(yytext[0]);
}

%%
