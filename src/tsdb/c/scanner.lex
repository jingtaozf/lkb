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

DIGIT           [0-9]
NUMBER          {DIGIT}+
DELIM           [ \t\n]
WHITESPACE      {DELIM}+
QUOTE           (\")
LETTER          [a-zA-Z]
SPECIAL         [-_]
IDENTIFIER      {LETTER}({LETTER}|{DIGIT}|{SPECIAL})*
STRING          {QUOTE}[^"]*{QUOTE}

%%

{r}{e}{t}{r}{i}{e}{v}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Retrieve\n");
  } /* if */
  return(Y_RETRIEVE);
}

{s}{e}{l}{e}{c}{t} {
  if(verbose_mode) {
    fprintf(stderr, "Select\n");
  } /* if */
  return(Y_RETRIEVE);
}

{u}{p}{d}{a}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Update\n");
  } /* if */
  return(Y_UPDATE);
}

{i}{n}{s}{e}{r}{t} {
  if(verbose_mode) {
    fprintf(stderr, "Insert\n");
  } /* if */
  return(Y_INSERT);
}

{d}{e}{l}{e}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Delete\n");
  } /* if */
  return(Y_DELETE);
}

{d}{r}{o}{p} {
  if(verbose_mode) {
    fprintf(stderr, "Drop\n");
  } /* if */
  return(Y_DROP);
}

{c}{r}{e}{a}{t}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Create\n");
  } /* if */
  return(Y_CREATE);
}

{a}{l}{t}{e}{r} {
  if(verbose_mode) {
    fprintf(stderr, "Alter\n");
  } /* if */
  return(Y_ALTER);
}

{i}{n}{f}{o} {
  if(verbose_mode) {
    fprintf(stderr, "Info\n");
  } /* if */
  return(Y_INFO);
}

{s}{e}{t} {
  if(verbose_mode) {
    fprintf(stderr, "Set\n");
  } /* if */
  return(Y_SET);
}

{t}{a}{b}{l}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Table\n");
  } /* if */
  return(Y_TABLE);
}

{w}{h}{e}{r}{e} {
  if(verbose_mode) {
    fprintf(stderr, "Where\n");
  } /* if */
  return(Y_WHERE);
}

{a}{d}{d} {
  if(verbose_mode) {
    fprintf(stderr, "Add\n");
  } /* if */
  return(Y_ADD);
}

{f}{r}{o}{m} {
  if(verbose_mode) {
    fprintf(stderr, "From\n");
  } /* if */
  return(Y_FROM);
}

{i}{n}{t}{o} {
  if(verbose_mode) {
    fprintf(stderr, "Into\n");
  } /* if */
  return(Y_INTO);
}

{v}{a}{l}{u}{e}{s} {
  if(verbose_mode) {
    fprintf(stderr, "Values\n");
  } /* if */
  return(Y_VALUES);
}

{r}{e}{l}{a}{t}{i}{o}{n}{s} {
  if(verbose_mode) {
    fprintf(stderr, "Relations\n");
  } /* if */
  return(Y_RELATIONS);
}

{t}{e}{s}{t} {
  if(verbose_mode) {
    fprintf(stderr, "Test\n");
  } /* if */
  return(Y_TEST);
}

:{i}{n}{t}{e}{g}{e}{r} {
  if(verbose_mode) {
    fprintf(stderr, "Integer Type\n");
  } /* if */
  return(Y_INTEGER_TYPE);
}

:{s}{t}{r}{i}{n}{g} {
  if(verbose_mode) {
    fprintf(stderr, "String Type\n");
  } /* if */
  return(Y_STRING_TYPE);
}

:{k}{e}{y} {
  if(verbose_mode) {
    fprintf(stderr, "Key\n");
  } /* if */
  return(Y_KEY);
}

{m}{a}{t}{c}{h} {
  if(verbose_mode) {
    fprintf(stderr, "Match\n");
  } /* if */
  return(Y_MATCH);
}

({q}{u}{i}{t})|({e}{x}{i}{t})|({c}{i}{a}{o})|({b}{y}{e}) {
  if(verbose_mode) {
    fprintf(stderr, "Quit\n");
  } /* if */
  return(Y_QUIT);
}


"<" {
  if(verbose_mode) {
    fprintf(stderr, "Less\n");
  } /* if */
  return(Y_LESS);
}

">" {
  if(verbose_mode) {
    fprintf(stderr, "Greater\n");
  } /* if */
  return(Y_GREATER);
}
        
"!=" {
  if(verbose_mode) {
    fprintf(stderr, "Not_Equal\n");
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
  *(char *)strrchr(&yytext[1], '"') = 0;
  yylval.string = (char *)strdup(&yytext[1]); /* Get rid of quotes */
  return(Y_STRING);
}

. {
  return(yytext[0]);
}

%%
