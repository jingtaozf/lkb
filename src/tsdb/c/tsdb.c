/*****************************************************************************\
|*        file: tsdb.c
|*      module: TSDB global variables
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#define TSDB_DOT_C

#include <stdio.h>
#include "globals.h"
#include "tsdb.h"

Tsdb tdsb = { 0, 0 };

FILE *tsdb_default_stream = (FILE *)NULL;
FILE *tsdb_error_stream = (FILE *)NULL;

#ifdef DEBUG
  FILE *tsdb_debug_stream = (FILE *)NULL;
  char *tsdb_debug_file = (char *)NULL;
#endif

int really_verbose_mode = FALSE;

char *tsdb_home = (char *)NULL;
char *tsdb_relations_file = (char *)NULL;
char *tsdb_data_path = (char *)NULL;
char *tsdb_result_path = (char *)NULL;
char *tsdb_result_prefix = (char *)NULL;
int tsdb_max_results = -1;

char *tsdb_input = (char *)NULL;
char *tsdb_pager = (char *)NULL;

Tsdb_relation **tsdb_relations = (Tsdb_relation **)NULL;
Tsdb_selection **tsdb_data = (Tsdb_selection **)NULL;
