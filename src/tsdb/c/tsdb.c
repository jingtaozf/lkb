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

#ifdef DEBUG
FILE *tsdb_debug_stream = (FILE *)NULL;
#endif

int really_verbose_mode = FALSE;

char *tsdb_home;
char *tsdb_relations_file;
char *tsdb_data_path;
char *tsdb_result_path;
char *tsdb_result_prefix;
BYTE tsdb_max_results = TSDB_MAX_RESULTS;
char *tsdb_input;
char *tsdb_pager;

Tsdb_relation **tsdb_relations = (Tsdb_relation **)NULL;
Tsdb_selection **tsdb_data = (Tsdb_selection **)NULL;
