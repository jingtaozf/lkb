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

#define TSDB_C

#include <stdio.h>
#include "globals.h"
#include "tsdb.h"

#if defined(NOFREE)
void free(void *foo) {
  ;
} /* free() */
#endif

/* temporary hack before tom gets the fucking history working */
Tsdb_selection *tsdb_last_result = (Tsdb_selection *)NULL;

Tsdb tsdb = { 
  0,                       /* status */
  (Tsdb_relation **)NULL,  /* relations */
  (Tsdb_selection **)NULL, /* data */
  (char *)NULL,            /* input */
  (char *)NULL,            /* home */
  (char *)NULL,            /* relations_file */
  (char *)NULL,            /* data_path */
  (char *)NULL,            /* result_path */
  (char *)NULL,            /* result_prefix */
  -1,                      /* max_results */
  (char *)NULL,            /* server */
  0,                       /* port */
  (char *)NULL,            /* pager */
  (char *)NULL,            /* query */
#ifdef DEBUG
  (char *)NULL,            /* debug_file */
#endif
  (Tsdb_history **)NULL,   /* history */
  -1                       /* history_position */
}; /* tsdb */

FILE *tsdb_default_stream = TSDB_DEFAULT_STREAM;
FILE *tsdb_error_stream = TSDB_ERROR_STREAM;
#ifdef DEBUG
  FILE *tsdb_debug_stream = (FILE *)NULL;
#endif
