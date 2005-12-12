/*****************************************************************************\
|*        file: tsdb_history.c
|*      module: 
|*     version: 
|*  written by: tom, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <errno.h>
extern int errno;

#include "globals.h"
#include "tsdb.h"
#include "errors.h"

#if defined(SUNOS) || defined(SOLARIS) || defined(LINUX) || defined(OSF)
#  include <sys/time.h>
#else
#  include <sys/times.h>
#endif

static int _history_position = -1;

Tsdb_history *tsdb_alloc_history() {

  Tsdb_history *bar;

  bar = (Tsdb_history*)malloc(sizeof(Tsdb_history));
  if (!bar) {
    return(NULL);
  } /* if */
  bar->command = -1;
  bar->result = NULL;
  bar->unique_tuples = -1;
  return(bar);

} /* tsdb_alloc_history() */

int tsdb_init_history(Tsdb *status) {

  int i;
  Tsdb_history **past, *bar;

  if (status->history_size < 1) {
    /* no history! */
    return(0);
  } /* if */
  
  status->history = (Tsdb_history **)malloc(status->history_size
                                            * sizeof(Tsdb_history *));
  if (!status->history) {
    status->history_size = 0;
    return(-1);
  } /* if  */
  past = status->history;
  for(i = 0; i < status->history_size; i++) {
    bar = tsdb_alloc_history();
    if (!bar) {
      status->history_size = 0;
      return(-1);
    } /* if */
    past[i] = bar;
  } /* for */
  _history_position = -1;
  return(1);

} /* tsdb_init_history() */

void tsdb_free_history(Tsdb_history* h) {
  if (h->result) {
    tsdb_free_selection(h->result);
    h->result=NULL;
  }
  h->command = -1;
  free(h);

} /* tsdb_free_history() */

void tsdb_set_history_size(int size) {

  Tsdb_history **foo;
  int i, max, min;
  
  if((tsdb.history_size < 1) && (size > 0)) {
    /* first time history */
    tsdb.history_size = size;
    tsdb_init_history(&tsdb);
    return;
  } /* if */
  
  foo = (Tsdb_history **)malloc(size * sizeof(Tsdb_history *));
  if (!foo) {
    tsdb.history_size = 0;
    return;
  } /* if */
  
  if(size > tsdb.history_size) {
    min = tsdb.history_size;
    max = size;
  } /* if */
  else {
    max = tsdb.history_size;
    min = size;
  } /* else */

  for(i = 0; i < min; i++) {
    foo[i] = tsdb.history[(i + _history_position) % tsdb.history_size];
  } /* for */
  
  if(max == size) {
    /* history made bigger */
    for (; i < max; i++) {
      foo[i] = tsdb_alloc_history();
    } /* for */
  } /* if */
  else {
    /* history made smaller */
    for(; i < max; i++) {
      tsdb_free_history(tsdb.history[(i + _history_position)
                                     % tsdb.history_size]);
    } /* for */
  } /* else */
  
  free(tsdb.history);
  tsdb.history = foo;
  _history_position = 0;
  tsdb.history_size = size;
  
} /* tsdb_set_history_size() */

void tsdb_add_to_history(Tsdb_selection *s, int n) {

  Tsdb_history *foo = tsdb.history[(_history_position)];
  int pos;

  if (tsdb.history_size < 1) {
    return;
  } /* if */
  
  pos = (_history_position + 1) % tsdb.history_size;

#ifdef HACK
  /* _hack_
   * because the bloody free_selection() calls mess up some of the original
   * structures in `tsdb.data' (which took me a full day to debug; tom should
   * pay me back for this |:-{); the attempt to find the source of the problem
   * is another nightmare: tsdb_select() is barely readable for me.  --- this
   * means trouble, eventually ...                   (9-aug-96  -  oe@coling)
   */
  if(tsdb.history[pos]->result) {
    tsdb_free_selection(tsdb.history[pos]->result);
  } /* if */
#endif
  tsdb.history[pos]->command = tsdb.command;
  tsdb.history[pos]->query = tsdb.query;
  tsdb.history[pos]->result = s;
  tsdb.history[pos]->unique_tuples = n;

  _history_position = pos;

} /* tsdb_add_to_history() */

Tsdb_history *tsdb_get_history(int item) {

  Tsdb_history **past = tsdb.history;
  int i;
  
  if (_history_position < 0 || item < -1) {
    return((Tsdb_history *)NULL);
  } /* if */
  if(item == -1) {
    return(past[_history_position % tsdb.history_size]);
  } /* if */
  for (i = 0; i < tsdb.history_size; i++) {
    if (past[(_history_position + i) % tsdb.history_size]->command == item) {
      return(past[(_history_position + i) % tsdb.history_size]);
    } /* if */
  } /* for */
  return((Tsdb_history *)NULL);

} /* tsdb_get_history() */
