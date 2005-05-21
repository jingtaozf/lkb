/*****************************************************************************\
|* [incr tsdb()] gc() hooks; signal swish(1) using TTIN and TTOU. we really 
|* hope these signals will be available on all target platforms.
\*****************************************************************************/

#include <signal.h>

int tsdb_wish_pid = -1;

int enable_gc_cursor(int pid) {

  if(pid) {
    tsdb_wish_pid = pid;
  } /* if */
  return(tsdb_wish_pid);

} /* enable_gc_cursor() */

int gc_start(int type) {

#ifdef SIGTTIN
  if(tsdb_wish_pid > 0) {
    return(kill(tsdb_wish_pid, SIGTTIN));
  } /* if */
#endif
  return(-1);

} /* gc_start() */

int gc_end(int type) {

#ifdef SIGTTOU
  if(tsdb_wish_pid > 0) {
    return(kill(tsdb_wish_pid, SIGTTOU));
  } /* if */
#endif
  return(-1);

} /* gc_end() */
