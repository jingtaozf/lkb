/*
 * this is my first attempt at foreign function coding in Allegro CL; because
 * i find the argument passing and conversion conventions most puzzling (and
 * have a suspicion that they involve copying and potential leaks), we play it
 * ultra-conservative: most larger objects are passed through temporary files;
 * as most machines have `/tmp' on swap these days, the overhead should barely
 * be noticeable.  anyway, this interface is not meant for at most one message
 * per second (if at all).                            (4-feb-99  -  oe@csli)
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>
#include "pvm3.h"
#include "itsdb.h"

double load_average(void);
int pvm_register(char *, int);
int pvm_quit(void);
int pvm_create(char *, char **, char *, char *);
int pvm_flush(void);
int pvm_transmit(int, int, char *);
int pvm_file_transmit(int, int, char *);
int pvm_poll(int, int, int, char *, int);
int pvm_collect(char *);
int pvm_vm_info(char *);
int pvm_task_info(int, char *);

int self = 0;
FILE *trace = (FILE *)NULL;
static int pending = -1;
static int psize = -1;
static char *buffer = (char *)NULL;
static int bsize = 0;
int debugp = 0;

double load_average(void) {

  extern int getloadavg(double *, int);
  double load;

  if(getloadavg(&load, 1) == 1) {
    return(load);
  } /* if */
  if(getloadavg(&load, 1) == 1) {
    return(load);
  } /* if */
  if(getloadavg(&load, 1) == 1) {
    return(load);
  } /* if */
  return((double)-1);

} /* load_average() */

int pvm_register(char *file, int debug) {

  int parent;

  debugp = debug;

  if(self) {
    pvm_quit();
  } /* if */

  if((self = pvm_mytid()) < 0) {
    pvm_perror("pvm_register()");
    fprintf(stderr, "pvm_register(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_exit();
    return(-1);
  } /* if */

  if((parent = pvm_parent()) > 0 && parent != PvmNoParent) {
    /*
     * for children of .parent. request notification when .parent. terminates
     */
    pvm_notify(PvmTaskExit, TASK_FAIL, 1, &parent);
  } /* if */
  else {
    /*
     * in the [incr tsdb()] setup we have exactly one parent (i.e. the task
     * that delegates items to its childs); request output accumulation for
     * children tasks we expect to create.
     */
    if(file == NULL
       || !strcmp(file, "/dev/null")) {
      pvm_catchout((FILE *)NULL);
      trace = (FILE *)NULL;
    } else if(*file
              && (trace = fopen(file, "w")) != NULL) {
      pvm_catchout(trace);
    } /* if */
    else {
      trace = stdout;
      pvm_catchout(trace);
    } /* else */
  } /* else */

  pvm_setopt(PvmRoute, PvmRouteDirect);
  return(self);

} /* pvm_register() */

int pvm_announce(char *class, char *version, char *user) {

  int tid;
  if((tid = pvm_mytid()) < 0) {
    pvm_perror("pvm_announce()");
    fprintf(stderr, "pvm_announce(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_exit();
    return(-1);
  } /* if */

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("pvm_announcet()");
    fprintf(stderr, "pvm_announce(): unable to register announce message.\n");
    fflush(stderr);
    return -1;
  }; /* if */
  if(pvm_pkint(&tid, 1, 1) < 0) {
    pvm_perror("pvm_announcet()");
    fprintf(stderr, "pvm_announce(): unable to register announce message.\n");
    fflush(stderr);
    return -1;
  }; /* if */
  if(pvm_pkstr(version) < 0) {
    pvm_perror("pvm_announcet()");
    fprintf(stderr, "pvm_announce(): unable to register announce message.\n");
    fflush(stderr);
    return -1;
  }; /* if */
  if(pvm_pkstr(user) < 0) {
    pvm_perror("pvm_announcet()");
    fprintf(stderr, "pvm_announce(): unable to register announce message.\n");
    fflush(stderr);
    return -1;
  }; /* if */
  if(pvm_putinfo(class, pvm_getsbuf(), PvmMboxDefault) < 0) {
    pvm_perror("pvm_announcet()");
    fprintf(stderr, "pvm_announce(): unable to register announce message.\n");
    fflush(stderr);
    return -1;
  } /* if */
  return 0;

} /* pvm_announce() */

int pvm_lookup(char *class, char *version, int vsize, char *user, int usize) {

  int buffer, tid;

  if ((buffer = pvm_recvinfo(class, 0, PvmMboxFirstAvail)) >= 0) {
    if(pvm_setrbuf(buffer) < 0) {
      pvm_perror("pvm_lookup()");
      fprintf(stderr, 
              "pvm_lookup(): unable to read (announce) message buffer.\n");
      fflush(stderr);
      pvm_quit();
      return -1;
    } /* if */
    if(pvm_upkint(&tid, 1, 1) < 0) {
      pvm_perror("pvm_lookup()");
      fprintf(stderr, 
              "pvm_lookup(): unable to read (announce) message buffer.\n");
      fflush(stderr);
      pvm_quit();
      return -1;
    } /* if */
    if(pvm_upkstr(version) < 0) {
      pvm_perror("pvm_lookup()");
      fprintf(stderr, 
              "pvm_lookup(): unable to read (announce) message buffer.\n");
      fflush(stderr);
      pvm_quit();
      return -1;
    } /* if */
    if(pvm_upkstr(user) < 0) {
      pvm_perror("pvm_lookup()");
      fprintf(stderr, 
              "pvm_lookup(): unable to read (announce) message buffer.\n");
      fflush(stderr);
      pvm_quit();
      return -1;
    } /* if */
    return tid;
  } /* if */
  return 0;

} /* pvm_lookup() */ 

int pvm_quit(void) {

  if(self > 0) {
    pvm_catchout((FILE *)NULL);
    if(trace != NULL && trace != stdout) {
      fclose(trace);
      trace = (FILE *)NULL;
    } /* if */
    pvm_exit();
    self = 0;
    bsize = 0;
    if(buffer != NULL) {
      free(buffer);
    } /* if */
  } /* if */

  return(0);

} /* pvm_quit() */

int pvm_create(char *task, char **argv, char *host, char *architecture) {

  int flag, tids[1];
  char *where;

  if(pvm_mytid() < 0) {
    return(-1);
  } /* if */

  flag = PvmTaskDefault;
  if(*host) {
    flag = PvmTaskHost;
    where = host;
  } else if(*architecture) {
    flag = PvmTaskArch;
    where = architecture;
  } else {
    where = (char *)NULL;
  } /* else */

  if(where != NULL) {
    if(pvm_spawn(task, argv, flag, where, 1, &tids[0]) == 1) {
      pvm_notify(PvmTaskExit, TASK_FAIL, 1, &tids[0]);
    } /* if */
    return(tids[0]);
  } /* if */
  return(-42);

} /* pvm_create() */

int pvm_flush(void) {

  if(pvm_probe(-1, -1) < 0) {
    return(-1);
  } /* if */
  if(trace != NULL) {
    fflush(trace);
  } /* if */
  return(0);

} /* pvm_flush() */

int pvm_transmit(int tid, int tag, char *string) {

  int n;
  double load;
  
  if(trace != NULL) {
    fflush(trace);
  } /* if */

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  n = strlen(string);
  if(pvm_pkint(&n, 1, 1) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  if(pvm_pkstr(string) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  load = load_average();
  if(pvm_pkdouble(&load, 1, 1) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if((n = pvm_send(tid, tag)) < 0 ) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to send message.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(trace != NULL) {
    fflush(trace);
  } /* if */

  return(n);

} /* pvm_transmit() */

int pvm_file_transmit(int tid, int tag, char *file) {

  int length, increment, n;
  double load;
  FILE *input;
  
  if(!bsize) {
    bsize = 4096;
    buffer = (char *)malloc(bsize + 1);
  } /* if */
  
  if(trace != NULL) {
    fflush(trace);
  } /* if */

  if((input = fopen(file, "r")) == NULL) {
    fprintf(stderr, "pvm_transmit(): unable to read `%s'.\n", file);
    fflush(stderr);
    return(-1);
  } /* if */

  length = 0;
  increment = bsize;
  while((n = fread(&buffer[length], 1, increment, input)) == increment
        && !ferror(input) && !feof(input)) {
    length += n;
    increment = bsize;
    bsize += increment;
    buffer = (char *)realloc(buffer, bsize + 1);
  } /* while */
  fclose(input);
  if(!debugp) {
    unlink(file);
  } /* if */

  if(!n) {
    fprintf(stderr, "pvm_transmit(): error reading `%s'.\n", file);
    fflush(stderr);
    return(-1);
  } /* if */
  length += n;
  buffer[length] = (char)0;

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  if(pvm_pkstr(buffer) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  load = load_average();
  if(pvm_pkdouble(&load, 1, 1) < 0) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if((n = pvm_send(tid, tag)) < 0 ) {
    pvm_perror("pvm_transmit()");
    fprintf(stderr, "pvm_transmit(): unable to send message.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(trace != NULL) {
    fflush(trace);
  } /* if */

  return(n);

} /* pvm_file_transmit() */

int pvm_poll(int tid, int tag, int block, char *output, int size) {

  int id, length, remote, mtag, n;
  double load;
  struct timeval timeout;
 
  pending = psize = -1;

  if(pvm_mytid() < 0) {
    pvm_perror("pvm_poll()");
    fprintf(stderr, "pvm_poll(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(trace != NULL) {
    fflush(trace);
  } /* if */

  if(!block) {
    id = pvm_nrecv(tid, tag);
  } /* if */
  else if(block == -1) {
    id = pvm_recv(tid, tag);
  } /* if */
  else {
    timeout.tv_sec = block;
    timeout.tv_usec = 0;
    id = pvm_trecv(tid, tag, &timeout);
  } /* else */

  if(trace != NULL) {
    fflush(trace);
  } /* if */

  if(!id) {
    return(0);
  } /* if */
  else if(id > 0) {
    if(pvm_bufinfo(id, &n, &mtag, &remote)) {
      pvm_perror("pvm_poll()");
      fprintf(stderr, "pvm_poll(): unable to read receive buffer.\n");
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    if(mtag != TASK_FAIL 
       && mtag != LISP_MESSAGE) {
      fprintf(stderr, "pvm_poll(): unknown message type `%i'.\n", mtag);
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    if(pvm_upkint(&length, 1, 1) < 0) {
      pvm_perror("pvm_poll()");
      fprintf(stderr, "pvm_poll(): unable to read receive buffer.\n");
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    if(trace != NULL) {
      fflush(trace);
    } /* if */
    if(mtag == TASK_FAIL) {
      n = sprintf(&output[0],
                  "((:tag . %i) (:remote . %i) (:corpse . %d))", 
                  mtag, remote, length);
      return(n);
    } /* if */
    n = sprintf(&output[0],
                "((:tag . %i) (:remote . %i) (:content . ", 
                mtag, remote);
    if(size < n + length + 20) {
      pending = pvm_setrbuf(0);
      psize = length;
      return(n + length + 20);
    } /* if */
    if(pvm_upkstr(&output[n]) < 0) {
      pvm_perror("pvm_poll()");
      fprintf(stderr, "pvm_poll(): unable to read receive buffer.\n");
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    n += length;
    if(pvm_upkdouble(&load, 1, 1) < 0) {
      pvm_perror("pvm_poll()");
      fprintf(stderr, "pvm_poll(): unable to read receive buffer.\n");
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    n += sprintf(&output[n], ") (:load . %.2f))", load);
    if(trace != NULL) {
      fflush(trace);
    } /* if */
    return(n);
  } /* else */
  else {
    pvm_perror("pvm_poll()");
    fprintf(stderr, "pvm_poll(): error receiving message.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* else */

} /* pvm_poll() */

int pvm_collect(char *output) {

  int mtag, remote, n;
  double load;

  if(pending < 0 || psize < 0 || pvm_setrbuf(pending) < 0) {
    pvm_perror("pvm_collect()");
    fprintf(stderr, "pvm_collect(): no (valid) pending receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_bufinfo(pending, &n, &mtag, &remote)) {
    pvm_perror("pvm_collect()");
    fprintf(stderr, "pvm_collect(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  n = sprintf(&output[0],
              "((:tag . %i) (:remote . %i) (:content . ", 
              mtag, remote);
  if(pvm_upkstr(&output[n]) < 0) {
    pvm_perror("pvm_collect()");
    fprintf(stderr, "pvm_collect(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  n += psize;
  pending = psize = -1;
  if(pvm_upkdouble(&load, 1, 1) < 0) {
    pvm_perror("pvm_poll()");
    fprintf(stderr, "pvm_collect(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  n += sprintf(&output[n], ") (:load . %.2f))", load);
  if(trace != NULL) {
    fflush(trace);
  } /* if */
  return(n);

} /* pvm_collect() */

int pvm_vm_info(char *file) {

  FILE *output;
  int nhost, narch, i;
  struct pvmhostinfo *hosts;

  if(pvm_config(&nhost, &narch, &hosts) < 0) {
    pvm_perror("pvm_vm_info()");
    fprintf(stderr, "pvm_vm_info(): error querying vm configuration.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if((output = fopen(file, "w")) == NULL) {
    fprintf(stderr, "pvm_vm_info(): unable to open `%s'.\n", file);
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  fprintf(output, 
          "((:nhosts . %i) (:narchitectures . %i)\n (:hosts\n", 
          nhost, narch);
  for(i = 0; i < nhost; i++) {
    fprintf(output,
            "   ((:tid . %i) (:name . \"%s\")\n"
            "    (:architecture . \"%s\") (:speed . %d))\n",
            hosts[i].hi_tid,
            (hosts[i].hi_name != NULL ? hosts[i].hi_name : ""),
            (hosts[i].hi_arch != NULL ? hosts[i].hi_arch : ""),
            hosts[i].hi_speed);
  } /* for */
  fprintf(output, "))\n");
  fflush(output);
  if((i = ftell(output)) <= 0) {
    fprintf(stderr, "pvm_vm_info(): error verifying `%s'.\n", file);
    fflush(stderr);
    return(-1);
  } /* if */
  fclose(output);
  return(i);

} /* pvm_vm_info() */

int pvm_task_info(int tid, char *file) {

  FILE *output;
  int ntask, i;
  struct pvmtaskinfo *tasks;

  if(pvm_tasks(tid, &ntask, &tasks) < 0) {
    pvm_perror("pvm_task_info()");
    fprintf(stderr, 
            "pvm_task_info(): error querying task [%d] information.\n",
            tid);
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if((output = fopen(file, "w")) == NULL) {
    fprintf(stderr, "pvm_task_info(): unable to open `%s'.\n", file);
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  fprintf(output, 
          "((:ntasks . %i)\n (:tasks\n", 
          ntask);
  for(i = 0; i < ntask; i++) {
    fprintf(output,
            "   ((:tid . %i) (:ptid . %i) (:pvmd . %i) (:flags . %i)"
            " (:pid . %i)\n   (:executable . \"%s\"))\n",
            tasks[i].ti_tid, tasks[i].ti_ptid, tasks[i].ti_host,
            tasks[i].ti_flag, tasks[i].ti_pid, 
            (tasks[i].ti_a_out != NULL ? tasks[i].ti_a_out : ""));
  } /* for */
  fprintf(output, "))\n");
  fflush(output);
  if((i = ftell(output)) <= 0) {
    fprintf(stderr, "pvm_task_info(): error verifying `%s'.\n", file);
    fflush(stderr);
    return(-1);
  } /* if */
  fclose(output);
  return(i);

} /* pvm_task_info() */
