/*
 * [incr tsdb()] --- Competence and Performance Profiling Environment
 * Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 */

/*****************************************************************************\
|* server side of (generic) C application program interface; the overall idea
|* is to _not_ pass information in Lisp syntax and avoid parsing on the client
|* side.  thus, protocol overhead is reduced and the client side greatly
|* simplified (well, as it stands, the client still phrases its reply in Lisp).
|*  --- naturally, the benefits come at a cost: the server now has to maintain
|* a distinction of protocols used in communicating with clients. 
|*
|* above all, we expect this to be a thin interface (few functions).
\*****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include "pvm3.h"
#include "itsdb.h"

int create_run(int, char *, int, char *, int, int, char *);
int process_item(int, int, char *, int, int, int, int, int);
int reconstruct_item(int, char *);
int complete_run(int, int, char *);

extern void pvm_flush(void);
extern int pvm_quit(void);

int create_run(int tid,
               char *data, int run_id, char *comment,
               int interactive, int protocol, char *custom) {

  int code, length, n;
  
  pvm_flush();

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, 
            "create_run(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  code = C_CREATE_RUN;
  if(pvm_pkint(&code, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  length = strlen(data);
  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if(pvm_pkstr(data) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&run_id, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  length = strlen(comment);
  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if(pvm_pkstr(comment) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&interactive, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&protocol, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  length = strlen(custom);
  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if(pvm_pkstr(custom) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if((n = pvm_send(tid, C_MESSAGE)) < 0 ) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to send message.\n");
    fflush(stderr);
    return(-1);
  } /* if */

  pvm_flush();

  return(0);

} /* create_run() */

int process_item(int tid, 
                 int i_id, char *i_input, int parse_id, int edges, 
                 int nanalyses, int derivationp, int interactive) {

  int code, length, n;
  
  pvm_flush();

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  code = C_PROCESS_ITEM;
  if(pvm_pkint(&code, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&i_id, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  length = strlen(i_input);
  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if(pvm_pkstr(i_input) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&parse_id, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&edges, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&nanalyses, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&derivationp, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&interactive, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if((n = pvm_send(tid, C_MESSAGE)) < 0 ) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to send message.\n");
    fflush(stderr);
    return(-1);
  } /* if */

  pvm_flush();

  return(n);

} /* process_item() */

int reconstruct_item(int tid, char *derivation) {

  return(0);

} /* reconstruct_item() */

int complete_run(int tid, int run_id, char *custom) {

  int code, n, length;

  pvm_flush();

  if(pvm_initsend(PvmDataDefault) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, 
            "complete_run(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  code = C_COMPLETE_RUN;
  if(pvm_pkint(&code, 1, 1) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  if(pvm_pkint(&run_id, 1, 1) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */

  length = strlen(custom);
  if(pvm_pkint(&length, 1, 1) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  if(pvm_pkstr(custom) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to write send buffer.\n");
    fflush(stderr);
    pvm_quit();
    return(-1);
  } /* if */
  
  if((n = pvm_send(tid, C_MESSAGE)) < 0 ) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to send message.\n");
    fflush(stderr);
    return(-1);
  } /* if */

  pvm_flush();

  return(0);

} /* complete_run() */
