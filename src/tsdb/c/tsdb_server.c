/*****************************************************************************\
|*        file: tsdb_server.c
|*      module: TSDB server (daemon) mode
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/param.h>
#include <errno.h>
#ifdef SIGTSTP
#include <sys/file.h>
#include <sys/ioctl.h>
#endif
#include <readline/readline.h>

#include "globals.h"
#include "tsdb.h"

extern int errno;
extern BOOL quit;
#ifdef SIGTSTP

void sigcld(void);
#endif

void tsdb_server_toplevel() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server_toplevel()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int server, new, child, n;
  struct sockaddr_in server_address, client_address;
#if defined(DEBUG) && defined(SERVER)
  struct hostent *host;
#endif

  if((server = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    exit(-1);
  } /* if */

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);
  server_address.sin_port = htons(tsdb.port);
  if(bind(server,
          (struct sockaddr *)&server_address, sizeof(server_address)) < 0) {
    exit(-1);
  } /* if */

  listen(server, TSDB_SERVER_QUEUE_LENGTH);

  while(TRUE) {
    n = sizeof(client_address);
    if((new = accept(server,
                     (struct sockaddr *)&client_address, &n)) < 0) {
#if defined(DEBUG) && defined(SERVER)
      fprintf(tsdb_debug_stream,
              "server_toplevel(): failed (invalid) accept(2).\n");
      fflush(tsdb_debug_stream);
#endif
      continue;
    } /* if */
#if defined(DEBUG) && defined(SERVER)
    if((host
        = gethostbyaddr(&client_address.sin_addr.s_addr, 4, AF_INET)) != NULL
       && host->h_name != NULL) {
      fprintf(tsdb_debug_stream,
              "server_toplevel(): connection from `%s'.\n", host->h_name);
    } /* if */
    else {
      fprintf(tsdb_debug_stream,
              "server_toplevel(): connection from unknown remote host.\n");
    } /* else */
    fflush(tsdb_debug_stream);
#endif    
    if((child = fork()) < 0) {
      exit(-1);
    } /* if */
    else if(child > 0) {
      close(new);
    } /* else */
    else {
      tsdb_server_instance(new);
    } /* else */
  } /* while */
} /* tsdb_server_toplevel() */

int tsdb_server_initialize() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server_initialize()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int i;

#ifdef SIGTTOU
  signal(SIGTTOU, SIG_IGN);
#endif
#ifdef SIGTTIN
  signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_IGN);
#endif

#ifndef NOFORK
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server.\n");
    return(-1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */

#ifdef SIGTSTP
  if(setpgrp(0, getpid()) == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to change process group.\n");
    return(-2);
  } /* if */
  if((i = open("/dev/tty", O_RDWR)) >= 0) {
    ioctl(i, TIOCNOTTY, (char *)NULL);
    close(i);
  } /* if */
#else
  if(setpgrp() == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to change process group.\n");
    return(-2);
  } /* if */
  signal(SIGHUP, SIG_IGN);
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server.\n");
    return(-1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */
#endif

  fclose(stdin);
  fclose(stdout);
  fclose(stderr);
  fclose(tsdb_default_stream);
  fclose(tsdb_error_stream);
#ifndef DEBUG
  for(i = 0; i < NOFILE; i++) {
    close(i);
  } /* for */
  errno = 0;
#endif

  chdir("/tmp");
  
  umask(0);
#endif

#ifdef SIGTSTP
  signal(SIGCLD, sigcld);
#else
  signal(SIGCLD, SIG_IGN);
#endif

  return(0);
} /* tsdb_server_initialize() */
#ifdef SIGTSTP

void sigcld(void) {

/*****************************************************************************\
|*        file: 
|*      module: sigcld()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigcld() is installed as the handler for SIGCLD signals to the TSDB server.
\*****************************************************************************/

  int pid;
  union wait status;

  while((pid = wait3(&status, WNOHANG, (struct rusage *)NULL)) > 0) {
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "sigcld(): [%d] relieved child # %d.\n", getpid(), pid);
    fflush(tsdb_debug_stream);
#endif
  } /* while */
} /* sigcld() */
#endif

void tsdb_server_instance(int socket) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server_instance()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int i;
  char *input = NULL;
  char host[512 + 1], prompt[80 + 1], *foo;
  int n_commands = 0;

  dup2(socket, 0);
  dup2(socket, 1);
  dup2(socket, 2);

  tsdb_default_stream = fdopen(socket, "w");
  tsdb_error_stream = fdopen(socket, "w");

  initialize_readline();
  rl_instream = fdopen(socket, "r");
  rl_outstream = fdopen(socket, "w");

  if(gethostname(&host[0], 512)) {
    perror("main(): gethostname(): ");
    host[0] = 0;
  } /* if */
  else {
    if((foo = strchr(host, '.')) != NULL) {
      *foo = 0;
    } /* if */
  } /* else */

  sprintf(prompt, "tsdb@%s (%d) # ", host, n_commands);

  while((!quit) && ((foo = readline(prompt)) != NULL)) {
    if(*foo) {
      if(input == NULL) {
        input = strdup(foo);
      } /* if */
      else {
        input = (char *)realloc(input, strlen(input) + strlen(foo) + 2);
        input = strcat(input, " ");
        input = strcat(input, foo);
      } /* else */
      free(foo);
      if(input != NULL && strchr(input, '.')) {
        tsdb_parse(input);
        add_history(input);
        free(input);
        input = (char *)NULL;
        sprintf(prompt, "tsdb@%s (%d) # ", host, ++n_commands);
      } /* if */
      else {
        sprintf(prompt, "> ", host, n_commands);
      } /* else */
    } /* if */
  } /* while */

  close(socket);
  exit(0);
} /* tsdb_server_instance() */
