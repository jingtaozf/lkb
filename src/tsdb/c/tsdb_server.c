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

void sigterm(void);
static _tsdb_server;

void tsdb_server() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int new, child, n;
  struct sockaddr_in server_address, client_address;
#if defined(DEBUG) && defined(SERVER)
  struct hostent *host;
#endif

  if((_tsdb_server = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    exit(-1);
  } /* if */

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);
  server_address.sin_port = htons(tsdb.port);
  if(bind(_tsdb_server,
          (struct sockaddr *)&server_address, sizeof(server_address)) < 0) {
    exit(-1);
  } /* if */

  listen(_tsdb_server, TSDB_SERVER_QUEUE_LENGTH);

  while(TRUE) {
    n = sizeof(client_address);
    if((new = accept(_tsdb_server,
                     (struct sockaddr *)&client_address, &n)) < 0) {
#if defined(DEBUG) && defined(SERVER)
      fprintf(tsdb_debug_stream,
              "server(): failed (invalid) accept(2); errno: %d.\n", errno);
      fflush(tsdb_debug_stream);
#endif
      continue;
    } /* if */
#if defined(DEBUG) && defined(SERVER)
    if((host
        = gethostbyaddr(&client_address.sin_addr.s_addr, 4, AF_INET)) != NULL
       && host->h_name != NULL) {
      fprintf(tsdb_debug_stream,
              "server(): connection from `%s'.\n", host->h_name);
    } /* if */
    else {
      fprintf(tsdb_debug_stream,
              "server(): connection from unknown remote host.\n");
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
      tsdb_server_child(new);
    } /* else */
  } /* while */
} /* tsdb_server() */

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
    return(1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */

#ifdef SIGTSTP
  if(setpgrp(0, getpid()) == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to change process group.\n");
    return(2);
  } /* if */
  if((i = open("/dev/tty", O_RDWR)) >= 0) {
    ioctl(i, TIOCNOTTY, (char *)NULL);
    close(i);
  } /* if */
#else
  if(setpgrp() == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to change process group.\n");
    return(2);
  } /* if */
  signal(SIGHUP, SIG_IGN);
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server.\n");
    return(1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */
#endif

#ifndef DEBUG
  fclose(stdin);
  fclose(stdout);
  fclose(stderr);
  fclose(tsdb_default_stream);
  fclose(tsdb_error_stream);
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
  signal(SIGTERM, sigterm);

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

void sigterm(void) {

/*****************************************************************************\
|*        file: 
|*      module: sigterm()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigterm() is installed as the handler for SIGTERM signals to the TSDB
|* server.
\*****************************************************************************/

  close(_tsdb_server);
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "sigterm(): [%d] going down on SIGTERM.\n", getpid());
    fflush(tsdb_debug_stream);
#endif
#ifdef DEBUG
  tsdb_close_debug(tsdb_debug_stream);
#endif
  exit(0);
} /* sigterm() */

void tsdb_server_child(int socket) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server_child()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int n;
  char *command = NULL;
  char host[512 + 1], prompt[80], input[1024], *foo;
  int n_commands = 0;

  tsdb_default_stream = fdopen(socket, "w");
  tsdb_error_stream = fdopen(socket, "w");

  if(gethostname(&host[0], 512)) {
    host[0] = 0;
  } /* if */
  else {
    if((foo = strchr(host, '.')) != NULL) {
      *foo = 0;
    } /* if */
  } /* else */


  while((!quit)) {
    sprintf(prompt, "tsdb@%s (%d) # ", host, n_commands);
    if(tsdb_socket_write(socket, &prompt[0], strlen(prompt)) == -1) {
      close(socket);
      exit(0);
    } /* if */
    
    if(tsdb_socket_readline(socket, &input[0], 1024) > 0) {
      if(command == NULL) {
        command = strdup(&input[0]);
      } /* if */
      else {
        command
          = (char *)realloc(command, strlen(command) + strlen(input) + 2);
        command = strcat(command, " ");
        command = strcat(command, input);
      } /* else */
      if(command != NULL && strchr(command, '.')) {
#if defined(DEBUG) && defined(SERVER)
        fprintf(tsdb_debug_stream,
                "server_child(): `%s'.\n", command);
        fflush(tsdb_debug_stream);
#endif
        tsdb_parse(command);
        free(command);
        command = (char *)NULL;
        sprintf(prompt, "tsdb@%s (%d) # ", host, ++n_commands);
      } /* if */
      else {
        sprintf(prompt, "> ", host, n_commands);
      } /* else */
    } /* if */
  } /* while */

  close(socket);
  exit(0);
} /* tsdb_server_child() */

int tsdb_socket_write(int socket, char *string, int n) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_socket_write()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int written, left;

  for(written = 0, left = n;
      left > 0;
      left -= written, string += written) {
    if((written = write(socket, string, left)) == -1) {
#if defined(DEBUG) && defined(SERVER)
      fprintf(tsdb_debug_stream,
              "socket_write(): write() error; errno: %d.\n", errno);
      fflush(tsdb_debug_stream);
#endif
      return(-1);
    } /* if */
  } /* for */
  return(n - left);
} /* tsdb_socket_write() */

int tsdb_socket_readline(int socket, char *string, int length) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_socket_readline()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* tsdb_socket_readline() reads from .socket. until (length - 1) characters
|* have been read, a newline has been seen or EOF is encountered; .string. is
|* always terminated by a 0 character (substituting for the closing newline if
|* one was found).
\*****************************************************************************/

  int i, n;
  char c;

  for(n = 0; n < (length - 1) && (i = read(socket, &c, 1)) == 1; n++) {
    if(c == '\n' || c == '\r') {
      (void)read(socket, &c, 1);
      string[n] = 0;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): `%s' (%d).\n", string, n + 1);
    fflush(tsdb_debug_stream);
#endif
      return(n + 1);
    } /* if */
    string[n] = c;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): `%c' (%d).\n", c, n + 1);
    fflush(tsdb_debug_stream);
#endif
  } /* for */
  if(i == -1) {
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "socket_readline(): read() error; errno: %d.\n", errno);
    fflush(tsdb_debug_stream);
#endif
    return(-1);
  } /* if */
  else if(n && !i) {
    string[n] = 0;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): `%s' (%d).\n", string, n + 1);
    fflush(tsdb_debug_stream);
#endif
    return(n + 1);
  } /* if */
  else {
    return(0);
  } /* else */
} /* tsdb_socket_readline() */
