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
#include <readline/readline.h>

#include "globals.h"
#include "tsdb.h"
#if defined(SUNOS)
#  include <fcntl.h>
#  include <sys/file.h>
#  include <sys/ioctl.h>
#endif

extern int errno;
#if defined(SUNOS)

void sigcld(int);
#endif

void sigterm_server(int);
void sigterm_client(int);
void sigsegv_server(int);
void sigsegv_client(int);

static int _server;
static int _client;
#if defined(DEBUG) && defined(SERVER)
static char *_query = (char *)NULL;
#endif

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

  int child, n;
  struct sockaddr_in server_address, client_address;
  struct linger linger;
#if defined(DEBUG) && defined(SERVER)
  struct hostent *host;
#endif

  if((_server = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    exit(-1);
  } /* if */

  n = TRUE;
  setsockopt(_server, SOL_SOCKET, SO_KEEPALIVE, (char *)&n, sizeof(n));
  linger.l_onoff = TRUE;
  linger.l_linger = 5;
  setsockopt(_server,
             SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);
  server_address.sin_port = htons(tsdb.port);
  if(bind(_server,
          (struct sockaddr *)&server_address, sizeof(server_address)) < 0) {
    exit(-1);
  } /* if */

  listen(_server, TSDB_SERVER_QUEUE_LENGTH);

  while(TRUE) {
    n = sizeof(client_address);
    if((_client = accept(_server,
                         (struct sockaddr *)&client_address, &n)) < 0) {
#if defined(DEBUG) && defined(SERVER)
      fprintf(tsdb_debug_stream,
              "server(): [%d] failed (invalid) accept(2); errno: %d.\n",
              getpid(), errno);
      fflush(tsdb_debug_stream);
#endif
      continue;
    } /* if */

#if defined(DEBUG) && defined(SERVER)
    if((host = gethostbyaddr((char *)&client_address.sin_addr.s_addr,
                             4, AF_INET)) != NULL
       && host->h_name != NULL) {
      fprintf(tsdb_debug_stream,
              "server(): [%d] connection from `%s'.\n",
              getpid(), host->h_name);
    } /* if */
    else {
      fprintf(tsdb_debug_stream,
              "server(): [%d] connection from unknown remote host.\n",
              getpid());
    } /* else */
    fflush(tsdb_debug_stream);
#endif    
    n = TRUE;
    setsockopt(_client, SOL_SOCKET, SO_KEEPALIVE, (char *)&n, sizeof(n));
    linger.l_onoff = TRUE;
    linger.l_linger = 5;
    setsockopt(_client, SOL_SOCKET, SO_LINGER,
               (char *)&linger, sizeof(linger));

    if((child = fork()) < 0) {
      exit(-1);
    } /* if */
    else if(child > 0) {
      close(_client);
    } /* else */
    else {
      signal(SIGTERM, sigterm_client);
      signal(SIGSEGV, sigsegv_client);
      signal(SIGBUS, sigsegv_client);
      tsdb_server_child(_client);
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

  for(i = 0; tsdb.relations[i] != NULL; i++) {
    if(tsdb_find_table(tsdb.relations[i]) == NULL) {
      fprintf(tsdb_error_stream,
              "server_initialize(): no data for relation `%s'.\n",
              tsdb.relations[i]->name);
      fflush(tsdb_error_stream);
      return(1);
    } /* if */
  } /* for */

#ifdef SIGTTOU
  signal(SIGTTOU, SIG_IGN);
#endif
#ifdef SIGTTIN
  signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_IGN);
#endif

#if !defined(NOFORK)
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server; errno: %d.\n",
            errno);
    return(1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */

#if defined(SUNOS)
  if(setpgrp(0, getpid()) == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): "
            "unable to change process group; errno: %d.\n",
            errno);
    return(2);
  } /* if */
  if((i = open("/dev/tty", O_RDWR)) >= 0) {
    ioctl(i, TIOCNOTTY, (char *)NULL);
    close(i);
  } /* if */
#else
  if(setsid() == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): "
            "unable to change process group; errno: %d.\n", errno);
    return(2);
  } /* if */
  signal(SIGHUP, SIG_IGN);
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server; errno: %d.\n",
            errno);
    return(1);
  } /* if */
  else if(i > 0) {
    exit(0);
  } /* if */
#endif /* #if defined(SUNOS) */

  fclose(stdin);
  fclose(stdout);
  fclose(stderr);
  fclose(tsdb_default_stream);
  fclose(tsdb_error_stream);

  for(i = 0; i < NOFILE; i++) {
#if defined(DEBUG)
    if(i == fileno(tsdb_debug_stream)) {
      continue;
    } /* if */
#endif
    close(i);
  } /* for */
  errno = 0;
#endif /* #if !defined(NOFORK) */

  chdir("/tmp");

  umask(0);

#if defined(SUNOS)
  signal(SIGCLD, sigcld);
#else
  signal(SIGCLD, SIG_IGN);
#endif

  signal(SIGTERM, sigterm_server);
  signal(SIGSEGV, sigsegv_server);
  signal(SIGBUS, sigsegv_server);

  return(0);

} /* tsdb_server_initialize() */
#if defined(SUNOS)

void sigcld(int signal) {

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

void sigterm_server(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: sigterm_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigterm() is installed as the handler for SIGTERM signals to the TSDB
|* server.
\*****************************************************************************/

  close(_server);
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "sigterm(): [%d] server going down on SIGTERM.\n",
            getpid());
    fflush(tsdb_debug_stream);
#endif
#if defined(DEBUG)
  tsdb_close_debug(tsdb_debug_stream);
#endif
  exit(0);
} /* sigterm_server() */

void sigterm_client(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: sigterm_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigterm() is installed as the handler for SIGTERM signals to the childs of
|* the TSDB server (clients, in a sense).
\*****************************************************************************/

  close(_client);
#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "sigterm(): [%d] client going down on SIGTERM.\n", getpid());
  fflush(tsdb_debug_stream);
#endif
  exit(0);
} /* sigterm_client() */

void sigsegv_server(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: sigsegv_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigsegv() is installed as the handler for SIGSEGV and SIGBUS signals to the
|* TSDB server.
\*****************************************************************************/

  close(_server);
#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "sigsegv(): [%d] server going down on %s.\n",
          getpid(), (signal == SIGSEGV ? "SIGSEGV" : "SIGBUS"));
  fflush(tsdb_debug_stream);
#endif
#if defined(DEBUG)
  tsdb_close_debug(tsdb_debug_stream);
#endif
  exit(1);
} /* sigsegv_server() */

void sigsegv_client(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: sigsegv_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sigterm() is installed as the handler for SIGSEGV and SIGBUS signals to
|* childs of the TSDB server (clients, in a sense).
\*****************************************************************************/

  close(_client);
#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "sigsegv(): [%d] client going down on %s",
          getpid(), (signal == SIGSEGV ? "SIGSEGV" : "SIGBUS"));
  if(_query != NULL) {
    fprintf(tsdb_debug_stream,
            ";\nsigsegv(): [%d] query: `%s'.\n", getpid(), _query);
  } /* if */
  else {
    fprintf(tsdb_debug_stream, ".\n");
  } /* else */
  fflush(tsdb_debug_stream);
#endif
  exit(0);
} /* sigsegv_client() */

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

  while(!(tsdb.status & TSDB_QUIT)) {
    fflush(tsdb_default_stream);
    fflush(tsdb_error_stream);
    sprintf(prompt, "tsdb@%s (%d) # ", host, n_commands);
    if(tsdb_socket_write(socket, &prompt[0], strlen(prompt)) == -1) {
      close(socket);
      exit(0);
    } /* if */
    
    if(tsdb_socket_readline(socket, &input[0], 1024) >= 0) {
      if(input[0]) {
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
                  "server_child(): [%d] `%s'.\n", getpid(), command);
          fflush(tsdb_debug_stream);
          if(_query == NULL) {
            _query = strdup(command);
          } /* if */
          else {
            free(_query);
            _query = strdup(command);
          } /* else */
#endif
          tsdb_parse(command);
          free(command);
          command = (char *)NULL;
          sprintf(prompt, "tsdb@%s (%d) # ", host, ++n_commands);
        } /* if */
        else {
          sprintf(prompt, "> ");
        } /* else */
      } /* if */
      else {
        tsdb.status |= TSDB_QUIT;
      } /* else */
    } /* if */
  } /* while */

#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "server_child(): [%d] exit.\n", getpid());
  fflush(tsdb_debug_stream);
#endif

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
              "socket_write(): [%d] write() error; errno: %d.\n",
              getpid(), errno);
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
    if(c == EOF) {
      return(-1);
    } /* if */
    if(c == '\r') {
      (void)read(socket, &c, 1);
    } /* if */
    if(c == '\n') {
      string[n] = 0;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): [%d] `%s' (%d).\n", getpid(), string, n + 1);
    fflush(tsdb_debug_stream);
#endif
      return(n + 1);
    } /* if */
    string[n] = c;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): [%d] `%c' (%d).\n", getpid(), c, n + 1);
    fflush(tsdb_debug_stream);
#endif
  } /* for */
  if(i == -1) {
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "socket_readline(): [%d] read() error; errno: %d.\n",
            getpid(), errno);
    fflush(tsdb_debug_stream);
#endif
    return(-1);
  } /* if */
  else if(n && !i) {
    string[n] = 0;
#if defined(DEBUG) && defined(SOCKET_READLINE)
    fprintf(tsdb_debug_stream,
            "socket_readline(): [%d] `%s' (%d).\n", getpid(), string, n + 1);
    fflush(tsdb_debug_stream);
#endif
    return(n + 1);
  } /* if */
  else {
    return(0);
  } /* else */
} /* tsdb_socket_readline() */

int tsdb_client() {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* 
\*****************************************************************************/

  int client;
  struct hostent *server;
  struct sockaddr_in server_address;

  if((server = gethostbyname(tsdb.server)) == NULL) {
    fprintf(tsdb_error_stream,
            "client(): unknown server host `%s'.\n", tsdb.server);
    fflush(tsdb_error_stream);
    return(-1);
  } /* if */

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = (unsigned long)server->h_addr;
  server_address.sin_port = htons(tsdb.port);

  if((client = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket()");
    return(-1);
  } /* if */
  if(connect(client,
             (struct sockaddr *)&server_address,
             sizeof(server_address)) < 0) {
    fprintf(tsdb_error_stream,
            "client(): connection refused by `%s'.\n", tsdb.server);
    fflush(tsdb_error_stream);
    return(-1);
  } /* if */
  
  write(client, "info relations", 15);
  sleep(20);
  return(0);

} /* tsdb_client() */
