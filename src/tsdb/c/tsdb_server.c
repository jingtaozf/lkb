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
#include "errors.h"

#if defined(SUNOS)
#  include <fcntl.h>
#  include <sys/file.h>
#  include <sys/ioctl.h>
#endif

extern int errno;
#if defined(SUNOS) || defined(LINUX)

void sigcld(int);
#endif

void sighup_server(int);
void sigterm_server(int);
void sigterm_client(int);
void sigsegv_server(int);
void sigsegv_client(int);

static int _server;
static int _n_clients;
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

  int child, n, status;
  struct sockaddr_in server_address, client_address;
  struct linger linger;
#if defined(DEBUG) && defined(SERVER)
  struct hostent *host;
#endif

  if((_server = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    exit(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  _n_clients = 0;
  signal(SIGPIPE, SIG_IGN);
  signal(SIGHUP, sighup_server);

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
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "server(): unable to bind to port %d.\n", tsdb.port);
    fflush(tsdb_debug_stream);
#endif
    exit(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  listen(_server, TSDB_SERVER_QUEUE_LENGTH);

#ifdef ALEP
  tsdb.command = 0;
#endif

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
              "server(): [%d] connection from `%s' "
              "(now %d active clients).\n",
              getpid(), host->h_name, (_n_clients + 1));
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

#if defined(ALEP)
    status = tsdb.status;
    (void)tsdb_server_child(_client);
    close(_client);
    tsdb.status = status;
    tsdb.status &= ~TSDB_QUIT;
    tsdb.status &= ~TSDB_HANGUP;
#else
    if((child = fork()) < 0) {
      exit(TSDB_OS_ERROR);
    } /* if */
    else if(child > 0) {
      close(_client);
      _n_clients++;
    } /* else */
    else {
      signal(SIGTERM, sigterm_client);
      signal(SIGSEGV, sigsegv_client);
      signal(SIGBUS, sigsegv_client);
      status = tsdb_server_child(_client);
      close(_client);
      kill(getppid(), SIGHUP);
      exit(status);
    } /* else */
#endif
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
    exit(TSDB_OK);
  } /* if */

#if defined(SUNOS)
  if(setpgrp(0, getpid()) == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): "
            "unable to change process group; errno: %d.\n",
            errno);
    return(TSDB_OS_ERROR);
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
    return(TSDB_OS_ERROR);
  } /* if */
  signal(SIGHUP, SIG_IGN);
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server; errno: %d.\n",
            errno);
    return(TSDB_OS_ERROR);
  } /* if */
  else if(i > 0) {
    exit(TSDB_OK);
  } /* if */
#endif

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
#endif

  chdir("/tmp");

  umask(0);

#if defined(SUNOS) || defined(LINUX)
  signal(SIGCLD, sigcld);
#else
  signal(SIGCLD, SIG_IGN);
#endif

  signal(SIGTERM, sigterm_server);
  signal(SIGSEGV, sigsegv_server);
  signal(SIGBUS, sigsegv_server);

  return(TSDB_OK);

} /* tsdb_server_initialize() */
#if defined(SUNOS) || defined(LINUX)

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

void sighup_server(int foo) {

/*****************************************************************************\
|*        file: 
|*      module: sighup_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* sighup() is installed as the handler for SIGHUP signals to the TSDB
|* server.
\*****************************************************************************/

  if(_n_clients > 0) {
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "sighup(): [%d] unregister client for SIGHUP "
            "(now %d client(s)).\n",
            getpid(), (_n_clients - 1));
    fflush(tsdb_debug_stream);
    _n_clients--;
#endif
    signal(SIGHUP, sighup_server);
  } /* if */
  else {
    close(_server);
#if defined(DEBUG) && defined(SERVER)
    fprintf(tsdb_debug_stream,
            "sighup(): [%d] server going down on SIGHUP.\n",
            getpid());
    fflush(tsdb_debug_stream);
#endif
#if defined(DEBUG)
    tsdb_close_debug(tsdb_debug_stream);
#endif
    exit(TSDB_OK);
  } /* else */
} /* sighup_server() */

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
  exit(TSDB_OK);
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
  exit(TSDB_OK);
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
  exit(TSDB_OS_ERROR);
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
|* sigsegv() is installed as the handler for SIGSEGV and SIGBUS signals to
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
  exit(TSDB_OS_ERROR);
} /* sigsegv_client() */

int tsdb_server_child(int socket) {

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
  int status;

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

  status = TSDB_OK;
#ifndef ALEP
  tsdb.command = 0;
#endif

  while(!(tsdb.status & TSDB_QUIT)) {
    fflush(tsdb_default_stream);
    fflush(tsdb_error_stream);
    if(tsdb.status & TSDB_TSDB_CLIENT) {
      sprintf(prompt, "%c",
              (status ? TSDB_CLIENT_CONNECT_ERROR : TSDB_CLIENT_CONNECT_OK));
    } /* if */
    else {
      sprintf(prompt, "tsdb@%s (%d) # ", host, tsdb.command);
    } /* else */

    if(tsdb_socket_write(socket, &prompt[0]) == -1) {
#if defined(DEBUG) && defined(SERVER)
      fprintf(tsdb_debug_stream,
              "server_child(): [%d] emergency exit.\n", getpid());
      fflush(tsdb_debug_stream);
#endif      
      return(TSDB_SOCKET_IO_ERROR);
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
          status = tsdb_parse(command, (FILE *)NULL);
          free(command);
          command = (char *)NULL;
          if(tsdb.status & TSDB_TSDB_CLIENT) {
            sprintf(prompt, "%c",
              (status ? TSDB_CLIENT_CONNECT_ERROR : TSDB_CLIENT_CONNECT_OK));
          } /* if */
          else {
            sprintf(prompt, "tsdb@%s (%d) # ", host, tsdb.command);
          } /* else */
          tsdb.command++;
        } /* if */
        else {
          if(!(tsdb.status & TSDB_TSDB_CLIENT)) {
            sprintf(prompt, "> ");
          } /* if */
          else {
            *prompt = (char)0;
          } /* else */
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

  return(TSDB_OK);

} /* tsdb_server_child() */

void tsdb_server_shutdown(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_server_shutdown()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

#ifdef ALEP
  kill(getpid(), signal);
#else
  kill(getppid(), signal);
#endif
  if(signal == SIGHUP) {
    sleep(2);
#ifdef ALEP
    kill(getpid(), signal);
#else
    kill(getppid(), signal);
#endif
  } /* if */

} /* tsdb_server_shutdown() */

int tsdb_socket_write(int socket, char *string) {

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

  int written, left, n;

  for(written = 0, left = n = strlen(string);
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
#ifdef ALEP

int tsdb_alep_client(char *query) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_alep_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 26-jul-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* 
\*****************************************************************************/

  int client, status, n;
  struct hostent *server;
  struct sockaddr_in server_address;
  struct linger linger;
  char *command, *foo, c;

  if(query == NULL 
     && !(tsdb.status & (TSDB_QUIT | TSDB_HANGUP))) {
    return(TSDB_OK);
  } /* if */

  if((server = gethostbyname(tsdb.server)) == NULL) {
    fprintf(tsdb_error_stream,
            "client(): unknown server host `%s'.\n", tsdb.server);
    fflush(tsdb_error_stream);
    return(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = *(unsigned long int *)server->h_addr;
  server_address.sin_port = htons(tsdb.port);

  if((client = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket()");
    return(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */
  
  linger.l_onoff = TRUE;
  linger.l_linger = 30;
  setsockopt(client, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));

  if(connect(client,
             (struct sockaddr *)&server_address,
             sizeof(server_address)) < 0) {
    fprintf(tsdb_error_stream,
            "alep_client(): connection refused by `%s' (port %d).\n",
            tsdb.server, tsdb.port);
    fflush(tsdb_error_stream);
    close(client);
    return(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  if(query != NULL) {
    command = (char *)malloc((strlen(query) < 4096 ? 4096 : strlen(query)));
  } /* if */
  else {
    command = (char *)malloc(42);
  } /* else */
  (void)sprintf(command, "set status %d.\n",
                (TSDB_TSDB_CLIENT
                 | (tsdb.status & TSDB_UNIQUELY_PROJECT)
                 | (tsdb.status & TSDB_TX_OUTPUT)));
  if(tsdb_socket_write(client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(client);
    tsdb_free(command);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  if((status = tsdb_client_clear_stream(client, TRUE))) {
    close(client);
    tsdb_free(command);
    return(status);
  } /* if */

  if(tsdb.status & (TSDB_QUIT | TSDB_HANGUP)) {
    (void)sprintf(command, 
                  "%s.\n",
                  ((tsdb.status & TSDB_QUIT) ? "shutdown" : "hangup"));
    if(tsdb_socket_write(client, command) != strlen(command)) {
      fprintf(tsdb_error_stream,
              "alep_client(): incomplete write on server socket.\n");
      fflush(tsdb_error_stream);
      close(client);
      tsdb_free(command);
      return(1);
    } /* if */
    status = tsdb_client_clear_stream(client, FALSE);
    close(client);
    tsdb_free(command);
    return(status);
  } /* if */

  for(foo = query; *foo; foo++);
  for(foo--; isspace(*foo); *foo = (char)0, foo--);
  if(*foo == '.') {
    *foo = (char)0;
  } /* if */
  for(foo = query; *foo && isspace(*foo); foo++);
  if(!strncmp(foo, "do", 2)
     || !strncmp(foo, "insert", 6)
     || !strncmp(foo, "commit", 6)
     || !strncmp(foo, "hangup", 6)
     || !strncmp(foo, "shutdown", 8)) {
    (void)sprintf(command, "%s.\n", query);
  } /* if */
  else {
    (void)sprintf(command, "%s > \"%s\".\n",
                  query, 
                  (tsdb.output != NULL ? tsdb.output : "tsdb2alep"));
  } /* else */
  if(tsdb_socket_write(client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(client);
    tsdb_free(command);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  if((status = tsdb_client_clear_stream(client, FALSE))) {
    close(client);
    tsdb_free(command);
    return(status);
  } /* if */

  (void)sprintf(command, "quit.\n");
  if(tsdb_socket_write(client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  close(client);
  tsdb_free(command);
  return(TSDB_OK);

} /* tsdb_alep_client() */
#endif

int tsdb_client_clear_stream(int stream, BOOL sink) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_client_clear_stream()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 29-jul-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* 
\*****************************************************************************/

  int n, position;
  char c, *foo;
  static int size = 4096;
  static char *buffer = (char *)NULL;

  if(buffer == NULL) {
    buffer = (char *)malloc(size + 1);
  } /* if */

  for(position = 0;
      ((n = read(stream, &c, 1) > 0)
       && c != TSDB_CLIENT_CONNECT_OK
       && c != TSDB_CLIENT_CONNECT_ERROR);
      buffer[position++] = c) {
    if(position >= size) {
      size *= 2;
      buffer = (char *)realloc(buffer, size + 1);
    } /* if */
  } /* for */
  buffer[position] = (char)0;

  if(n == 1 && c == TSDB_CLIENT_CONNECT_OK) {
#ifdef DEBUG
    if(position && !sink) {
      if((foo = strrchr(buffer, '\n')) != NULL) {
        *foo = (char)0;
      } /* if */
      fprintf(tsdb_debug_stream,
              "client_clear_stream(): `%s'.\n", buffer);
      fflush(tsdb_debug_stream);
    } /* if */
    else {
      fprintf(tsdb_debug_stream,
              "client_clear_stream(): success.\n");
      fflush(tsdb_debug_stream);
    } /* else */
#endif
    return(TSDB_OK);
  } /* if */
  if(n == 1 && c == TSDB_CLIENT_CONNECT_ERROR) {
    if(position && !sink) {
      fprintf(tsdb_error_stream,
              buffer);
      fflush(tsdb_error_stream);
    } /* if */
    else {
      fprintf(tsdb_error_stream,
              "client_clear_stream(): unknown error from server.\n");
      fflush(tsdb_error_stream);
    } /* else */
    return(TSDB_UNSPECIFIC_SERVER_ERROR);
  } /* if */

  fprintf(tsdb_error_stream,
          "client_clear_stream(): read() error; failed to clear stream.\n");
  if(position && !sink) {
    if((foo = strrchr(buffer, '\n')) != NULL) {
      *foo = (char)0;
    } /* if */
    fprintf(tsdb_error_stream,
            "client_clear_stream(): `%s'.\n",
            buffer);
    fflush(tsdb_error_stream);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */

} /* tsdb_client_clear_stream() */
