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
#include <unistd.h>
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
static void _sigcld(int);
#endif

static void _sighup_server(int);
static void _sigterm_server(int);
static void _sigterm_client(int);
static void _sigsegv_server(int);
static void _sigsegv_client(int);
static void _sigalrm_client(int);

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
  signal(SIGHUP, _sighup_server);

  n = TRUE;
  setsockopt(_server, SOL_SOCKET, SO_KEEPALIVE, (char *)&n, sizeof(n));
  setsockopt(_server, SOL_SOCKET, SO_REUSEADDR, (char *)&n, sizeof(n));
  linger.l_onoff = TRUE;
  linger.l_linger = 2;
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
              "server(): [%d] failed (invalid) accept(2) [%d].\n",
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
              "server(): [%d] connect from `%s' "
              "(now %d client(s)).\n",
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
    setsockopt(_client, SOL_SOCKET, SO_REUSEADDR, (char *)&n, sizeof(n));
    linger.l_onoff = TRUE;
    linger.l_linger = 2;
    setsockopt(_client, SOL_SOCKET, SO_LINGER,
               (char *)&linger, sizeof(linger));

#if defined(ALEP)
    (void)tsdb_server_child(_client);
    close(_client);
    tsdb.status = (tsdb.status & (TSDB_LOCK | TSDB_SERVER_MODE));
#else
    if((child = fork()) < 0) {
      exit(TSDB_OS_ERROR);
    } /* if */
    else if(child > 0) {
      close(_client);
      _n_clients++;
    } /* else */
    else {
      signal(SIGTERM, _sigterm_client);
      signal(SIGSEGV, _sigsegv_client);
      signal(SIGBUS, _sigsegv_client);
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

  int i, n, foo;
  struct sockaddr_in server_address;
  struct linger linger;

  if((foo = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to create server socket.\n");
    fflush(tsdb_error_stream);
    close(foo);
    exit(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  n = TRUE;
  setsockopt(foo, SOL_SOCKET, SO_REUSEADDR, (char *)&n, sizeof(n));
  linger.l_onoff = FALSE;
  setsockopt(foo,
             SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);
  server_address.sin_port = htons(tsdb.port);
  if(bind(foo,
          (struct sockaddr *)&server_address, sizeof(server_address)) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to bind to port %d.\n", tsdb.port);
    fflush(tsdb_error_stream);
    close(foo);
    exit(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  /*
   * _fix_me_ 
   * try to see whether there actually is a tsdb(1) server on that port (it
   * might be a white mouse or worse |:-); set alarm() and make an attempt to
   * obtain tsdb(1) server status information.
   *                                              (31-jul-96 -- oe)
   */
  close(foo);

  if(tsdb.relations == NULL) {
    exit(TSDB_UNKNOWN_ERROR);
  } /* if */

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
            "server_initialize(): unable to fork(2) server [%d].\n",
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
            "unable to change process group [%d].\n",
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
            "unable to change process group [%d].\n", errno);
    return(TSDB_OS_ERROR);
  } /* if */
  signal(SIGHUP, SIG_IGN);
  if((i = fork()) < 0) {
    fprintf(tsdb_error_stream,
            "server_initialize(): unable to fork(2) server [%d].\n",
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
  signal(SIGCLD, _sigcld);
#else
  signal(SIGCLD, SIG_IGN);
#endif

  signal(SIGTERM, _sigterm_server);
  signal(SIGSEGV, _sigsegv_server);
  signal(SIGBUS, _sigsegv_server);

  return(TSDB_OK);

} /* tsdb_server_initialize() */
#if defined(SUNOS) || defined(LINUX)

static void _sigcld(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigcld()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sigcld() is installed as the handler for SIGCLD signals to the TSDB server.
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
} /* _sigcld() */
#endif

static void _sighup_server(int foo) {

/*****************************************************************************\
|*        file: 
|*      module: _sighup_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sighup() is installed as the handler for SIGHUP signals to the TSDB
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
    signal(SIGHUP, _sighup_server);
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
} /* _sighup_server() */

static void _sigterm_server(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigterm_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sigterm() is installed as the handler for SIGTERM signals to the TSDB
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
} /* _sigterm_server() */

static void _sigterm_client(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigterm_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sigterm() is installed as the handler for SIGTERM signals to the childs of
|* the TSDB server (clients, in a sense).
\*****************************************************************************/

  close(_client);
#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "sigterm(): [%d] client going down on SIGTERM.\n", getpid());
  fflush(tsdb_debug_stream);
#endif
  exit(TSDB_OK);
} /* _sigterm_client() */

static void _sigsegv_server(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigsegv_server()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 2-aug-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* _sigsegv() is installed as the handler for SIGSEGV and SIGBUS signals to the
|* TSDB server.
\*****************************************************************************/

  char foo[MAXPATHLEN + 1];

  close(_server);
#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "sigsegv(): [%d] server going down on %s.\n",
          getpid(), (signal == SIGSEGV ? "SIGSEGV" : "SIGBUS"));

#if defined(SUNOS)
  if(getwd(&foo[0]) != NULL) {
#else
  if(getcwd(&foo[0], MAXPATHLEN + 1) != NULL) {
#endif
    fprintf(tsdb_debug_stream,
            "sigsegv(): [%d] core(5) dumped to `%s/core'.\n",
            getpid(), foo);
 
  } /* if */
  fflush(tsdb_debug_stream);
  fclose(tsdb_debug_stream);
#endif
  sleep(1);
  abort();

} /* _sigsegv_server() */

static void _sigsegv_client(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigsegv_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sigsegv() is installed as the handler for SIGSEGV and SIGBUS signals to
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
} /* _sigsegv_client() */

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
      fclose(tsdb_default_stream);
      fclose(tsdb_error_stream);
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
          tsdb.command++;
          free(command);
          command = (char *)NULL;
          if(tsdb.status & TSDB_TSDB_CLIENT) {
            sprintf(prompt, "%c",
              (status ? TSDB_CLIENT_CONNECT_ERROR : TSDB_CLIENT_CONNECT_OK));
          } /* if */
          else {
            sprintf(prompt, "tsdb@%s (%d) # ", host, tsdb.command);
          } /* else */
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
    } /* if */
  } /* while */

#if defined(DEBUG) && defined(SERVER)
  fprintf(tsdb_debug_stream,
          "server_child(): [%d] exit.\n", getpid());
  fflush(tsdb_debug_stream);
#endif

  fclose(tsdb_default_stream);
  fclose(tsdb_error_stream);
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
              "socket_write(): [%d] write() error [%d].\n",
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
            "socket_readline(): [%d] read() error [%d].\n",
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

static void _sigalrm_client(int signal) {

/*****************************************************************************\
|*        file: 
|*      module: _sigalrm_client()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|* _sigalrm() is installed as the handler for SIGALRM signals to a client to
|* the TSDB server to guarantee (proper) termination even if something wedges.
\*****************************************************************************/

  close(_client);
#if defined(DEBUG)
  fprintf(tsdb_debug_stream,
          "sigalrm(): client going down on SIGALRM.\n");
  fflush(tsdb_debug_stream);
#endif
  exit(TSDB_SERVER_CONNECTION_ERROR);
} /* _sigalrm_client() */

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

  int status, n;
  struct hostent *server;
  struct sockaddr_in server_address;
  struct linger linger;
  char *command, *foo, c;

  if(query == NULL 
     && !(tsdb.status & (TSDB_QUIT | TSDB_HANGUP | TSDB_STATUS))) {
    fprintf(tsdb_error_stream,
            "alep_client(): missing `-query' option or client action.\n");
    fflush(tsdb_error_stream);
    return(TSDB_NO_QUERY_ERROR);
  } /* if */

  if((server = gethostbyname(tsdb.server)) == NULL) {
    fprintf(tsdb_error_stream,
            "alep_client(): unknown server host `%s'.\n", tsdb.server);
    fflush(tsdb_error_stream);
    return(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */

  bzero((char *)&server_address, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_addr.s_addr = *(unsigned long int *)server->h_addr;
  server_address.sin_port = htons(tsdb.port);

  if((_client = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket()");
    return(TSDB_SERVER_CONNECTION_ERROR);
  } /* if */
  
  setsockopt(_client, SOL_SOCKET, SO_KEEPALIVE, (char *)&n, sizeof(n));
  setsockopt(_client, SOL_SOCKET, SO_REUSEADDR, (char *)&n, sizeof(n));
  linger.l_onoff = TRUE;
  linger.l_linger = 2;
  setsockopt(_client, SOL_SOCKET, SO_LINGER, (char *)&linger, sizeof(linger));

  if(connect(_client,
             (struct sockaddr *)&server_address,
             sizeof(server_address)) < 0) {
    if(!(tsdb.status & TSDB_STATUS)) {
      fprintf(tsdb_error_stream,
              "alep_client(): connection refused by `%s' (port %d).\n",
              tsdb.server, tsdb.port);
      fflush(tsdb_error_stream);
    } /* if */
    close(_client);
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
                 | (tsdb.status & TSDB_IMPLICIT_COMMIT)
                 | (tsdb.status & TSDB_UNIQUELY_PROJECT)
                 | (tsdb.status & TSDB_TX_OUTPUT)));

  signal(SIGALRM, _sigalrm_client);
  alarm(TSDB_SERVER_CONNECT_TIMEOUT);
  if(tsdb_socket_write(_client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(_client);
    tsdb_free(command);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  if((status = tsdb_client_clear_stream(_client, TRUE))) {
    tsdb_client_close(_client);
    tsdb_free(command);
    return(status);
  } /* if */

  (void)sprintf(command, "set ofs \"%s\".\n", tsdb.ofs);
  if(tsdb_socket_write(_client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(_client);
    tsdb_free(command);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  if((status = tsdb_client_clear_stream(_client, TRUE))) {
    tsdb_client_close(_client);
    tsdb_free(command);
    return(status);
  } /* if */
  alarm(0);
  signal(SIGALRM, SIG_IGN);

  status = TSDB_OK;
  if(tsdb.status & (TSDB_QUIT | TSDB_HANGUP)) {
    (void)sprintf(command, 
                  "%s.\n",
                  ((tsdb.status & TSDB_QUIT) ? "shutdown" : "hangup"));
    if(tsdb_socket_write(_client, command) != strlen(command)) {
      fprintf(tsdb_error_stream,
              "alep_client(): incomplete write on server socket.\n");
      fflush(tsdb_error_stream);
      status = TSDB_SOCKET_IO_ERROR;
    } /* if */
    close(_client);
    tsdb_free(command);
    return(status);
  } /* if */

  if(tsdb.home != NULL
     && (foo = tsdb_obtain_server_home(_client)) != NULL) {
    if(strcmp(tsdb.home, foo)) {
      fprintf(tsdb_error_stream,
              "alep_client(): database mismatch (`%s').\n",
              foo);
      fflush(tsdb_error_stream);
      tsdb_client_close(_client);
      tsdb_free(command);
      return(TSDB_SERVER_HOME_MISMATCH);
    } /* if */
  } /* if */
  else if(foo == NULL) {
    
  } /* if */

  if(tsdb.status & TSDB_STATUS) {
    status = tsdb_obtain_server_status(_client);
    tsdb_client_close(_client);
    tsdb_free(command);
    return(((status & TSDB_LOCK) ? TSDB_LOCK_ERROR : TSDB_OK));
  } /* if */

  for(foo = query; *foo; foo++);
  for(foo--; isspace(*foo); *foo = (char)0, foo--);
  if(*foo == '.') {
    *foo = (char)0;
  } /* if */
  for(foo = query; *foo && isspace(*foo); foo++);
  if(!strncmp(foo, "insert", 6)
     || !strncmp(foo, "set", 3)
     || !strncmp(foo, "commit", 6)
     || !strncmp(foo, "hangup", 6)
     || !strncmp(foo, "shutdown", 8)) {
    (void)sprintf(command, "%s.\n", query);
  } /* if */
  else {
    (void)sprintf(command, "%s > \"%s\".\n",
                  query, 
                  (tsdb.output != NULL ? tsdb.output : "/dev/null"));
  } /* else */
  if(tsdb_socket_write(_client, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(_client);
    tsdb_free(command);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  if((status = tsdb_client_clear_stream(_client, FALSE))) {
    tsdb_client_close(_client);
    tsdb_free(command);
    return(status);
  } /* if */

  tsdb_client_close(_client);
  tsdb_free(command);
  return(TSDB_OK);

} /* tsdb_alep_client() */
#endif

int tsdb_obtain_server_status(int client) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_obtain_server_status()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int status;
  char buffer[1024 + 1], *foo, *bar;

  (void)sprintf(&buffer[0], "info status.\n");
  if(tsdb_socket_write(client, &buffer[0]) != strlen(&buffer[0])) {
    fprintf(tsdb_error_stream,
            "obtain_server_status(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */

  if(tsdb_socket_readline(client, &buffer[0], 1024) >= 0
     && buffer[0]) {
    if((status = tsdb_client_clear_stream(client, TRUE))) {
      return(status);
    } /* if */
    if((foo = strrchr(&buffer[0], ':')) != NULL) {
      foo++;
      if(*foo) {
        if((status = strtol(foo, &bar, 0))
           || bar != foo) {
          return(status);
        } /* if */
      } /* if */
    } /* if */
  } /* if */

  fprintf(tsdb_error_stream,
          "obtain_server_status(): incomplete or invalid data from server.\n");
  fflush(tsdb_error_stream);
  return(TSDB_SOCKET_IO_ERROR);

} /* tsdb_obtain_server_status() */

char *tsdb_obtain_server_home(int client) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_obtain_server_home()
|*     version: 
|*  written by: oe, coli saarbruecken
|* last update: 
|*  updated by: 
|*****************************************************************************|
|*
\*****************************************************************************/

  int status;
  char buffer[1024 + 1], *foo, *bar;

  (void)sprintf(&buffer[0], "info home.\n");
  if(tsdb_socket_write(client, &buffer[0]) != strlen(&buffer[0])) {
    fprintf(tsdb_error_stream,
            "obtain_server_status(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    tsdb.errno = TSDB_SOCKET_IO_ERROR;
    return((char *)NULL);
  } /* if */

  if(tsdb_socket_readline(client, &buffer[0], 1024) >= 0
     && buffer[0]) {
    if((status = tsdb_client_clear_stream(client, TRUE))) {
      tsdb.errno = status;
      return((char *)NULL);
    } /* if */
    if((foo = strrchr(&buffer[0], '`')) != NULL) {
      foo++;
      if(*foo) {
        if((bar = strrchr(foo, '\'')) != NULL) {
          *bar = (char)0;
#if defined(DEBUG) && defined(OBTAIN_SERVER_HOME)
          fprintf(tsdb_debug_stream,
                  "obtain_server_home(): got `%s'.\n", foo);
          fflush(tsdb_debug_stream);
#endif
          tsdb.errno = TSDB_OK;
          return(foo);
        } /* if */
      } /* if */
    } /* if */
  } /* if */

  fprintf(tsdb_error_stream,
          "obtain_server_home(): incomplete or invalid data from server.\n");
  fflush(tsdb_error_stream);
  tsdb.errno = TSDB_SOCKET_IO_ERROR;
  return((char *)NULL);

} /* tsdb_obtain_server_home() */

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

int tsdb_client_close(int stream) {

/*****************************************************************************\
|*        file: 
|*      module: tsdb_client_close()
|*     version: 
|*  written by: oe, dfki saarbruecken
|* last update: 29-jul-96
|*  updated by: oe, coli saarbruecken
|*****************************************************************************|
|* 
\*****************************************************************************/

  char command[] = "quit.\n";

  if(tsdb_socket_write(stream, command) != strlen(command)) {
    fprintf(tsdb_error_stream,
            "alep_client_close(): incomplete write on server socket.\n");
    fflush(tsdb_error_stream);
    close(stream);
    return(TSDB_SOCKET_IO_ERROR);
  } /* if */
  close(stream);
  return(TSDB_OK);

} /* tsdb_client_close() */
