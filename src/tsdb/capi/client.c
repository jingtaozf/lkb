#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <sys/utsname.h>

#include "mfile.h"

#include "itsdb.h"
#include "pvm3.h"

extern double load_average(void);
extern int pvm_quit(void);
extern int pvm_file_transmit(int, int, char *);
extern int pvm_mfile_transmit(int, int, struct MFILE *);

int slave(void);
int client_register(int, int);

struct MFILE *client_create_run(void);
struct MFILE *client_process_item(void);
struct MFILE *client_reconstruct_item(void);
struct MFILE *client_complete_run(void);

int redirect_stdout(FILE *);
char *make_tmp_file(char *);
char *current_user(void);
char *current_time(void);
char *current_host(void);
char *current_os(void);
void copy_file(char *, FILE *);

static int (*callbacks[4])() = { 
  (int (*)())NULL,
  (int (*)())NULL,
  (int (*)())NULL,
  (int (*)())NULL
}; /* callbacks */
static struct MFILE *client_output;
static int self = 0;

int capi_register(int (*create_run)(char *, int, char *, int, char *),
                  int (*process_item)(int, char *, int, int, int, int, int),
                  int (*reconstruct_item)(char *),
                  int (*complete_run)(int, char *)) {

  int master;

  if((self = pvm_mytid()) < 0) {
    pvm_perror("capi_register()");
    fprintf(stderr, 
            "capi_register(): unable to initialize virtual machine.\n");
    fflush(stderr);
    pvm_exit();
    return(-1);
  } /* if */

  if((master = pvm_parent()) > 0 && master != PvmNoParent) {
    /*
     * for children of .parent. request notification when .parent. terminates
     */
    pvm_notify(PvmTaskExit, TASK_FAIL, 1, &master);
    if(client_register(self, master) < 0) {
      pvm_exit();
      return(-1);
    } /* if */
  } /* if */
  pvm_setopt(PvmRoute, PvmRouteDirect);

  callbacks[C_CREATE_RUN] = create_run;
  callbacks[C_PROCESS_ITEM] = process_item;
  callbacks[C_RECONSTRUCT_ITEM] = reconstruct_item;
  callbacks[C_COMPLETE_RUN] = complete_run;

  return(0);

} /* capi_register() */

int slave(void) {

  int id, size, tag, remote;
  int code;
  struct timeval timeout;
  struct MFILE *file;

  timeout.tv_sec = 1;
  timeout.tv_usec = 0;

  while(1) {
    if((id = pvm_trecv(-1, -1, &timeout)) < 0) {
      pvm_perror("slave()");
      fprintf(stderr, "slave(): error receiving message.\n");
      fflush(stderr);
      pvm_quit();
      return(-1);
    } /* if */
    if(id) {
      if(pvm_bufinfo(id, &size, &tag, &remote)) {
        pvm_perror("slave()");
        fprintf(stderr, "slave(): unable to read receive buffer.\n");
        fflush(stderr);
        pvm_quit();
        return(-1);
      } /* if */
      if(tag == TASK_FAIL) {
        return(0);
      } /* if */
      if(tag != C_MESSAGE) {
        fprintf(stderr, "slave(): unknown message type `%i'.\n", tag);
        fflush(stderr);
        pvm_quit();
        return(-1);
      } /* if */
      
      if(pvm_upkint(&code, 1, 1) < 0) {
        pvm_perror("slave()");
        fprintf(stderr, "slave(): unable to read receive buffer.\n");
        fflush(stderr);
        pvm_quit();
        return(-1);
      } /* if */

      switch(code) {
      case C_CREATE_RUN:
        file = client_create_run();
        break;
      case C_PROCESS_ITEM:
        file = client_process_item();
        break;
      case C_COMPLETE_RUN:
        file = client_complete_run();
        break;
      case C_RECONSTRUCT_ITEM:
        file = client_reconstruct_item();
        break;
      default:
        file = (struct MFILE *)NULL;
      } /* switch */

      if(file != NULL ) {
        pvm_mfile_transmit(remote, LISP_MESSAGE, file);
	mclose(file);
      } /* if */
    } /* if */
  } /* for */

} /* slave() */

int client_register(int self, int master) {

  char *file;
  FILE *output;

  file = make_tmp_file(".pvm.cio");
  if((output = fopen(file, "w")) != NULL) {
    fprintf(output,
            "(:register %d :raw)",
            self);
    fclose(output);
    if(file != NULL && !access(file, F_OK|R_OK)) {
      return(pvm_file_transmit(master, LISP_MESSAGE, file));
    } /* if */
  } /* if */
  return(-1);

} /* client_register() */

struct MFILE *client_create_run() {

  int run_id, interactive, length;
  char *data, *comment, *custom;

  if(pvm_upkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return 0;
  } /* if */
  data = (char *)malloc(length + 1);
  if(pvm_upkstr(data) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data);
    return 0;
  } /* if */

  if(pvm_upkint(&run_id, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data);
    return 0;
  } /* if */

  if(pvm_upkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data);
    return 0;
  } /* if */
  comment = (char *)malloc(length + 1);
  if(pvm_upkstr(comment) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data); free(comment);
    return 0;
  } /* if */

  if(pvm_upkint(&interactive, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data); free(comment);
    return 0;
  } /* if */

  if(pvm_upkint(&length, 1, 1) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data);
    return 0;
  } /* if */
  custom = (char *)malloc(length + 1);
  if(pvm_upkstr(custom) < 0) {
    pvm_perror("create_run()");
    fprintf(stderr, "create_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(data); free(custom);
    return 0;
  } /* if */

  if((client_output = mopen()) == NULL) {
    fprintf(stderr, "create_run(): unable to create output mfile.\n");
    free(data); free(comment); free(custom);
    return ((struct MFILE *)NULL);
  } /* if */

  capi_printf("\n (:return :create-run (\n"
              "  (:data . \"%s\") (:run-id . %d) (:interactive . %d)\n"
              "  (:comment . \"%s\") (:start . \"%s\")\n"
              "  (:custom . \"%s\")\n"
              "  (:user . \"%s\")  (:host . \"%s\") (:os . \"%s\")\n",
              data, run_id, interactive,
              comment, current_time(),
              custom,
              current_user(), current_host(), current_os());
  if(callbacks[C_CREATE_RUN] != NULL) {
    if(callbacks[C_CREATE_RUN](data, run_id, comment, interactive) < 0) {
      fprintf(stderr, "create_run(): erroneous client return value.\n");
    } /* if */
  } /* if */
  capi_printf("))");
  free(data); free(comment); free(custom);
  return client_output;

} /* client_create_run() */

struct MFILE *client_process_item() {

  int i_id, parse_id, edges, exhaustive, derivationp, interactive, length;
  char *i_input;

  if(pvm_upkint(&i_id, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return 0;
  } /* if */

  if(pvm_upkint(&length, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return 0;
  } /* if */
  i_input = (char *)malloc(length + 1);
  if(pvm_upkstr(i_input) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */

  if(pvm_upkint(&parse_id, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */

  if(pvm_upkint(&edges, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */

  if(pvm_upkint(&exhaustive, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */
  
  if(pvm_upkint(&derivationp, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */
  
  if(pvm_upkint(&interactive, 1, 1) < 0) {
    pvm_perror("process_item()");
    fprintf(stderr, "process_item(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(i_input);
    return 0;
  } /* if */
  
  if((client_output = mopen()) == NULL) {
    fprintf(stderr, "process_item(): unable to create output mfile.\n");
    free(i_input);
    return 0;
  } /* if */
  
  capi_printf("\n (:return :process-item (\n"
              "  (:i-id . %d) (:parse-id . %d)\n"
              "  (:edges . %d) (:exhaustive . %d)"
              "  (:derivationp . %d) (:interactive . %d)\n"
              "  (:i-load . %f)\n",
              i_id, parse_id,
              edges, exhaustive, 
              derivationp, interactive,
              load_average());
  if(callbacks[C_PROCESS_ITEM] != NULL) {
    if(callbacks[C_PROCESS_ITEM](i_id, i_input, parse_id, edges, 
                                 exhaustive, derivationp, interactive) < 0) {
      fprintf(stderr, "process_item(): erroneous client return value.\n");
    } /* if */
  } /* if */
  capi_printf("  (:a-load . %f)))", load_average());
  free(i_input);
  return(client_output);
  
} /* client_process_item() */

struct MFILE *client_reconstruct_item() {

  return((struct MFILE *)NULL);

} /* client_reconstruct_item() */

struct MFILE *client_complete_run() {

  int run_id, length;
  char *custom;
  
  if(pvm_upkint(&run_id, 1, 1) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return 0;
  } /* if */

  if(pvm_upkint(&length, 1, 1) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    return 0;
  } /* if */
  custom = (char *)malloc(length + 1);
  if(pvm_upkstr(custom) < 0) {
    pvm_perror("complete_run()");
    fprintf(stderr, "complete_run(): unable to read receive buffer.\n");
    fflush(stderr);
    pvm_quit();
    free(custom);
    return 0;
  } /* if */
  
  if((client_output = mopen()) == NULL) {
    fprintf(stderr, "complete_run(): unable to create output mfile.\n");
    free(custom);
    return 0;
  } /* if */
  capi_printf("\n (:return :complete-run (\n"
              "  (:run-id . %d)\n"
              "  (:custom . \"%s\")\n",
              run_id, custom);
  if(callbacks[C_COMPLETE_RUN] != NULL) {
    if(callbacks[C_COMPLETE_RUN](run_id) < 0) {
      fprintf(stderr, "complete_run(): erroneous client return value.\n");
    } /* if */
  } /* if */
  capi_printf("  (:end . \"%s\")))", current_time());
  free(custom);
  return(client_output);

} /* client_complete_run() */

int client_open_item_summary() {

  if((client_output = mopen()) == NULL) {
    return -1;
  } /* if */
  if (pvm_recvinfo("itsdb", 0, PvmMboxDefault) < 0) {
    mclose(client_output);
    return -1;
  } /* if */
  capi_printf("\n (:account :item-summary (\n"
              "  (:time . \"%s\")\n"
              "  (:user . \"%s\")  (:host . \"%s\") (:os . \"%s\")\n",
              current_time(),
              current_user(), current_host(), current_os());
  return 0;
} /* client_open_item_summary() */

int client_send_item_summary() {

  int buffer, remote;

  if(client_output == NULL) return -1;

  if ((buffer = pvm_recvinfo("itsdb", 0, PvmMboxDefault)) < 0) {
    mclose(client_output);
    return -1;
  } /* if */

  if(pvm_setrbuf(buffer) < 0) {
    mclose(client_output);
    return -1;
  } /* if */
  if(pvm_upkint(&remote, 1, 1) < 0) {
    mclose(client_output);
    return -1;
  } /* if */

  if(client_output != NULL &&
     mlength(client_output) > 0) {
    capi_printf("  (:a-load . %f)))", load_average());
    pvm_mfile_transmit(remote, LISP_MESSAGE, client_output);
    mclose(client_output);
  } /* if */
  return 0;

} /* client_send_item_summary() */

int capi_printf(char *format, ...) {
  va_list ap;
  int n;

  va_start(ap, format);
  n = vmprintf(client_output, format, ap);
  va_end(ap);
  return(n);

} /* capi_printf() */

int pvm_mfile_transmit(int tid, int tag, struct MFILE *file) {

  int length, n;
  double load;

  length = mlength(file);
  
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
  
  if(pvm_pkstr(file->buff) < 0) {
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

  return(n);

} /* pvm_mfile_transmit() */

int redirect_stdout(FILE *stream) {

  static int one = -1;
  
  if(stream != NULL) {
    fflush(stream);
    one = dup(fileno(stdout));
    close(fileno(stdout));
    dup(fileno(stream));
    return(1);
  } /* if */
  else if(one >= 0) {
    fflush(stdout);
    close(fileno(stdout));
    dup(one);
    close(one);
    one = -1;
    return(1);
  } /* else */
  return(0);

} /* redirect_stdout() */

char *make_tmp_file(char *prefix) {

  static char *buffer = (char *)NULL;
  static int id = 42;

  if(buffer == NULL) {
    if((buffer = (char *)malloc(MAXNAMLEN + 1)) == NULL) {
      return("/tmp/.pvm.cio.anonymous.unknown");
    } /* if */
  } /* if */

  sprintf(buffer, "/tmp/%s.%s.%d.%d", prefix, current_user(), self, id++);
  return(buffer);

} /* make_tmp_file() */

char *current_user(void) {

  static char *user = (char *)NULL;
  extern char *cuserid(char *);

  if(user != NULL) {
    return(user);
  } /* if */
  if((user = cuserid((char *)NULL)) != NULL) {
    user = strdup(user);
    return(user);
  } /* if */
  return("anonymous");

} /* current_user() */

char *current_time() {

  time_t foo;
  struct tm *now;
  static char *result = (char *)NULL;
  
  if(result == NULL) {
    if((result = (char *)malloc(42)) == NULL) return("now");
  } /* if */

  if((foo = time(&foo)) > 0 && (now = localtime(&foo)) != NULL) {
    sprintf(result, "%d-%d-%d %d:%02d:%02d",
            now->tm_mday, now->tm_mon + 1, now->tm_year + 1900,
            now->tm_hour, now->tm_min, now->tm_sec);
  } /* if */
  else {
    sprintf(result, "now");
  } /* else */
  return(result);

} /* current_time() */

char *current_host() {

  static char name[1024];
  
  if(gethostname(name, 1024) == -1) {
    strcpy(name, "unknown");
  } /* if */
  return(name);

} /* current_host() */

char *current_os() {

  static char name[1024];
  struct utsname info;
 
  if(uname(&info) == -1) {
    strcpy(name, "no os");
  } /* if */
  else {
    sprintf(name,
            "%s %s %s %s %s", 
            info.sysname, info.nodename, info.release,
            info.version, info.machine);
  } /* else */
  return(name);

} /* current_os() */

void copy_file(char *file, FILE *output) {

  FILE *input;
  int c;

  if((input = fopen(file, "r")) != NULL) {
    while((c = fgetc(input)) != EOF) {
      fputc(c, output);
    } /* while */
    fputc('\n', output);
    fflush(output);
    fclose(input);
  } /* if */
  
} /* copy_file() */

#ifdef MAIN
int main() {

  
  if(!capi_register(NULL, NULL, NULL, NULL)) {
    return(slave());
  } /* if */
  return(-1);

} /* main() */
#endif

