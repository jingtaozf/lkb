#define TASK_FAIL 42
#define REMOTE_SHUTDOWN 43

#define LISP_MESSAGE 50
#define C_MESSAGE 51

#define C_CREATE_RUN 0
#define C_PROCESS_ITEM 1
#define C_COMPLETE_RUN 2
#define C_RECONSTRUCT_ITEM 3

#define capi_putc(char) (capi_printf("%c", char) == 1 ? char : EOF)
extern int capi_printf(char *format, ...);
extern int capi_register(int (*)(char *, int, char *, int, int, char *),
                         int (*)(int, char *, int, int, 
                                 int, int, int),
                         int (*)(char *),
                         int (*)(int, char *));
extern int slave(void);
