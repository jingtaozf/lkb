/*
 * [incr tsdb()] --- Competence and Performance Profiling Environment
 *
 * Copyright (c) 1996 -- 2005 Stephan Oepen (oe@csli.stanford.edu)
 * Copyright (c) 2006 -- 2008 Stephan Oepen (oe@ifi.uio.no)
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

#define TASK_FAIL 42
#define REMOTE_SHUTDOWN 43

#define LISP_MESSAGE 50
#define C_MESSAGE 51

#define C_CREATE_RUN 0
#define C_PROCESS_ITEM 1
#define C_COMPLETE_RUN 2
#define C_RECONSTRUCT_ITEM 3

#define capi_putc(char) (capi_printf("%c", char) == 1 ? char : EOF)
extern int capi_printf(const char *format, ...);
extern int capi_register(int (*)(const char *, int, const char *,
                                 int, int, const char *),
                         int (*)(int, const char *, int, int, 
                                 int, int, int, const char *),
                         int (*)(const char *),
                         int (*)(int, const char *));
extern int slave(void);
extern int client_open_item_summary();
extern int client_send_item_summary();
