/*
 * [incr tsdb()] --- Competence and Performance Profiling Environment
 * Copyright (c) 1996 -- 2005 Ulrich Callmeier (ulrich@callmeier.de)
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

#ifndef _MFILE_H_
#define _MFILE_H_

#define MFILE_BUFF_SIZE 16384
#define MFILE_MAX_LINE 1024

struct MFILE
{
  int size;
  char *buff;
  char *ptr;
};

struct MFILE *mopen();
void mclose( struct MFILE *f );
void mflush( struct MFILE *f );
int mprintf( struct MFILE *f, char *format, ... );
int vmprintf( struct MFILE *f, char *format, va_list ap );
int mlength( struct MFILE *f );
char *mstring(struct MFILE *);
#endif
