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
