/* pseudo files kept in memory */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "mfile.h"

struct MFILE *mopen()
{
  struct MFILE *f;

  f = malloc( sizeof( struct MFILE ) );
  assert( f != NULL );
  
  f->size = MFILE_BUFF_SIZE;
  f->ptr = f->buff = malloc( f->size );

  assert( f->buff != NULL );

  return f;
}

void mflush( struct MFILE *f )
{
  f->ptr = f->buff;
  f->ptr[ 0 ] = '\0';
}

int mlength( struct MFILE *f )
{
  return f->ptr - f->buff;
}

char *mstring(struct MFILE *file) {
  return(file->buff);
} /* mstring() */

void mclose( struct MFILE *f )
{
  free( f->buff );
  free( f );
}

int vmprintf( struct MFILE *f, char *format, va_list ap )
{
  int n;

  if(f->size - (f->ptr - f->buff) < MFILE_MAX_LINE)
    {
      n = (f->ptr - f->buff);
      f->size += MFILE_BUFF_SIZE;
      
      f->buff = realloc( f->buff, f->size );
      assert( f->buff != NULL );
      
      f->ptr = f->buff + n;
    }

  n = vsprintf( f->ptr, format, ap );

  if (n >= MFILE_MAX_LINE)
    {
      fprintf( stderr, "mprintf(): line too long for mprintf (%s)...\n", format );
      exit(1);
    }
  
  f->ptr += n;

  return n;
}

int mprintf( struct MFILE *f, char *format, ... )
{
  va_list ap;
  int n;

  va_start(ap, format);
  n = vmprintf( f, format, ap );
  va_end( ap );

  return n;
}


