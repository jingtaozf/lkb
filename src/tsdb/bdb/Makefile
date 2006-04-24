SHELL = /bin/sh

SOLARIS_LIBS = \
  -lsocket -lnsl -lkstat -lc

LINUX_CFLAGS = -fPIC

LINUX_X86_32_LIBS = -ldb

LINUX_X86_64_LIBS = -ldb

OSF_LIBS = 

MACOS_CFLAGS = -dynamic

MACOS_LIBS =   ../../../lib/macos.ppc.32/libpvm3.a -lSystemStubs -lc

MACOS_LD = ld -bundle /usr/lib/bundle1.o -flat_namespace \
  -undefined warning

CP = cp
CC = gcc -Wall -g -O6 -I../../../include
LD = ld -G -z defs 
AR = ar -cr
RANLIB = ranlib

solaris: 
	make clean;
	$(CC) $(SOLARIS_CFLAGS) -c $(CC_SWITCHES) bdb.c
	ld -G -z defs -o ../../tsdb/solaris/bdb.so bdb.o \

linux.x86.32: 
	make clean;
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) bdb.c
	gcc -shared -o ../../tsdb/linux.x86.32/bdb.so bdb.o \
	  $(LINUX_X86_32_LIBS)

linux.x86.64: 
	make clean;
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) bdb.c
	gcc -shared -o ../../tsdb/linux.x86.64/bdb.so bdb.o \
          $(LINUX_X86_64_LIBS)

osf: 
	make clean;
	$(CC) $(OSF_CFLAGS) -c $(CC_SWITCHES) bdb.c
	ld -shared -error_unresolved -o ../../tsdb/osf/bdb.so bdb.o $(OSF_LIBS)

macos: 
	make clean;
	$(CC) $(MACOS_CFLAGS) -c bdb.c
	$(MACOS_LD) -o ../../pvm/macos.ppc.32/bdb.dylib bdb.o \
	  $(MACOS_LIBS)

clean: 
	rm -f *.so *.o core a.out *~ so_locations



