SHELL = /bin/sh

SOLARIS_LIBS = \
   5lib/getloadavg.o -L../../../lib/solaris -lpvm3 -lsocket -lnsl -lkstat -lc

LINUX_CFLAGS = -fPIC

LINUX_X86_32_LIBS = \
   -L../../../lib/linux.x86.32 -lpvm3

LINUX_X86_64_LIBS = \
   -L../../../lib/linux.x86.64 -lpvm3

OSF_LIBS = \
    olib/getloadavg.o -L../../../lib/osf -lpvm3 -lc

MACOS_CFLAGS = -dynamic

MACOS_LIBS = \
  ../../../lib/macos.ppc.32/libpvm3.a -lSystemStubs -lc

MACOS_LD = ld -bundle /usr/lib/bundle1.o -flat_namespace \
  -undefined warning

CP = cp
CC = gcc -Wall -O6 -I../../../include
LD = ld -G -z defs 
AR = ar -cr
RANLIB = ranlib

linux.x86.32: 
	make clean;
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) acl.c
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) mfile.c
	gcc -shared -o ../../pvm/linux.x86.32/pvm.so acl.o \
	  $(LINUX_X86_32_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) gc.c
	gcc -shared -o ../linux.x86.32/gc.so gc.o
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) capi.c
	gcc -shared -o ../linux.x86.32/capi.so capi.o acl.o \
	  $(LINUX_X86_32_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) client.c
	gcc -shared -o ../../../lib/linux.x86.32/libitsdb.so \
	  client.o acl.o mfile.o $(LINUX_X86_32_LIBS)
	$(AR) ../../../lib/linux.x86.32/libitsdb.a \
	  client.o acl.o mfile.o
	$(RANLIB) ../../../lib/linux.x86.32/libitsdb.a

linux.x86.64: 
	make clean;
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) acl.c
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) mfile.c
	gcc -shared -o ../../pvm/linux.x86.64/pvm.so acl.o \
          $(LINUX_X86_64_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) gc.c
	gcc -shared -o ../linux.x86.64/gc.so gc.o
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) capi.c
	gcc -shared -o ../linux.x86.64/capi.so capi.o acl.o \
          $(LINUX_X86_64_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) client.c
	gcc -shared -o ../../../lib/linux.x86.64/libitsdb.so \
	  client.o acl.o mfile.o $(LINUX_X86_64_LIBS)
	$(AR) ../../../lib/linux.x86.64/libitsdb.a \
	  client.o acl.o mfile.o
	$(RANLIB) ../../../lib/linux.x86.64/libitsdb.a

solaris: acl.o gc.o capi.o client.o mfile.o ../../../include/itsdb.h
	$(LD) -o ../../pvm/solaris/pvm.so acl.o $(SOLARIS_LIBS)
	$(LD) -o ../solaris/capi.so capi.o acl.o $(SOLARIS_LIBS)
	$(LD) -o ../solaris/gc.so gc.o $(SOLARIS_LIBS)
	$(LD) -o ../../../lib/solaris/libitsdb.so \
	  client.o acl.o mfile.o $(SOLARIS_LIBS)
	$(AR) ../../../lib/solaris/libitsdb.a \
	  client.o acl.o mfile.o 5lib/getloadavg.o \
	  ../../../lib/solaris/libpvm3.a

osf: 
	make clean;
	$(CC) $(OSF_CFLAGS) -c $(CC_SWITCHES) acl.c
	ld -shared -error_unresolved -o ../../pvm/osf/pvm.so acl.o $(OSF_LIBS)
	$(CC) $(OSF_CFLAGS) -c $(CC_SWITCHES) gc.c
	ld -shared -error_unresolved -o ../osf/gc.so gc.o -lc
	$(CC) $(OSF_CFLAGS) -c $(CC_SWITCHES) capi.c
	ld -shared -error_unresolved -o ../osf/capi.so capi.o acl.o $(OSF_LIBS)
	$(CC) $(OSF_CFLAGS) -c $(CC_SWITCHES) client.c
	ld -shared -error_unresolved -o ../../../olib/libitsdb.so \
	  client.o acl.o $(OSF_LIBS)
	$(AR) ../../../olib/libitsdb.a \
          client.o acl.o olib/getloadavg.o olib/libpvm3.a

macos: 
	make clean;
	$(CC) $(MACOS_CFLAGS) -c acl.c
	$(CC) $(MACOS_CFLAGS) -c mfile.c
	$(MACOS_LD) -o ../../pvm/macos.ppc.32/pvm.dylib acl.o \
	  $(MACOS_LIBS)
	$(CC) $(MACOS_CFLAGS) -c gc.c
	$(MACOS_LD) -o ../macos.ppc.32/gc.dylib gc.o -lc
	$(CC) $(MACOS_CFLAGS) -c capi.c
	$(MACOS_LD) -o ../macos.ppc.32/capi.dylib capi.o acl.o \
	  $(MACOS_LIBS)
	$(CC) $(MACOS_CFLAGS) -c client.c
	$(MACOS_LD) -o ../../../lib/macos.ppc.32/libitsdb.dylib \
	  client.o acl.o mfile.o $(MACOS_LIBS)
	$(AR) ../../../lib/macos.ppc.32/libitsdb.a \
	  client.o acl.o mfile.o
	$(RANLIB) ../../../lib/macos.ppc.32/libitsdb.a

client: client.c ../../../include/itsdb.h acl.o mfile.o
	$(CC) -DMAIN -c client.c
	$(CC) -DMAIN -o client client.o acl.o mfile.o $(LINUX_X86_64_LIBS)

client.o: client.c
	rm -f client.o
	$(CC) -DDEBUG $(SOLARIS_CFLAGS) -c $(CC_SWITCHES) client.c

test: test.c ../../../include/itsdb.h
	gcc -o test -g -O0 -I../../../include \
	  -L../../../lib/linux.x86.64 -litsdb -lpvm3 test.c

make debug:
	make clean; make CFLAGS="-g -DDEBUG"

clean: 
	rm -f *.so *.o core a.out *~ so_locations

.c.o:
	$(CC) $(SOLARIS_CFLAGS) -c $(CC_SWITCHES) $<


