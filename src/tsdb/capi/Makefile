SHELL = /bin/sh

SOLARIS_LIBS = \
   5lib/getloadavg.o -L5lib -lpvm3 -lsocket -lnsl -lkstat -lc

LINUX_LIBS = \
   llib/getloadavg.o -Lllib -lpvm3 -lc

OSF_LIBS = \
    olib/getloadavg.o -Lolib -lpvm3 -lc

CP = cp
CC = gcc -Wall -g -fpic
LD = ld -G -z defs 
AR = ar -cr
RANLIB = ranlib

solaris: acl.o gc.o capi.o client.o itsdb.h
	$(LD) -o ../../pvm/solaris/pvm.so acl.o $(SOLARIS_LIBS)
	$(LD) -o ../solaris/capi.so capi.o acl.o $(SOLARIS_LIBS)
	$(LD) -o ../solaris/gc.so gc.o
	$(LD) -o ../../../lib/solaris/libitsdb.so \
	  client.o acl.o $(SOLARIS_LIBS)
	$(AR) ../../../lib/solaris/libitsdb.a \
	  client.o acl.o 5lib/getloadavg.o 5lib/libpvm3.a

linux: 
	make clean;
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) acl.c
	$(LD) -o ../../pvm/linux/pvm.so acl.o $(LINUX_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) gc.c
	$(LD) -o ../linux/gc.so gc.o
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) capi.c
	$(LD) -o ../linux/capi.so capi.o acl.o $(LINUX_LIBS)
	$(CC) $(LINUX_CFLAGS) -c $(CC_SWITCHES) client.c
	$(LD) -o ../../../lib/linux/libitsdb.so \
	  client.o acl.o $(LINUX_LIBS)
	$(AR) ../../../lib/linux/libitsdb.a \
	  client.o acl.o llib/getloadavg.o
	$(RANLIB) ../../../lib/linux/libitsdb.a

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

client: client.c itsdb.h
	gcc -Wall -g -DMAIN -c client.c
	gcc -DMAIN -o client client.o acl.o $(SOLARIS_LIBS)

client.o: client.c
	rm -f client.o
	$(CC) -DDEBUG $(SOLARIS_CFLAGS) -c $(CC_SWITCHES) client.c

make debug:
	make clean; make CFLAGS="-g -DDEBUG"

clean: 
	rm -f *.so *.o core a.out *~ so_locations

.c.o:
	$(CC) $(SOLARIS_CFLAGS) -c $(CC_SWITCHES) $<


