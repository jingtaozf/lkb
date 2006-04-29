MRSDIR=$$HOME/lkb
ECLDIR=/usr/local
ECL=$(ECLDIR)/bin/ecl

CPPFLAGS=-I. -I$(ECLDIR)/lib/ecl/h
LDFLAGS=-L$(ECLDIR)/lib/ecl -Wl,-R$(ECLDIR)/lib/ecl \
	-lecl -lstdc++

CFLAGS=-g -Wall
CXXFLAGS=-g -Wall

fspp: fspp.o libfspp.a 
	gcc -o $@ $(LDFLAGS) fspp.o libfspp.a

libfspp.a:
	( \
	echo "(load \"$(MRSDIR)/src/general/loadup\")"; \
	echo "(setq lib-dir (make-pathname :directory '(:relative)))"; \
	echo "(mk::ecl-initialize-libbuild :module \"fspp\")"; \
	echo "(compile-system \"preprocessor\" :force t)"; \
	echo "(mk::ecl-finalize-libbuild :module \"fspp\")"; \
	) | $(ECL)


clean:
	rm *.o

allclean:
	rm *.o *.a