# -----------------------------------------------------------------------------
#        file: Makefile
#      module: top-level TSDB Makefile
#     version:
#  written by: andrew p. white, tom fettig & oe, dfki saarbruecken
# last update:
#  updated by:
# -----------------------------------------------------------------------------


#
# macros for portability
#
OBJ = o
DOT =
YACC_OUTPUT = y.tab
LEX_OUTPUT = lex.yy
CC = gcc $(CFLAGS) -O8 $(XCFLAGS)
YACC = bison -t -o $(YACC_OUTPUT).c
LEX = flex -8
MV = /bin/mv
CP = /bin/cp
LINUX_LIBS = -Lllib -lreadline -ltermcap -lregex
SUNOS_LIBS = -L4lib $(LINUX_LIBS) -liberty
HPUX_LIBS = -L7lib -lreadline -ltermcap -lregex -liberty
SOLARIS_LIBS = -L5lib $(LINUX_LIBS) -liberty -lsocket -lnsl 

#
# compiler flags
#
#	  -DFAST_INSERT \
#         -DJOIN_PATH \
#         -DMALLOC -DNOFORK \
#         -DCOMPRESSED_DATA \
#
XCFLAGS = -DDEBUG \
          -DALEP \
	  -DREAD_TABLE \
          -DJOIN \
          -DCOMMIT \
          -DPRINT_SELECTION \
          -DINITIALIZE \
          -DSERVER \
	  -DNOFREE \
          -DCOMPRESSED_DATA \
          -g -Iinclude -pipe

#
# dependendies
#
solaris : main.$(OBJ)$(DOT) libtsdb.a
	$(CC) -o tsdb main.$(OBJ)$(DOT) libtsdb.a $(LIBS) $(SOLARIS_LIBS)
#
#
sunos : main.$(OBJ)$(DOT) libtsdb.a
	$(CC) -o tsdb main.$(OBJ)$(DOT) libtsdb.a $(LIBS) $(SUNOS_LIBS)
#
#
linux : main.$(OBJ)$(DOT) libtsdb.a
	$(CC) -o tsdb main.$(OBJ)$(DOT) libtsdb.a $(LIBS) $(LINUX_LIBS)
#
#
hpux : main.$(OBJ)$(DOT) libtsdb.a
	$(CC) -o tsdb main.$(OBJ)$(DOT) libtsdb.a $(LIBS) $(HPUX_LIBS)
#
#
libtsdb.a : tsdb.$(OBJ)$(DOT) \
            parser.$(OBJ)$(DOT) scanner.$(OBJ)$(DOT) \
            tsdb_sql.$(OBJ) tsdb_io.$(OBJ) tsdb_utility.$(OBJ) \
            tsdb_debug.$(OBJ) $(DOT) tsdb_engine.$(OBJ) $(DOT) \
            tsdb_server.$(OBJ) $(DOT) tsdb_tools.$(OBJ) $(DOT) \
            tsdb_history.$(OBJ)
	ar cr libtsdb.a tsdb.$(OBJ)$(DOT) \
                        tsdb_sql.$(OBJ)$(DOT) \
                        tsdb_tools.$(OBJ)$(DOT) tsdb_history.$(OBJ)$(DOT) \
                        tsdb_utility.$(OBJ)$(DOT) \
                        tsdb_engine.$(OBJ)$(DOT) tsdb_server.$(OBJ)$(DOT) \
                        tsdb_io.$(OBJ)$(DOT) tsdb_debug.$(OBJ)$(DOT) \
                        scanner.$(OBJ)$(DOT) parser.$(OBJ)$(DOT)
	ranlib libtsdb.a
#
#
main.$(OBJ) : main.c tsdb.h globals.h errors.h
	$(CC) -c main.c
#
#
tsdb.$(OBJ) : tsdb.c tsdb.h globals.h errors.h
	$(CC) -c tsdb.c
#
#
tsdb_sql.$(OBJ) : tsdb_sql.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_sql.c
#
#
tsdb_utility.$(OBJ) : tsdb_utility.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_utility.c
#
tsdb_engine.$(OBJ) : tsdb_engine.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_engine.c
#

tsdb_io.$(OBJ) : tsdb_io.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_io.c
#
#
tsdb_server.$(OBJ) : tsdb_server.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_server.c
#
#
tsdb_debug.$(OBJ) : tsdb_debug.c tsdb.h globals.h errors.h
	$(CC) -c tsdb_debug.c
#
#
parser.$(OBJ) : parser.c $(DOT) tsdb.h $(DOT) globals.h errors.h
	$(CC) -c parser.c
#
#
parser.c : parser.y errors.h
	$(YACC) -d -v parser.y
	$(MV) $(YACC_OUTPUT).c parser.c
	$(MV) $(YACC_OUTPUT).h parser.h
#
#
scanner.$(OBJ) : scanner.c $(DOT) parser.h $(DOT) globals.h errors.h
	$(CC) -c scanner.c
#
#
scanner.c : scanner.lex tsdb.h errors.h
	$(LEX) -I scanner.lex
	$(MV) $(LEX_OUTPUT).c scanner.c
#
#
clean :
	$(RM) libtsdb.a tsdb
	$(RM) $(LEX_OUTPUT).*
	$(RM) $(YACC_OUTPUT).*
	$(RM) *.$(OBJ)
	$(RM) y.*
	$(RM) parser.c
	$(RM) parser.h
	$(RM) scanner.c
	$(RM) *.output
	$(RM) *~
#
#
tar:
	tar cf - main.c tsdb_sql.c \
          tsdb_utility.c tsdb_engine.c \
          tsdb_io.c tsdb_debug.c scanner.c parser.c tsdb.c \
          tsdb.h globals.h errors.h parser.h \
	  Makefile \
	| gzip -9 -c > tsdb.tgz

