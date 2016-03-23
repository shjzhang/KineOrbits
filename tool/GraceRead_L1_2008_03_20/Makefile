#GRACE Level 1A/1B read software release on 2008-03-20

#Use -Aa flag when compiling on a HP platform
#CFLAGS = -Aa
CFLAGS = 
CC = gcc

C_SOURCE = \
  Bin2AsciiLevel1.c \
  gps1x2rnx.c \

INCLUDES = \
  atti_flt.h \
  atti_flt_prototypes.h \
  atti_flt_utils_prototypes.h \
  atti.h \
  atti_input_names.h \
  atti_prototypes.h \
  atti_reg_prototypes.h \
  atti_sim.h \
  atti_utils.h \
  Cal_inc.h \
  cstats.h \
  GRACEbj_iolib.h \
  GRACEdefs.h \
  GRACEfiletype.h \
  GRACEgpslib.h \
  GRACEiolib.h \
  GRACEio_prototypes.h \
  GRACEio_utils.h \
  GRACEprototype.h \
  GRACEreport.h \
  GRACEsyspath.h \
  GRACEtelemetry.h \
  TimeLib.h \

#     $Id: Makefile_readsw_bottom,v 1.2 2004/08/31 18:11:14 wib Exp $


C_OBJ_FILES =  $(C_SOURCE:%.c=obj/%.o)

C_EXECS =  $(C_SOURCE:%.c=%.e)

all: $(C_EXECS) LinkTime.o

LinkTime.o: FORCE
	./MakeLinkTime > LinkTime.c
	$(CC) -g $(CFLAGS) -c LinkTime.c

%.e: %.c lib/lib.a LinkTime.o
	$(CC) -g $(CFLAGS) $< LinkTime.o lib/lib.a -lm -o $@

lib/lib.a: FORCE
	cd lib; make


FORCE:

