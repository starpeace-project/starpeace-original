#
# Makefile for TESTFDI
#

CL = cl.exe

LINK = link.exe

CFLAGS = /nologo /ML /W3 /O2 /c 

LINK_FLAGS = /nologo /incremental:no


#
# Targets
#

all: testfdi.exe

clean: 
    -@if exist fdiutil.obj erase fdiutil.obj
    -@if exist testfdi.obj erase testfdi.obj
    -@if exist testfdi.exe erase testfdi.exe


#
# Rules
#

.c.obj:
    $(CL) $(CFLAGS) $*.c


#
# Linking
#

TESTFDI_OBJS = fdiutil.obj testfdi.obj ..\fdiz.lib

testfdi.exe: $(TESTFDI_OBJS)
    $(LINK) $(LINK_FLAGS) $(TESTFDI_OBJS) /out:testfdi.exe


#
# Object files
#

testfdi.obj: testfdi.c testfdi.h ..\fdi_int.h ..\types.h

fdiutil.obj: fdiutil.c testfdi.h ..\fdi_int.h ..\types.h



