#
# Makefile for TESTFCI
#

CL = cl.exe

LINK = link.exe

CFLAGS = /nologo /ML /W3 /O2 /c 

LINK_FLAGS = /nologo /incremental:no


#
# Targets
#

all: testfci.exe

clean: 
    -@if exist fciutil.obj erase fciutil.obj
    -@if exist testfci.obj erase testfci.obj
    -@if exist testfci.exe erase testfci.exe


#
# Rules
#

.c.obj:
    $(CL) $(CFLAGS) $*.c


#
# Linking
#

TESTFCI_OBJS = fciutil.obj testfci.obj ..\fciz.lib

testfci.exe: $(TESTFCI_OBJS)
    $(LINK) $(LINK_FLAGS) $(TESTFCI_OBJS) /out:testfci.exe


#
# Object files
#

testfci.obj: testfci.c testfci.h ..\fci_int.h ..\types.h

fciutil.obj: fciutil.c testfci.h ..\fci_int.h ..\types.h
