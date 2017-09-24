#ifndef CABDLL
#define CABDLL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#include <dos.h>
#include <sys/stat.h>

#include "windows.h"
#include "fdi.h"

  typedef int (WINAPI *PDECOMPRESSNOTIFIER) ( void* UserId, FDINOTIFICATIONTYPE fdint, PFDINOTIFICATION pfdin);

  // fdi... 
	
	BOOL WINAPI DecompressCabFiles( char* CabFile, char* Dest, void* UserData, PDECOMPRESSNOTIFIER Notifier ); 

#endif