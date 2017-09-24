/*--------------------------------------------------------------------------
THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED
TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
PARTICULAR PURPOSE.

Copyright (C) 1993 - 2000.  Microsoft Corporation.  All rights reserved.]

MODULE:   simple.c

PURPOSE:  Implements the body of the service.
          The default behavior is to open a
          named pipe, \\.\pipe\simple, and read
          from it.  It the modifies the data and
          writes it back to the pipe.

FUNCTIONS:
          ServiceStart(DWORD dwArgc, LPTSTR *lpszArgv);
          ServiceStop( );

COMMENTS: The functions implemented in simple.c are
          prototyped in service.h
--------------------------------------------------------------------------*/


#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <tchar.h>
#include "service.h"
#include "SPCheck.h"
// this event is signalled when the
// service should end
//
HANDLE  hServerStopEvent = NULL;


//
//  FUNCTION: ServiceStart
//
//  PURPOSE: Actual code of the service that does the work.
//
//  PARAMETERS:
//    dwArgc   - number of command line arguments
//    lpszArgv - array of command line arguments
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    The default behavior is to open a
//    named pipe, \\.\pipe\simple, and read
//    from it.  It the modifies the data and
//    writes it back to the pipe.  The service
//    stops when hServerStopEvent is signalled
//
VOID ServiceStart (DWORD dwArgc, LPTSTR *lpszArgv)
{
   HANDLE                  hPipe = INVALID_HANDLE_VALUE;
   HANDLE                  hEvents[2] = {NULL, NULL};
   PSECURITY_DESCRIPTOR    pSD = NULL;
   SECURITY_ATTRIBUTES     sa;
   char tmpBuffer[512];
   unsigned long CheckTime = 1000;



   ///////////////////////////////////////////////////
   //
   // Service initialization
   //

   // report the status to the service control manager.
   //
   if (!ReportStatusToSCMgr(
                           SERVICE_START_PENDING, // service state
                           NO_ERROR,              // exit code
                           3000))                 // wait hint
      goto cleanup;

   // create the event object. The control handler function signals
   // this event when it receives the "stop" control code.
   //
   hServerStopEvent = CreateEvent(
                                 NULL,    // no security attributes
                                 TRUE,    // manual reset event
                                 FALSE,   // not-signalled
                                 NULL);   // no name

   if ( hServerStopEvent == NULL)
      goto cleanup;

   hEvents[0] = hServerStopEvent;

   // report the status to the service control manager.
   //
   if (!ReportStatusToSCMgr(
                           SERVICE_START_PENDING, // service state
                           NO_ERROR,              // exit code
                           3000))                 // wait hint
      goto cleanup;

   // create the event object object use in overlapped i/o
   //
   hEvents[1] = CreateEvent(
                           NULL,    // no security attributes
                           TRUE,    // manual reset event
                           FALSE,   // not-signalled
                           NULL);   // no name

   if ( hEvents[1] == NULL)
      goto cleanup;

   // report the status to the service control manager.
   //
   if (!ReportStatusToSCMgr(
                           SERVICE_START_PENDING, // service state
                           NO_ERROR,              // exit code
                           3000))                 // wait hint
      goto cleanup;

   // create a security descriptor that allows anyone to write to
   //  the pipe...
   //
   pSD = (PSECURITY_DESCRIPTOR) malloc( SECURITY_DESCRIPTOR_MIN_LENGTH );

   if (pSD == NULL)
      goto cleanup;

   if (!InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION))
      goto cleanup;

   // add a NULL disc. ACL to the security descriptor.
   //
   if (!SetSecurityDescriptorDacl(pSD, TRUE, (PACL) NULL, FALSE))
      goto cleanup;

   sa.nLength = sizeof(sa);
   sa.lpSecurityDescriptor = pSD;
   sa.bInheritHandle = TRUE;


   // report the status to the service control manager.
   //
   if (!ReportStatusToSCMgr(
                           SERVICE_START_PENDING, // service state
                           NO_ERROR,              // exit code
                           3000))                 // wait hint
      goto cleanup;


   // report the status to the service control manager.
   //
   if (!ReportStatusToSCMgr(
                           SERVICE_RUNNING,       // service state
                           NO_ERROR,              // exit code
                           0))                    // wait hint
      goto cleanup;

   //
   // End of initialization
   //
   ////////////////////////////////////////////////////////

   ////////////////////////////////////////////////////////
   //
   // Service is now running, perform work until shutdown
   //
   if (GetPrivateProfileString("INIT", "CheckTime", "", tmpBuffer, 512, "SPClean.INI")!=0)
   {
		CheckTime = atol(tmpBuffer);
   };

   while (WaitForSingleObject(hServerStopEvent, CheckTime)==WAIT_TIMEOUT)
   {
	    OpAbort = FALSE;
		StarPeaceCheck();
   }

   cleanup:

   if (hPipe != INVALID_HANDLE_VALUE )
      CloseHandle(hPipe);

   if (hServerStopEvent)
      CloseHandle(hServerStopEvent);

   if (hEvents[1]) // overlapped i/o event
      CloseHandle(hEvents[1]);

   if ( pSD )
      free( pSD );

}


//
//  FUNCTION: ServiceStop
//
//  PURPOSE: Stops the service
//
//  PARAMETERS:
//    none
//
//  RETURN VALUE:
//    none
//
//  COMMENTS:
//    If a ServiceStop procedure is going to
//    take longer than 3 seconds to execute,
//    it should spawn a thread to execute the
//    stop code, and return.  Otherwise, the
//    ServiceControlManager will believe that
//    the service has stopped responding.
//
VOID ServiceStop()
{
	OpAbort = TRUE;
	if ( hServerStopEvent )
		SetEvent(hServerStopEvent);
}
