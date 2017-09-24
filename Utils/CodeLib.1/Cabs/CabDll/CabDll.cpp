
#include "windows.h"
#include "CabDll.h"


  BOOL APIENTRY DllMain(HANDLE hInst, DWORD ul_reason_being_called, LPVOID lpReserved)
    {
      return 1;
    }

  // Decompression stuff


  PDECOMPRESSNOTIFIER UserNotifier;
	char								DestDir[MAX_PATH];
	int									CurrentHandle;
	int									CurrentSize; 
	int									CopiedSoFar;	
	void*							  TheUserId;

  
  FNALLOC(mem_alloc)
   {
     return malloc(cb);
   }


  FNFREE(mem_free)
   {
     free(pv);
   }


  int FAR DIAMONDAPI file_open(char FAR *pszFile, int oflag, int pmode)   
   {
     return _open(pszFile, oflag, pmode);
   }


  UINT FAR DIAMONDAPI file_read (int hf, void FAR *pv, UINT cb)
   {
	   return _read(hf, pv, cb);
   }


  UINT FAR DIAMONDAPI file_write (int hf, void FAR *pv, UINT cb)
   {
     int res = 0;
     if ((hf == CurrentHandle) && (UserNotifier))
	    {
				FDINOTIFICATION fdin;
			  CopiedSoFar += cb;

				float aux = CopiedSoFar;
				int Percent = (aux/CurrentSize)*100;
				fdin.cb = Percent;
				res = UserNotifier( TheUserId, fdintPROGRESS, &fdin );  
		  }
		 if (res == 0)
		   {
			   return _write(hf, pv, cb);
			 }
		 else 
		   {
         return res;   
			 }
   }


  int  FAR DIAMONDAPI file_close(int hf)
   {
 	   return _close(hf);
   }


  long FAR DIAMONDAPI file_seek(int hf, long dist, int seektype)
   {
     return _lseek(hf, dist, seektype);
   }


  FNFDINOTIFY(notification_function)
   {
      int res;
			if (UserNotifier)
			  {
          res = UserNotifier( pfdin->pv, fdint, pfdin ); 
				}
			else 
			  {
				  res = 0;
				}
       
			
			switch (fdint)
	      {
		      case fdintCOPY_FILE:	// file to be copied
				  {
						
						if (res == 0)
						  {
				        int		handle;
						    char	destination[256];
						    sprintf(destination,"%s%s",	DestDir,	pfdin->psz1);
						    handle        = file_open( destination, _O_BINARY | _O_CREAT | _O_WRONLY | _O_SEQUENTIAL, _S_IREAD | _S_IWRITE );
								CurrentHandle = handle;
								CurrentSize		= pfdin->cb; 
								CopiedSoFar		= 0;	
						    return handle;
							}
						else 
						  {
							  return res;
							}
					}
		       case fdintCLOSE_FILE_INFO:	// close the file, set relevant info
            {
              HANDLE  handle;
    					DWORD   attrs;
              char    destination[256];
 			
              sprintf( destination, "%s%s", DestDir, pfdin->psz1 );
						  file_close(pfdin->hf);
						  handle = CreateFile(destination, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

							if (handle != INVALID_HANDLE_VALUE)
							 {
                 FILETIME    datetime;
                 if (TRUE == DosDateTimeToFileTime(pfdin->date, pfdin->time, &datetime ))
								  {
                    FILETIME    local_filetime;
                    if (TRUE == LocalFileTimeToFileTime( &datetime, &local_filetime ))
							    	  {
                        SetFileTime( handle, &local_filetime, NULL, &local_filetime );
											}
                   }

                 CloseHandle(handle);
               }

              attrs = pfdin->attribs;
              attrs &= (_A_RDONLY | _A_HIDDEN | _A_SYSTEM | _A_ARCH);
							SetFileAttributes( destination, attrs );
							CurrentHandle = 0;
							CurrentSize		= 0; 
							CopiedSoFar		= 0;	
						  return TRUE;
            }

		       case fdintNEXT_CABINET:	// file continued to next cabinet
				     return res;
	        }
	
	   return res;
   }


  BOOL WINAPI DecompressCabFiles( char* CabFile, char* Dest, void* UserData, PDECOMPRESSNOTIFIER Notifier ) 
   {
	   HFDI			      fFDI;
	   ERF			      erf;
	   FDICABINETINFO	fCabInfo;
	   int				    hFile;
	   char*          pos;
	   char			      cabinet_name[256];
	   char			      cabinet_path[256];


     UserNotifier = Notifier;
		 TheUserId    = UserData;
		 strcpy( DestDir, Dest );

     fFDI = FDICreate( mem_alloc, mem_free, file_open, file_read, file_write, file_close, file_seek, cpu80386, &erf );

	   if (fFDI)
	    {
    	  hFile = file_open( CabFile, _O_BINARY | _O_RDONLY | _O_SEQUENTIAL, 0 );
   	    if (hFile == -1)
	       {
		       FDIDestroy(fFDI);
		       return FALSE;
	       }

	      if (FALSE == FDIIsCabinet(	fFDI, hFile, &fCabInfo ))
	        {
		        _close(hFile);
  	        (void) FDIDestroy(fFDI);
		        return FALSE;
	        }
	      else
	        {
		        _close(hFile);
	        }

	      pos = strrchr( CabFile, '\\');

	      if (pos == NULL)
	       {
		       strcpy( cabinet_name, CabFile );
		       strcpy( cabinet_path, "" );
	       }
	      else
	       {
		       strcpy( cabinet_name, pos + 1 );
		       strncpy(cabinet_path, CabFile, (int) (pos - CabFile) + 1 );
		       cabinet_path[ (int) (pos - CabFile) + 1 ] = 0;
	       }

	      if (!FDICopy(	fFDI, cabinet_name, cabinet_path, 0, notification_function, NULL, UserData ))
	       {
		       FDIDestroy(fFDI);
		       return FALSE;
	       }
        else 
          if (FDIDestroy(fFDI) != TRUE)
	         {
		         return FALSE;
	         }
          else 
           {
             return TRUE;  
           }
	    }
    else 
      {
        return FALSE;
      }

   }

