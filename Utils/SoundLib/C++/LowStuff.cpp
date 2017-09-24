#include <windows.h>
#include <dsound.h>


#define Release(intf)  if (intf != NULL) { intf->Release(); intf = NULL; } 
#define export __declspec(dllexport) __cdecl 

typedef void (__stdcall *ReadCallback)(LPVOID Data, DWORD Size, LPVOID UserData);

// DirectSound object
LPDIRECTSOUND lpDSound = NULL;

// LibStatus
int LibStatus = 0;


extern "C"
  {

    // Initializes sound engine

    BOOL export InitSoundEngine(HWND hWindow)
      {
        if ( SUCCEEDED(DirectSoundCreate(NULL, &lpDSound, NULL)) )
          {
            if ( SUCCEEDED(lpDSound->SetCooperativeLevel(hWindow, DSSCL_NORMAL)) )
			{
			  LibStatus = (int)lpDSound;
              return TRUE;
			}
            else
              {
                Release(lpDSound);
                return FALSE;
              }
          }
        else 
          return FALSE;
      }


    // Finalizes sound engine

    void export DoneSoundEngine()
      {
        Release(lpDSound);
		lpDSound = NULL;
        LibStatus = 0;
      }


    // Creates a DirectSound buffer

    BOOL export CreateSound(LPDIRECTSOUNDBUFFER& lpBuffer, DWORD dwBufSize, DWORD dwFreq, DWORD dwBitsPerSample, DWORD dwBlkAlign, BOOL bStereo)
      {

        if (lpDSound != NULL)
          {
            DSCAPS dsCaps;
            memset(&dsCaps, 0, sizeof DSCAPS);  
            dsCaps.dwSize = sizeof DSCAPS;
            lpDSound->GetCaps(&dsCaps);

            // Set up wave format structure.
            PCMWAVEFORMAT pcmwf;
            memset(&pcmwf, 0, sizeof PCMWAVEFORMAT);
            pcmwf.wf.wFormatTag      = WAVE_FORMAT_PCM;
            pcmwf.wf.nChannels       = bStereo ? 2 : 1;
            pcmwf.wf.nSamplesPerSec  = dwFreq;
            pcmwf.wf.nBlockAlign     = (WORD)dwBlkAlign;
            pcmwf.wf.nAvgBytesPerSec = pcmwf.wf.nSamplesPerSec * pcmwf.wf.nBlockAlign;
            pcmwf.wBitsPerSample     = (WORD)dwBitsPerSample;

            // Set up DSBUFFERDESC structure.
            DSBUFFERDESC dsbdesc;
            memset(&dsbdesc, 0, sizeof DSBUFFERDESC);  
            dsbdesc.dwSize        = sizeof DSBUFFERDESC;
            dsbdesc.dwFlags       = dwBufSize < dsCaps.dwUnlockTransferRateHwBuffers * 120 /*ms*/ ? DSBCAPS_CTRLDEFAULT : DSBCAPS_CTRLDEFAULT | DSBCAPS_LOCSOFTWARE;
            dsbdesc.dwBufferBytes = dwBufSize; 
            dsbdesc.lpwfxFormat   = (LPWAVEFORMATEX)&pcmwf;

            if ( SUCCEEDED(lpDSound->CreateSoundBuffer(&dsbdesc, &lpBuffer, NULL)) )
              return TRUE;
            else
              return FALSE;
          }
        else
          {
            lpBuffer = NULL;
            return FALSE;
          }
      }


    // Fills in a sound buffer

    BOOL export FillSound(LPDIRECTSOUNDBUFFER lpBuffer, DWORD dwSize, ReadCallback ReadData, LPVOID UserData)
      {
        // Lock data in buffer for writing
        LPVOID pData1;
        LPVOID pData2;
        DWORD  dwData1Size;
        DWORD  dwData2Size;

        if (lpBuffer != NULL && ReadData != NULL)
          {
            if ( SUCCEEDED(lpBuffer->Lock(0, dwSize, &pData1, &dwData1Size, &pData2, &dwData2Size, DSBLOCK_FROMWRITECURSOR)) )
              {
                try
                  {
                    if (dwData1Size > 0)
                      ReadData(pData1, dwData1Size, UserData);

                    if (dwData2Size > 0)
                      ReadData(pData2, dwData2Size, UserData);

                    lpBuffer->Unlock(pData1, dwData1Size, pData2, dwData2Size);
                    return TRUE;
                  }
                catch(int)
                  {
                    lpBuffer->Unlock(pData1, dwData1Size, pData2, dwData2Size);
                    return FALSE;
                  }
              }
            else
              return FALSE;
          }
        else
          return FALSE;
      }


    // Stops a sound

    void export StopsSound(LPDIRECTSOUNDBUFFER lpBuffer)
      {
        if (lpBuffer != NULL)
          {
            DWORD dwStatus;
            if ( SUCCEEDED(lpBuffer->GetStatus(&dwStatus)) )
              {
                if ((dwStatus & DSBSTATUS_PLAYING) == DSBSTATUS_PLAYING)
                  lpBuffer->Stop();
              }
          }
      }


    // Plays a sound

    void export PlaysSound(LPDIRECTSOUNDBUFFER lpBuffer, BOOL bLoopIt, BOOL bRestartIt, int Pan, int Volume, int StartOfs)
      {
        if (lpBuffer != NULL)
          {
            DWORD dwStatus;
            if ( SUCCEEDED(lpBuffer->GetStatus(&dwStatus)) )
              {
                StopsSound(lpBuffer);
                if (bRestartIt)
				  {
				    lpBuffer->SetPan(Pan);
					lpBuffer->SetVolume(Volume);
                    lpBuffer->SetCurrentPosition(StartOfs);
				  }
                lpBuffer->Play(0, 0, bLoopIt ? DSBPLAY_LOOPING : 0);
              }
          }
      }

    void export DuplicateSound(LPDIRECTSOUNDBUFFER lpBuffer, LPDIRECTSOUNDBUFFER& lpBufferCopy)
      {
        lpBufferCopy = NULL;
        if (lpBuffer != NULL && lpDSound != NULL)
          {
            lpDSound->DuplicateSoundBuffer(lpBuffer, &lpBufferCopy);
          }
      }

    void export SetsSoundPan(LPDIRECTSOUNDBUFFER lpBuffer, int Pan)
      {
        if (lpBuffer != NULL)
          {
            DWORD dwStatus;
            if ( SUCCEEDED(lpBuffer->GetStatus(&dwStatus)) )
        	  lpBuffer->SetPan(Pan);
          }
      }

    void export SetsSoundVolume(LPDIRECTSOUNDBUFFER lpBuffer, int Volume)
      {
        if (lpBuffer != NULL)
          {
            DWORD dwStatus;
            if ( SUCCEEDED(lpBuffer->GetStatus(&dwStatus)) )
        	  lpBuffer->SetVolume(Volume);
          }
      }

	// Exports DSound object

	LPDIRECTSOUND export GetDSound()
		{
			return (lpDSound);
		}

	int export GetStatus()
	{
		return (LibStatus);
	}


  } // extern "C"
