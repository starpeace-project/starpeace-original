unit RiffIO;

interface

implementation

end.
(*
/*----------------------------------------------------------------------------*\
    rlefile.c - file handling for RLEAPP
	
\*----------------------------------------------------------------------------*/
// COPYRIGHT:
//
//   (C) Copyright Microsoft Corp. 1993.  All rights reserved.
//
//   You have a royalty-free right to use, modify, reproduce and
//   distribute the Sample Files (and/or any modified version) in
//   any way you find useful, provided that you agree that
//   Microsoft has no warranty obligations or liability for any
//   Sample Application Files which are modified.
//

#include <windows.h>
#include <mmsystem.h>

#include "rleapp.h"
#include "gmem.h"
#include "dib.h"
#include "rle.h"

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/

#define FOURCC( ch0, ch1, ch2, ch3 )                         \
		( (DWORD)(BYTE)(ch0) | ( (DWORD)(BYTE)(ch1) << 8 ) |    \
                ( (DWORD)(BYTE)(ch2) << 16 ) | ( (DWORD)(BYTE)(ch3) << 24 ) )

#define RIFF_RIFF       FOURCC('R','I','F','F')
#define RIFF_LIST       FOURCC('L','I','S','T')
#define RIFF_RLE0       FOURCC('R','L','E','0')
#define RIFF_RLEh       FOURCC('R','L','E','h')
#define RIFF_DIBh       FOURCC('D','I','B','h')
#define RIFF_WAVh       FOURCC('W','A','V','h')
#define RIFF_RGBq       FOURCC('r','g','b','q')
#define RIFF_DIBb       FOURCC('D','I','B','b')

#define RIFF_WAVb       FOURCC('W','A','V','b')
#define RIFF_PALb       FOURCC('P','A','L','b')
#define RIFF_PAD        FOURCC('p','a','d','d')

#define RIFF_WAVE       FOURCC('W','A','V','E')
#define RIFF_DATA       FOURCC('d','a','t','a')
#define RIFF_FMT        FOURCC('f','m','t',' ')


typedef struct {
	DWORD       dwType;
	DWORD       dwSize;
} RIFF, *PRIFF, FAR *LPRIFF;

typedef struct {
	DWORD       dwRiff;
	DWORD       dwSize;
	DWORD       dwType;
} RIFFHDR;

typedef struct {
	DWORD       dwFlags;
	DWORD       dwNumFrames;
	DWORD       dwMaxBuffer;
	DWORD       dwFrameRate;
} RLEHDR;
				/* stats about RLE frames */
/* flags for _lseek */
#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0

DWORD FAR PASCAL lread(int fh, LPVOID p, DWORD len);
DWORD FAR PASCAL lwrite(int fh, LPVOID p, DWORD len);
DWORD FAR PASCAL lseek(int fh, DWORD off, WORD w);
int   FAR PASCAL lopen(LPSTR sz, WORD acc);
int   FAR PASCAL lclose(int fh);

#define LWRITE(fh, p, len)         {if (lwrite(fh,p,len) != (len)) goto error;}
#define LREAD(fh, p, len)          {if (lread(fh,p,len) != (len)) goto error;}
#define WRITERIFF(fh,dwType,p,len) {if (WriteRiff(fh,dwType,p,len) != (len)) goto error;}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
DWORD WriteRiff(int fh, DWORD dwType, LPVOID p, DWORD dwSize)
{
    RIFF    riff;

    riff.dwType = dwType;
    riff.dwSize = dwSize;

    LWRITE(fh, &riff, sizeof(riff));

    if (p != NULL)
        LWRITE(fh, p, (dwSize+1)&~1);

    return dwSize;
error:
    return (UINT)-1L;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
DWORD WriteList(int fh, DWORD dwList, DWORD dwType)
{
    DWORD   off;
    RIFFHDR riff;

    if (!(dwList == RIFF_RIFF || dwList == RIFF_LIST))
	return (UINT)-1;

    off = lseek(fh, 0L, SEEK_CUR);

    riff.dwRiff = dwList;
    riff.dwSize = 0L;
    riff.dwType = dwType;

    LWRITE(fh, &riff, sizeof(riff));

    return off;
error:
    return (UINT)-1L;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL EndList(int fh, DWORD offList)
{
    DWORD off;
    DWORD dwLen;

    off = lseek(fh, 0L, SEEK_CUR);
    dwLen = off - offList - sizeof(RIFF);

    lseek(fh, offList+sizeof(DWORD), SEEK_SET);
    LWRITE(fh, &dwLen, sizeof(DWORD));
    lseek(fh, off, SEEK_SET);

    return TRUE;
error:
    return FALSE;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL OpenRiffFile(LPSTR szFile, int iFrame, DWORD dwType)
{
    unsigned    fh;
    HANDLE      hdib;
    BITMAPINFOHEADER bi;
    RGBQUAD     argbq[256];
    LPBITMAPINFOHEADER lpbi;
    DWORD       off;
    RIFF        riff;
    RIFFHDR     riffhdr;
    LPSTR       pbits;
    LPSTR       prgb;
    int         wWaveRead = 0;
    DWORD	dwWaveSize = 0L;	// Total size of all wave data
    int         i;
    LPWAVEHDR	pwh;
    RLEHDR      rlehdr;

    totFrames = 0;		// length of movie; displayed on status bar
    fh = lopen(szFile, OF_READ);

    if (fh == -1) 
	return FALSE;

    LREAD(fh,(LPVOID)&riffhdr,sizeof(riffhdr));

    if (riffhdr.dwRiff != RIFF_RIFF || riffhdr.dwType != dwType)
        goto exit;

    rlehdr.dwFlags      = 0L;
    rlehdr.dwNumFrames  = 0L;
    rlehdr.dwMaxBuffer  = 0L;
    rlehdr.dwFrameRate  = 0L;

    if (pWaveFormat)			// Free the movie's wave header
	GFreePtr(pWaveFormat);
    if (pwhMovie)                       // Free the movie's wave data
        GFreePtr(pwhMovie);

    pWaveFormat = NULL;
    pwhMovie = NULL;

    for (;;)
    {
        if (lread(fh,(LPVOID)&riff,sizeof(riff)) != sizeof(riff))
	    break;

        off = lseek(fh, 0L, SEEK_CUR);

        switch (riff.dwType)
        {
        case RIFF_RLEh:
            LREAD(fh,(LPVOID)&rlehdr,sizeof(RLEHDR));
            totFrames = (WORD)rlehdr.dwNumFrames;
            break;

        case RIFF_DIBh:
            LREAD(fh,(LPVOID)&bi,sizeof(bi));

            if (bi.biClrUsed == 0)
                bi.biClrUsed = 1 << bi.biBitCount;

	    bi.biSizeImage = (DWORD)DIBWIDTHBYTES(bi) * bi.biHeight;

	    if (riff.dwSize > sizeof(bi))
                LREAD(fh,(LPVOID)argbq,bi.biClrUsed*sizeof(RGBQUAD));

            break;

        case RIFF_RGBq:
            LREAD(fh,(LPVOID)argbq,bi.biClrUsed*sizeof(RGBQUAD));
            break;

        case RIFF_DIBb:
	    hdib = GAlloc(bi.biSize+bi.biClrUsed*sizeof(RGBQUAD)+riff.dwSize);

	    if (!hdib)
                goto errormem;

	    lpbi = GLock(hdib);

	    prgb  = (LPSTR)lpbi + bi.biSize;
	    pbits = (LPSTR)prgb + bi.biClrUsed * sizeof(RGBQUAD);

	    bi.biSizeImage = riff.dwSize;
            MemCopy((LPVOID)lpbi,(LPVOID)&bi,sizeof(bi));
            MemCopy((LPVOID)prgb,(LPVOID)argbq,(int)bi.biClrUsed*sizeof(RGBQUAD));

            LREAD(fh,pbits,riff.dwSize);

	    InsertFrame(hdib,iFrame,TRUE);

	    if (iFrame == -1)			// What frame was that?
		i = numFrames - 1;
	    else
		i = iFrame;

	    if (iFrame >= 0)
		iFrame++;
            break;

        case RIFF_WAVh:
	    pWaveFormat = GAllocPtr(riff.dwSize);

	    if (pWaveFormat == NULL)
                goto errormem;

            LREAD(fh,(LPVOID)pWaveFormat,(WORD)riff.dwSize);
	    gSamplesPerSec = pWaveFormat->nSamplesPerSec;
            gChannels = pWaveFormat->nChannels;
            break;

        case RIFF_WAVb:
            if (pwhMovie)
            {
                pwh = GReAllocPtr(pwhMovie,
                    sizeof(WAVEHDR)+pwhMovie->dwBufferLength+riff.dwSize);

                if (!pwh)
                {
                    pwh = GAllocPtr(sizeof(WAVEHDR)+pwhMovie->dwBufferLength+riff.dwSize);

                    if (!pwh)
                        goto errormem;

                    MemCopy(pwh,pwhMovie,pwhMovie->dwBufferLength);

                    GFreePtr(pwhMovie);
                }

                pwhMovie = pwh;
		pwhMovie->lpData = (LPVOID)(pwhMovie+1);
            }
            else
            {
                pwh = GAllocPtr(riff.dwSize + sizeof(WAVEHDR));

                if (!pwh)
                    goto errormem;

		pwh->lpData          = (LPVOID)(pwh+1);
		pwh->dwBufferLength  = 0;
		pwh->dwBytesRecorded = 0;
		pwh->dwUser          = 0;
		pwh->dwFlags         = WHDR_DONE;
                pwh->dwLoops         = 0;

                pwhMovie = pwh;
	    }

            LREAD(fh, (BYTE huge * )(pwh->lpData) + pwh->dwBufferLength, riff.dwSize);
	    dwWaveSize += riff.dwSize;
            pwh->dwBufferLength += riff.dwSize;
            break;

        case RIFF_LIST:
            //
            //  we want to desend into all list chunks, so treat
            //  the list chunk as a small bogus chunk.
            //
            riff.dwSize = sizeof(DWORD);        // LIST form type.
            break;

        default:
        case RIFF_PALb:
        case RIFF_PAD:
            // ignore
            break;
	}

        lseek(fh, off + (riff.dwSize+1)&~1L, SEEK_SET);

	if (WinYield())
	    break;
    }

exit1:
    if ((rlehdr.dwFrameRate > 1*FramesSecScale) && (rlehdr.dwFrameRate < 32*FramesSecScale))
	FramesSec = rlehdr.dwFrameRate;
    else if (dwWaveSize && numFrames)
        FramesSec = muldiv32(numFrames,(long)gChannels*gSamplesPerSec*FramesSecScale,dwWaveSize);
    else
        FramesSec = 15 * FramesSecScale;

/* HACK!!  Remember the start and size of the wave data */

    if (pwhMovie)
    {
        glpData         = pwhMovie->lpData;
        gdwBufferLength = pwhMovie->dwBufferLength;
    }

    lclose(fh);
    return TRUE;

errormem:
    ErrMsg("Out of Memory Error");
    goto exit1;

error:
    ErrMsg("Read Error");
    goto exit1;

exit:
    lclose(fh);
    return FALSE;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL OpenRl0File(LPSTR szFile, int iFrame)
{
    return OpenRiffFile(szFile, iFrame, RIFF_RLE0);
}


/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
void SaveRl0File(LPSTR szFile,int startFrame,int nFrames)
{
    unsigned    fh;
    int         i;
    HANDLE      hdib;
    HPALETTE    hpal;
    int         curSave = curFrame;
    LPBITMAPINFOHEADER lpbi;
    BITMAPINFOHEADER bi;
#if 0    
    RIFF        riff;
#endif
    RIFFHDR     riffhdr;
    RLEHDR	rlehdr;

    fh = lopen(szFile, OF_READWRITE|OF_CREATE);

    if (fh == -1) {
	ErrMsg("Error Creating File");
	return;
    }

    StartWait();

    //
    // write the RIFF header.
    //
    riffhdr.dwRiff = RIFF_RIFF;
    riffhdr.dwType = RIFF_RLE0;
    riffhdr.dwSize = 0;             // patch later
    LWRITE(fh, (LPVOID)&riffhdr, sizeof(RIFFHDR));

    //
    // write the RLE header.
    //
    rlehdr.dwFlags      = 0L;
    rlehdr.dwNumFrames  = (DWORD)numFrames;
    rlehdr.dwMaxBuffer  = 0L;
    rlehdr.dwFrameRate  = FramesSec;

    for (i=startFrame; i<startFrame+nFrames; i++)
    {
	lpbi = GLock(FrameRle(i));
	rlehdr.dwMaxBuffer = max(rlehdr.dwMaxBuffer,lpbi->biSizeImage);
    }

    WriteRiff(fh, RIFF_RLEh, (LPVOID)&rlehdr, sizeof(RLEHDR));
#if 0    
    riff.dwType = RIFF_RLEh;
    riff.dwSize = sizeof(RLEHDR);
    LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
    LWRITE(fh, (LPVOID)&rlehdr, sizeof(RLEHDR));
#endif

    //
    // write the WAV header.
    //
    if (pWaveFormat)
    {
	WriteRiff( fh, RIFF_WAVh, (LPVOID)pWaveFormat, GSize(HIWORD(pWaveFormat)));
#if 0	
	riff.dwType = RIFF_WAVh;
	riff.dwSize = GSize(HIWORD(pWaveFormat));  //!!!
        LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
        LWRITE(fh, (LPVOID)pWaveFormat, (WORD)riff.dwSize);
#endif	
    }

    //
    //  write the WAV bits?
    //
    if (pwhMovie)
    {
        pwhMovie->dwBufferLength = gdwBufferLength;
        pwhMovie->lpData = glpData;

	WriteRiff(fh, RIFF_WAVb, pwhMovie->lpData, ALIGNULONG(pwhMovie->dwBufferLength));
#if 0
	riff.dwType = RIFF_WAVb;
        riff.dwSize = pwhMovie->dwBufferLength;
	riff.dwSize = ALIGNULONG(riff.dwSize);
        LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
        LWRITE(fh, pwhMovie->lpData, riff.dwSize);
#endif	
    }

    //
    // write all the frames.
    //
    for (i=startFrame; i<startFrame+nFrames; i++)
    {
	hdib = FrameRle(i);
	hpal = FramePalette(i);
	lpbi = GLock(hdib);

	//
	// write a DIB header. (if needed)
	//
	if (i==startFrame ||
	    bi.biWidth != lpbi->biWidth ||
	    bi.biHeight != lpbi->biHeight ||
	    bi.biCompression != lpbi->biCompression)
	{
	    bi = *lpbi;
	    WriteRiff(fh, RIFF_DIBh, (LPVOID)lpbi, lpbi->biSize);
#if 0	    
	    riff.dwType = RIFF_DIBh;
	    riff.dwSize = lpbi->biSize;
            LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
            LWRITE(fh, (LPVOID)lpbi, (WORD)riff.dwSize);
#endif	    
	}

	//
	// write a palette if needed
	//
	if (!(FrameFlags(i) & F_PALSHARED))
	{
	    SetDibUsage(hdib,hpal,DIB_RGB_COLORS);
	    WriteRiff(fh, RIFF_RGBq, (LPSTR)lpbi+lpbi->biSize, lpbi->biClrUsed * sizeof(RGBQUAD));
#if 0	    
	    riff.dwType = RIFF_RGBq;
	    riff.dwSize = lpbi->biClrUsed * sizeof(RGBQUAD);
            LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
            LWRITE(fh, (LPSTR)lpbi+lpbi->biSize, (WORD)riff.dwSize);
#endif	    
	    SetDibUsage(hdib,hpal,DIB_PAL_COLORS);
	}
	//
	//  write the DIB bits
	//
	WriteRiff(fh, RIFF_DIBb, DibXY(lpbi,0,0), lpbi->biSizeImage);
#if 0    
	riff.dwType = RIFF_DIBb;
	riff.dwSize = lpbi->biSizeImage;
        LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
        LWRITE(fh, DibXY(lpbi,0,0), riff.dwSize);
#endif
	ShowFrame(i);

	if (WinYield())
	    break;
    }

    //
    // patch the RIFFHDR
    //
    riffhdr.dwRiff = RIFF_RIFF;
    riffhdr.dwType = RIFF_RLE0;
    riffhdr.dwSize = lseek(fh, 0L, SEEK_CUR) - sizeof(RIFF);

    lseek(fh, 0L, SEEK_SET);
    LWRITE(fh, (LPVOID)&riffhdr, sizeof(RIFFHDR));

    lclose(fh);

    ShowFrame(curSave);
    EndWait();

    return;

error:
    lclose(fh);

    ShowFrame(curSave);
    EndWait();

    ErrMsg("Error Writing file");
    return;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL SaveWavFile(LPSTR szFile,int startFrame,int nFrames)
{
    unsigned    fh;
#if 0    
    DWORD       dwSize;
    RIFF        riff;
#endif
    RIFFHDR     riffhdr;

    if (!pWaveFormat || !pwhMovie)
	return FALSE;

    fh = lopen(szFile, OF_READWRITE|OF_CREATE);

    if (fh == -1) {
	ErrMsg("Error Creating File");
	return FALSE;
    }

    StartWait();

    //
    // write the RIFF header.
    //
    riffhdr.dwRiff = RIFF_RIFF;
    riffhdr.dwType = RIFF_WAVE;
    riffhdr.dwSize = 0;			// patch it later
    LWRITE(fh, (LPVOID)&riffhdr, sizeof(RIFFHDR));

    //
    // write the WAV header.
    //
    WriteRiff(fh, RIFF_FMT, (LPVOID)pWaveFormat, GSize(HIWORD(pWaveFormat)));
#if 0
    riff.dwType = RIFF_FMT;
    riff.dwSize = GSize(HIWORD(pWaveFormat));  //!!!
    LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
    LWRITE(fh, (LPVOID)pWaveFormat, (WORD)riff.dwSize);
#endif

    //
    //  write the WAV bits?
    //
    pwhMovie->dwBufferLength = gdwBufferLength;
    pwhMovie->lpData = glpData;

    WriteRiff(fh, RIFF_DATA, pwhMovie->lpData, pwhMovie->dwBufferLength);
#if 0    
    riff.dwSize = pwhMovie->dwBufferLength;
    riff.dwType = RIFF_DATA;

    LWRITE(fh, (LPVOID)&riff, sizeof(RIFF));
    LWRITE(fh, pwhMovie->lpData, dwSize);


    //
    // word align
    //
    if (riff.dwSize & 1)
        LWRITE(fh, (LPVOID)&riff, 1);

#endif

    //
    // patch the RIFFHDR
    //
    riffhdr.dwRiff = RIFF_RIFF;
    riffhdr.dwType = RIFF_WAVE;
    riffhdr.dwSize = lseek(fh, 0L, SEEK_CUR) - sizeof(RIFF);
    lseek(fh, 0L, SEEK_SET);
    LWRITE(fh, (LPVOID)&riffhdr, sizeof(RIFFHDR));

    lclose(fh);
    EndWait();
    return TRUE;

error:
    ErrMsg("Error writing file");
    lclose(fh);
    EndWait();
    return FALSE;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/

#define BUFSIZE         2048
#define BUFALIGN(ul)    (((ul)+BUFSIZE-1) & ~(BUFSIZE-1))


/*----------------------------------------------------------------------------*\
  
// *********************************************************************    
//  BAD CODE WARNING:
// *********************************************************************    
//
// This code requires that the header be the first thing
// followed by data.  THIS IS NOT CORRECT!!!!!!
// This code will be fixed in a next version.
//
// Do not use this code!!!!!
//

\*----------------------------------------------------------------------------*/
BOOL OpenWavFile(LPSTR szFile)
{
    unsigned    fh;
    RIFF        riff;
    RIFFHDR     riffhdr;
    LPWAVEHDR	pwh;
    DWORD	dwWaveSize;

    fh = lopen(szFile, OF_READ);
    if (fh == -1)
	return FALSE;

    //
    // read the RIFF header.
    //
    LREAD(fh, (LPVOID)&riffhdr, sizeof(RIFFHDR));
    if (riffhdr.dwRiff != RIFF_RIFF || riffhdr.dwType != RIFF_WAVE) 
    {
        lclose(fh);
	return FALSE;
    }

    if (numFrames == 0) 
    {
	ErrMsg("Can't Open Wave File onto Empty Movie");
        lclose(fh);
	return FALSE;
    }

    StartWait();

    //
    // read the WAVE header
    //
    LREAD(fh, (LPVOID)&riff, sizeof(RIFF));

    if (riff.dwType != RIFF_FMT) 
    {
	ErrMsg("File Corrupted");
	goto openwave_barf;
    }

/* Destroy the current waves of this movie */

    if (pWaveFormat)
	GFreePtr(pWaveFormat);
    if (pwhMovie)
        GFreePtr(pwhMovie);
    pWaveFormat = NULL;
    pwhMovie = NULL;

/* Load the wave header in */

    pWaveFormat = GAllocPtr(riff.dwSize);
    if (pWaveFormat == NULL) 
    {
	ErrMsg("Out of Memory Error");
	goto openwave_barf;
    }

    LREAD(fh,(LPVOID)pWaveFormat,(WORD)riff.dwSize);
    gSamplesPerSec = pWaveFormat->nSamplesPerSec;
    gChannels = pWaveFormat->nChannels;

    //
    // read the DATA header
    //
    LREAD(fh, (LPVOID)&riff, sizeof(RIFF));
    if (riff.dwType != RIFF_DATA) 
    {
	ErrMsg("File Corrupted");
	goto openwave_barf;
    }

    //
    // read the actual wave data
    //
    pwh = GAllocPtr(riff.dwSize + sizeof(WAVEHDR));
    if (!pwh) 
    {
	ErrMsg("Out of Memory Error");
	goto openwave_barf;
    }
    pwh->lpData          = (LPVOID)(pwh+1);
    pwh->dwBufferLength  = riff.dwSize;
    pwh->dwBytesRecorded = 0;
    pwh->dwUser          = 0;
    pwh->dwFlags         = WHDR_DONE;
    pwh->dwLoops         = 0;

    LREAD(fh,pwh->lpData,riff.dwSize);
    dwWaveSize = pwh->dwBufferLength;

    if (dwWaveSize && numFrames)
        FramesSec = muldiv32(numFrames,(long)gChannels*gSamplesPerSec*FramesSecScale,dwWaveSize);
    else
        FramesSec = 15 * FramesSecScale;

    pwhMovie = pwh;

    glpData         = pwh->lpData;
    gdwBufferLength = pwh->dwBufferLength;

    lclose(fh);
    EndWait();
    return TRUE;

error:
openwave_barf:
    lclose(fh);
    EndWait();
    return FALSE;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL NEAR OpenRleFile(LPSTR szFile, int iFrame)
{
    unsigned    fh;
    HANDLE      hdib;
    BOOL	fValidDibRead = FALSE;

    totFrames = 0;
    fh = lopen(szFile, OF_READ);

    if (fh == -1)
	return FALSE;

    while (hdib = OpenDIB((LPSTR)MAKEINTATOM(fh)))
    {
	fValidDibRead = TRUE;
	InsertFrame(hdib, iFrame, TRUE);

	if (iFrame >= 0)
	    iFrame++;

	if (WinYield())
	    break;
    }

    lclose(fh);
    FramesSec = 15 * FramesSecScale;

    return fValidDibRead;
}

/*----------------------------------------------------------------------------*\
   Load in a whole slew of frames one at a time if they are called
   foo000.dib
   foo001.dib
   foo002.dib  ...etc....  in this case you would use a filename of
                           foo%03d.dib to load in this sequence.
   The sequence can start with 0 or 1, and go arbitrarily high as long as
   every filename can be generated with some printf statement.
\*----------------------------------------------------------------------------*/

BOOL OpenFrames(LPSTR szFrame, int iFrame)
{
    char buf[80];
    int i;
    BOOL fFileFound = FALSE;

    totFrames = 0;		// length of movie; displayed on status bar
    // or wsprintf in the "load a slew of frames" won't work
    AnsiLower(szFrame);

    for (i=0; (wsprintf(buf,szFrame,i) &&
        (fFileFound = OpenRleFile(buf,iFrame)) && !WinYield()) || i==0; i++)
        ;

    return fFileFound;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
BOOL OpenMovieFile(LPSTR szFrame, int iFrame)
{
    BOOL f;

    StartWait();
    fLoading++;

    f = OpenWavFile(szFrame) ||
        OpenRl0File(szFrame, iFrame) ||
        OpenRleFile(szFrame, iFrame) ||
        OpenFrames (szFrame, iFrame);

    fLoading--;
    EndWait();

    if (!f)  // We never did load a valid file!
	ErrMsg("File Open Error");

    return f;
}

/*----------------------------------------------------------------------------*\
\*----------------------------------------------------------------------------*/
void SaveRleFile(LPSTR szFile,int startFrame,int nFrames)
{
    unsigned    fh;
    int         i;
    HANDLE      hdib;
    HPALETTE    hpal;
    int         curSave = curFrame;

    fh = lopen(szFile, OF_READWRITE|OF_CREATE);

    if (fh == -1)
	return;

    if (startFrame > 0)			// Validate the first frame of movie
	RenderFrame(startFrame);

    StartWait();
    for (i=startFrame; i<startFrame+nFrames; i++)
    {
	hdib = FrameDib(i);
	hpal = FramePalette(i);

	SetDibUsage(hdib,hpal,DIB_RGB_COLORS);
        WriteDIB((LPSTR)MAKEINTATOM(fh),hdib);
	SetDibUsage(hdib,hpal,DIB_PAL_COLORS);

	ShowFrame(i);
	if (WinYield())
	    break;
    } 
    PurgeFrames();
    ShowFrame(curSave);
    EndWait();

    lclose(fh);

    return;
}

/***************************************************************************\
*
*   routines for file I/O
*
\***************************************************************************/

DWORD FAR PASCAL lread(int fh, LPVOID p, DWORD len)
{
    return mmioRead((HMMIO)fh, p, len);
}

DWORD FAR PASCAL lwrite(int fh, LPVOID p, DWORD len)
{
    return mmioWrite((HMMIO)fh, p, len);
}

DWORD FAR PASCAL lseek(int fh, DWORD off, WORD w)
{
    return mmioSeek((HMMIO)fh,off,w);
}

int FAR PASCAL lopen(LPSTR sz, WORD acc)
{
    HMMIO hmmio;

    hmmio = mmioOpen(sz, NULL, acc | MMIO_ALLOCBUF);

    if (hmmio == NULL)
        return -1;

    return (int)hmmio;
}

int FAR PASCAL lclose(int fh)
{
    return mmioClose((HMMIO)fh, 0);
}
*)
