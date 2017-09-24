unit FilenameUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Classes, SysUtils, Windows, CommDlg, ShellApi;

  // Temporary storage
  function GetTmpPath : string;
  function GetTmpFilename( const Prefix : string ): string;
  function GetTmpFileWithPath( const Path, Prefix : string ): string;

  // Other folders
  function WindowsPath : string;

  const
    InvalidFilenameChars = [ '\', '/', ':', '*', '?', '<', '>', '|', '"' ];
    ValidIdentifierChars = [ 'a'..'z', 'A'..'Z', '0'..'9', '_' ];

  function GetDisplayFilename( const Filename : string ) : string;
  function ExplorerIsHidingExtension( const anExt : string ) : boolean;

  function PathIsUNC( const FilePath : string ) : boolean;

  function ExtractFileNameOnly( const FilePath : string ) : string;
  function ExtractDriveOnly( const FilePath : string ) : string;

  function GetFileNameDesc( const Filename : string ) : string;

  function GetRenamedFilename( const OldName, NewName : string ) : string;

  // Convert from a list of files in the form '"file1" "file2 with spaces" "etc"' in a TStrings, and viceversa
  function SplitFileList( const Files : string ) : TStrings;
  function JoinFileList( const Files : TStrings ) : string;

  // PChar-based stuff

  function AddrDir( const Filename : string ) : pchar;
  function AddrFilename( const Filename : string ) : pchar;
  function AddrFileExt( const Filename : string ) : pchar;

  // Converts a filename to a valid identifier
  function FilenameToId( const Filename : string ) : string;

  // Drive stuff ---------------------------------------------

  function VolumeID( Drive : char ) : string;

implementation

  uses
    StrUtils;
    
  function VolumeID( Drive : char ) : string;
    var
      OldErrorMode      : integer;
      NotUsed, VolFlags : integer;
      Buf               : array [0..MAX_PATH] of char;
    begin
      OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
      try
        if GetVolumeInformation( pchar( Drive + ':\' ), Buf, sizeof( Buf ), nil, NotUsed, VolFlags, nil, 0 )
          then SetString( Result, Buf, StrLen( Buf ) )
          else Result := '';
        if Drive < 'a'
          then Result := AnsiUpperCaseFileName( Result )
          else Result := AnsiLowerCaseFileName( Result );
      finally
        SetErrorMode( OldErrorMode );
      end;
    end;

  function SplitFileList( const Files : string ) : TStrings;
    var
      ChPos    : integer;
      OldPos   : integer;
      FileList : string;
    begin
      FileList := Trim( Files );
      Result   := TStringList.Create;
      if FileList[1] <> '"'   // Only one file?
        then Result.Add( FileList ) // Yes!
        else
          begin
            OldPos := 1;
            repeat
              ChPos := Pos( '"', FileList, OldPos + 1 );
              if ChPos <> 0
                then
                  begin
                    Result.Add( copy( FileList, OldPos + 1, ChPos - OldPos - 1 ) );

                    // Now skip to next quote (")
                    ChPos := Pos( '"', FileList, ChPos + 1 );
                  end
                else Result.Add( copy( FileList, OldPos + 1, MaxInt ) );
              OldPos := ChPos;
            until ChPos = 0;
          end;
    end;

  function JoinFileList( const Files : TStrings ) : string;
    var
      i : integer;
    begin
      Result := '';
      with Files do
        for i := 0 to Count - 1 do
          Result := Result + '"' + Trim( Strings[i] ) + '" ';
    end;

  function WindowsPath : string;
    begin
      SetLength( result , MAX_PATH );
      SetLength( result , GetWindowsDirectory( pchar( result ), MAX_PATH ) );
    end;

  // Converts a filename to a valid identifier
  function FilenameToId( const Filename : string ) : string;
    var
      i : integer;
    begin
      SetString( Result, pchar(Filename), length( Filename ) );
      if Result[1] in [ '0'..'9' ]
        then Result := '_' + Result;
      for i := 0 to length( Result ) - 1 do
        if not ( pchar(Result)[i] in ValidIdentifierChars )
          then pchar(Result)[i] := '_';
      if ( Result = UpperCase( Result ) ) or ( Result = LowerCase( Result ) )
        then Result := CapitalizeStr( Result );
    end;
    
  function GetTmpPath : string;
    begin
      SetLength( Result, MAX_PATH );
      SetLength( Result, GetTempPath( MAX_PATH, pchar( Result ) ) );
    end;

  function GetTmpFileWithPath( const Path, Prefix : string ): string;
     begin
       SetLength( Result, MAX_PATH );
       GetTempFileName( pchar( Path ), pchar( Prefix ), 0, pchar( Result ) );
       SetLength( Result, StrLen( pchar( Result ) ) );
     end;

  function GetTmpFilename( const Prefix : string ): string;
     begin
       SetLength( Result, MAX_PATH );
       GetTempFileName( pchar( GetTmpPath ), pchar( Prefix ), 0, pchar( Result ) );
       SetLength( Result, StrLen( pchar( Result ) ) );
     end;

  function PathIsUNC( const FilePath : string ) : boolean;
    begin
      Result := (FilePath[1] = '\') and (FilePath[2] = '\');
    end;

  function GetDisplayFilename( const Filename : string ) : string;
    begin
      SetLength( Result, GetFileTitle( pchar(Filename), nil, 0 ) - 1 );
      GetFileTitle( pchar(Filename), pchar(Result), length(Result) + 1 );
    end;

  function ExplorerIsHidingExtension( const anExt : string ) : boolean;
    const
      Sign = 'Esc#Pwaf';
    begin
      Result := AddrFileExt( GetDisplayFileName( Sign + anExt )) = #0;
    end;

  function ExtractDriveOnly( const FilePath : string ) : string;
    begin
      Result := copy( FilePath, 1, AddrDir( FilePath ) - pchar( FilePath ) );
    end;

  function ExtractFileNameOnly( const FilePath : string ) : string;
    var
      FileName : pchar;
      FileExt  : pchar;
    begin
      Filename := AddrFilename( FilePath );
      FileExt  := AddrFileExt ( FilePath );
      SetString( Result, Filename, FileExt - Filename );
    end;

  function GetFileNameDesc( const Filename : string ) : string;
    var
      Info : TSHFileInfo;
    begin
      if SHGetFileInfo( pchar(FileName), 0, Info, sizeof(TSHFileInfo), SHGFI_TYPENAME ) <> 0
        then Result := Info.szTypeName
        else Result := '';
    end;

  function GetRenamedFilename( const OldName, NewName : string ) : string;
    var
      NewExt : pchar;
      OldExt : pchar;
    begin
      NewExt := AddrFileExt( NewName );
      if NewExt = #0
        then
          begin
            OldExt := AddrFileExt( OldName );
            Result := NewName + copy( OldName, OldExt - pchar( OldName ), MaxInt );
          end
        else
          Result := NewName;
    end;

  function AddrDir( const Filename : string ) : pchar;
    begin
      if Filename[2] <> ':'
        then Result := @Filename
        else Result := pchar(Filename) + 2;
    end;

  function AddrFilename( const Filename : string ): pchar;
    begin
      Result := pchar(Filename) + length(Filename) - 1; // Go to the end..

      while ( Result <> pchar(Filename) ) and not ( Result[-1] in ['\', ':'] ) do
        dec( Result );
    end;

  function AddrFileExt( const Filename : string ): pchar;
    var
      StrEnd : pchar;
    begin
      StrEnd := pchar(FileName) + length(FileName) - 1;

      Result := StrEnd;
      while ( Result <> pchar(Filename) ) and not (Result[0] in ['.', '\', ':']) do
        dec( Result );

      if ( Result = pchar(Filename) ) or (Result[0] <> '.')
        then Result := StrEnd + 1;  // Return pointer to #0
    end;

end.
(*
//--------------------------------------------------------------------------
// Canonicalizes a path.
BOOL PathCanonicalize(LPTSTR lpszDst, LPCTSTR lpszSrc)
    {
    LPCTSTR lpchSrc;
    LPCTSTR lpchPCEnd;           // Pointer to end of path component.
    LPTSTR lpchDst;
    BOOL fUNC;
    int cbPC;

    fUNC = PathIsUNC(lpszSrc);    // Check for UNCness.

    // Init.
    lpchSrc = lpszSrc;
    lpchDst = lpszDst;

    while (*lpchSrc)
        {
        // this should just return the count
        lpchPCEnd = GetPCEnd(lpchSrc);
        cbPC = (lpchPCEnd - lpchSrc)+1;

        // Check for slashes.
        if (cbPC == 1 && *lpchSrc == TEXT('\\'))
            {
            // Just copy them.
            *lpchDst = TEXT('\\');
            lpchDst++;
            lpchSrc++;
            }
        // Check for dots.
        else if (cbPC == 2 && *lpchSrc == TEXT('.'))
            {
            // Skip it...
            // Are we at the end?
            if (*(lpchSrc+1) == TEXT('\0'))
                {
                lpchDst--;
                lpchSrc++;
                }
            else
                lpchSrc += 2;
            }
        // Check for dot dot.
        else if (cbPC == 3 && *lpchSrc == TEXT('.') && *(lpchSrc + 1) == TEXT('.'))
            {
            // make sure we aren't already at the root
            if (!PathIsRoot(lpszDst))
                {
                // Go up... Remove the previous path component.
                lpchDst = (LPTSTR)PCStart(lpszDst, lpchDst - 1);
                }
            else
                {
                // When we can't back up, remove the trailing backslash
                // so we don't copy one again. (C:\..\FOO would otherwise
                // turn into C:\\FOO).
                if (*(lpchSrc + 2) == TEXT('\\'))
                    {
                    lpchSrc++;
                    }
                }
            lpchSrc += 2;       // skip ".."
            }
        // Everything else
        else
            {
            // Just copy it.
            lstrcpyn(lpchDst, lpchSrc, cbPC);
            lpchDst += cbPC - 1;
            lpchSrc += cbPC - 1;
            }
        // Keep everything nice and tidy.
        *lpchDst = TEXT('\0');
        }

    // Check for weirdo root directory stuff.
    NearRootFixups(lpszDst, fUNC);

    return TRUE;
    }


// Modifies:
//      szRoot
//
// Returns:
//      TRUE if a drive root was found
//      FALSE otherwise
//
BOOL PathStripToRoot(LPTSTR pszRoot)
{
    while(!PathIsRoot(pszRoot))
    {
        if (!PathRemoveFileSpec(pszRoot))
        {
            // If we didn't strip anything off,
            // must be current drive
            return(FALSE);
        }
    }

    return(TRUE);
}


// concatinate lpszDir and lpszFile into a properly formed path
// and canonicalizes any relative path pieces
//
// returns:
//  pointer to destination buffer
//
// lpszDest and lpszFile can be the same buffer
// lpszDest and lpszDir can be the same buffer
//
// assumes:
//      lpszDest is MAX_PATH bytes
//
//

LPTSTR PathCombine(LPTSTR lpszDest, LPCTSTR lpszDir, LPCTSTR lpszFile)
{
    TCHAR szTemp[MAX_PATH];
    LPTSTR pszT;

    if (!lpszFile || *lpszFile==TEXT('\0')) {

        lstrcpyn(szTemp, lpszDir, ARRAYSIZE(szTemp));       // lpszFile is empty

    } else if (lpszDir && *lpszDir && PathIsRelative(lpszFile)) {

        lstrcpyn(szTemp, lpszDir, ARRAYSIZE(szTemp));
        pszT = PathAddBackslash(szTemp);
        if (pszT) {
            int iLen = lstrlen(szTemp);
            if ((iLen + lstrlen(lpszFile)) < ARRAYSIZE(szTemp)) {
                lstrcpy(pszT, lpszFile);
            } else
                return NULL;
        } else
            return NULL;

    } else if (lpszDir && *lpszDir &&
        *lpszFile == TEXT('\\') && !PathIsUNC(lpszFile)) {

        lstrcpyn(szTemp, lpszDir, ARRAYSIZE(szTemp));
        // Note that we do not check that an actual root is returned;
        // it is assumed that we are given valid parameters
        PathStripToRoot(szTemp);

        pszT = PathAddBackslash(szTemp);
        if (pszT)
        {
            // Skip the backslash when copying
            lstrcpyn(pszT, lpszFile+1, ARRAYSIZE(szTemp) - 1 - (pszT-szTemp));
        } else
            return NULL;

    } else {

        lstrcpyn(szTemp, lpszFile, ARRAYSIZE(szTemp));     // already fully qualified file part

    }

    PathCanonicalize(lpszDest, szTemp); // this deals with .. and . stuff

    return lpszDest;
}

// rips the last part of the path off including the backslash
//      C:\foo      -> C:\      ;
//      C:\foo\bar  -> C:\foo
//      C:\foo\     -> C:\foo
//      \\x\y\x     -> \\x\y
//      \\x\y       -> \\x
//      \\x         -> ?? (test this)
//      \foo        -> \  (Just the slash!)
//
// in/out:
//      pFile   fully qualified path name
// returns:
//      TRUE    we stripped something
//      FALSE   didn't strip anything (root directory case)
//

BOOL PathRemoveFileSpec(LPTSTR pFile)
{
    LPTSTR pT;
    LPTSTR pT2 = pFile;

    for (pT = pT2; *pT2; pT2 = CharNext(pT2)) {
        if (*pT2 == TEXT('\\'))
            pT = pT2;             // last "\" found, (we will strip here)
        else if (*pT2 == TEXT(':')) {   // skip ":\" so we don't
            if (pT2[1] ==TEXT('\\'))    // strip the "\" from "C:\"
                pT2++;
            pT = pT2 + 1;
        }
    }
    if (*pT == 0)
        return FALSE;   // didn't strip anything

    //
    // handle the \foo case
    //
    else if ((pT == pFile) && (*pT == TEXT('\\'))) {
        // Is it just a '\'?
        if (*(pT+1) != TEXT('\0')) {
            // Nope.
            *(pT+1) = TEXT('\0');
            return TRUE;        // stripped something
        }
        else        {
            // Yep.
            return FALSE;
        }
    }
    else {
        *pT = 0;
        return TRUE;    // stripped something
    }
}

// add a backslash to a qualified path
//
// in:
//  lpszPath    path (A:, C:\foo, etc)
//
// out:
//  lpszPath    A:\, C:\foo\    ;
//
// returns:
//  pointer to the NULL that terminates the path


LPTSTR PathAddBackslash(LPTSTR lpszPath)
{
    LPTSTR lpszEnd;

    // try to keep us from tromping over MAX_PATH in size.
    // if we find these cases, return NULL.  Note: We need to
    // check those places that call us to handle their GP fault
    // if they try to use the NULL!
    int ichPath = lstrlen(lpszPath);
    if (ichPath >= (MAX_PATH - 1))
    {
        Assert(FALSE);      // Let the caller know!
        return(NULL);
    }

    lpszEnd = lpszPath + ichPath;

    // this is really an error, caller shouldn't pass
    // an empty string
    if (!*lpszPath)
        return lpszEnd;

    /* Get the end of the source directory
    */
    switch(*CharPrev(lpszPath, lpszEnd)) {
    case TEXT('\\'):
        break;

    default:
        *lpszEnd++ = TEXT('\\');
        *lpszEnd = TEXT('\0');
    }
    return lpszEnd;
}


// Returns a pointer to the last component of a path string.
//
// in:
//      path name, either fully qualified or not
//
// returns:
//      pointer into the path where the path is.  if none is found
//      returns a poiter to the start of the path
//
//  c:\foo\bar  -> bar
//  c:\foo      -> foo
//  c:\foo\     -> c:\foo\      ( is this case busted?)
//  c:\         -> c:\          ( this case is strange)
//  c:          -> c:
//  foo         -> foo


LPTSTR PathFindFileName(LPCTSTR pPath)
{
    LPCTSTR pT;

    for (pT = pPath; *pPath; pPath = CharNext(pPath)) {
        if ((pPath[0] == TEXT('\\') || pPath[0] == TEXT(':')) && pPath[1] && (pPath[1] != TEXT('\\')))
            pT = pPath + 1;
    }

    return (LPTSTR)pT;   // const -> non const
}

//---------------------------------------------------------------------------
// Returns TRUE if the given string is a UNC path.
//
// TRUE
//      "\\foo\bar"
//      "\\foo"         <- careful
//      "\\"
// FALSE
//      "\foo"
//      "foo"
//      "c:\foo"

BOOL PathIsUNC(LPCTSTR pszPath)
{
    return DBL_BSLASH(pszPath);
}

//---------------------------------------------------------------------------
// Return TRUE if the path isn't absoulte.
//
// TRUE
//      "foo.exe"
//      ".\foo.exe"
//      "..\boo\foo.exe"
//
// FALSE
//      "\foo"
//      "c:bar"     <- be careful
//      "c:\bar"
//      "\\foo\bar"

BOOL PathIsRelative(LPCTSTR lpszPath)
{
    // The NULL path is assumed relative
    if (*lpszPath == 0)
        return TRUE;

    // Does it begin with a slash ?
    if (lpszPath[0] == TEXT('\\'))
        return FALSE;
    // Does it begin with a drive and a colon ?
    else if (!IsDBCSLeadByte(lpszPath[0]) && lpszPath[1] == TEXT(':'))
        return FALSE;
    // Probably relative.
    else
        return TRUE;
}

#pragma data_seg(".text", "CODE")

const TCHAR c_szColonSlash[] = TEXT(":\\");

#pragma data_seg()

// check if a path is a root
//
// returns:
//  TRUE for "\" "X:\" "\\foo\asdf" "\\foo\"
//  FALSE for others

BOOL  PathIsRoot(LPCTSTR pPath)
{
    if (!IsDBCSLeadByte(*pPath))
    {
        if (!lstrcmpi(pPath + 1, c_szColonSlash))                  // "X:\" case
            return TRUE;
    }

    if ((*pPath == TEXT('\\')) && (*(pPath + 1) == 0))        // "\" case
        return TRUE;

    if (DBL_BSLASH(pPath))      // smells like UNC name
    {
        LPCTSTR p;
        int cBackslashes = 0;

        for (p = pPath + 2; *p; p = CharNext(p)) {
            if (*p == TEXT('\\') && (++cBackslashes > 1))
               return FALSE;   /* not a bare UNC name, therefore not a root dir */
        }
        return TRUE;    /* end of string with only 1 more backslash */
                        /* must be a bare UNC, which looks like a root dir */
    }
    return FALSE;
}

BOOL OnExtList(LPCTSTR pszExtList, LPCTSTR pszExt)
{
    for (; *pszExtList; pszExtList += lstrlen(pszExtList) + 1)
    {
        if (!lstrcmpi(pszExt, pszExtList))
        {
            return TRUE;        // yes
        }
    }

    return FALSE;
}

#pragma data_seg(".text", "CODE")
// what about .cmd?
const TCHAR achExes[] = TEXT(".bat\0.pif\0.exe\0.com\0");
#pragma data_seg()

// determine if a path is a program by looking at the extension
//
BOOL PathIsExe(LPCTSTR szFile)
{
    LPCTSTR temp = PathFindExtension(szFile);
    return OnExtList((LPCTSTR) achExes, temp);
}
*)