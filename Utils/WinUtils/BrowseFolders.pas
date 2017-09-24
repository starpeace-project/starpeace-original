unit BrowseFolders;

// Copyright (c) 1997 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, ShellAPI, ShlObj, SysUtils, Classes, Forms,
    PidlPath;

  function BrowseNetworkFolder( const BrowseTitle : string ) : string;
  function BrowseLocalFolder( const BrowseTitle : string ) : string;
  function BrowseFolder( const BrowseTitle : string ) : string;
  function BrowseFolderDialog( const BrowseTitle : string; RootFolder : PItemIDList; Options : integer ) : string;

  type
    TBrowseCallbackProc = function ( WinHandle : HWND; uMsg : uint; Param, lpData : LPARAM ) : integer; stdcall;

  // In case you need to specify a parent or a callback function:
  function BrowseFolderDialogEx( ParentHandle : HWND; const BrowseTitle : string; RootFolder : PItemIDList; Options : integer; Callback : TBrowseCallbackProc; lpData : uint ) : string;
  function BrowseNetworkFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;
  function BrowseLocalFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;
  function BrowseFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;

implementation

  function BrowseFolderDialog( const BrowseTitle : string; RootFolder : PItemIDList; Options : integer ) : string;
    begin
      Result := BrowseFolderDialogEx( Screen.ActiveForm.Handle, BrowseTitle, RootFolder, Options, nil, 0 );
    end;

  function BrowseNetworkFolder( const BrowseTitle : string ) : string;
    begin
      Result := BrowseNetworkFolderEx( Screen.ActiveForm.Handle, BrowseTitle, nil, 0 );
    end;

  function BrowseLocalFolder( const BrowseTitle : string ) : string;
    begin
      Result := BrowseLocalFolderEx( Screen.ActiveForm.Handle, BrowseTitle, nil, 0 );
    end;

  function BrowseFolder( const BrowseTitle : string ) : string;
    begin
      Result := BrowseFolderEx( Screen.ActiveForm.Handle, BrowseTitle, nil, 0 );
    end;

  function BrowseFolderDialogEx( ParentHandle : HWND; const BrowseTitle : string; RootFolder : PItemIDList; Options : integer; Callback : TBrowseCallbackProc; lpData : uint ) : string;
    var
      BrowseInfo : TBrowseInfo;
      SelName    : array [0..MAX_PATH] of char;
    begin
      with BrowseInfo do
        begin
          hwndOwner      := ParentHandle;
          pidlRoot       := RootFolder;
          pszDisplayName := SelName;
          lpszTitle      := pchar( BrowseTitle );
          ulFlags        := Options;
          lpfn           := Callback;
          lParam         := lpData;
        end;
      Result := PidlToFilePath( SHBrowseForFolder( BrowseInfo ) );
    end;

  function BrowseNetworkFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;
    var
      Root : PItemIDList;
    begin
      SHGetSpecialFolderLocation( ParentHandle, CSIDL_NETWORK, Root );
      Result := BrowseFolderDialogEx( ParentHandle, BrowseTitle, Root, BIF_RETURNONLYFSDIRS, Callback, lpData );
    end;

  function BrowseLocalFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;
    var
      Root : PItemIDList;
    begin
      SHGetSpecialFolderLocation( ParentHandle, CSIDL_DRIVES, Root );
      Result := BrowseFolderDialogEx( ParentHandle, BrowseTitle, Root, BIF_RETURNONLYFSDIRS, Callback, lpData );
    end;

  function BrowseFolderEx( ParentHandle : HWND; const BrowseTitle : string; Callback : TBrowseCallbackProc; lpData : uint ) : string;
    begin
      Result := BrowseFolderDialogEx( ParentHandle, BrowseTitle, nil, BIF_RETURNONLYFSDIRS, Callback, lpData );
    end;

end.
