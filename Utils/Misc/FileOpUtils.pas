unit FileOpUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, ShellApi;

  function CopyFile( const SourceName, DestName : string; Flags : dword ) : boolean;
  function MoveFile( const SourceName, DestName : string; Flags : dword ) : boolean;
  function RenameFile( const SourceName, DestName : string; Flags : dword ) : boolean;
  function DeleteFile( const SourceName : string; Flags : dword ) : boolean;

  // Use these in case you have more than one file to process (The format is file1#0file2#0...filen#0#0)
  // You should use the function JoinStringList( List : TStringList ),
  // or JoinStrings( Strings : array of string ), both in the unit StrUtils

  function CopyFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function MoveFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function RenameFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function DeleteFiles( SourceName : pchar; Flags : dword ) : boolean;

  // In case you need to specify a Parent:
  function CopyFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
  function MoveFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
  function RenameFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
  function DeleteFileEx( ParentHandle : HWND; const SourceName : string; Flags : dword ) : boolean;

  function CopyFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function MoveFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function RenameFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
  function DeleteFilesEx( ParentHandle : HWND; SourceName : pchar; Flags : dword ) : boolean;

implementation

  uses
    Forms;
    
  function CopyFile( const SourceName, DestName : string; Flags : dword ) : boolean;
    begin
      Result := CopyFileEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function MoveFile( const SourceName, DestName : string; Flags : dword ) : boolean;
    begin
      Result := MoveFileEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function RenameFile( const SourceName, DestName : string; Flags : dword ) : boolean;
    begin
      Result := RenameFileEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function DeleteFile( const SourceName : string; Flags : dword ) : boolean;
    begin
      Result := DeleteFileEx( Screen.ActiveForm.Handle, SourceName, Flags );
    end;

  function CopyFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    begin
      Result := CopyFilesEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function MoveFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    begin
      Result := MoveFilesEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function RenameFiles( SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    begin
      Result := RenameFilesEx( Screen.ActiveForm.Handle, SourceName, DestName, Flags );
    end;

  function DeleteFiles( SourceName : pchar; Flags : dword ) : boolean;
    begin
      Result := DeleteFilesEx( Screen.ActiveForm.Handle, SourceName, Flags );
    end;

  function CopyFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
    var
      fos      : TSHFILEOPSTRUCT;
      Filename : string;
    begin
      SetString( Filename, pchar(SourceName), length( SourceName ) + 1 );
      pchar(Filename)[length(SourceName) + 1] := #0;
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_COPY;
          pFrom  := pchar( Filename );
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function MoveFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
    var
      fos      : TSHFILEOPSTRUCT;
      Filename : string;
    begin
      SetString( Filename, pchar(SourceName), length( SourceName ) + 1 );
      pchar(Filename)[length(SourceName) + 1] := #0;
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_MOVE;
          pFrom  := pchar( Filename );
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function RenameFileEx( ParentHandle : HWND; const SourceName, DestName : string; Flags : dword ) : boolean;
    var
      fos      : TSHFILEOPSTRUCT;
      Filename : string;
    begin
      SetString( Filename, pchar(SourceName), length( SourceName ) + 1 );
      pchar(Filename)[length(SourceName) + 1] := #0;
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_MOVE;
          pFrom  := pchar( Filename );
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function DeleteFileEx( ParentHandle : HWND; const SourceName : string; Flags : dword ) : boolean;
    var
      fos      : TSHFILEOPSTRUCT;
      Filename : string;
    begin
      SetString( Filename, pchar(SourceName), length( SourceName ) + 1 );
      pchar(Filename)[length(SourceName) + 1] := #0;
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_DELETE;
          pFrom  := pchar( Filename );
          pTo    := nil;
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  // Multiple files

  function CopyFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    var
      fos : TSHFILEOPSTRUCT;
    begin
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_COPY;
          pFrom  := SourceName;
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function MoveFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    var
      fos : TSHFILEOPSTRUCT;
    begin
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_MOVE;
          pFrom  := SourceName;
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function RenameFilesEx( ParentHandle : HWND; SourceName : pchar; const DestName : string; Flags : dword ) : boolean;
    var
      fos : TSHFILEOPSTRUCT;
    begin
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_MOVE;
          pFrom  := SourceName;
          pTo    := pchar( DestName );
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

  function DeleteFilesEx( ParentHandle : HWND; SourceName : pchar; Flags : dword ) : boolean;
    var
      fos : TSHFILEOPSTRUCT;
    begin
      with fos do
        begin
          Wnd    := ParentHandle;
          wFunc  := FO_DELETE;
          pFrom  := SourceName;
          pTo    := nil;
          if Flags = 0
            then fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_SILENT
            else fFlags := Flags;
          hNameMappings := nil;
          lpszProgressTitle := nil;
        end;
      Result := SHFileOperation( fos ) = 0;
    end;

end.
