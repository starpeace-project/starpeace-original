unit ExcMagicUtils;

{$I ExcMagicDefines.inc}

interface

uses Windows, SysUtils;

{$IFDEF EXCMAGIC_DEBUG}
  procedure DebugMsg( Msg: String );
  procedure DebugFmt( Fmt: String; Args: array of const );
{$ENDIF}
  function  DumpBytes( Addr: Pointer; Size: Integer ): String;
  function  WriteMem( Address,NewBytes: Pointer; Size: Integer ): Boolean;
  function  ReadMem( Address,Dest: Pointer; Size: Integer ): Boolean;
  function  CompareMem( Ptr1,Ptr2: Pointer; Size: Integer ): Integer;
  function  LocalTimeStr: String;
  function  StrChar2Char( Src: String; Char1,Char2: Char ): String;
{$IFNDEF EXCMAGIC_Delphi5plus}
  procedure FreeAndNil(var Obj);
{$ENDIF}

implementation

{$IFDEF EXCMAGIC_DEBUG}
procedure DebugMsg( Msg: String );
const
  CRLF: Word = $0A0D;
  SDebugFile = 'ExcMagic.Debug';
var
  DebugFile: THandle;
  Written: Cardinal;
begin
  DebugFile := CreateFile( SDebugFile, GENERIC_READ or GENERIC_WRITE,
                     0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if DebugFile <> INVALID_HANDLE_VALUE then
    begin
      SetFilePointer( DebugFile, 0, nil, FILE_END );
      WriteFile( DebugFile, Msg[1], Length(Msg), Written, nil);
      WriteFile( DebugFile, CRLF, SizeOf(CRLF), Written, nil);
      CloseHandle( DebugFile );
    end;
end;

procedure DebugFmt( Fmt: String; Args: array of const );
begin
  DebugMsg( Format( Fmt, Args ) );
end;
{$ENDIF}

function WriteMem( Address,NewBytes: Pointer; Size: Integer ): Boolean;
begin
  Result := False;
  if (Address <> nil) and (NewBytes <> nil) then
    Result := WriteProcessMemory( GetCurrentProcess, Address, NewBytes, Size, DWORD(nil^) );
end;

function ReadMem( Address,Dest: Pointer; Size: Integer ): Boolean;
begin
  Result := False;
  if (Address <> nil) and (Dest <> nil) then
    Result := ReadProcessMemory( GetCurrentProcess, Address, Dest, Size, DWORD(nil^) );
end;

function CompareMem( Ptr1,Ptr2: Pointer; Size: Integer ): Integer;
type
  PByte = ^Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Size-1 do
    begin
      Result := PByte(Ptr1)^ - PByte(Ptr2)^;
      if Result <> 0 then Break;
      Inc(PByte(Ptr1));
      Inc(PByte(Ptr2));
    end;
end;

function DumpBytes( Addr: Pointer; Size: Integer ): String;
var
  i: Integer;
  B: Byte;
begin
  if Addr = nil then Result := 'nil'
  else
    begin
      Result := '';
      for i := 0 to Size-1 do
        begin
          if ReadMem( Addr, @B, 1 ) then
            Result := Result + Format('%.2X ',[B])
          else
            begin
              Result := Result + 'err';
              Break;
            end;
          Inc(Dword(Addr));
        end;
    end;
end;

function LocalTimeStr: String;
var
  STime: TSystemTime;
begin
  GetLocalTime( STime );
  with STime do
    Result := Format('%.2d:%.2d:%.2d %.2d/%.2d/%.4d',
      [ wHour,wMinute,wSecond,wDay,wMonth,wYear ]);
end;

function StrChar2Char( Src: String; Char1,Char2: Char ): String;
var
  i: Integer;
begin
  Result := Src;
  for i := 1 to Length(Src) do
    if Result[i] = Char1 then Result[i] := Char2;
end;

{$IFNDEF EXCMAGIC_Delphi5plus}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;
{$ENDIF}

end.
