unit ExcMemMap;

interface

uses
  Windows, SysUtils, Classes;

type
  EMMFileError = class( Exception );

  TMMFileStream = class( TStream )
  private
    FFile     : THandle;
    FMode     : Dword;
    FFileSize : Dword;
    FCurOffset: Dword;
    FMapping  : THandle;
    FView     : Pointer;
    FViewOfs  : Dword;
    FViewSize : Dword;
    FGranula  : Dword;
    FMaxView  : Dword;
    FReviews  : Dword;
    procedure ReView(DesiredOffset,DesiredDataSize: Dword );
    function  GetRawData(Offset: Dword): Pointer;
    property  RawData[Offset: Dword]: Pointer read GetRawData;

  public
    constructor Create( FileName: String; Mode: Longint; MapName: String );
    destructor  Destroy; override;

    function    MapData( Offset,Size: Dword ): Pointer;
    procedure   UnMapData;

    function    ReadByte: Byte;
    function    ReadWord: Word;
    function    ReadDword: Dword;
    function    ReadPointer: Pointer;

  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

uses ExcMagicUtils;

const
  VIEW_SIZE = 1; // in granulas (each granula on all current win32 platforms = 64K)

{ TMMFileStream }

constructor TMMFileStream.Create( FileName: String; Mode: Longint; MapName: String );
const
  AccessMode: array[0..2] of DWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE
  );
  ShareMode: array[0..4] of DWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE
  );
  MapProtect: array[0..2] of Dword = (
    PAGE_READONLY,
    0,
    PAGE_READWRITE
  );
var
  si: TSystemInfo;
begin
  inherited Create;

  if (Mode and 3) = fmOpenWrite then
    raise EMMFileError.Create( 'Invalid file mode: fmOpenWrite' );

  GetSystemInfo( si );
  FGranula := si.dwAllocationGranularity;
  FMode    := Mode;
  FMaxView := 0;
  FReviews := 0;
{$IFDEF EXCMAGIC_DEBUG}
  DebugMsg( Format('MMF: granula 0x%X, viewsize 0x%X, creating %s',[FGranula,FGranula*VIEW_SIZE,FileName]) );
{$ENDIF}

  FFile := CreateFile( PChar(FileName),
                       AccessMode[Mode and 3],
                       ShareMode[(Mode and $F0) shr 4],
                       nil,
                       OPEN_EXISTING,
                       0,
                       0 );
  if FFile <> INVALID_HANDLE_VALUE then
    begin
      FFileSize := GetFileSize( FFile, nil );
      FCurOffset:= 0;
{$IFDEF EXCMAGIC_DEBUG}
      DebugMsg( Format('MMF: F_handle %X, mapping %s',[FFile,MapName]) );
{$ENDIF}
      FMapping  := CreateFileMapping( FFile,
                                      nil,
                                      MapProtect[Mode and 3],
                                      0,
                                      0,
                                      PChar(StrChar2Char(MapName,'\','_')) );
      if FMapping = 0 then
        begin
          CloseHandle( FFile );
          raise EMMFileError.Create( 'Can''t create file mapping for ' +
            FileName + ': ' + SysErrorMessage(GetLastError) );
        end;
{$IFDEF EXCMAGIC_DEBUG}
      DebugMsg( Format('MMF: M_handle %X, result %X',[FMapping,GetLastError]) );
{$ENDIF}
    end
  else
    raise EMMFileError.Create( 'Can''t open file ' +
      FileName + ': ' + SysErrorMessage(GetLastError) );
end;

destructor TMMFileStream.Destroy;
begin
{$IFDEF EXCMAGIC_DEBUG}
  DebugMsg( Format('MMF: destroying. file %X, map %X, view %p',[FFile,FMapping,FView]) );
  DebugMsg( Format('     stat: maxviewsize %d, reviews %d', [FMaxView,FReviews]) );
{$ENDIF}
  if FView <> nil then UnmapViewOfFile( FView );
  CloseHandle( FMapping );
  CloseHandle( FFile    );
  inherited;
end;

procedure TMMFileStream.ReView( DesiredOffset,DesiredDataSize: Dword );
const
  DesiredAccess: array[0..2] of Dword = (
    FILE_MAP_READ,
    FILE_MAP_WRITE,
    FILE_MAP_WRITE
  );
begin
  if (FView <> nil) and
     (DesiredOffset >= FViewOfs) and
     (DesiredOffset + DesiredDataSize <= FViewOfs + FViewSize) then Exit;
{
               view offset
             /
 |     |     |     |     |     |     |
 |     |     |     |     |     |     |
                 |###|
                 |###|
                 ^
                 desired offset and size
}

  UnMapData;
  { move left to nearest granula }
  FViewOfs  := (DesiredOffset div FGranula) * FGranula;
  { }
  FViewSize := ((DesiredOffset + DesiredDataSize - FViewOfs) div (FGranula*VIEW_SIZE) + 1) * (FGranula*VIEW_SIZE);
  if Dword(FViewOfs + FViewSize) > FFileSize then FViewSize := FFileSize - Dword(FViewOfs);

  // some statistics
  if FViewSize > FMaxView then FMaxView := FViewSize;
  Inc( FReviews );

  FView := MapViewOfFile( FMapping,
                          DesiredAccess[FMode and 3],
                          0,
                          FViewOfs,
                          FViewSize
                          );
  if FView = nil then
    begin
      raise EMMFileError.Create( 'Can''t create file view: ' + SysErrorMessage(GetLastError) );
    end;
end;

function TMMFileStream.GetRawData(Offset: Dword): Pointer;
begin
  Result := Pointer( Dword(FView) + Offset - FViewOfs );
end;

function TMMFileStream.MapData(Offset,Size: Dword): Pointer;
begin
  ReView( Offset, Size );
  Result := RawData[Offset];
end;

procedure TMMFileStream.UnMapData;
begin
  if FView <> nil then UnmapViewOfFile( FView );
  FView := nil;
end;

function TMMFileStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning : FCurOffset := Offset;
    soFromCurrent   : FCurOffset := Longint(FCurOffset) + Offset;
    soFromEnd       : FCurOffset := Longint(FFileSize)  + Offset;
  end;
  Result := FCurOffset;
end;

function TMMFileStream.Write(const Buffer; Count: Integer): Longint;
begin
  Inc( FCurOffset, Count );
  Result := Count;
end;

function TMMFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  ReView( FCurOffset, Count );
  Move( RawData[FCurOffset]^, Buffer, Count );
  Inc( FCurOffset, Count );
  Result := Count;
end;

function TMMFileStream.ReadByte: Byte;
begin
  ReView( FCurOffset, SizeOf(Byte) );
  Result := PByte( RawData[FCurOffset] )^;
  Inc( FCurOffset, SizeOf(Byte) );
end;

function TMMFileStream.ReadDword: Dword;
begin
  ReView( FCurOffset, SizeOf(Dword) );
  Result := PDword( RawData[FCurOffset] )^;
  Inc( FCurOffset, SizeOf(Dword) );
end;

function TMMFileStream.ReadPointer: Pointer;
type
  PPointer = ^Pointer;
begin
  ReView( FCurOffset, SizeOf(Pointer) );
  Result := PPointer( RawData[FCurOffset] )^;
  Inc( FCurOffset, SizeOf(Pointer) );
end;

function TMMFileStream.ReadWord: Word;
begin
  ReView( FCurOffset, SizeOf(Word) );
  Result := PWord( RawData[FCurOffset] )^;
  Inc( FCurOffset, SizeOf(Word) );
end;


end.
