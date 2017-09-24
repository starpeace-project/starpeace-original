{+--------------------------------------------------------------------------+
 | Class:       TIDEStream
 | Created:     8.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: A simple and effective interface to the IDE's text buffer
 | Version:     1.0
 | Status:      FreeWare
 | Disclaimer:
 | This is provided as is, expressly without a warranty of any kind.
 | You use it at your own risc.
 +--------------------------------------------------------------------------+}
unit mwIDEStream;

interface

uses
  Windows, 
  SysUtils, 
  Messages, 
  Classes,
  ToolIntf,
  EditIntf,
  ExptIntf;

type
  TIDEStream = class(TMemoryStream)
  private
    fStreamTextLen:Longint;
    function GetAsPChar:PChar;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteText(Text: PChar);
    property Capacity;
    property AsPChar:PChar read GetAsPChar;
    function GetText:PChar;
    property StreamTextLen:Longint read fStreamTextLen;
  published
  end;

var
  fToolServices : TIToolServices;
  fModuleInterface: TIModuleInterface;
  fEditorInterface: TIEditorInterface;
  ActualReader: TIEditReader;
  ActualWriter: TIEditWriter;

implementation

function GetProjectName: String;
begin
  Result:= fToolServices.GetProjectName;
end;  { GetProjectName }

function GetCurrentFile: String;
begin
  Result:= fToolServices.GetCurrentFile;
end;   { GetCurrentFile }

function GetToolServieces:TIToolServices;
var
  FileExt: String;
begin
  fToolServices:= ExptIntf.ToolServices;
 if GetProjectName = '' then raise exception.create('Sorry, a project must be open');
   FileExt:= ExtractFileExt(GetCurrentFile);
  if FileExt = '.dfm' then raise exception.create('Sorry, must be a PAS or DPR file');

end;  { GetToolServieces }

procedure GetEditReader;
begin
  GetToolServieces;
  fModuleInterface:= fToolServices.GetModuleInterface(GetCurrentFile);
  fEditorInterface:= fModuleInterface.GetEditorInterface;
  ActualReader:= fEditorInterface.CreateReader;
end;  { GetEditReader }

procedure GetEditWriter;
begin
  GetToolServieces;
  fModuleInterface:= fToolServices.GetModuleInterface(GetCurrentFile);
  fEditorInterface:= fModuleInterface.GetEditorInterface;
  ActualWriter:= fEditorInterface.CreateWriter;
end;  { GetEditWriter }

procedure FreeEditReader;
begin
  ActualReader.Free;
  fEditorInterface.Free;
  fModuleInterface.Free;
end;  { GetEditorInterface }

procedure FreeEditWriter;
begin
  ActualWriter.Free;
  fEditorInterface.Free;
  fModuleInterface.Free;
end;  { GetEditorInterface }

destructor TIDEStream.Destroy;
begin
  inherited Destroy;
end;  { Destroy }

constructor TIDEStream.Create;
begin
  inherited Create;
  fStreamTextLen:= 0;
end;  { Create }

function TIDEStream.GetAsPChar:PChar;
const
  TheEnd: Char = #0;
begin
  Position:= Size;
  Write(TheEnd, 1);
  SetPointer(Memory, Size -1);
  Result:= Memory;
end;  { GetAsPChar }

function TIDEStream.GetText:PChar;
const
  BuffLen = 16383;
var
  TextBuffer: PChar;
  Readed, BuffPos: LongInt;
begin
  Clear;
  GetMem(TextBuffer, BuffLen +1);
  BuffPos:= 0;
  GetEditReader;
  try
    repeat
      Readed:= ActualReader.GetText(BuffPos, TextBuffer, Bufflen);
      Write(TextBuffer^, Readed);
      inc(BuffPos, Readed);
    until Readed < BuffLen;
  finally
  FreeEditReader;
  FreeMem(TextBuffer, BuffLen +1);
  end;
  fStreamTextLen:= Size;
  Result:= AsPchar;
end;

procedure TIDEStream.WriteText(Text: PChar);
begin
  GetEditWriter;
  try
    ActualWriter.CopyTo(0);
    ActualWriter.DeleteTo(fStreamTextLen -1);
    ActualWriter.Insert(Text);
  finally
    FreeEditWriter;
  end;
end;

end.
