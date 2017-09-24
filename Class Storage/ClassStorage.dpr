library ClassStorage;

uses
  ShareMem,
  SysUtils,
  Classes,
  ClassStorageInt in 'ClassStorageInt.pas',
  NativeClassStorage in 'NativeClassStorage.pas';

var
  TheNativeClassStorage : TClassStorage  = nil;
  GlobalConfigHandler   : IConfigHandler = nil;

function TheClassStorage : TClassStorage; stdcall; export;
  begin
    result := TheNativeClassStorage;
  end;

function TheGlobalConfigHandler : IConfigHandler; stdcall; export;
  begin
    result := GlobalConfigHandler;
  end;

procedure SetGlobalConfigHandler( Handler : IConfigHandler ); stdcall; export;
  begin
    GlobalConfigHandler := Handler;
  end;

procedure InitTheClassStorage; stdcall; export;
  begin
    TheNativeClassStorage := TNativeClassStorage.Create;
  end;
                                                               
procedure DoneTheClassStorage; stdcall; export;
  begin
    TheNativeClassStorage.Free;
  end;

exports
  TheClassStorage,
  TheGlobalConfigHandler,
  SetGlobalConfigHandler,
  InitTheClassStorage,
  DoneTheClassStorage;

begin
  IsMultiThread := true;
end.


