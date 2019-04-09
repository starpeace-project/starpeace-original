unit ClassStorage;

interface

  uses
    ClassStorageInt, Variants;

{$ifdef NotStorageDll}
  function  TheClassStorage : TClassStorage;
  procedure InitTheClassStorage;
  procedure DoneTheClassStorage;
  function  TheGlobalConfigHandler : IConfigHandler;
  procedure SetGlobalConfigHandler( Handler : IConfigHandler );
{$else}
  function  TheClassStorage : TClassStorage; stdcall;
  procedure InitTheClassStorage;             stdcall;
  procedure DoneTheClassStorage;             stdcall;
  function  TheGlobalConfigHandler : IConfigHandler;            stdcall;
  procedure SetGlobalConfigHandler( Handler : IConfigHandler ); stdcall;
{$endif}
implementation

{$ifdef NotStorageDll}
uses
  NativeClassStorage;

var
  TheNativeClassStorage : TClassStorage  = nil;
  GlobalConfigHandler   : IConfigHandler = nil;

function TheClassStorage : TClassStorage;
  begin
    result := TheNativeClassStorage;
  end;

function TheGlobalConfigHandler : IConfigHandler;
  begin
    result := GlobalConfigHandler;
  end;

procedure SetGlobalConfigHandler( Handler : IConfigHandler );
  begin
    GlobalConfigHandler := Handler;
  end;

procedure InitTheClassStorage;
  begin
    TheNativeClassStorage := TNativeClassStorage.Create;
  end;

procedure DoneTheClassStorage;
  begin
    TheNativeClassStorage.Free;
  end;

{$else}
  function  TheClassStorage : TClassStorage;                    stdcall; external 'classstorage.dll';
  procedure InitTheClassStorage;                                stdcall; external 'classstorage.dll';
  procedure DoneTheClassStorage;                                stdcall; external 'classstorage.dll';
  function  TheGlobalConfigHandler : IConfigHandler;            stdcall; external 'classstorage.dll';
  procedure SetGlobalConfigHandler( Handler : IConfigHandler ); stdcall; external 'classstorage.dll';
{$endif}

end.
