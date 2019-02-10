unit ConfigHandler;

interface

  uses
    VoyagerInterfaces, Config, Classes, Controls, Registry, VCLUtils;

  type
    TConfigHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, IConfigHolder )
        public
          constructor Create(TestVer : boolean);
        private
          fTestVer : boolean;
        // IConfigHandler
        private
          function  ReadString  ( System : boolean; DataOwner, DataId, DefaultValue : string ) : string;
          function  ReadInteger ( System : boolean; DataOwner, DataId : string; DefaultValue : integer ) : integer;
          function  ReadBoolean ( System : boolean; DataOwner, DataId : string; DefaultValue : boolean ) : boolean;
          procedure WriteString ( System : boolean; DataOwner, DataId, Value : string );
          procedure WriteInteger( System : boolean; DataOwner, DataId : string; Value : integer );
          procedure WriteBoolean( System : boolean; DataOwner, DataId : string; Value : boolean );
          procedure RetrieveUserList( UserList : TStrings );
        private
          function InitRegistry( System : boolean; DataOwner : string ) : TRegistry;
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
      end;

  const
    tidMetaHandler_ConfigHandler = 'ConfigHandler';

  const
    VOYAGER_ROOT = '\SOFTWARE\Wow6432Node\Starpeace\Client\';
    TEST_VOYAGER_ROOT = '\SOFTWARE\Wow6432Node\Starpeace Test\Client\';

implementation

  uses
    Windows, VoyagerEvents, URLParser, SysUtils;

  constructor TConfigHandler.Create(TestVer : boolean);
    begin
      inherited Create;
      fTestVer := TestVer;
    end;

  function TConfigHandler.ReadString( System : boolean; DataOwner, DataId, DefaultValue : string ) : string;
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          try
            result := Reg.ReadString( DataId );
            if result = ''
              then result := DefaultValue;
          except
            result := DefaultValue;
          end;
        finally
          Reg.Free;
        end;
      except
        result := DefaultValue;
      end;
    end;

  function TConfigHandler.ReadInteger( System : boolean; DataOwner, DataId : string; DefaultValue : integer ) : integer;
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          try
            result := Reg.ReadInteger( DataId );
          except
            result := DefaultValue;
          end;
        finally
          Reg.Free;
        end;
      except
        result := DefaultValue;
      end;
    end;

  function TConfigHandler.ReadBoolean( System : boolean; DataOwner, DataId : string; DefaultValue : boolean ) : boolean;
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          try
            result := Reg.ReadBool( DataId );
          except
            result := DefaultValue;
          end;
        finally
          Reg.Free;
        end;
      except
        result := DefaultValue;
      end;
    end;

  procedure TConfigHandler.WriteString( System : boolean; DataOwner, DataId, Value : string );
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          Reg.WriteString( DataId, Value );
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TConfigHandler.WriteInteger( System : boolean; DataOwner, DataId : string; Value : integer );
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          Reg.WriteInteger( DataId, Value );
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TConfigHandler.WriteBoolean( System : boolean; DataOwner, DataId : string; Value : boolean );
    var
      Reg : TRegistry;
    begin
      try
        Reg := InitRegistry( System, DataOwner );
        try
          Reg.WriteBool( DataId, Value );
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TConfigHandler.RetrieveUserList( UserList : TStrings );
    var
      Reg : TRegistry;
      //aUserList : TStrings;
      i : integer;
      KeyName : string;
    begin
      try
        Reg := InitRegistry( false, 'QuickLogon' );
        try
          //aUserList := TStringList.Create;
          Reg.GetKeyNames( UserList );
          {
          if aUserList.Count>0
            then
              begin
                for i:=0 to aUserList.Count-1 do
                  begin
                    KeyName := 'QuickLogon\' + aUserList[i];
                    if ReadString( false, KeyName, 'MasterUserName',  '')<>''
                      then UserList.Add(aUserList[i]);
                  end;
              end;
              }
        finally
          Reg.Free;
          // aUserList.Free;
        end;
      except
      end;
    end;
    
  function TConfigHandler.InitRegistry( System : boolean; DataOwner : string ) : TRegistry;
    var
      KeyName : string;
    begin
      result := TRegistry.Create;
      try
        if System
          then
            begin
              result.RootKey := HKEY_LOCAL_MACHINE;
              if fTestVer
                then KeyName := TEST_VOYAGER_ROOT + 'System\' + DataOwner
                else KeyName := VOYAGER_ROOT + 'System\' + DataOwner
            end
          else
            begin
              result.RootKey := HKEY_CURRENT_USER;
              if fTestVer
                then KeyName := TEST_VOYAGER_ROOT + 'Users\' + DataOwner
                else KeyName := VOYAGER_ROOT + 'Users\' + DataOwner;
            end;
        if not result.OpenKey( KeyName, true )
          then raise Exception.Create( 'Cannot open registry key: ' + KeyName );
      except
        result.Free;
        raise;
      end;
    end;

  function TConfigHandler.getName : string;
    begin
      result := tidMetaHandler_ConfigHandler;
    end;

  function TConfigHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopNonVisual];
    end;

  function TConfigHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TConfigHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TConfigHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TConfigHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      ConfigHolder : IConfigHolder absolute info;
      ChangeInfo : boolean absolute info;
    begin
      result := evnHandled;
      case EventId of
        evnAnswerConfigHolder :
          ConfigHolder := self;
        evnShutDown :
          fMasterURLHandler := nil;
        evnChangeConfig:
          fTestVer := ChangeInfo
        else
          result := evnHandled;
      end;
    end;

  function TConfigHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TConfigHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;


end.

