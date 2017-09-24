unit VisualClassesHandler;

interface

  uses
    VoyagerInterfaces, Classes, IniClasses, Controls, VisualClassManager, VCLUtils;

  type
    TVisualClassesHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
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
          fCurrClass        : TVisualClass;
          fCurrClassId      : integer;
      end;

  const
    tidMetaHandler_VisualClassesHandler = 'VisualClassesHandler';

  const
    evnAnswerVisualClassData = 7000;

  type
    TValueType = (vtInteger, vtBoolean, vtString);

  type
    TVisualClassItem =
      record
        ClassId   : integer;    
        ValueName : string;
        Section   : string;
        case ValueType : TValueType of
          vtInteger :
            ( IntValue    : integer;
              IntDefValue : integer );
          vtBoolean :
            ( BoolValue    : boolean;
              BoolDefValue : boolean );
          vtString :
            ( StrValue    : string[255];
              StrDefValue : string[255] );
      end;


  // Useful functions. (The best way to use this.)

  function ReadInteger( ClassId : integer; Section, Value : string; DefValue : integer; MasterHandler : IURLHandler ) : integer;
  function ReadBoolean( ClassId : integer; Section, Value : string; DefValue : boolean; MasterHandler : IURLHandler ) : boolean;
  function ReadString ( ClassId : integer; Section, Value : string; DefValue : string;  MasterHandler : IURLHandler ) : string;
  

implementation

  uses
    VoyagerEvents, LocalCacheManager;


  // TVisualClassesHandler

  function TVisualClassesHandler.getName : string;
    begin
      result := tidMetaHandler_VisualClassesHandler;
    end;

  function TVisualClassesHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonVisual];
    end;

  function TVisualClassesHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TVisualClassesHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TVisualClassesHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TVisualClassesHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      Item        : TVisualClassItem absolute info;
      //ClassesPath : string;
    begin
      if EventId = evnAnswerVisualClassData
        then
          begin
            if fCurrClassId <> Item.ClassId
              then
                begin
                  fCurrClassId := Item.ClassId;
                  fCurrClass   := LocalCacheManager.ClassManager.ClassById[fCurrClassId];
                end;
            case Item.ValueType of
              vtInteger :
                if fCurrClass <> nil
                  then Item.IntValue := fCurrClass.ReadInteger( Item.Section, Item.ValueName, Item.IntDefValue )
                  else Item.IntValue := Item.IntDefValue;
              vtBoolean :
                if fCurrClass <> nil
                  then Item.BoolValue := fCurrClass.ReadBool( Item.Section, Item.ValueName, Item.BoolDefValue )
                  else Item.BoolValue := Item.BoolDefValue;
              vtString :
                if fCurrClass <> nil
                  then Item.StrValue := fCurrClass.ReadString( Item.Section, Item.ValueName, Item.StrDefValue )
                  else Item.StrValue := Item.StrDefValue;
            end;
            result := evnHandled;
          end
        else
          begin
            result := evnNotHandled;
            if EventId = evnShutDown
                then fMasterURLHandler := nil  //.rag
          end;
    end;

  function TVisualClassesHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TVisualClassesHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;


  // Useful functions

  function ReadInteger( ClassId : integer; Section, Value : string; DefValue : integer; MasterHandler : IURLHandler ) : integer;
    var
      Item : TVisualClassItem;
    begin
      Item.ClassId     := ClassId;
      Item.ValueName   := Value;
      Item.Section     := Section;
      Item.ValueType   := vtInteger;
      Item.IntDefValue := DefValue;
      MasterHandler.HandleEvent( evnAnswerVisualClassData, Item );
      result := Item.IntValue;
    end;

  function ReadBoolean( ClassId : integer; Section, Value : string; DefValue : boolean; MasterHandler : IURLHandler ) : boolean;
    var
      Item : TVisualClassItem;
    begin
      Item.ClassId      := ClassId;
      Item.ValueName    := Value;
      Item.Section      := Section;
      Item.ValueType    := vtBoolean;
      Item.BoolDefValue := DefValue;
      MasterHandler.HandleEvent( evnAnswerVisualClassData, Item );
      result := Item.BoolValue;
    end;

  function ReadString( ClassId : integer; Section, Value : string; DefValue : string; MasterHandler : IURLHandler ) : string;
    var
      Item : TVisualClassItem;
    begin
      Item.ClassId     := ClassId;
      Item.ValueName   := Value;
      Item.Section     := Section;
      Item.ValueType   := vtString;
      Item.StrDefValue := DefValue;
      MasterHandler.HandleEvent( evnAnswerVisualClassData, Item );
      result := Item.StrValue;
    end;

    
end.



