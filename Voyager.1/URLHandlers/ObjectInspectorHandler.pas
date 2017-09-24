unit ObjectInspectorHandler;

interface

  uses
    Classes, VoyagerServerInterfaces, VoyagerInterfaces, Controls,
    ObjectInspectorHandleViewer;

  const
    tidParmName_ClassId  = 'ClassId';
    tidParmName_ObjectId = 'ObjectId';
    tidParmName_xPos     = 'x';
    tidParmName_yPos     = 'y';

  const
    htmlAction_ShowObject = 'SHOWOBJECT';

  type
    TMetaObjectInpectorHandler =
      class( TInterfacedObject, IMetaURLHandler )
        public
          constructor Create;
          destructor Destroy; override;
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TObjectInpectorHandler =
      class( TInterfacedObject, IURLHandler )
        private
          constructor Create(MetaHandler : TMetaObjectInpectorHandler);
          destructor  Destroy; override;
        private
          fMasterURLHandler : IMasterURLHandler;
          fControl          : TObjectInspectorHandlerViewer;
          fClientView       : IClientView;
          fClassId          : integer;
          fObjectId         : integer;
          fXPos             : word;
          fYPos             : word;
          fLastWorld        : string;
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
      end;

  const
    tidHandlerName_ObjInspector = 'ObjectInspector';

implementation

  uses
    SysUtils, URLParser, ServerCnxHandler, ServerCnxEvents, Events, Protocol;

  // TMetaObjectInpectorHandler

  constructor TMetaObjectInpectorHandler.Create;
    begin
      inherited;
    end;

  destructor TMetaObjectInpectorHandler.Destroy;
    begin
      inherited;
    end;

  function TMetaObjectInpectorHandler.getName : string;
    begin
      result := tidHandlerName_ObjInspector;
    end;

  function TMetaObjectInpectorHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaObjectInpectorHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaObjectInpectorHandler.Instantiate : IURLHandler;
    begin
      result := TObjectInpectorHandler.Create(self);
    end;

  // TObjectInpectorHandler

  constructor TObjectInpectorHandler.Create(MetaHandler : TMetaObjectInpectorHandler);
    begin
      inherited Create;
      fControl := TObjectInspectorHandlerViewer.Create( nil );
      fControl.URLHadler := self;
    end;

  destructor TObjectInpectorHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  function TObjectInpectorHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      action : string;
    begin
      try
        action := GetURLAction(URL);
        if action = htmlAction_ShowObject
          then
            begin
              fXPos     := StrToInt(GetParmValue(URL, tidParmName_xPos));
              fYPos     := StrToInt(GetParmValue(URL, tidParmName_yPos));
              fClassId  := StrToInt(GetParmValue(URL, tidParmName_ClassId));
              fObjectId := StrToInt(GetParmValue(URL, tidParmName_ObjectId));
              try
                fControl.ChangeObject(fClientView, fXPos, fYPos, fClassId, fObjectId);
                result := urlNotHandled;
              except
                result := urlHandled;
              end;
            end
          else
            if (action = urlAction_FindSuppliers) or (action = urlAction_FindClients)
              then result := fControl.PropSheetContainer.HandleURL(URL, false)
              else result := fControl.PropSheetContainer.HandleURL(URL, true);
      except
        result := urlNotHandled;
      end;
    end;

  function TObjectInpectorHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      RefreshObject : TRefreshObjectInfo absolute info;
    begin
      result := evnHandled;
      try
        case EventId of
          evnRefresh :
            begin
              fControl.PropSheetContainer.Refresh;
              result := evnHandled;
            end;
          evnHandlerExposed :
            begin
              fControl.Exposed;
              if fLastWorld <> ''
                then
                  begin
                    if (fLastWorld <> fClientView.getDAAddr) and (fControl.PropSheetContainer <> nil)
                      then
                        begin
                          fLastWorld := fClientView.getDAAddr;
                          fControl.PropSheetContainer.ClearConnections;
                        end;
                  end
                else fLastWorld := fClientView.getDAAddr;
              result := evnHandled;
            end;
          evnHandlerUnexposed :
            begin
              fMasterURLHandler.HandleURL('?frame_Id=SupplyFinder&frame_Close=YES');
              fMasterURLHandler.HandleURL('?frame_Id=ClientFinder&frame_Close=YES');
              fControl.Unexposed;
              result := evnHandled;
            end;
          evnRefreshObject :
            if (RefreshObject.KindOfChange = fchStructure) and (RefreshObject.ObjId = fControl.PropSheetContainer.GetObjectId)
              then
                begin
                  fControl.PropSheetContainer.Refresh;
                  result := evnHandled;
                end;
          evnLogonStarted :
            begin
              fControl.PropSheetContainer.ClearConnections;
              fMasterURLHandler.HandleURL('?frame_Id=SupplyFinder&frame_Close=YES');
              fMasterURLHandler.HandleURL('?frame_Id=ClientFinder&frame_Close=YES');
              fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes' );
              result := evnHandled;
            end;
          else result := evnNotHandled;
        end;
      except
        result := evnNotHandled;
      end;
    end;

  function TObjectInpectorHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TObjectInpectorHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

end.
