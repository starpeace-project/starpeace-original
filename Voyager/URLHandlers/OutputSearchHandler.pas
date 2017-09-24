unit OutputSearchHandler;

interface

  uses
    Classes, VoyagerServerInterfaces, VoyagerInterfaces, Controls,
    OutputSearchHandlerViewer, VCLUtils;

  const
    tidParmName_xPos       = 'x';
    tidParmName_yPos       = 'y';
    tidParmName_Fluid      = 'Fluid';
    tidParmName_Town       = 'Town';
    tidParmName_Owner      = 'Owner';
    tidParmName_Roles      = 'Roles';
    tidParmName_Count      = 'Count';
    tidParmName_World      = 'WorldName';
    tidParmName_ActionPage = 'ActionPage';

  const
    htmlAction_FindOutputs = 'FINDSUPPLIERS';

  type
    TMetaOutputSearchHandler =
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

    TOutputSearchHandler =
      class( TInterfacedObject, IURLHandler )
        public
          constructor Create(MetaHandler : TMetaOutputSearchHandler);
          destructor  Destroy; override;
        private
          fMasterURLHandler : IMasterURLHandler;
          fControl          : TOutputSearchViewer;
          fClientView       : IClientView;
          fxPos             : integer;
          fyPos             : integer;
          fFluid            : string;
          fWorld            : string;
          fActionPage       : string;
          {
          fTown             : string;
          fOwner            : string;
          fRoles            : integer;
          fCount            : integer;
          }
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
      end;

implementation

  uses
    SysUtils, URLParser, ServerCnxHandler, ServerCnxEvents, Events, Protocol;

  // TMetaOutputSearchHandler

  constructor TMetaOutputSearchHandler.Create;
    begin
      inherited;
    end;

  destructor TMetaOutputSearchHandler.Destroy;
    begin
      inherited;
    end;

  function TMetaOutputSearchHandler.getName : string;
    begin
      result := tidHandlerName_SupplyFinder;
    end;

  function TMetaOutputSearchHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaOutputSearchHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaOutputSearchHandler.Instantiate : IURLHandler;
    begin
      result := TOutputSearchHandler.Create(self);
    end;


  // TOutputSearchHandler

  constructor TOutputSearchHandler.Create(MetaHandler : TMetaOutputSearchHandler);
    begin
      inherited Create;
      fControl := TOutputSearchViewer.Create( nil );
      //fControl.URLHadler := self;
    end;

  destructor TOutputSearchHandler.Destroy;
    begin
      RemoveComponentFreeAndNil(fControl);
      inherited;
    end;

  function TOutputSearchHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      try
        if GetURLAction(URL) = htmlAction_FindOutputs
          then
            try
              fXPos       := StrToInt(GetParmValue(URL, tidParmName_xPos));
              fYPos       := StrToInt(GetParmValue(URL, tidParmName_yPos));
              fFluid      := GetParmValue(URL, tidParmName_Fluid);
              fWorld      := GetParmValue(URL, tidParmName_World);
              fActionPage := GetParmValue(URL, tidParmName_ActionPage);
              fControl.InitViewer(fClientView, fXPos, fYPos, fWorld, fFluid, fActionPage);
              fControl.MasterURLHandler := fMasterURLHandler;
              result := urlHandled;
            except
              result := urlNotHandled;
            end
          else result := urlNotHandled;
      except
        result := urlNotHandled;
      end;
    end;

  function TOutputSearchHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      msg : TLinkSearchMsg;
    begin
      result := evnNotHandled;
      case EventId of
        evnRenderInputSuppliers :
          begin
            msg := TLinkSearchMsg(info);
            fControl.RenderResults(msg.Fluid, msg.Result);
            result := evnHandled;
          end;
        evnShutDown :   //.rag
          begin
            fMasterURLHandler := nil;
            fClientView       := nil;
            RemoveComponentFreeAndNil(fControl); //.rag (Ojo, no cambiar sino no se libera pila de basuras)
          end;
      end;
    end;

  function TOutputSearchHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TOutputSearchHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

end.
