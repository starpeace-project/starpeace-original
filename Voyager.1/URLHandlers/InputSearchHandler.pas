unit InputSearchHandler;

interface

  uses
    Classes, VoyagerServerInterfaces, VoyagerInterfaces, Controls,
    InputSearchHandlerViewer;

  const
    tidParmName_xPos  = 'x';
    tidParmName_yPos  = 'y';
    tidParmName_Fluid = 'Fluid';
    tidParmName_Town  = 'Town';
    tidParmName_Owner = 'Owner';
    tidParmName_Roles = 'Roles';
    tidParmName_Count = 'Count';
    tidParmName_World = 'WorldName';

  const
    htmlAction_FindInputs = 'FINDCLIENTS';

  type
    TMetaInputSearchHandler =
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

    TInputSearchHandler =
      class( TInterfacedObject, IURLHandler )
        private
          constructor Create(MetaHandler : TMetaInputSearchHandler);
          destructor  Destroy; override;
        private
          fMasterURLHandler : IMasterURLHandler;
          fControl          : TInputSearchViewer;
          fClientView       : IClientView;
          fxPos             : integer;
          fyPos             : integer;
          fFluid            : string;
          fWorld            : string;
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
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
      end;

implementation

  uses
    SysUtils, URLParser, ServerCnxHandler, ServerCnxEvents, Events, Protocol;

  // TMetaInputSearchHandler

  constructor TMetaInputSearchHandler.Create;
    begin
      inherited;
    end;

  destructor TMetaInputSearchHandler.Destroy;
    begin
      inherited;
    end;

  function TMetaInputSearchHandler.getName : string;
    begin
      result := tidHandlerName_ClientFinder;
    end;

  function TMetaInputSearchHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaInputSearchHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaInputSearchHandler.Instantiate : IURLHandler;
    begin
      result := TInputSearchHandler.Create(self);
    end;


  // TInputSearchHandler

  constructor TInputSearchHandler.Create(MetaHandler : TMetaInputSearchHandler);
    begin
      inherited Create;
      fControl := TInputSearchViewer.Create( nil );
      //fControl.URLHadler := self;
    end;

  destructor TInputSearchHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  function TInputSearchHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      try
        if GetURLAction(URL) = htmlAction_FindInputs
          then
            try
              fXPos  := StrToInt(GetParmValue(URL, tidParmName_xPos));
              fYPos  := StrToInt(GetParmValue(URL, tidParmName_yPos));
              fFluid := GetParmValue(URL, tidParmName_Fluid);
              fWorld := GetParmValue(URL, tidParmName_World);
              fControl.InitViewer(fClientView, fXPos, fYPos, fWorld, fFluid);
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

  function TInputSearchHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      msg : TLinkSearchMsg;
    begin
      result := evnNotHandled;
      case EventId of
        evnRenderOutputClients :
          begin
            msg := TLinkSearchMsg(info);
            fControl.RenderResults(msg.Fluid, msg.Result);
            result := evnHandled;
          end;
      end;
    end;

  function TInputSearchHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TInputSearchHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fControl.MasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

end.
