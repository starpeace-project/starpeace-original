unit SheetHandlers;

interface

  uses
    Classes, Controls, VoyagerInterfaces, VoyagerServerInterfaces,
    ObjectInspectorInterfaces, SyncObjs, VCLUtils;

  const
    htmlAction_RefreshSheet = 'REFRESHSHEET';

  type
    TSheetHandler =
      class(TInterfacedObject, IPropertySheetHandler)
        protected
          fContainer  : IPropertySheetContainerHandler;
          fLoaded     : boolean;
          fLastUpdate : integer;
          fExposed    : boolean;
        public
          function  GetContainer : IPropertySheetContainerHandler;
          procedure SetContainer(aContainer : IPropertySheetContainerHandler); virtual;
          procedure UnsetContainer; virtual;
          procedure StartSettingProperties; virtual;
          procedure StopSettingProperties; virtual;
          function  Busy : boolean; virtual;
          function  CreateControl(Owner : TControl) : TControl; virtual;
          function  GetControl : TControl; virtual;
          procedure RenderProperties(Properties : TStringList); virtual;
          procedure SetFocus; virtual;
          procedure LostFocus; virtual;
          procedure Clear; virtual;
          procedure Refresh; virtual;
          function  HandleURL(URL : TURL) : TURLHandlingResult; virtual;
          function  Exposed : boolean;
          procedure Lock; virtual;
          procedure Unlock; virtual;
          function  Loaded : boolean; virtual;
      end;

    TLockableSheetHandler =
      class(TSheetHandler)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fLock : TCriticalSection;
        public
          procedure Lock;   override;
          procedure Unlock; override;
      end;

implementation

  uses
    URLParser;

  function TSheetHandler.GetContainer : IPropertySheetContainerHandler;
    begin
      result := fContainer;
    end;

  procedure TSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      fContainer := aContainer;
    end;

  procedure TSheetHandler.UnsetContainer;
    begin
      fContainer := nil;
    end;

  procedure TSheetHandler.StartSettingProperties;
    begin
    end;

  procedure TSheetHandler.StopSettingProperties;
    begin
    end;

  function  TSheetHandler.Busy : boolean;
    begin
      result := false;
    end;

  function  TSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      result := nil;
    end;

  procedure TSheetHandler.RenderProperties(Properties : TStringList);
    begin
    end;

  function TSheetHandler.GetControl : TControl;
    begin
      result := nil;
    end;

  procedure TSheetHandler.SetFocus;
    begin
      fExposed := true;
      fLoaded  := true;
      inc(fLastUpdate);
    end;

  procedure TSheetHandler.Clear;
    begin
      fExposed := false;
      fLoaded  := false;
      inc(fLastUpdate);
    end;

  procedure TSheetHandler.LostFocus;
    begin
      //fExposed := false;
    end;

  procedure TSheetHandler.Refresh;
    begin
      fLoaded := false;
      Clear;
      SetFocus;
    end;

  function TSheetHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      try
        if GetURLAction(URL) = htmlAction_RefreshSheet
          then
            begin
              Refresh;
              result := urlHandled;
            end
          else result := urlNotHandled;
      except
        result := urlNotHandled;
      end;
    end;

  function TSheetHandler.Exposed : boolean;
    begin
      result := fExposed;
    end;

  procedure TSheetHandler.Lock;
    begin
    end;

  procedure TSheetHandler.Unlock;
    begin
    end;

  function TSheetHandler.Loaded : boolean;
    begin
      result := fLoaded;
    end;

  // TLockableSheetHandler

  constructor TLockableSheetHandler.Create;
    begin
      inherited Create;
      fLock := TCriticalSection.Create;
    end;

  destructor TLockableSheetHandler.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  procedure TLockableSheetHandler.Lock;
    begin
      fLock.Enter;
    end;

  procedure TLockableSheetHandler.Unlock;
    begin
      fLock.Leave;
    end;


end.
