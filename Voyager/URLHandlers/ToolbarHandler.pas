unit ToolbarHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Controls, ToolbarHandlerViewer, VCLUtils{, GMChat};

  type
    TToolbarHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler )
        public
          constructor Create( aNestedMetaHandler, aAdsHandler : IMetaURLHandler );
          destructor  Destroy; override;
        private
          fControl            : TToolbarHandlerView;
          fNestedHandler      : IURLHandler;
          //fAdsHandler       : IURLHandler;
          fMasterURLHandler   : IMasterURLHandler;
          fClientView         : IClientView;
          fUnreadMsgs         : integer;
          //fGMView             : TGMView;
          fToolbarInitialized : boolean;
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
      end;

  const
    tidMetaHandlerName_Toolbar = 'ToolbarView';

implementation

  uses
    Events, ServerCnxHandler, SysUtils, MathUtils, ServerCnxEvents, Forms, Config, Graphics;

  constructor TToolbarHandler.Create( aNestedMetaHandler, aAdsHandler : IMetaURLHandler );
    begin
      inherited Create;                                              
      fNestedHandler := aNestedMetaHandler.Instantiate;
      //fAdsHandler    := aAdsHandler.Instantiate;
      fControl       := TToolbarHandlerView.Create( nil );  
      //fToolbarInitialized := true;
    end;

  destructor TToolbarHandler.Destroy;
    begin
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
      inherited;
    end;

  function TToolbarHandler.getName : string;
    begin
      result := tidMetaHandlerName_Toolbar;
    end;

  function TToolbarHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TToolbarHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TToolbarHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TToolbarHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := fNestedHandler.HandleURL( URL );
    end;

  function TToolbarHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      RefreshTycoon       : TRefreshTycoonInfo       absolute info;
      RefreshDate         : TRefreshDateInfo         absolute info;
      NotifyCompanionship : TNotifyCompanionshipInfo absolute info;
      SetCompany          : TSetCompanyInfo          absolute info;
      NewMailInfo         : TNewMailInfo             absolute info;
      Command             : word                     absolute info;
      ChatMsgInfo         : TChatMsgInfo             absolute info;
      Error               : TLogonError              absolute info;
      PendMailInfo        : TNewMailInfo;
//      auxStr              : string;
      CachePath           : string;
      //ConfigHolder        : IConfigHolder;
    begin
      result := evnHandled;
      case EventId of
        evnRefreshTycoon :
          if fToolbarInitialized and (fControl.Parent <> nil)
            then
              with fControl do
                try
                  if (RefreshTycoon.Ranking >= 0) and (fClientView.getCompanyId <> 0)
                    then UserName.Caption := IntToStr( succ(RefreshTycoon.Ranking) ) + '. ' + fClientView.getUserName
                    else UserName.Caption := fClientView.getUserName;
                  // fControl.CompanyName.Left := fControl.UserName.Left + fControl.UserName.Width + 10;
                  //auxStr := FormatMoney( RefreshTycoon.Money );
    //              if RefreshTycoon.NetProfit > 0
      //              then
        //              begin
          //              auxStr := auxStr + ' (+' + FormatMoney( RefreshTycoon.NetProfit ) + ')'
            //          end
              //      else
                //      if RefreshTycoon.NetProfit < 0
                  //      then
                    //      begin
                      ///      auxStr := auxStr + ' (' + FormatMoney( RefreshTycoon.NetProfit ) + ')';
                         // end;
  //                Money.Caption := auxStr;
                  if fClientView.getCompanyId <> 0
                    then Money.Caption := FormatMoney( RefreshTycoon.Money )
                    else Money.Caption := '';
                  {
                  if RefreshTycoon.NetProfit >= 0
                    then
                      begin
                        //fControl.Money.Hint := auxStr + ' (+' + FormatMoney( RefreshTycoon.NetProfit ) + ')';
                        MsgTrouble1Off.Visible := true;
                        MsgTrouble1On.Visible  := false;
                      end
                    else
                      begin
                        //fControl.Money.Hint := auxStr + ' (' + FormatMoney( RefreshTycoon.NetProfit ) + ')';
                        MsgTrouble1On.Visible  := true; // not fControl.MsgTrouble1On.Visible;
                        MsgTrouble1Off.Visible := false; // not fControl.MsgTrouble1Off.Visible;
                      end;
                      }
                  // fControl.MoneyDelta.Left := fControl.Money.Left + fControl.Money.Width + 10;
                  MoneyDelta.Caption := '(' + MathUtils.FormatMoney( RefreshTycoon.NetProfit ) + '/h)';
                  if RefreshTycoon.NetProfit>=0
                    then
                      begin
                        MoneyDelta.Font.Color := $00B1BC96;
                        MoneyDelta.Font.Style := [];
                      end
                    else
                      begin
                        MoneyDelta.Font.Color := $000080FF;
                        MoneyDelta.Font.Style := [fsBold];
                      end;
                  FacCounter.Left := UserName.Left + UserName.Width + 10;
                  FacCounter.Caption := IntToStr(RefreshTycoon.FacCount);
                  if RefreshTycoon.FacMax < high(RefreshTycoon.FacMax)
                    then FacCounter.Caption := FacCounter.Caption + '/' + IntToStr(RefreshTycoon.FacMax);
                  FacIcon.Left := FacCounter.Left + FacCounter.Width + 4;
                except
                  asm
                    nop
                  end;
                end;
        evnNotifyCompanionship :
          if fToolbarInitialized
            then
              begin
                if NotifyCompanionship.Names <> ''
                  then
                    begin
                      fControl.MsgCompanionshipOn.Visible  := true;
                      fControl.MsgCompanionshipOff.Visible := false;
                      fControl.MsgCompanionshipOn.Hint     := NotifyCompanionship.Names;
                    end
                  else
                    begin
                      fControl.MsgCompanionshipOff.Visible := true;
                      fControl.MsgCompanionshipOn.Visible  := false;
                    end;
              end;
        evnSetCompany :
          begin
            fControl.Container.Enabled := true;
            fControl.BlockTicker.Enabled := true;
            fControl.CompanyName.Caption := SetCompany.Name;
            fToolbarInitialized := true;
          end;
        evnRefreshDate :
          if fToolbarInitialized
            then
              begin
                fControl.Date.Caption := FormatDateTime( 'mmm d, yyyy', RefreshDate.Date );
              end;
        evnHandlerExposed :
          begin
            fNestedHandler.getControl.Align  := alClient;
            fNestedHandler.getControl.Parent := fControl.Container;
            {
            fAdsHandler.getControl.Align     := alClient;
            fAdsHandler.getControl.Parent    := fControl.AdsContainer;
            fAdsHandler.HandleURL( fClientView.getWorldURL + 'visual/voyager/advertisement/largebanner.asp?frame_NoScrollBars=True&frame_NoBorder=yes' );
            }
            fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
            fControl.BusyClipPath := CachePath + 'OtherImages\busy.gif';
            fControl.CompanyName.Caption := fClientView.getCompanyName;
            fMasterURLHandler.HandleEvent( evnAnswerPendingMail, PendMailInfo );
            HandleEvent( evnNewMail, PendMailInfo );
            fControl.Date.Caption := FormatDateTime( 'mmm d, yyyy', fClientView.getDate );
            fControl.ClientView := fClientView;
            fControl.MasterURLHandler := fMasterURLHandler;
          end;
        evnSystemBusy :
          begin
            fControl.Busy;
            result := evnHandled;
          end;
        evnSystemIdle :
          begin
            fControl.NotBusy;
            result := evnHandled;
          end;
        evnNewMail :
          if NewMailInfo.count > 0
            then
              begin
                inc( fUnreadMsgs, NewMailInfo.count );
                fControl.UnreadMsgs := fUnreadMsgs;
              end;
        evnKeyCommand :
          case Command of
            {
            keyHidden :
              //fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=CrimeHandler&frame_Class=CrimeHandler&frame_Align=left&frame_Width=400&frame_Visibility=switch' );
              fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Class=UniversalMapHandler&frame_Action=SetUserInfo&frame_Align=left&UserName=' + fClientView.getUserName + '&Password=' + fClientView.getUserPassword + '&Logon=yes&frame_Visibility=switch' );
            }
            keyCmdFullScreen :
              if fControl.Height = 0
                then
                  begin
                    fControl.Height := 200;
                    fControl.Height := 100;
                  end
                else fControl.Height := 0;
            {
            keyGMClient :
              begin
                fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
                if ConfigHolder.ReadBoolean( true, '', 'GMSystem', false )
                  then
                    begin
                      if fGMView = nil
                        then
                          begin
                            fGMView := TGMView.Create( Application );
                            fGMView.MasterURLHandler := fMasterURLHandler;
                            fGMView.ClientView       := fClientView;
                            fGMView.Show;
                          end
                        else
                          begin
                            fGMView.Hide;
                            fGMView.Free;
                            fGMView := nil;
                          end;
                    end;
              end;
            }
          end;
        evnLanguageSet :
          fControl.ReloadHints;
        evnLogonStarted :
          begin
            fControl.StopHints;
            fControl.MsgCompanionshipOff.Visible := true;
            fControl.MsgCompanionshipOn.Visible  := false;
            fControl.MsgCompanionshipOn.Hint     := '';
            fControl.Container.Enabled           := false;
            fControl.BlockTicker.Enabled         := false;
          end;
        evnLogonCompleted :
          if Error = errNone
            then fControl.StartHints;
        evnShutDown :  //.rag
          begin
            fNestedHandler      := nil;
            fMasterURLHandler   := nil;
            fClientView         := nil;
            RemoveComponentFreeAndNil(fControl); //.rag .Free;
          end;  
        {
        evnChatMsg :
          fControl.BlockTicker.Caption := ChatMsgInfo.Msg;
        }
        {
        else
          if fNestedHandler <> nil
            then result := fNestedHandler.HandleEvent( EventId, info )
            else result := evnError;
        }
      end;
    end;

  function TToolbarHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TToolbarHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fNestedHandler.setMasterURLHandler( URLHandler );
      //fAdsHandler.setMasterURLHandler( URLHandler );
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;


end.


