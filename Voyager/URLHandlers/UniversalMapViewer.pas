unit UniversalMapViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, PDTabControl, GradientBox, BlockTicker, ComCtrls,
  FramedButton, VisualControls,
  RDOInterfaces, WinSockRDOConnection, RDOServer, RDOObjectProxy,
  VoyagerInterfaces, VoyagerServerInterfaces, InternationalizerComponent,
  ImgList, CustomWebBrowser;

type
  TUniversalMapView =
    class(TVisualControl)
        GradientBox1: TGradientBox;
        Tabs: TPDTabControl;
        lbName: TLabel;
        Panel1: TPanel;
        Label11: TLabel;
        WorldList: TListView;
        Panel2: TPanel;
        Ticker: TBlockTicker;
        PlanetImages: TImageList;
        Panel3: TPanel;
        Jump: TFramedButton;
        Refresh: TFramedButton;
        Label9: TLabel;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        RadioButton3: TRadioButton;
        RadioButton4: TRadioButton;
        RightLine: TPanel;
        TickerTimer: TTimer;
        UnvMapIcon: TImage;
        InternationalizerComponent1: TInternationalizerComponent;
        RulesTicker: TBlockTicker;
        procedure JumpClick(Sender: TObject);
        procedure RefreshClick(Sender: TObject);
        procedure WorldListChange(Sender: TObject; Item: TListItem;
          Change: TItemChange);
        procedure TickerTimerTimer(Sender: TObject);
        procedure HideClick(Sender: TObject);
        procedure UnvMapIconClick(Sender: TObject);
        procedure TabsTabChange(Sender: TObject);
        procedure WorldListCustomDrawItem(Sender: TCustomListView;
          Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
        procedure WorldListMouseMove(Sender: TObject; Shift: TShiftState; X,
          Y: Integer);
        procedure WorldListAdvancedCustomDrawItem(Sender: TCustomListView;
          Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
          var DefaultDraw: Boolean);
    procedure RulesTickerClick(Sender: TObject);
    procedure WorldListExit(Sender: TObject);
      private
        fCache            : string;
        fMasterURLHandler : IMasterURLHandler;
        fDSAddr           : string;
        fDSPort           : integer;
        fWorlds           : TStringList;
        fUserName         : string;
        fPassword         : string;
        fMasterUser       : string;
        fLogon            : boolean;
        fWebControl       : TCustomWebBrowser;
        fDefaultWebSrv    : string;
        fDefaultPage      : string;
      private
        procedure SetMasterURLHandler(const aMasterURLHandler : IMasterURLHandler);
      public
        property Cache            : string            write fCache;
        property MasterURLHandler : IMasterURLHandler write SetMasterURLHandler;
        property DSAddr           : string            read fDSAddr write fDSAddr;
        property DSPort           : integer           read fDSPort write fDSPort;
        property UserName         : string            write fUsername;
        property Password         : string            write fPassword;
        property Logon            : boolean           write fLogon;
        property MasterUser       : string            read fMasterUser write fMasterUser;
      public
        procedure Start;
        procedure RemoveWeb;
      public
        EnableDblClick : boolean;
      private
        procedure threadedGetWorlds( const parms : array of const );
        procedure syncGetWorlds( const parms : array of const );
        procedure syncGetInfoHTML( const parms : array of const );
      private
        fLastTab : integer;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;

var
  UniversalMapView: TUniversalMapView;

implementation

  {$R *.DFM}

  uses
    Threads, Config, ServerCnxEvents, Literals, ClientMLS, Events, VCLUtils, CoolSB, ServerCnxHandler;
    
  const
    tidWorldProp_Count      = 'Count';
    tidWorldProp_Key        = 'Key';
    tidWorldProp_Population = 'General/Population';
    tidWorldProp_Investors  = 'General/Investors';
    tidWorldProp_Online     = 'General/Online';
    tidWorldProp_Date       = 'General/Date';
    tidWorldProp_ISAddr     = 'Interface/IP';
    tidWorldProp_ISPort     = 'Interface/Port';
    tidWorldProp_URL        = 'Interface/URL';
    tidWorldProp_Running    = 'Interface/Running';
    tidWorldProp_Rules      = 'Interface/Rules';

  const
    AreaIds : array[0..2] of string = ('America', 'Europe', 'Asia'); // >> Generalize this later!!!
    //AreaIds : array[0..2] of string = ('Literal401', 'Literal402', 'Literal403'); // >> Generalize this later!!!

  procedure TUniversalMapView.Start;
    var
      ConfigHolder : IConfigHolder;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      DSAddr := ConfigHolder.ReadString( true, '', 'DSAddr', 'dir.starpeace.co.uk' );

      // Patch Exodus
      if (DSAddr = '') or (DSAddr = 'dir.starpeace.co.uk')
        then
          begin
            DSAddr := 'dir.starpeace.co.uk';
            ConfigHolder.WriteString(true, '', 'DSAddr', DSAddr);
          end;

      DSPort := ConfigHolder.ReadInteger( true, '', 'DSPort', 1111 );
      WorldList.Items.Clear;
      WorldList.ViewStyle := vsList;
      with WorldList.Items.Add do
        begin
          Caption := GetLiteral('Literal404');
          ImageIndex := -1;
        end;
      Fork( threadedGetWorlds, priNormal, [AreaIds[Tabs.CurrentTab]] );
    end;

  procedure TUniversalMapView.RemoveWeb;
    begin
      if (fWebControl<>nil)
        then
          try
           RemoveComponentFreeAndNil(fWebControl);
          except
            fWebControl := nil;
          end;
    end;

  procedure TUniversalMapView.threadedGetWorlds( const parms : array of const );
    var
      Area      : string absolute parms[0].vPChar;
      WorldList : TStringList;
      props     : TStringList;
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      key       : string;
      count     : integer;
    begin
      try
        WorldList    := TStringList.Create;
        try
          WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
          DSCnx        := WSDSCnx;
          DSCnx.Server := fDSAddr;
          DSCnx.Port   := fDSPort;
          DSProxy      := TRDOObjectProxy.Create as IDispatch;
          if DSCnx.Connect( 60000 )
            then
              begin
                DSProxy.SetConnection( DSCnx );
                DSProxy.BindTo( 'DirectoryServer' );
                DSProxy.TimeOut := 40000;
                session         := DSProxy.RDOOpenSession;
                if session <> 0
                  then
                    begin
                      try
                        DSProxy.BindTo( session );
                        //DSProxy.RDOCurrentKey
                        key := 'Root/Areas/' + Area + '/Worlds';
                        DSProxy.WaitForAnswer := true;
                        props := TStringList.Create;
                        try
                          props.Add( tidWorldProp_Population );
                          props.Add( tidWorldProp_Investors );
                          props.Add( tidWorldProp_Online );
                          props.Add( tidWorldProp_Date );
                          props.Add( tidWorldProp_ISAddr );
                          props.Add( tidWorldProp_ISPort );
                          props.Add( tidWorldProp_URL );
                          props.Add( tidWorldProp_Running );
                          props.Add( tidWorldProp_Rules );
                          WorldList.Text := DSProxy.RDOQueryKey( key, props.text );
                        finally
                          props.Free;
                        end;
                        //WorldList.Text := DSProxy.RDOGetKeyNames;
                      finally
                        DSProxy.RDOEndSession;
                      end;
                    end;
              end;
        finally
          try
            count := StrToInt(WorldList.Values[tidWorldProp_Count]);
          except
            count := 0;
          end;
          if count=0
            then Join(syncGetInfoHTML, [Area] )
            else
              begin
                WorldList.Values['Area'] := Area;
                Join( syncGetWorlds, [WorldList] );
              end;
        end;
      except
      end;
    end;

  procedure TUniversalMapView.syncGetWorlds( const parms : array of const );
    var
      Worlds : TStringList absolute parms[0].vPointer;
      count  : integer;
      i      : integer;
    begin
      try
        EnableDblClick := false;
        try
          count := StrToInt(Worlds.Values[tidWorldProp_Count]);
        except
          count := 0;
        end;
        if WorldList<>nil
          then
            begin
              WorldList.Items.Clear;
              if Worlds.Count > 0
                then
                  begin
                    WorldList.ViewStyle := vsIcon;
                    for i := 0 to pred(count) do
                      with WorldList.Items.Add do
                        begin
                          Caption := Worlds.Values[tidWorldProp_Key + IntToStr(i)];
                          SubItems.Add( Worlds.Values[tidWorldProp_Population + IntToStr(i)] );
                          SubItems.Add( Worlds.Values[tidWorldProp_Investors + IntToStr(i)] );
                          SubItems.Add( Worlds.Values[tidWorldProp_Online + IntToStr(i)] );
                          SubItems.Add( Worlds.Values[tidWorldProp_Date + IntToStr(i)] );
                          //SubItems.Add( Worlds.Values[tidWorldProp_Rules + IntToStr(i)] );
                          SubItems.Add( IntToStr(100 + random(100)) );
                          if Worlds.Values[tidWorldProp_Running + IntToStr(i)] = 'true'
                            then
                              begin
                                ImageIndex := i mod (PlanetImages.Count - 1);
                                Data := pointer(true);
                              end
                            else
                              begin
                                ImageIndex := PlanetImages.Count - 1;
                                Caption := Caption + #13#10 + GetLiteral('Literal405');
                                Data := pointer(false);
                              end;
                        end;
                    EnableDblClick := true;
                  end
                else
                  begin
                    WorldList.ViewStyle := vsList;
                    with WorldList.Items.Add do
                      begin
                        Caption := GetLiteral('Literal406');
                        ImageIndex := -1;
                      end;
                  end;
            end;
      finally
        fWorlds.Free;
        fWorlds := Worlds;
      end;
      if not WorldList.Visible
        then WorldList.Visible := true;
      RemoveWeb;
    end;

  procedure TUniversalMapView.syncGetInfoHTML( const parms : array of const );
    var
      Area : string absolute parms[0].vPChar;
    begin
      EnableDblClick := false;
      if fWebControl=nil
        then
          begin
            fWebControl      := TCustomWebBrowser.Create(self);
            fWebControl.Align := alClient;
            fWebControl.HideBorders := true;
            fWebControl.HideScrollbars := true;
            fWebControl.SetDefaultPage(fDefaultPage);
          end;
      TControl(fWebControl).Parent := Panel1;

      fWebControl.Stop;
      fWebControl.Navigate(fDefaultWebSrv+Area);

      if not fWebControl.Visible
        then fWebControl.Visible := true;
      if WorldList.Visible
        then WorldList.Visible := false;
    end;

  procedure TUniversalMapView.JumpClick(Sender: TObject);
    var
      URL        : string;
      ClientView : IClientView;
      dummy      : integer;
    begin
      if (WorldList.Selected <> nil) and (WorldList.Selected.Data<>nil) and EnableDblClick
        then
          begin
            if fMasterUser <> ''
              then
                begin
                  ClientView := nil;
                  fMasterURLHandler.HandleEvent( evnAnswerClientView, ClientView );
                  if (ClientView <> nil) and (ClientView.getUserName<>'')
                    then
                      begin
                        fUserName := ClientView.getUserName;
                        fMasterUser := ClientView.getUserMasterName;
                        fPassword := ClientView.getUserPassword;
                      end;
                end;
            URL :=
              '?' +
              'frame_Action=Logon&' +
              'frame_Id=CnxHandler&' +
              'frame_Class=CnxHandler&' +
              'frame_NoBorder=yes&' +
              'frame_NoScrollBar=yes&' +
              'frame_Align=client&' +
              'Threaded=yes&' +
              {'frame_Target=Main&' +}
              'UserName=' + fUserName + '&' +
              'Password=' + fPassword + '&' +
              htmlParmName_MasterUserName +'='+ fMasterUser + '&' +
              'WorldName=' + WorldList.Selected.Caption + '&' +
              'ISAddr=' + fWorlds.Values[tidWorldProp_ISAddr + IntToStr(WorldList.Selected.Index)] + '&' +
              'ISPort=' + fWorlds.Values[tidWorldProp_ISPort + IntToStr(WorldList.Selected.Index)] + '&' +
              'DSAddr=' + fDSAddr + '&' +
              'DSPort=' + IntToStr(fDSPort) + '&' +
              'Area=' + fWorlds.Values['Area'] + '&' +
              'ResultURL=' + fWorlds.Values[tidWorldProp_URL + IntToStr(WorldList.Selected.Index)] + 'Visual/Voyager/NewLogon/logonComplete.asp';
            try
              try
                fMasterURLHandler.HandleURL( '?frame_Id=ObjectInspector&frame_Close=yes' );
                fMasterURLHandler.HandleEvent( evnLogonStarted, dummy );
              except
                fMasterURLHandler.HandleEvent( evnLogonStarted, dummy );
              end;
            except
            end;
            EnableDblClick := false;
            fMasterURLHandler.HandleURL( URL );
            Jump.Enabled := false;
          end;
    end;

  procedure TUniversalMapView.RefreshClick(Sender: TObject);
    begin
      Start;
    end;

  procedure TUniversalMapView.WorldListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    var
      tmpItem: TListItem;
      SelItem: TListItem;
      {$ifopt d+}
      s : string;
      {$endif}
    begin
      if (WorldList.ItemFocused<>nil)
        then tmpItem := WorldList.ItemFocused
        else
          if (WorldList.Selected<>nil)
            then tmpItem := WorldList.Selected
            else tmpItem := nil;
      Jump.Enabled := (tmpItem <> nil) and boolean(tmpItem.Data);
      SelItem := WorldList.Selected;
      if (Jump.Enabled)
        then
          try
            Ticker.Caption := GetFormattedLiteral('Literal407',
                                                   [
                                                     tmpItem.Caption,
                                                     fWorlds.Values[tidWorldProp_Population + IntToStr(tmpItem.Index)],
                                                     fWorlds.Values[tidWorldProp_Investors + IntToStr(tmpItem.Index)],
                                                     fWorlds.Values[tidWorldProp_Date + IntToStr(tmpItem.Index)],
                                                     fWorlds.Values[tidWorldProp_Online + IntToStr(tmpItem.Index)]
                                                   ]
                                                 );
            if ((SelItem<>nil) and (fWorlds.Values[tidWorldProp_Rules + IntToStr(SelItem.Index)]<>''))
              then
                begin
                  RulesTicker.Top :=  Panel3.Top;
                  RulesTicker.Caption := format(GetLiteral('Literal501'), [SelItem.Caption]);
                  if (not RulesTicker.Visible)
                    then Ticker.Height := Ticker.Height-RulesTicker.Height;
                  RulesTicker.Visible := true;
                  RulesTicker.Tag := SelItem.Index;
                end
              else
                begin
                  if RulesTicker.Visible
                    then Ticker.Height := Ticker.Height+RulesTicker.Height;
                  RulesTicker.Visible := false;
                  RulesTicker.Tag := -1;
                end;
          except
            {$ifopt d+}
            try
              s := 'TUniversalMapView.WorldListChange : '+ tmpItem.Caption;
              OutputDebugString(pchar(s));
              s := format('TUniversalMapView.WorldListChange :WorldsList %d ', [fWorlds.Count]);
              OutputDebugString(pchar(s));
            except
            end;
            {$Endif}
          end
        else
          begin
            if RulesTicker.Visible
              then Ticker.Height := Ticker.Height+RulesTicker.Height;
            Ticker.Caption := GetLiteral('Literal411');
            RulesTicker.Visible := false;
            RulesTicker.Tag := -1;
          end;
    end;

  procedure TUniversalMapView.TickerTimerTimer(Sender: TObject);
    begin
      Ticker.Tick;
      if RulesTicker.Visible
        then RulesTicker.Tick;
    end;

  procedure TUniversalMapView.HideClick(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Close=yes' );
    end;

  procedure TUniversalMapView.UnvMapIconClick(Sender: TObject);
    begin
      if not fLogon
        then fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Close=yes' );
    end;

  procedure TUniversalMapView.TabsTabChange(Sender: TObject);
    begin
      if Tabs.CurrentTab <> fLastTab
        then
          begin
            EnableDblClick := false;
            fLastTab := Tabs.CurrentTab;
            WorldList.Items.Clear;
            WorldList.ViewStyle := vsList;
            with WorldList.Items.Add do
              begin
                Caption := GetLiteral('Literal412');
                ImageIndex := -1;
              end;
            Fork( threadedGetWorlds, priNormal, [AreaIds[Tabs.CurrentTab]] );
          end;
    end;


  procedure TUniversalMapView.WorldListCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    var
      BRect  : TRect;
      TxRect : TRect;
      BitMap : TBitmap;
    begin
      with TListView(Sender) do
        if ((Item=Selected) or (ItemFocused=Item))
          then
            begin
              BitMap := TBitmap.Create;
              BRect  := Item.DisplayRect(drBounds);
              TxRect := Item.DisplayRect(drLabel);

              LargeImages.GetBitmap( Item.ImageIndex, Bitmap);
              if (Item=Selected)
                then Canvas.Pen.Color := clLime
                else Canvas.Pen.Color := clTeal;

              Canvas.Polygon([Point(Brect.Left,Brect.Top), Point(Brect.Right-1, Brect.Top), Point(Brect.Right-1, BRect.Bottom-1), Point(Brect.Left, BRect.Bottom-1)]);
              BitMap.TransparentMode := tmAuto;
              BRect  := Item.DisplayRect(drIcon);

              with BRect do
                begin
                  Left   := Left + (((Right - Left) - BitMap.Canvas.ClipRect.Right) div 2);
                  Top   := Top + (((Bottom - Top) - BitMap.Canvas.ClipRect.Bottom) div 2);
                  // Left   := Left+ 3;
                  // Top    := Top + 3;
                  // Right  := Right - 3;
                  // Bottom := Bottom - 3;
                end;
              Canvas.Draw(BRect.Left, BRect.Top, Bitmap);
//              Canvas.CopyRect(BRect, BitMap.Canvas, BitMap.Canvas.ClipRect);
              Drawtext(Canvas.Handle, pchar(Item.Caption), length(Item.Caption), TxRect, DT_CENTER);
              DefaultDraw := false ;
              Bitmap.Free;
            end ;
    end;

  procedure TUniversalMapView.WorldListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      with TListView(Sender) do
        begin
          ItemFocused := TListView(Sender).GetItemAt(X, Y);
          if (ItemFocused=Nil)
            then Cursor := crDefault
            else Cursor := crHandPoint;
        end;
    end;

procedure TUniversalMapView.WorldListAdvancedCustomDrawItem( Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    //var
//      BRect : TRect;
  //    BitMap : TBitmap;
    begin
    {
      with TListView(Sender) do
        if ((Item=Selected) or (ItemFocused=Item)) //and (Stage = cdPostPaint)
          then
            begin
              BitMap := TBitmap.Create;
              BRect := Item.DisplayRect(drBounds);
              LargeImages.GetBitmap( Item.ImageIndex, Bitmap);
              if (Item=Selected)
                then Canvas.Pen.Color := clLime
                else Canvas.Pen.Color := clTeal;
              Canvas.Polygon([Point(Brect.Left,Brect.Top), Point(Brect.Right-1, Brect.Top), Point(Brect.Right-1, BRect.Bottom-1), Point(Brect.Left, BRect.Bottom-1)]);
              //Canvas.Draw (Item.Position.x, Item.Position.y, Bitmap);
              //Canvas.TextOut( BRect.Left+(BRect.Right-BRect.Left) div 2 - Canvas.TextExtent(Item.Caption).cx div 2 , BRect.Bottom-15, Item.Caption) ;
              DefaultDraw := false ;
              Bitmap.Free;
            end ;
            }
    end;

procedure TUniversalMapView.SetMasterURLHandler( const aMasterURLHandler: IMasterURLHandler);
  var
    fConfigHolder : IConfigHolder;
  begin
    fMasterURLHandler := aMasterURLHandler;
    if fMasterURLHandler<>nil
      then
        begin
          fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, fConfigHolder );
          if fConfigHolder<>nil
            then fDefaultWebSrv := fConfigHolder.ReadString(true, '', 'DefaultSite', '') + ActiveLanguage + '/visual/voyager/misc/umap.asp?area=';
          fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, fDefaultPage);
          fDefaultPage := fDefaultPage+ 'misc\htmlerr'+ActiveLanguage+'.htm';
        end;
  end;


procedure TUniversalMapView.SetParent(which: TWinControl);
  begin
    inherited;
    if which<>nil
      then
        begin
          RulesTicker.Visible := false;
          if InitSkinImage
            then
              begin
                InitializeCoolSB(WorldList.Handle);
                if hThemeLib <> 0
                  then
                    SetWindowTheme(WorldList.Handle, ' ', ' ');
                CoolSBEnableBar(WorldList.Handle, false, true);
              end;
        end;
  end;

procedure TUniversalMapView.RulesTickerClick(Sender: TObject);
  var
    URL : string;
  begin
    URL := fWorlds.Values[tidWorldProp_Rules + IntToStr(RulesTicker.tag)] + '?frame_Id=WebMainView&frame_Class=HTMLView&frame_Visibility=switch&frame_Align=client&frame_NoBorder=Yes&frame_ToHistory=yes';
    fMasterURLHandler.HandleURL( URL );
        //    'http://www.microsoft.com?frame_Id=WebMainView&frame_Class=HTMLView&frame_Visibility=switch&frame_Align=client&frame_NoBorder=Yes&frame_ToHistory=yes'
  end;

procedure TUniversalMapView.WorldListExit(Sender: TObject);
begin
    // test
end;

end.


