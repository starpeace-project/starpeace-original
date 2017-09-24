unit MapIsoHandler;

interface

  uses
    Windows, VoyagerInterfaces, VoyagerServerInterfaces, Classes, Controls,
    FiveTypes, MapIsoView, Config, ExtCtrls, VCLUtils;

  type               
    TMetaMapIsoHandler =
      class( TInterfacedObject, IMetaURLHandler )
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TMapIsoHandler =
      class( TInterfacedObject, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl : TMapIsoViewer;
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
          fClientView       : IClientView;
          fConfigHolder     : IConfigHolder;
          fActionButton     : TMouseButton;
          fActionShift      : TShiftState;
          fObjectId         : integer;
          fStatusEnabled    : boolean;
          fIdleTextTimer    : TTimer;
          fSelSoundId       : string;
          fClickSoundId     : string;
          fChatSoundId      : string;
          fLastObj          : TObjId;
        private
          procedure threadedGetObjStatusText( const parms : array of const );
          procedure syncGetObjStatusText( const parms : array of const );
          procedure threadedGetContextText( const parms : array of const );
          procedure syncGetContextText( const parms : array of const );
          procedure threadedPlaySelectionSound( const parms : array of const );
          procedure treadedUnfocusObject( const parms : array of const );
        private
          procedure ShowContextText;
        private
          procedure OnObjectSelected( ObjId : TObjId; i, j : integer );
          procedure OnFacilityBuild( i, j: integer; const FacClass : string; visclass : integer );
          procedure OnFacilityBuildAbort;
          procedure OnCircuitBuild( const CircuitSegments : TSegmentReport; CKind, Cost : integer );
          procedure OnCircuitEditChange( cost : integer; var allowed : boolean );
          procedure OnCircuitBuildAbort;
          procedure OnRegionChange( Sender : TObject; const Origin, Size : TPoint );
          procedure OnSmallMapClick( Sender : TObject; i, j : integer );
          //procedure OnSuperGran( Sender : TObject );
          procedure OnMouseUp( Sender : TObject; Button : TMouseButton; Shift : TShiftState; x, y : integer);
          procedure OnMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; x, y : integer);
          procedure OnToolPressed( ToolId : integer );
          function  OnMouseOnObject( SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnMouseOnObjectRetCode;
          function  OnObjectClicked( SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnObjectClickedRetCode;
          procedure OnIdleTextChange( Sender : TObject );
          procedure OnAreaSelection( imin, jmin, imax, jmax : integer; value : single );
        private
          procedure threadedBuildFacility( const parms : array of const );
          procedure syncBuildFacility( const parms : array of const );
          procedure syncCreateDummyFacility( const parms : array of const );
          procedure threadedCircuitBuild( const parms : array of const );
          procedure syncCircuitBuild( const parms : array of const );
          procedure syncCreateDummyCircuits( const parms : array of const );
          procedure threadedDefineZone( const parms : array of const );
          //procedure syncInvalidateSurface( const parms : array of const );
          procedure threadedWipeCircuit( const parms : array of const );
          procedure threadedConnect( const parms : array of const );
          procedure syncConnect( const parms : array of const );
        private
          //fDoGran       : boolean;
          fQWOn         : boolean;
          fLastFacCount : integer;
        public
          procedure ShowQuickWeb;
          procedure HideQuickWeb;
        private
          procedure ClearStatusLine;
          procedure LoadCursors;
      end;

  const
    tidURLHandler_MapIsoHandler = 'MapIsoView';
    tidSound_Selection          = 'select.wav';
    tidSound_Click              = 'click.wav';
    tidSound_Chat               = 'system.wav';

  const
    htmlAction_Build         = 'BUILD';
    htmlAction_BuildRoad     = 'BUILDROAD';
    htmlAction_BuildRailroad = 'BUILDRAILROAD';
    htmlAction_DemolishRoad  = 'DEMOLISHROAD';
    htmlAction_PickOnMap     = 'PICKONMAP';
    htmlAction_Select        = 'SELECT';
    htmlAction_MoveTo        = 'MOVETO';
    htmlAction_DefineZone    = 'DEFINEZONE';
    htmlParm_FacilityClass   = 'FacilityClass';
    htmlParm_VisualClassId   = 'VisualClassId';
    htmlParm_FacilityXPos    = 'x';
    htmlParm_FacilityYPos    = 'y';
    htmlParm_ZoneId          = 'ZoneId';

  const
    crConnect = 100;


implementation

  uses
    Events, ServerCnxHandler, SysUtils, MathUtils, Threads, MapTypes, FocusTypes,
    URLParser, SoundLib, LanderTypes, Protocol, Forms, VisualClassesHandler, GameTypes,
    Map, Graphics, ChatListHandler, FiveControl, HintBox, ServerCnxEvents, MapTestDlg,
    MessageBox, Literals, ChangeLog, ClientMLS, URLNotification, ObjectInspectorHandler;

  type
    PSegmentReport = ^TSegmentReport;

  const
    clPositiveMoney = clWhite;
    clNegativeMoney = clRed;

  const
    sidDestroyRoad = 1;
    sidPickOnMap   = 2;

  // TMetaMapIsoHandler

  function TMetaMapIsoHandler.getName : string;
    begin
      result := tidURLHandler_MapIsoHandler;
    end;

  function TMetaMapIsoHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaMapIsoHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaMapIsoHandler.Instantiate : IURLHandler;
    begin
      result := TMapIsoHandler.Create;
    end;


  // TMapIsoHandler

  type
    TLocalFiveControl = class(TFiveControl);


  constructor TMapIsoHandler.Create;

    procedure RegisterSelectionKinds;
      begin
        fControl.MapView.RegisterSelectionKind(sidDestroyRoad, crConnect, OnMouseOnObject, OnObjectClicked);
        fControl.MapView.RegisterSelectionKind(sidPickOnMap, crConnect, OnMouseOnObject, OnObjectClicked);
      end;

    begin
      inherited;
      fControl := TMapIsoViewer.Create( nil );
      fControl.MapView.OnFacilityBuild      := OnFacilityBuild;
      fControl.MapView.OnFacilityBuildAbort := OnFacilityBuildAbort;
      fControl.MapView.OnCircuitBuild       := OnCircuitBuild;
      fControl.MapView.OnCircuitChange      := OnCircuitEditChange;
      fControl.MapView.OnCircuitBuildAbort  := OnCircuitBuildAbort;
      fControl.MapView.OnRegionChange       := OnRegionChange;
      fControl.MapView.OnAreaSelected       := OnAreaSelection;
      fLastFacCount  := -1;
      fStatusEnabled := true;
      fIdleTextTimer := TTimer.Create( nil );
      fIdleTextTimer.Interval := 20000;
      fIdleTextTimer.OnTimer := OnIdleTextChange;
      RegisterSelectionKinds;
    end;

  destructor TMapIsoHandler.Destroy;
    begin
      // fConfigHolder.WriteInteger( false, fClientView.getUserName, 'lastX', fControl.MapView.Origin );
      // fConfigHolder.WriteInteger( false, fClientView.getUserName, 'lastX', fClientView.getWorldXSize div 2 );
      RemoveComponentFreeAndNil(fControl); //.rag
      inherited;
    end;

  function TMapIsoHandler.HandleURL( URL : TURL ) : TURLHandlingResult;

    const
      ZoneColors : array[0..9] of TColor =
        ($00595959, clMaroon, clTeal, $00BBFFC0, $0043A34F, $001E4823, $0088D9D7, $00D87449, $00FFFFFF, $00884439);

    function DefineZone( URL : TURL ) : TURLHandlingResult;
      var
        ZoneId : integer;
        exs : TAreaExclusions;
      begin
        ZoneId := StrToInt(GetParmValue( URL, htmlParm_ZoneId ));
        fControl.ShowSurface( 1 );
        if (ZoneId>=znReserved) and (ZoneId<=znOffices)
          then exs := [axRoadAround, axBuilding]
          else exs := [];
        fControl.MapView.StartAreaSelection( ZoneId, ZoneColors[ZoneId], exs);
        result := urlHandled;
      end;

    function Build( URL : TURL ) : TURLHandlingResult;
      var
        FacClass       : string;
        FacVisualClass : integer;
      begin
        FacClass       := GetParmValue( URL, htmlParm_FacilityClass );
        FacVisualClass := StrToInt( GetParmValue( URL, htmlParm_VisualClassId ));
        fControl.ShowSurface( 1 );
        fControl.MapView.StartFacilityBuild( FacClass, FacVisualClass, true );
        result := urlHandled;
      end;

    function MoveTo( URL : TURL; select : boolean ) : TURLHandlingResult;
      var
        strX, strY : string;
        x, y       : integer;
      begin
        strX := GetParmValue( URL, htmlParm_FacilityXPos );
        strY := GetParmValue( URL, htmlParm_FacilityYPos );
        if (strX <> '') and (strY <> '')
          then
            begin
              x := StrToInt( strX );
              y := StrToInt( strY );
              if (x <> 0) or (y <> 0)
                then
                  if select
                    then
                      begin
                        with fControl do
                          begin
                            MapView.MoveAndSelect( y, x );
                          end;
                      end
                    else fControl.MapView.MoveTo( y, x );
            end;
        result := urlHandled;
      end;

    function BuildRoad : TURLHandlingResult;
      var
        AlarmCost : integer;
      begin
        fStatusEnabled := false;
        if (fClientView<>nil)
          then AlarmCost := round(fClientView.getMoney)
          else AlarmCost := 20000000;
        fControl.MapView.StartCircuitBuild( cirRoads, AlarmCost );
        result := urlHandled;
      end;

    function BuildRailroad : TURLHandlingResult;
      var
        AlarmCost : integer;
      begin
        fStatusEnabled := false;
        if (fClientView<>nil)
          then AlarmCost := round(fClientView.getMoney)
          else AlarmCost := 20000000;
        fControl.MapView.StartCircuitBuild( cirRailroads, AlarmCost);
        result := urlHandled;
      end;

    function DemolishRoad : TURLHandlingResult;
      begin
        //fControl.MapView.StartSelection( sidDestroyRoad );
        fControl.MapView.StartAreaSelection( -1, clWhite, [] );
        result := urlHandled;
      end;

    function PickOnMap : TURLHandlingResult;
      begin
        fStatusEnabled := false;
        fControl.SecondaryText.Caption := GetLiteral('Literal214');
        fControl.HintText.Caption := GetLiteral('Literal215');
        fControl.MapView.StartSelection( sidPickOnMap );
        result := urlHandled;
      end;

    var
      action : string;
    begin
      fControl.MapView.AbortCurrentSelection;
      result := fControl.MapView.HandleURL( URL );
      action := GetURLAction( URL );
      if action = htmlAction_Build
        then result := Build( URL )
        else
          if action = htmlAction_Select
            then result := MoveTo( URL, true )
            else
              if action = htmlAction_MoveTo
                then result := MoveTo( URL, false )
                else
                  if action = htmlAction_BuildRoad
                    then result := BuildRoad
                    else
                      if action = htmlAction_BuildRailroad
                        then result := BuildRailroad
                        else
                          if action = htmlAction_DemolishRoad
                            then result := DemolishRoad
                            else
                              if action = htmlAction_PickOnMap
                                then result := PickOnMap
                                else
                                  if action = htmlAction_DefineZone
                                    then result := DefineZone( URL );
    end;

  function TMapIsoHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      RefreshTycoon       : TRefreshTycoonInfo       absolute info;
      RefreshDate         : TRefreshDateInfo         absolute info;
      RefreshSeason       : TRefreshSeasonInfo       absolute info;
      RefreshAreaInfo     : TRefreshAreaInfo         absolute info;
      EndOfPeriod         : TEndOfPeriodInfo         absolute info;
      TycoonRetired       : TTycoonRetiredInfo       absolute info;
      ChatMsg             : TChatMsgInfo             absolute info;
      NotifyCompanionship : TNotifyCompanionshipInfo absolute info;
      SetCompany          : TSetCompanyInfo          absolute info;
      RefreshObject       : TRefreshObjectInfo       absolute info;
      UserListChange      : TUserListChangeInfo      absolute info;
      MoveTo              : TMoveToInfo              absolute info;
      ChatOverMap         : boolean                  absolute info;
      KeyCommand          : word                     absolute info;
      enabled             : boolean                  absolute info;
      volume              : single                   absolute info;
      hideshowdata        : THideShowFacilityData    absolute info;
      FrameClosed         : string                   absolute info;
      SelectionInfo       : TGetSelectionInfo;
      cachepath           : string;
      // startX, startY      : integer;

    procedure InitMapViewOptions;
      var
        UName: string;
      begin
        with fControl, fConfigHolder do
          begin
            UName := fClientView.getUserName;
            MapView.SoundsEnabled := ReadBoolean(false, UName, 'EnableSounds', true);
            if MapView.SoundsEnabled
              then fMasterURLHandler.HandleURL( '?frame_Id=JukeBox&frame_Class=JukeBox&frame_Action=Play')
              else fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Stop');
            MapView.GlassBuildings := ReadBoolean(false, UName, 'UseTransparency', true);
            MapView.AnimateBuildings := ReadBoolean(false, UName, 'AnimateBuildings', true);
            MapView.CarsEnabled := ReadBoolean(false, UName, 'ShowCars', true);
            MapView.PlanesEnabled := ReadBoolean(false, UName, 'ShowPlanes', true);
            MapView.TranspOverlays := ReadBoolean(false, UName, 'TransparentOverlays', true);
            MapView.SoundsVolume := StrToFloat(ReadString(false, UName, 'SoundFxVolume', '1'));
            SmallMapFrame.Width  := ReadInteger(false, UName, 'SmallMapWidth', SmallMapFrame.Width);
            SmallMapFrame.Height := ReadInteger(false, UName, 'SmallMapHeight', SmallMapFrame.Height);
            if SmallMapFrame.Width > Width-10
              then SmallMapFrame.Width := Width-10;
            if SmallMapFrame.Width<20
              then SmallMapFrame.Width := 70;

            if SmallMapFrame.Height> Height-40
              then SmallMapFrame.Height := Height-40;
            if SmallMapFrame.Height<40
              then SmallMapFrame.Height := 40;

            SmallMap.ZoomLevel  := ReadInteger(false, UName, 'ZoomLevel', SmallMap.ZoomLevel);
            // fMasterURLHandler.HandleURL('?frame_Id=MP3Handler&frame_Action=SetVolume&MediaId=MainSoundTrack&Volume=' + fConfigHolder.ReadString(false, fClientView.getUserName, 'SoundFxVolume', '50'));
          end;
      end;

    procedure SaveMapViewOptions;
      var
        UName: string;
      begin
        with fControl, fConfigHolder do
          begin
            UName := fClientView.getUserName;
            WriteInteger(false, UName, 'SmallMapWidth', SmallMapFrame.Width);
            WriteInteger(false, UName, 'SmallMapHeight', SmallMapFrame.Height);
            WriteInteger(false, UName, 'ZoomLevel', SmallMap.ZoomLevel);
          end;
      end;

    begin
      if (fControl<>nil)
        then
          begin
            result := fControl.MapView.HandleEvent( EventId, info );
            case EventId of
              evnHandlerExposed :
                begin
                  if fControl.MapView.Parent = nil
                    then
                      begin
                        LoadCursors;
                        fControl.MapView.Parent := fControl;
                        fControl.Links.FlatScrollBars := true;
                        fControl.MapView.ImageSuit := fClientView.getSeason;
                        fControl.OnResize( self );
                        // startX := fConfigHolder.ReadInteger( false, fClientView.getUserName, 'lastX', fClientView.getWorldXSize div 2 );
                        // startY := fConfigHolder.ReadInteger( false, fClientView.getUserName, 'lastX', fClientView.getWorldXSize div 2 );
                        // fControl.MapView.MoveTo( startY, startX );
                        //fControl.Date.Caption := FormatDateTime( 'mmmm d, yyyy', RefreshDate.Date );
                        fControl.SmallMap.OnSelect := OnSmallMapClick;
                        (*
                        fControl.UserName.Caption         := fClientView.getUserName;
                        if fClientView.getCompanyId <> 0
                          then fControl.Money.Caption := FormatMoney( fClientView.getMoney )
                          else fControl.Money.Caption := '';
                        fControl.SuperGran.OnClick        := OnSuperGran;
                        *)
                        fControl.MapView.OnObjectSelected := OnObjectSelected;
                        fControl.MapView.OnMouseUp        := OnMouseUp;
                        fControl.MapView.OnMouseDown      := OnMouseDown;
                        fControl.OnToolPressed            := OnToolPressed;
                        fControl.InitSmallMap;

                        fControl.InitLinks;
                        fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, cachepath );
                        ChangeLogView.LogPath := cachepath + 'news\news' + ActiveLanguage + '.rtf';
                        if not FileExists( ChangeLogView.LogPath )
                          then ChangeLogView.LogPath := cachepath + 'news\news0.rtf';
                        ChangeLogView.LastEntry := fConfigHolder.ReadInteger( false, fClientView.getUserMasterName, 'LastNewsEntry', 0 );
                        ChangeLogView.RenderNews;

                        if ChangeLogView.NewEntries and (ChangeLogView.ShowModal = mrOk)
                          then fConfigHolder.WriteInteger( false, fClientView.getUserMasterName, 'LastNewsEntry', ChangeLogView.LastEntry );
                        //fControl.MapView.Parent := nil;
                      end;
                  fClientView.ClientAware;
                  fControl.SetChasedUser( fClientView.getChasedUser );
                  fIdleTextTimer.Enabled := true;
                  InitMapViewOptions;
                  //if (URLFrameNotification<>nil) and (URLFrameNotification.fIsTutor)
                    //then fControl.TutorBtn.Visible := true;
                end;
              evnHandlerUnexposed :
                begin
                  SaveMapViewOptions;
                  fIdleTextTimer.Enabled := false;
                end;
              evnSetCompany :
                begin
                  fControl.ClientView := fClientView;
                  fControl.InitLinks;
                  fControl.MapView.Refresh;
                end;
              evnFrameClosed :
                begin
                  if (FrameClosed = tidHandlerName_ObjInspector) and fControl.Settings.Selected
                    then
                      begin
                        fControl.Settings.Selected := false;
                        HideQuickWeb;
                      end;
                end;
              evnLogonCompleted :
                begin
                  if fControl<>nil
                    then
                      begin
                        fControl.Settings.Selected := false;
                        HideQuickWeb;
                      end;
                end;
              evnRefreshArea :
                begin
                  //fControl.MapView.Focus.QueryUpdate( false );
                  with RefreshAreaInfo.Area do
                    fControl.MapView.RefreshRegion( Top, Left, Bottom, Right );
                end;
              evnEndOfPeriod :;
              evnTycoonRetired :;
              {
              evnChatMsg :
                begin
                  Fork( threadedPlaySelectionSound, priHigher, [fChatSoundId] );
                end;
              }
              evnMoveTo :
                fControl.MapView.MoveTo( MoveTo.Pos.y, MoveTo.Pos.x );
              evnUserChaseStarted :
                fControl.SetChasedUser( fClientView.getChasedUser );
              evnUserChaseAborted :
                fControl.SetChasedUser( '' );
              evnRefreshObject :
                if RefreshObject.KindOfChange <> fchDestruction
                  then
                    begin
                      SelectionInfo.id     := msgGetSelectionInfo;
                      SelectionInfo.Result := false;
                      fControl.MapView.Focus.Dispatch(SelectionInfo);
                      if SelectionInfo.Result and (SelectionInfo.Selection.id = RefreshObject.ObjId)
                        then Fork( threadedGetObjStatusText, priNormal, [RefreshObject.ObjId] );
                    end
                  else
                    begin
                      HideQuickWeb;
                      fControl.Settings.Enabled := false;
                    end;
              evnChatOverMap :
                fControl.SetMsgComposerVisibility( ChatOverMap );
              evnKeyCommand :
                case KeyCommand of
                  keyCmdESC :
                    begin
                      fControl.MapView.AbortFacilityBuild;
                      fControl.MapView.AbortCircuitBuild;
                      fControl.MapView.AbortCurrentSelection;
                    end;
                  keyCmdKeyN :;
                  keyCmdKeyS :;
                  keyCmdKeyW :;
                  keyCmdKeyE :;
                  keyCmdKeyNE :;
                  keyCmdKeySE :;
                  keyCmdKeySW :;
                  keyCmdKeyNW :;
                  keySetSeason0 :
                    fControl.MapView.ImageSuit := 0;
                  keySetSeason1 :
                    fControl.MapView.ImageSuit := 1;
                  keySetSeason2 :
                    fControl.MapView.ImageSuit := 2;
                  keySetSeason3 :
                    fControl.MapView.ImageSuit := 3;
                end;
              evnRefreshSeason :
                fControl.MapView.ImageSuit := RefreshSeason.Season;
              evnRefreshTycoon :
                begin
                  if fLastFacCount <> RefreshTycoon.FacCount
                    then
                      begin
                        if fLastFacCount <> -1
                          then fMasterURLHandler.HandleURL( 'local?frame_Id=Favorites&frame_Class=Favorites&frame_Align=top&frame_height=95&frame_Hidden=no' );
                        fLastFacCount := RefreshTycoon.FacCount;
                      end;
                end;
              evnGlassBuildings :
                fControl.MapView.GlassBuildings := enabled;
              evnAnimateBuildings :
                fControl.MapView.AnimateBuildings := enabled;
              evnShowCars :
                fControl.MapView.CarsEnabled := enabled;
              evnShowPlanes :
                fControl.MapView.PlanesEnabled := enabled;
              evnTranspOverlays :
                fControl.MapView.TranspOverlays := enabled;
              evnSoundsEnabled :
                begin
                  fControl.MapView.SoundsEnabled := enabled;
                  if enabled
                    then fMasterURLHandler.HandleURL( '?frame_Id=JukeBox&frame_Class=JukeBox&frame_Action=Play')
                    else fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Stop');
                end;
              evnSetSoundVolume :
                fControl.MapView.SoundsVolume := volume;
              evnHideShowFacility :
                if hideshowdata.show
                  then fControl.MapView.ShowFacilities([hideshowdata.facid])
                  else fControl.MapView.HideFacilities([hideshowdata.facid]);
              evnHideFacility :
                  fControl.MapView.HideFacilities([hideshowdata.facid]);
              evnShowAllFacilities :
                fControl.MapView.ShowFacilities([0..255]);
              evnHideAllFacilities :
                fControl.Mapview.HideFacilities([0..255]);
              evnShutDown :  //.rag
                begin
                  if fSelSoundId <> ''
                    then SoundLib.UnloadSound( fSelSoundId );
                  if fClickSoundId <> ''
                    then SoundLib.UnloadSound( fClickSoundId );
                  if fChatSoundId <> ''
                    then SoundLib.UnloadSound( fChatSoundId );
                  SaveMapViewOptions;
                  fIdleTextTimer.Enabled := false;
                  freeandnil(fIdleTextTimer);
                  fMasterURLHandler := nil;
                  fClientView       := nil;
                  fConfigHolder     := nil;
                  RemoveComponentFreeAndNil(fControl); //.rag
                end;
              evnGetSelObjInfo : // [iroel]
                with TSelObjectInfo(info) do
                  begin
                    x   := fControl.fCurrX;
                    y   := fControl.fCurrY;
                    obj := fControl.fCurrObj;
                  end;
              else
                exit;
            end;
          end;
      result := evnHandled;
    end;

  function TMapIsoHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMapIsoHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    var
      CachePath : string;
    begin
      fMasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      URLHandler.HandleEvent( evnAnswerConfigHolder, fConfigHolder );
      fControl.MasterURLHandler := URLHandler;
      fControl.ClientView := fClientView;
      fControl.MapView.setMasterURLHandler( URLHandler );
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
      fSelSoundId := CachePath + 'Sound\' + tidSound_Selection;
      if not SoundLib.LoadSoundFromFile( fSelSoundId )
        then fSelSoundId := '';
      fClickSoundId := CachePath + 'Sound\' + tidSound_Click;
      if not SoundLib.LoadSoundFromFile( fClickSoundId )
        then fClickSoundId := '';
      {
      fChatSoundId := CachePath + 'Sound\' + tidSound_Chat;
      if not SoundLib.LoadSoundFromFile( fChatSoundId )
        then fChatSoundId := '';
      }
    end;

  procedure TMapIsoHandler.threadedGetObjStatusText( const parms : array of const );
    var
      ObjId     : TObjId absolute parms[0].vInteger;
      s1, s2    : string;
      ErrorCode : TErrorCode;
    begin
      if fStatusEnabled
        then
          begin
            s1 := fClientView.ObjectStatusText( sttSecondary, ObjId, ErrorCode );
            s2 := fClientView.ObjectStatusText( sttHint, ObjId, ErrorCode );
            Threads.Join( syncGetObjStatusText, [s1, s2] );
          end;
    end;

  procedure TMapIsoHandler.syncGetObjStatusText( const parms : array of const );
    var
      text1 : string;
      text2 : string;
    begin
      text1 := parms[0].vPChar;
      text2 := parms[1].vPChar;
      if text1 <> ''
        then
          begin
            fControl.SecondaryText.Caption := text1;
            fIdleTextTimer.Enabled := false;
          end
        else
          begin
            ShowContextText;
            fIdleTextTimer.Enabled := true;
          end;
      fControl.SecondaryText.Hint := fControl.SecondaryText.Caption;
      if text2 <> ''
        then fControl.HintText.Caption := text2
        else fControl.HintText.Caption := GetLiteral('Literal216');
      fControl.HintText.Hint := fControl.HintText.Caption;
    end;

  procedure TMapIsoHandler.threadedGetContextText( const parms : array of const );
    var
      text : string;
    begin
      text := fClientView.ContextText;
      Join( syncGetContextText, [text] );
    end;

  procedure TMapIsoHandler.syncGetContextText( const parms : array of const );
    var
      text : string absolute parms[0].vPChar;
    begin
      if fIdleTextTimer.Enabled
        then fControl.SecondaryText.Caption := text;
    end;


  procedure TMapIsoHandler.threadedPlaySelectionSound( const parms : array of const );
    var
      SoundId : string absolute parms[0].vPChar;
    begin
      SoundLib.PlaySound( SoundId, 0, false, 1, 1, 0 );
    end;

  procedure TMapIsoHandler.treadedUnfocusObject( const parms : array of const );
    var
      ObjId   : integer absolute parms[0].vInteger;
      useless : TErrorCode;
    begin
      fClientView.UnfocusObject( fLastObj, useless );
    end;
    
  procedure TMapIsoHandler.ShowContextText;
    begin
      Fork( threadedGetContextText, priLower, [0] );
    end;

  procedure TMapIsoHandler.OnObjectSelected( ObjId : TObjId; i, j : integer );
    begin
      if fControl<>nil
        then
          begin
            if fControl.SmallMap <> nil
              then fControl.SmallMap.Selection := Point(j, i);
            fObjectId := ObjId;
            fControl.SetCurrObj( ObjId, j, i );
            Fork( threadedGetObjStatusText, priNormal, [ObjId] );
            if ObjId <> 0
              then
                begin
                  Fork( threadedPlaySelectionSound, priHigher, [fSelSoundId] );
                  fControl.Settings.Enabled := true;
                  if fQWOn or (fActionButton = mbRight)
                    then ShowQuickWeb;
                end
              else
                begin
                  HideQuickWeb;
                  fControl.Settings.Enabled := false;
                  Fork( treadedUnfocusObject, priNormal, [fLastObj] );
                end;
          end;
      fLastObj := ObjId;
    end;

  {
  procedure TMapIsoHandler.OnFacilityBuild(i, j: integer; const FacClass : string);
    var
      ErrorCode : TErrorCode;
    begin
      fStatusEnabled := true;
      fControl.MapView.Cursor := crDefault;
      fClientView.NewFacility( FacClass, fClientView.getCompanyId, j, i, ErrorCode );
      case ErrorCode of
        NOERROR : ;
        ERROR_ZoneMissmatch :
          Application.MessageBox(
            'The Mayor of the city doesn''t allow a building of this type ' +
            'on this location. Please note that the zone of this location (indicated by color) has ' +
            'to match with the zone type of the facility you intended to build. Buildings zone type ' +
            'are shown besides the facility icon on the Build Panel.',
            'CONSTRUCTION REQUEST REJECTED', MB_ICONWARNING or MB_OK );
        ERROR_AreaNotClear :
          Application.MessageBox(
            'You cannot build there. Area not clear.',
            'CONSTRUCTION REQUEST REJECTED', MB_ICONWARNING or MB_OK );
        else
          Application.MessageBox(
            'Cannot build facility here.',
            'Error', MB_ICONERROR or MB_OK );
      end;
    end;
  }
  procedure TMapIsoHandler.OnFacilityBuild(i, j : integer; const facclass : string; visclass : integer);
    begin
      fStatusEnabled := true;
      fControl.MapView.Cursor := crDefault;
      Fork( threadedBuildFacility, priNormal, [i, j, facclass, visclass] );
    end;

  procedure TMapIsoHandler.OnFacilityBuildAbort;
    begin
      fStatusEnabled := true;
      fControl.MapView.Cursor := crDefault;
    end;

  {
  procedure TMapIsoHandler.OnCircuitBuild( const CircuitSegments : TSegmentReport; CKind, Cost : integer );
    var
      SegIdx    : integer;
      ErrorCode : TErrorCode;
    begin
      ClearStatusLine;
      fControl.MapView.Cursor := crDefault;
      SegIdx := 0;
      ErrorCode := NOERROR;
      with CircuitSegments do
        while (SegIdx < SegmentCount) and (ErrorCode = NOERROR) do
          begin
            fClientView.CreateCircuitSeg( CKind, fClientView.getTycoonId, Segments[SegIdx].x1, Segments[SegIdx].y1, Segments[SegIdx].x2, Segments[SegIdx].y2, cost, ErrorCode );
            inc( SegIdx );
          end;
    end;
  }

  procedure TMapIsoHandler.OnCircuitBuild( const CircuitSegments : TSegmentReport; CKind, Cost : integer );
    var
      pSegReport : PSegmentReport; 
    begin
      {$IFDEF POWERUSER}
      Cost := 0;
      {$ENDIF}
      ClearStatusLine;
      fControl.MapView.Cursor := crDefault;
      new( pSegReport );
      pSegReport.SegmentCount := CircuitSegments.SegmentCount;
      getmem( pSegReport.Segments, pSegReport.SegmentCount*sizeof(pSegReport.Segments[0]) );
      move( CircuitSegments.Segments[0], pSegReport.Segments[0], pSegReport.SegmentCount*sizeof(pSegReport.Segments[0]) );
      Fork( threadedCircuitBuild, priNormal, [pSegReport, CKind, Cost] );
    end;

  procedure TMapIsoHandler.OnCircuitEditChange( cost : integer; var allowed : boolean );
    begin
      fControl.SecondaryText.Caption := GetFormattedLiteral('Literal217', [FormatMoney( cost )]);
      fControl.HintText.Caption := GetLiteral('Literal218');
    end;

  procedure TMapIsoHandler.OnCircuitBuildAbort;
    begin
      ClearStatusLine;
      fControl.MapView.Cursor := crDefault;
    end;

  procedure TMapIsoHandler.OnRegionChange(Sender : TObject; const Origin, Size : TPoint);
    begin
      if fControl.SmallMap <> nil
        then fControl.SmallMap.SetArea(Origin, Size, 2 shl TLocalFiveControl(fControl.MapView).GetZoomLevel);
    end;

  procedure TMapIsoHandler.OnSmallMapClick(Sender : TObject; i, j : integer);
    begin
      fControl.MapView.MoveTo(i, j);
    end;

  (*
  procedure TMapIsoHandler.OnSuperGran( Sender : TObject );
    var
      i         : integer;
      s         : string;
      x, y      : integer;
      report    : TObjectReport;
      ObjId     : TObjId;
      ErrorCode : TErrorCode;
    begin
      fDoGran := not fDoGran;
      i       := 0;
      while not fDoGran do
        begin
          inc( i );
          fControl.SuperGran.Caption := IntToStr(i);
          x := random(500);
          y := random(500);
          report := fClientView.ObjectsInArea( x, y, random(500 - x), random(500 - y), ErrorCode );
          if report.ObjectCount > 0
            then
              begin
                x := report.Objects[0].x;
                y := report.Objects[0].y;
                ObjId := fClientView.ObjectAt( x, y, ErrorCode );
                s := fClientView.ObjectStatusText( sttMain, ObjId, ErrorCode );
                if s <> ''
                  then fClientView.SayThis( '', '<SUPERGRAN> ' + s, ErrorCode );
                s := fClientView.ObjectStatusText( sttSecondary, ObjId, ErrorCode );
                if s <> ''
                  then fClientView.SayThis( '', '<SUPERGRAN> ' + s, ErrorCode );
              end;
          fClientView.DisposeObjectReport( report );
          Application.ProcessMessages;
        end;
      fControl.SuperGran.Caption := 'Call Super Gran';
    end;
  *)

  procedure TMapIsoHandler.OnMouseUp( Sender : TObject; Button : TMouseButton; Shift : TShiftState; x, y : integer);
    begin
      fActionShift  := Shift;
      fActionButton := Button;
    end;

  procedure TMapIsoHandler.OnMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; x, y : integer);
    begin
      Fork( threadedPlaySelectionSound, priHigher, [fClickSoundId] );
    end;

  procedure TMapIsoHandler.OnToolPressed( ToolId : integer );
    begin
      fControl.MapView.AbortCurrentSelection;
      case ToolId of
        idQuickWeb :
          if not fQWOn
            then ShowQuickWeb
            else HideQuickWeb;
        {
        idShowMap :
          begin
            SurfaceViewer.ClientView := fClientView;
            SurfaceViewer.Show;
          end;
        }
        idVisOpt :
          begin
            fStatusEnabled := false;
            fControl.SecondaryText.Caption := GetLiteral('Literal219');
            fControl.HintText.Caption := GetLiteral('Literal220');
            fControl.MapView.StartSelection( sidPickOnMap );
          end;
      end;
    end;

  function TMapIsoHandler.OnMouseOnObject( SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnMouseOnObjectRetCode;
    begin
      case SelId of
        sidDestroyRoad :
          if ObjInfo.objkind = okRoad
            then result := mooCanSelect
            else result := mooCannotSelect;
        sidPickOnMap :
          if ObjInfo.objkind = okBuilding
            then result := mooCanSelect
            else result := mooCannotSelect;
        else
          result := mooCannotSelect;
      end;
    end;

  function TMapIsoHandler.OnObjectClicked( SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnObjectClickedRetCode;
    var
      ErrorCode     : TErrorCode;
      SelectionInfo : TGetSelectionInfo;
      {
      FacId         : TObjId;
      report        : string;
      }
    begin
      Fork( threadedPlaySelectionSound, priHigher, [fClickSoundId] );
      case SelId of
        sidDestroyRoad :
          if ObjInfo.objkind = okRoad
            then
              begin
                fClientView.BreakCircuitAt( cirRoads, fClientView.getTycoonId, ObjInfo.c, ObjInfo.r, ErrorCode );
                result := ocAbort;
                case ErrorCode of
                  NOERROR :
                    result := ocGoOn;
                  ERROR_AccessDenied :
                    ShowMsgBox( GetLiteral('Literal221'), GetLiteral('Literal222'), 0, true, false );
                  else
                    ShowMsgBox( GetLiteral('Literal223'), GetLiteral('Literal224'), 0, true, false );
                end;
              end
            else result := ocAbort;
        sidPickOnMap :
          if ObjInfo.objkind = okBuilding
            then
              begin
                //result := ocAbort;
                SelectionInfo.id     := msgGetSelectionInfo;
                SelectionInfo.Result := false;
                fControl.MapView.Focus.Dispatch( SelectionInfo );
                Fork( threadedConnect, priNormal, [SelectionInfo.Selection.id, ObjInfo.c, ObjInfo.r] );
                result := ocGoOn;
              end
            else result := ocAbort;
        else
          result := ocAbort;
      end;
      if result = ocAbort
        then ClearStatusLine;
    end;

  procedure TMapIsoHandler.OnIdleTextChange( Sender : TObject );
    begin
      if fStatusEnabled
        then ShowContextText;
    end;

  {
  procedure TMapIsoHandler.OnAreaSelection( imin, jmin, imax, jmax : integer; value : single );
    var
      ErrorCode : TErrorCode;
    begin
      fClientView.DefineZone( fClientView.getTycoonUId, round(value), jmin, imin, jmax, imax, ErrorCode );
      if not fControl.MapOptionsPanel.Visible
        then fControl.MapBtn.Click;
      fControl.Tabs.CurrentTab := 1;
    end;
  }
  procedure TMapIsoHandler.OnAreaSelection( imin, jmin, imax, jmax : integer; value : single );
    var
      i : integer;
    begin
      if value >= 0
        then
          begin
            {
            if not fControl.MapOptionsPanel.Visible
              then fControl.MapBtn.Click;
            }
            //fControl.Tabs.CurrentTab := 1;
            i := round(value);
            Fork( threadedDefineZone, priNormal, [jmin, imin, jmax, imax, i] );
          end
        else Fork( threadedWipeCircuit, priNormal, [jmin, imin, jmax, imax] );
    end;

  procedure TMapIsoHandler.threadedBuildFacility( const parms : array of const );
    var
      ErrorCode : TErrorCode;
      i, j      : integer;
      facclass  : string;
      visclass  : integer;
    begin
      try
        i := parms[0].vInteger;
        j := parms[1].vInteger;
        facclass := parms[2].vPChar;
        visclass := parms[3].vInteger;
        Join( syncCreateDummyFacility, [i, j, visclass] );
        fClientView.NewFacility( facclass, fClientView.getCompanyId, j, i, ErrorCode );
        Join( syncBuildFacility, [ErrorCode, i, j, visclass] );
      except
      end;
    end;

  procedure TMapIsoHandler.syncBuildFacility( const parms : array of const );
    var
      ErrorCode : integer absolute parms[0].vInteger;
      L1, L2 : string;
    begin
      fControl.MapView.RemoveDummyFacility( parms[1].VInteger, parms[2].VInteger, parms[3].VInteger, ErrorCode <> NOERROR );
      case ErrorCode of
        NOERROR : ;
        ERROR_ZoneMissmatch :
          begin
            L1 := GetLiteral('Literal225');
            L2 := GetLiteral('Literal226');
          end;
        ERROR_AreaNotClear :
          begin
            L1 := GetLiteral('Literal230');
            L2 := GetLiteral('Literal231');
          end;
        ERROR_TooManyFacilities :
          begin
            L1 := GetLiteral('Literal232');
            L2 := GetLiteral('Literal233');
          end;
        ERROR_BuildingTooClose:
          begin
            L1 := GetLiteral('Literal232');
            L2 := GetLiteral('Literal491');
          end;
        else
          begin
            L1 := GetLiteral('Literal234');
            L2 := GetLiteral('Literal235');
          end;
      end;
     if (ErrorCode<>NOERROR)
       then ShowMsgBox( L1, L2, 0, true, false );
    end;

  procedure TMapIsoHandler.syncCreateDummyFacility( const parms : array of const );
    begin
      fControl.MapView.CreateDummyFacility( parms[0].VInteger, parms[1].VInteger, parms[2].VInteger );
    end;

  procedure TMapIsoHandler.threadedCircuitBuild( const parms : array of const );
    var
      CircuitSegments : PSegmentReport;
      CKind, Cost     : integer;
      SegIdx          : integer;
      ErrorCode       : TErrorCode;
    begin
      try
        CircuitSegments := PSegmentReport(parms[0].vPointer);
        CKind           := parms[1].vInteger;
        Cost            := parms[2].vInteger;
        SegIdx          := 0;
        ErrorCode       := NOERROR;
        Join( syncCreateDummyCircuits, [CircuitSegments, CKind] );
        with CircuitSegments^ do
          while (SegIdx < SegmentCount) and (ErrorCode = NOERROR) do
            begin
              fClientView.CreateCircuitSeg( CKind, fClientView.getTycoonId, Segments[SegIdx].x1, Segments[SegIdx].y1, Segments[SegIdx].x2, Segments[SegIdx].y2, cost div SegmentCount, ErrorCode );
              inc( SegIdx );
            end;
        Join( syncCircuitBuild, [CircuitSegments, CKind] );
      except
      end;
    end;

  procedure TMapIsoHandler.syncCircuitBuild( const parms : array of const );
    var
      CircuitSegments : PSegmentReport absolute parms[0].VPointer;
      CKind           : integer;
      i               : integer;
    begin
      CKind := parms[1].vInteger;
      with CircuitSegments^ do
        for i := 0 to pred(SegmentCount) do
          fControl.MapView.RemoveDummyCircuitSeg( CKind, Segments[i].x1, Segments[i].y1, Segments[i].x2, Segments[i].y2 );
    end;

  procedure TMapIsoHandler.syncCreateDummyCircuits( const parms : array of const );
    var
      CircuitSegments : PSegmentReport absolute parms[0].VPointer;
      CKind           : integer;
    begin
      CKind := parms[1].vInteger;
      fControl.MapView.CreateDummyCircuitSegs( CKind, CircuitSegments^ );
    end;

  procedure TMapIsoHandler.threadedDefineZone( const parms : array of const );
    var
      imin, jmin, imax, jmax : integer;
      value                  : integer;
      ErrorCode              : TErrorCode;
    begin
      jmin  := parms[0].vInteger;
      imin  := parms[1].vInteger;
      jmax  := parms[2].vInteger;
      imax  := parms[3].vInteger;
      value := parms[4].VInteger;
      fClientView.DefineZone( fClientView.getTycoonUId, value, jmin, imin, jmax, imax, ErrorCode );
      //Join( syncInvalidateSurface, [0] );
    end;

  {
  procedure TMapIsoHandler.syncInvalidateSurface( const parms : array of const );
    begin
      fControl.MapView.InvalidateCurrentSurface;
    end;
  }

  procedure TMapIsoHandler.threadedWipeCircuit( const parms : array of const );
    var
      imin, jmin, imax, jmax : integer;
      ErrorCode              : TErrorCode;
    begin
      jmin  := parms[0].vInteger;
      imin  := parms[1].vInteger;
      jmax  := parms[2].vInteger;
      imax  := parms[3].vInteger;
      fClientView.WipeCircuit( cirRoads, fClientView.getTycoonId, jmin, imin, jmax, imax, ErrorCode );
    end;

  procedure TMapIsoHandler.threadedConnect( const parms : array of const );
    var
      SelectedId : integer;
      x, y       : integer;
      ErrorCode  : TErrorCode;
      FacId      : integer;
      report     : string;
    begin
      try
        SelectedId := parms[0].vInteger;
        x          := parms[1].vInteger;
        y          := parms[2].vInteger;
        FacId := fClientView.ObjectAt( x, y, ErrorCode );
        if ErrorCode = NOERROR
          then
            begin
              report := fClientView.ConnectFacilities( SelectedId, FacId, ErrorCode );
              Join( syncConnect, [report, ErrorCode] );
            end
      except
      end;
    end;

  procedure TMapIsoHandler.syncConnect( const parms : array of const );
    var
      report    : string;
      ErrorCode : TErrorCode;
    begin
      report    := parms[0].vPChar;
      ErrorCode := parms[1].vInteger;
      case ErrorCode of
        NOERROR :
          if report <> ''
            then
              begin
                HintBoxWindow.HintText := report;
                HintBoxWindow.Show;
              end
            else ShowMsgBox( GetLiteral('Literal236'), GetLiteral('Literal237'), 0, true, false );
        else ShowMsgBox( GetLiteral('Literal238'), GetLiteral('Literal239'), 0, true, false );
      end;
    end;

  {
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=ObjectInspector&frame_Class=ObjectInspector&frame_Align=bottom' );

  procedure TMapIsoHandler.ShowQuickWeb;
    var
      PageName      : string;
      PagePath      : string;
      URL           : string;
      SelectionInfo : TGetSelectionInfo;
    begin
      if fObjectId <> 0
        then
          begin
            SelectionInfo.id     := msgGetSelectionInfo;
            SelectionInfo.Result := false;
            fControl.MapView.Focus.Dispatch(SelectionInfo);
            assert(SelectionInfo.Result);
            with SelectionInfo.Selection do
              begin
                PagePath := VisualClassesHandler.ReadString( ClassId, 'Paths', 'Voyager', 'Visual/Voyager/IsoMap/', fMasterURLHandler );
                PageName := VisualClassesHandler.ReadString( ClassId, 'VoyagerActions', 'QuickWeb', 'UnknownEdit.asp', fMasterURLHandler );
                URL := fClientView.getWorldURL +
                       PagePath +
                       PageName +
                       '?x=' + IntToStr(c) +
                       '&y=' + IntToStr(r) +
                       '&ClassId=' + IntToStr(Id) +
                       '&SecurityId=' + fClientView.getSecurityId +
                       '&WorldName=' + fClientView.getWorldName +
                       '&DAAddr=' + fClientView.getDAAddr +
                       '&DAPort=' + IntToStr(fClientView.getDAPort);
                fControl.ShowURLInQuickWeb( URL );
              end
          end
        else fControl.HideQuickWeb;
    end;
  }

  procedure TMapIsoHandler.ShowQuickWeb;
    var
      URL           : string;
      SelectionInfo : TGetSelectionInfo;
    begin
      if fObjectId <> 0
        then
          begin
            SelectionInfo.id     := msgGetSelectionInfo;
            SelectionInfo.Result := false;
            fControl.MapView.Focus.Dispatch(SelectionInfo);
            assert(SelectionInfo.Result);
            with SelectionInfo.Selection do
              begin
                URL := '?frame_Action=ShowObject' +
                       '&frame_Id=ObjectInspector' +
                       '&frame_Class=ObjectInspector' +
                       //'&frame_Height=30%' +
                       '&frame_Align=bottom' +
                       '&x=' + IntToStr(c) +
                       '&y=' + IntToStr(r) +
                       '&ObjectId=' + IntToStr(SelectionInfo.Selection.id) +
                       '&ClassId=' + IntToStr(ClassId);
                fMasterURLHandler.HandleURL( URL );
              end;
            fQWOn := true;
            //fControl.Settings.Text := 'HIDE';
            fControl.Settings.Selected := true;
          end
        else HideQuickWeb;
    end;

  procedure TMapIsoHandler.HideQuickWeb;
    begin
      fQWOn := false;
      fMasterURLHandler.HandleURL( '?frame_Id=ObjectInspector&frame_Close=yes' );
      //fControl.Settings.Text := 'SETTINGS';
      fControl.Settings.Selected := false;
    end;

  procedure TMapIsoHandler.ClearStatusLine;
    begin
      fStatusEnabled := true;
      fControl.SecondaryText.Caption := GetLiteral('Literal240');
      fControl.HintText.Caption := GetLiteral('Literal241');
    end;

  procedure TMapIsoHandler.LoadCursors;
    var
      CachePath : string;
      cursor    : HCURSOR;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
      cursor := LoadCursorFromFile( pchar(CachePath + 'Cursors\Connect.cur') );
      Screen.Cursors[crConnect] := cursor;
      cursor := LoadCursorFromFile( pchar(CachePath + 'Cursors\Forbiden.cur') );
      Screen.Cursors[crNo] := cursor;
      cursor := LoadCursorFromFile( pchar(CachePath + 'Cursors\Handpnt.cur') );
      Screen.Cursors[crHandPoint] := cursor;
    end;

end.


