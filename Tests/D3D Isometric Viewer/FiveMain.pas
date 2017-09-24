unit FiveMain;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, Buttons, Buffer, GameTypes, GameControl, FiveControl,
    LanderTypes, MapTypes, FocusTypes, AxlDebug, VoyagerInterfaces, ToolWin, ComCtrls,
    VoyagerServerInterfaces, FiveIsometricMap, FiveTypes, FullScreenWindowMgr,
    DirectDraw;

  type
    TFiveMainForm = class(TForm)
      MainPanel: TPanel;
      Timer: TTimer;
      AuxPanel: TPanel;
      Notebook: TNotebook;
      ToolsPanel: TPanel;
      FullScreen: TSpeedButton;
      Debug: TSpeedButton;
      Build: TSpeedButton;
      StatusText: TLabel;
      BuildFloatingPanel: TSpeedButton;
      DebugLog: TRichEdit;
      MapPanel: TPanel;
      sbUp: TSpeedButton;
      sbCenter: TSpeedButton;
      sbDown: TSpeedButton;
      sbRight: TSpeedButton;
      sbLeft: TSpeedButton;
      sbZoomUp: TSpeedButton;
      sbZoomDown: TSpeedButton;
      sbViewSelection: TSpeedButton;
      sbViewArea: TSpeedButton;
      sbMaxZoom: TSpeedButton;
      sbMinZoom: TSpeedButton;
      SeasonTimer: TTimer;
    ShutDownTimer: TTimer;
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure MainPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure TimerTimer(Sender: TObject);
      procedure ToolClick(Sender: TObject);
      procedure BuildFloating(Sender: TObject);
      procedure sbUpClick(Sender: TObject);
      procedure sbDownClick(Sender: TObject);
      procedure sbLeftClick(Sender: TObject);
      procedure sbRightClick(Sender: TObject);
      procedure sbZoomUpClick(Sender: TObject);
      procedure sbZoomDownClick(Sender: TObject);
      procedure sbCenterClick(Sender: TObject);
      procedure sbViewSelectionClick(Sender: TObject);
      procedure sbViewAreaClick(Sender: TObject);
      procedure sbMaxZoomClick(Sender: TObject);
      procedure sbMinZoomClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure SeasonTimerTimer(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShutDownTimerTimer(Sender: TObject);
    public
      destructor Destroy;  override;
    private
      fControl      : TFiveControl;
      fIsometricMap : TFiveIsometricMap;
      procedure IsometricMapSelect(Sender : TObject; i, j : integer);
    private  // Mouse support
      fScrolling : boolean;
      fMouseX    : integer;
      fMouseY    : integer;
    private
      fMasterURLHandler : IMasterURLHandler;
      fURLHandler       : IURLHandler;
      procedure FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
      procedure FacilityBuildAbort;
      procedure CircuitBuild(const CircuitSegments : TSegmentReport; CKind, Cost : integer);
      procedure CircuitBuildAbort;
      procedure UpdateURLHandler(const WorldManager : ILocalCacheManager);
      procedure ObjectSelected(which : integer; i, j : integer);
      procedure RegionChanged(Sender : TObject; const Origin, Size : TPoint);
    private // keyboard support
      fLastChar     : char;
      fUpKeyDown    : boolean;
      fDownKeyDown  : boolean;
      fLeftKeyDown  : boolean;
      fRightKeyDown : boolean;
    public
      procedure KeyPress(var key : char); override;
      procedure KeyDown(var key : Word; Shift : TShiftState); override;
      procedure KeyUp(var key : Word; Shift : TShiftState); override;
    private
      FloatingPanel    : TPanel;
      fFSWindowManager : TFullScreenWindowManager;
      fDirectDraw      : IDirectDraw7;
    private
      procedure SetMainPanelDefaultRegion;
      {$IFDEF SHOWAVAILVIDEOMEM}
      procedure ApplicationOnIdle(Sender : TObject; var Done : boolean);
      {$ENDIF}
    end;

  var
    FiveMainForm: TFiveMainForm;

implementation

  uses
    Warnings, ShutDown, ServerCnxEvents, VoyagerUIEvents, LocalCacheManager,
    ClientView, CircuitsHandler, Protocol, CaptureCoords, SoundLib, Threads,
    DDrawD3DManager, LogFile, SelectVideoMode, IsoProfile, DirectDrawUtils;

  {$R *.DFM}

  type
    TLocalFiveControl = class(TFiveControl);

  const
    cThinIsometricDelta  = 5;
    cThickIsometricDelta = 20;
    cIsometricZoomDelta  = 15;

  type
    TDebugConsole =
      class(TInterfacedObject, IDebugConsole)
        public
          constructor Create(Target : TRichEdit);
        private // IDebugConsole
          fTarget : TRichEdit;
          procedure WriteDebugStr(const which : string);
      end;

  // TDebugConsole

  constructor TDebugConsole.Create(Target : TRichEdit);
    begin
      inherited Create;
      fTarget := Target;
    end;

  procedure TDebugConsole.WriteDebugStr(const which : string);
    begin
      if fTarget.Visible
        then fTarget.Lines.Add(which);
      FiveMainForm.StatusText.Caption := which;
    end;

  // TFiveMainForm

  procedure TFiveMainForm.FormCreate(Sender: TObject);
    {
    const
      cMinScreenWidth  = 640;
      cMinScreenHeight = 480;
      cMaxScreenWidth  = 1024;
      cMaxScreenHeight = 768;
      cMinScreenBits   = 15;
    var
      i           : integer;
      VideoModes  : PVideoModes;
      VideoMode   : PVideoMode;
      UsableModes : TList;
    }
    begin
      InitDDrawD3D(Handle, 1024, 768);
      {
      // Allow user to select video mode
      VideoModes := DirectDrawMgr.AvailableVideoModes;
      SelectVideoModeForm := TSelectVideoModeForm.Create(Self);
      UsableModes := TList.Create;
      try
        try
          with SelectVideoModeForm do
            begin
              VideoModesList.ItemIndex := 0;
              for i := 0 to pred(VideoModes.Count) do
                if (VideoModes.Modes[i].vmWidth >= cMinScreenWidth) and (VideoModes.Modes[i].vmWidth <= cMaxScreenWidth) and
                   (VideoModes.Modes[i].vmHeight >= cMinScreenHeight) and (VideoModes.Modes[i].vmHeight <= cMaxScreenHeight) and
                   (VideoModes.Modes[i].vmBitCount >= cMinScreenBits)
                  then
                    begin
                      new(VideoMode);
                      VideoMode^ := VideoModes.Modes[i];
                      UsableModes.Add(VideoMode);
                      VideoModesList.Items.Add(IntToStr(VideoMode.vmWidth) + 'x' + IntToStr(VideoMode.vmHeight) + 'x' + IntToStr(VideoMode.vmBitCount));
                    end;
              ShowModal;
              VideoMode := UsableModes[VideoModesList.ItemIndex];
              DirectDrawMgr.SetVideoMode(VideoMode.vmWidth, VideoMode.vmHeight, VideoMode.vmBitCount, true);
            end;
        finally
          SelectVideoModeForm.Free;
        end;
      finally
        for i := 0 to pred(UsableModes.Count) do
          dispose(PVideoMode(UsableModes[i]));
        UsableModes.Free;
      end;
      SetBounds(0, 0, Screen.Width, Screen.Height);
      }
      // >>> should check if using ddraw
      fDirectDraw := DDrawD3DMgr.DirectDraw; 
      fFSWindowManager := TFullScreenWindowManager.Create(Handle, DDrawD3DMgr.DirectDraw, DDrawD3DMgr.PrimarySurface, DDrawD3DMgr.BackBuffer);
      {$IFDEF SHOWAVAILVIDEOMEM}
      Application.OnIdle := ApplicationOnIdle;
      {$ENDIF}
      if Debugging
        then AxlDebug.Console := TDebugConsole.Create(DebugLog);
      FloatingPanel := TPanel.Create(Self);
      with FloatingPanel do
        begin
          Left   := 100;
          Top    := 100;
          Width  := 200;
          Height := 200;
          Visible := false;
          Parent := Self;
        end;
      {$IFNDEF FLIP}
      if fFSWindowManager <> nil
        then fFSWindowManager.RegisterWindow(FloatingPanel.Handle, false, 0);
      {$ENDIF}
      InitSoundLibrary(Handle);
      SetLogFile('C:\Temp\' + ExtractFileName(Application.ExeName) + '.log');
      InitIsoViewerProfiles;
    end;

  procedure TFiveMainForm.FormShow(Sender: TObject);
    var
      Agents     : TInitializationAgents;
      ClientView : IClientView;
      Informant  : IWarningInformant;
      msg        : TGetMapperMsg;
    begin
      fControl := TFiveControl.Create(Self);
      fControl.Init(DDrawD3DMgr.Direct3DDevice, DDrawD3DMgr.PrimarySurface, DDrawD3DMgr.BackBuffer, fFSWindowManager);
      //fControl.Init(DDrawD3DMgr.PrimarySurface, DDrawD3DMgr.BackBuffer, fFSWindowManager);
      with fControl do
        begin
          Align     := alClient;
          ZoomLevel := 3;   // ord(zr32x64);
          Parent    := MainPanel;
          OnFacilityBuild      := FacilityBuild;
          OnFacilityBuildAbort := FacilityBuildAbort;
          OnCircuitBuild       := CircuitBuild;
          OnCircuitBuildAbort  := CircuitBuildAbort;
          OnObjectSelected     := ObjectSelected;
          OnRegionChange       := RegionChanged;
        end;
      SetMainPanelDefaultRegion; // >> should check if using direct draw
      fURLHandler := IMetaURLHandler(fControl).Instantiate;
      fMasterUrlHandler := TTestMasterUrlHandler.Create;
      fMasterUrlHandler.HandleEvent(evnAnswerClientView, ClientView);
      InitViewer(ClientView, nil, Agents);
      Agents.Manager.Load(ClientView.getWorldURL);
      UpdateURLHandler(Agents.Manager);
      fControl.Document := Agents.Document;
      fURLHandler.setMasterURLHandler(fMasterUrlHandler);
      // IsometricMap
      fIsometricMap := TFiveIsometricMap.Create(Self);
      fIsometricMap.Align    := alClient;
      fIsometricMap.Parent   := MapPanel;
      fIsometricMap.OnSelect := IsometricMapSelect;
      msg.id := msgGetMapper;
      fControl.Focus.Dispatch(msg);
      fIsometricMap.Mapper := msg.Mapper;
      Informant := fControl.Focus.GetInformant;
      if Informant <> nil
        then Informant.AttachTarget(fIsometricMap);
    end;

  destructor TFiveMainForm.Destroy;
    var
      Informant : IWarningInformant;
    begin
      Height := 10;
      Informant := fControl.Focus.GetInformant;
      if Informant <> nil
        then Informant.DetachTarget(fIsometricMap);
      fURLHandler := nil;
      DoShutDown;
      AxlDebug.Console := nil;
      fControl.Parent := nil;
      fControl.Owner.RemoveComponent(fControl);
      inherited;
    end;

  procedure TFiveMainForm.IsometricMapSelect(Sender : TObject; i, j : integer);
    begin
      fControl.MoveTo(i, j);
    end;

  procedure TFiveMainForm.ObjectSelected(which : integer; i, j : integer);
    begin
      if fIsometricMap <> nil
        then fIsometricMap.Selection := Point(j, i);
    end;

  procedure TFiveMainForm.RegionChanged(Sender : TObject; const Origin, Size : TPoint);
    begin
      if fIsometricMap <> nil
        then fIsometricMap.SetArea(Origin, Size, 2 shl TLocalFiveControl(fControl).GetZoomLevel);
    end;

  procedure TFiveMainForm.KeyPress(var key : char);
    const
      SurfaceScale : array [0 .. 2, 0 .. 1] of TColorScalePt =
        (
         ((value : 0; color : clLime), (value : 10; color : clBlue)),
         ((value : 0; color : clLime), (value : 10; color : clRed)),
         ((value : 0; color : clBlue), (value : 10; color : clYellow))
        );
    var
      LastChar : char;
    begin
      inherited;
      LastChar := fLastChar;
      fLastChar := upcase(key);
      case upcase(key) of
        'B':
          fControl.StartFacilityBuild('', 2112);
        'C':
          fControl.StartCircuitBuild(cirRoads);
        'R':
          fControl.StartCircuitBuild(cirRailroads);
        'M':
          if CaptureCoordinates.ShowModal = mrOK
            then fControl.MoveTo(StrToInt(CaptureCoordinates.X.Text), StrToInt(CaptureCoordinates.Y.Text));
        'X':
          fControl.ShowFacilityCnxs;
        'S':
          fControl.ShowSurface('test', ssUnder, rmInmediate, Slice(SurfaceScale[random(3)], 2));
        'A':
          fControl.StartAreaSelection(5, clLime, [axWater, axRoad, axConcrete, axBuilding]);
        'L':
          fControl.ShowLoosingFacilities(1);
        #27:
          case LastChar of
            'B':
              fControl.AbortFacilityBuild;
            'C', 'R':
              fControl.AbortCircuitBuild;
            'X':
              fControl.HideFacilityCnxs;
            'S':
              fControl.HideCurrentSurface;
            'A':
              fControl.AbortAreaSelection;
            'L':
              fControl.HideLoosingFacilities;
          end
        else
          fLastChar := LastChar;
      end;
    end;

  procedure TFiveMainForm.KeyDown(var key : Word; shift : TShiftState);
    var
      ScrollStartInfo : TEvnScrollStartInfo;
      ScrollInfo      : TEvnScrollInfo;
    begin
      case key of
        VK_ADD:
          fControl.ZoomIn;
        VK_SUBTRACT:
          fControl.ZoomOut;
        VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
          begin
            if not fScrolling
              then
                begin
                  fScrolling := true;
                  SetCapture(MainPanel.Handle);
                  ScrollStartInfo.MousePos := Point(0, 0);
                  fURLHandler.HandleEvent(evnScrollStart, ScrollStartInfo);
                end;
            case key of
              VK_UP:
                fUpKeyDown := true;
              VK_DOWN:
                fDownKeyDown := true;
              VK_LEFT:
                fLeftKeyDown := true;
              VK_RIGHT:
                fRightKeyDown := true;
            end;
            if fUpKeyDown
              then
                if not fDownKeyDown
                  then ScrollInfo.DirInfo[scrVertical] := sbsNegative
                  else ScrollInfo.DirInfo[scrVertical] := sbsNone
              else
                if fDownKeyDown
                  then ScrollInfo.DirInfo[scrVertical] := sbsPositive
                  else ScrollInfo.DirInfo[scrVertical] := sbsNone;
            if fLeftKeyDown
              then
                if not fRightKeyDown
                  then ScrollInfo.DirInfo[scrHorizontal] := sbsNegative
                  else ScrollInfo.DirInfo[scrHorizontal] := sbsNone
              else
                if fRightKeyDown
                  then ScrollInfo.DirInfo[scrHorizontal] := sbsPositive
                  else ScrollInfo.DirInfo[scrHorizontal] := sbsNone;
            ScrollInfo.MousePos := Point(0, 0);
            fURLHandler.HandleEvent(evnScroll, ScrollInfo);
          end;
      end;
    end;

  procedure TFiveMainForm.KeyUp(var key : Word; Shift : TShiftState);
    var
      ScrollEndInfo : TEvnScrollEndInfo;
    begin
      if (key = VK_UP) or (key = VK_DOWN) or (key = VK_LEFT) or (key = VK_RIGHT)
        then
          begin
            case key of
              VK_UP:
                fUpKeyDown := false;
              VK_DOWN:
                fDownKeyDown := false;
              VK_LEFT:
                fLeftKeyDown := false;
              VK_RIGHT:
                fRightKeyDown := false;
            end;
            fScrolling := Timer.Enabled or fUpKeyDown or fDownKeyDown or fLeftKeyDown or fRightKeyDown;
            if not fScrolling
              then
                begin
                  ReleaseCapture;
                  ScrollEndInfo.MousePos := Point(0, 0);
                  fURLHandler.HandleEvent(evnScrollEnd, ScrollEndInfo);
                end;
          end;
    end;

  procedure TFiveMainForm.FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
    begin
      fControl.Cursor := crDefault;
      fControl.Focus.QueryUpdate(false);
      // >>>>
    end;

  procedure TFiveMainForm.FacilityBuildAbort;
    begin
      fControl.Cursor := crDefault;
    end;

  procedure TFiveMainForm.CircuitBuild(const CircuitSegments : TSegmentReport; CKind, Cost : integer);
    var
      SegIdx     : integer;
      ErrorCode  : TErrorCode;
      ClientView : IClientView;
    begin
      fControl.Cursor := crDefault;
      fMasterUrlHandler.HandleEvent(evnAnswerClientView, ClientView);
      with CircuitSegments do
        for SegIdx := 0 to SegmentCount - 1 do
          begin
            ClientView.CreateCircuitSeg(CKind, 0, Segments[SegIdx].x1, Segments[SegIdx].y1, Segments[SegIdx].x2, Segments[SegIdx].y2, Cost, ErrorCode);
            if ErrorCode = NOERROR
              then fControl.Focus.QueryUpdate(false);
          end;
    end;

  procedure TFiveMainForm.CircuitBuildAbort;
    begin
      fControl.Cursor := crDefault;
    end;

  procedure TFiveMainForm.UpdateURLHandler(const WorldManager : ILocalCacheManager);
    var
      WorldInfo : TSetWorldInfo;
    begin
      WorldInfo.MapImage := WorldManager.GetLandMap;
      WorldInfo.BuildClasses := WorldManager.GetBuildingClasses;
      fMasterUrlHandler.HandleEvent(evnSetWorldInfo, WorldInfo);
    end;

  procedure TFiveMainForm.MainPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
      ScrollStartInfo : TEvnScrollStartInfo;
      ScrollEndInfo   : TEvnScrollEndInfo;
      Border          : integer;
    begin
      fMouseX := x;
      fMouseY := y;
      if not fScrolling
        then
          begin
            fScrolling := true;
            SetCapture(MainPanel.Handle);
            ScrollStartInfo.MousePos.x := x;
            ScrollStartInfo.MousePos.y := y;
            fURLHandler.HandleEvent(evnScrollStart, ScrollStartInfo);
            Timer.Enabled := true;
          end
        else
          begin
            Border := MainPanel.BorderWidth;
            if (x >= Border) and (y >= Border) and (x <= MainPanel.Width - Border) and (y <= MainPanel.Height - Border)
              then
                begin
                  Timer.Enabled := false;
                  fScrolling := false;
                  ReleaseCapture;
                  ScrollEndInfo.MousePos.x := x;
                  ScrollEndInfo.MousePos.y := y;
                  fURLHandler.HandleEvent(evnScrollEnd, ScrollEndInfo);
                end;
          end;
    end;

  procedure TFiveMainForm.TimerTimer(Sender: TObject);
    const
      ScrollArea = 10;
    var
      ScrollInfo : TEvnScrollInfo;
      dx, dy     : integer;
    begin
      if fMouseX < ScrollArea
        then dx := (2 shl fControl.ZoomLevel)
        else
          if Width - fMouseX < ScrollArea
            then dx := -(2 shl fControl.ZoomLevel)
            else dx := 0;
      if fMouseY < ScrollArea
        then dy := (2 shl fControl.ZoomLevel)
        else
          if Height - fMouseY < ScrollArea
            then dy := -(2 shl fControl.ZoomLevel)
            else dy := 0;
      if dx = 0
        then ScrollInfo.DirInfo[scrHorizontal] := sbsNone
        else
          if dx > 0
            then ScrollInfo.DirInfo[scrHorizontal] := sbsNegative
            else ScrollInfo.DirInfo[scrHorizontal] := sbsPositive;
      if dy = 0
        then ScrollInfo.DirInfo[scrVertical] := sbsNone
        else
          if dy > 0
            then ScrollInfo.DirInfo[scrVertical] := sbsNegative
            else ScrollInfo.DirInfo[scrVertical] := sbsPositive;
      ScrollInfo.MousePos.x := fMouseX;
      ScrollInfo.MousePos.y := fMouseY;
      fURLHandler.HandleEvent(evnScroll, ScrollInfo);
    end;

  procedure TFiveMainForm.ToolClick(Sender: TObject);
    var
      rgn   : HRGN;
      aprgn : HRGN;
    begin
      if FullScreen.Down
        then
          begin
            AuxPanel.Visible := false;
            SetMainPanelDefaultRegion; // >>> should check if using DDraw
          end
        else
          begin
            if Build.Down
              then Notebook.PageIndex := 0
              else Notebook.PageIndex := 1;
            AuxPanel.Visible := true;
            {
            if BufferMgr.IsUsingDDraw
              then
                begin
            }
                  rgn := CreateRectRgn(0, 0, 0, 0);
                  GetWindowRgn(MainPanel.Handle, rgn);
                  aprgn := CreateRectRgn(AuxPanel.Left, AuxPanel.Top, AuxPanel.Left + AuxPanel.Width, AuxPanel.Top + AuxPanel.Height);
                  CombineRgn(aprgn, rgn, aprgn, RGN_OR);
                  SetWindowRgn(MainPanel.Handle, aprgn, true);
            //    end;
          end;
    end;

  procedure TFiveMainForm.BuildFloating(Sender: TObject);
    begin
      FloatingPanel.Visible := not FloatingPanel.Visible;
      if FloatingPanel.Visible
        then FloatingPanel.BringToFront;
    end;

  procedure TFiveMainForm.sbUpClick(Sender: TObject);
    var
      org : TPoint;
    begin
      org := fIsometricMap.Origin;
      fIsometricMap.Origin := Point(org.x, org.y - cThickIsometricDelta);
    end;

  procedure TFiveMainForm.sbDownClick(Sender: TObject);
    var
      org : TPoint;
    begin
      org := fIsometricMap.Origin;
      fIsometricMap.Origin := Point(org.x, org.y + cThickIsometricDelta);
    end;

  procedure TFiveMainForm.sbLeftClick(Sender: TObject);
    var
      org : TPoint;
    begin
      org := fIsometricMap.Origin;
      fIsometricMap.Origin := Point(org.x - cThickIsometricDelta, org.y);
    end;

  procedure TFiveMainForm.sbRightClick(Sender: TObject);
    var
      org : TPoint;
    begin
      org := fIsometricMap.Origin;
      fIsometricMap.Origin := Point(org.x + cThickIsometricDelta, org.y);
    end;

  procedure TFiveMainForm.sbZoomUpClick(Sender: TObject);
    var
      zoom : TZoomLevel;
    begin
      zoom := fIsometricMap.ZoomLevel;
      if zoom < high(zoom) - cIsometricZoomDelta
        then fIsometricMap.ZoomLevel := zoom + cIsometricZoomDelta
        else fIsometricMap.ZoomLevel := high(zoom);
    end;

  procedure TFiveMainForm.sbZoomDownClick(Sender: TObject);
    var
      zoom : TZoomLevel;
    begin
      zoom := fIsometricMap.ZoomLevel;
      if zoom > low(zoom) + cIsometricZoomDelta
        then fIsometricMap.ZoomLevel := zoom - cIsometricZoomDelta
        else fIsometricMap.ZoomLevel := low(zoom);
    end;

  procedure TFiveMainForm.sbCenterClick(Sender: TObject);
    begin
      if fIsometricMap <> nil
        then fIsometricMap.SynchronizeSelection;
    end;

  procedure TFiveMainForm.sbViewSelectionClick(Sender: TObject);
    begin
      if fIsometricMap <> nil
        then fIsometricMap.ViewSelection;
    end;

  procedure TFiveMainForm.sbViewAreaClick(Sender: TObject);
    begin
      {if fIsometricMap <> nil
        then fIsometricMap.MakeAreaVisible;}
    end;

  procedure TFiveMainForm.sbMaxZoomClick(Sender: TObject);
    begin
      fIsometricMap.ZoomLevel := low(fIsometricMap.ZoomLevel);
    end;

  procedure TFiveMainForm.sbMinZoomClick(Sender: TObject);
    begin
      fIsometricMap.ZoomLevel := high(fIsometricMap.ZoomLevel);
    end;

  procedure TFiveMainForm.FormDestroy(Sender: TObject);
    begin
      LogIsoViewerProfiles;
      DoneSoundLibrary;
    end;

  procedure TFiveMainForm.SeasonTimerTimer(Sender: TObject);
    begin
      fControl.ImageSuit := (fControl.ImageSuit + 1) mod 4;
    end;

  procedure TFiveMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
      MouseEventData : TMouseEventData;
    begin
      MouseEventData.Shift := Shift;
      MouseEventData.x := X;
      MouseEventData.y := Y;
      fControl.HandleEvent(evnMouseMove, MouseEventData);
    end;

  procedure TFiveMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      MouseEventData : TMouseEventData;
    begin
      MouseEventData.Button := Button;
      MouseEventData.Shift := Shift;
      MouseEventData.x := X;
      MouseEventData.y := Y;
      fControl.HandleEvent(evnMouseDown, MouseEventData);
    end;

  procedure TFiveMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      MouseEventData : TMouseEventData;
    begin
      MouseEventData.Button := Button;
      MouseEventData.Shift := Shift;
      MouseEventData.x := X;
      MouseEventData.y := Y;
      fControl.HandleEvent(evnMouseUp, MouseEventData);
    end;

  procedure TFiveMainForm.SetMainPanelDefaultRegion;
    var
      rgn    : HRGN;
      //tmprgn : HRGN;
      fcrgn  : HRGN;
    begin
      {
      rgn := CreateRectRgn(0, 0, 0, 0);
      tmprgn := CreateRectRgn(0, 0, MainPanel.BorderWidth, MainPanel.Height);
      CombineRgn(rgn, rgn, tmprgn, RGN_OR);
      tmprgn := CreateRectRgn(0, 0, MainPanel.Width, MainPanel.BorderWidth);
      CombineRgn(rgn, rgn, tmprgn, RGN_OR);
      tmprgn := CreateRectRgn(MainPanel.Width - (MainPanel.BorderWidth + MainPanel.BevelWidth), 0, MainPanel.Width, MainPanel.Height);
      CombineRgn(rgn, rgn, tmprgn, RGN_OR);
      tmprgn := CreateRectRgn(0, MainPanel.Height - (MainPanel.BorderWidth + ToolsPanel.Height), MainPanel.Width, MainPanel.Height);
      CombineRgn(rgn, rgn, tmprgn, RGN_OR);
      }
      rgn := CreateRectRgn(0, 0, MainPanel.Width, MainPanel.Height);
      fcrgn := CreateRectRgn(fControl.Left, fControl.Top, fControl.Left + fControl.Width, fControl.Top + fControl.Height);
      CombineRgn(rgn, rgn, fcrgn, RGN_DIFF);
      SetWindowRgn(MainPanel.Handle, rgn, false);
    end;

  procedure TFiveMainForm.ShutDownTimerTimer(Sender: TObject);
    begin
      Close;
    end;

  {$IFDEF SHOWAVAILVIDEOMEM}
  procedure TFiveMainForm.ApplicationOnIdle(Sender : TObject; var Done : boolean);
    var
      ddscaps      : TDDSCaps2;
      totalmemfree : dword;
      freemem      : dword;
      megs         : dword;
      tmprest      : dword;
      kilobytes    : dword;
      bytes        : dword;
      memreport    : string;
    begin
      InitRecord(ddscaps, sizeof(ddscaps));
      with ddscaps do
        begin
          ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
          //ddsCaps.dwCaps := DDSCAPS_TEXTURE;
          //ddsCaps.dwCaps2 := DDSCAPS2_TEXTUREMANAGE;
        end;
      if Succeeded(fDirectDraw.GetAvailableVidMem(ddscaps, totalmemfree, freemem))
        then
          begin
            megs := totalmemfree div (1024*1024);
            tmprest := totalmemfree mod (1024*1024);
            kilobytes := tmprest div 1024;
            bytes := tmprest mod 1024;
            memreport := IntToStr(megs) + ' Megs ' + IntToStr(kilobytes) + ' K and ' + IntToStr(bytes) + ' bytes.';
            megs := freemem div (1024*1024);
            tmprest := freemem mod (1024*1024);
            kilobytes := tmprest div 1024;
            bytes := tmprest mod 1024;
            memreport := memreport + ' ' + IntToStr(megs) + ' Megs ' + IntToStr(kilobytes) + ' K and ' + IntToStr(bytes) + ' bytes.';
            WriteDebugstr(memreport);
          end;
    end;
  {$ENDIF}
  
end.
