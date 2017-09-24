unit FiveMain;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, Buttons, Buffer, GameTypes, GameControl, FiveControl,
    LanderTypes, MapTypes, AxlDebug, VoyagerInterfaces, ToolWin, ComCtrls,
    VoyagerServerInterfaces, FiveIsometricMap, FiveTypes;

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
      procedure FacilityBuild(i, j : integer; const facclass : string);
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
      FloatingPanel : TPanel;
    end;

  var
    FiveMainForm: TFiveMainForm;

implementation

  uses
    Warnings, ShutDown, ServerCnxEvents, VoyagerUIEvents, LocalCacheManager,
    ClientView, CircuitsHandler, Protocol, CaptureCoords, SoundLib, Threads;

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
    end;


  // TFiveMainForm

  procedure TFiveMainForm.FormCreate(Sender: TObject);
    begin
      SetBounds(0, 0, Screen.Width, Screen.Height);
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
      InitSoundLibrary(Handle);
    end;

  procedure TFiveMainForm.FormShow(Sender: TObject);
    var
      Agents     : TInitializationAgents;
      ClientView : IClientView;
      Informant  : IWarningInformant;
      msg        : TGetMapperMsg;
    begin
      fControl := TFiveControl.Create(Self);
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
      fURLHandler := IMetaURLHandler(fControl).Instantiate;
      fMasterUrlHandler := TTestMasterUrlHandler.Create;
      fMasterUrlHandler.HandleEvent(evnAnswerClientView, ClientView);
      InitViewer(ClientView, Agents);
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
        VK_BACK:
          {
          if ssShift in shift
            then fControl.RotateCW
            else fControl.RotateCCW;
          };
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

  procedure TFiveMainForm.FacilityBuild(i, j : integer; const facclass : string);
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
    begin
      if FullScreen.Down
        then AuxPanel.Visible := false
        else
          begin
            if Build.Down
              then Notebook.PageIndex := 0
              else Notebook.PageIndex := 1;
            AuxPanel.Visible := true;
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
      DoneSoundLibrary;
    end;

  procedure TFiveMainForm.SeasonTimerTimer(Sender: TObject);
    begin
      //fControl.ImageSuit := (fControl.ImageSuit + 1) mod 4; 
    end;

end.
