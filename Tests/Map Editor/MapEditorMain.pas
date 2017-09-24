unit MapEditorMain;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, Buttons, Buffer, GameTypes, GameControl, MapControl,
    LanderTypes, MapTypes, FocusTypes, AxlDebug, ToolWin, ComCtrls,
    FiveIsometricMap, FiveTypes, VoyagerInterfaces, Menus;

  type
    TMapEditorMainForm = class(TForm)
      MainPanel: TPanel;
      Timer: TTimer;
      AuxPanel: TPanel;
      MapPanel: TPanel;
      Splitter: TSplitter;
      Splitter1: TSplitter;
      AccidentImagesScrollBox: TScrollBox;
      MainMenu: TMainMenu;
      File1: TMenuItem;
      Season1: TMenuItem;
      View1: TMenuItem;
      LoadMap: TMenuItem;
      SaveMap: TMenuItem;
      Reload: TMenuItem;
      ZoomIn: TMenuItem;
      ZoomOut: TMenuItem;
      RotateCW: TMenuItem;
      RotateCCW: TMenuItem;
      Winter: TMenuItem;
      Spring: TMenuItem;
      Summer: TMenuItem;
      Fall: TMenuItem;
      Panel1: TPanel;
      SmalMapZoomIn: TSpeedButton;
      SmallMapZoomOut: TSpeedButton;
      Dezone: TMenuItem;
      Edit1: TMenuItem;
      Splitter2: TSplitter;
      OpenFileDialog: TOpenDialog;
      N1: TMenuItem;
      Exit: TMenuItem;
      PlaceAccident1: TMenuItem;
    Panel2: TPanel;
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure MainPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure TimerTimer(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure ImgViewerClicked(Sender : TObject);
      procedure SaveMapClick(Sender: TObject);
      procedure LoadMapClick(Sender: TObject);
      procedure SmalMapZoomInClick(Sender: TObject);
      procedure SmallMapZoomOutClick(Sender: TObject);
      procedure ZoomInClick(Sender: TObject);
      procedure ZoomOutClick(Sender: TObject);
      procedure RotateCWClick(Sender: TObject);
      procedure RotateCCWClick(Sender: TObject);
      procedure DezoneClick(Sender: TObject);
      procedure WinterClick(Sender: TObject);
      procedure SpringClick(Sender: TObject);
      procedure SummerClick(Sender: TObject);
      procedure FallClick(Sender: TObject);
      procedure ReloadClick(Sender: TObject);
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure PlaceAccident1Click(Sender: TObject);
    procedure AccidentImagesScrollBoxResize(Sender: TObject);
    private
      fAccidentImgViewers : TList;
      procedure InitAccidentList;
      procedure RefreshAccidentListImages;
    public
      destructor Destroy;  override;
    private
      fControl  : TMapControl;
      fSmallMap : TFiveIsometricMap;
      procedure SmallMapSelect(Sender : TObject; i, j : integer);
    private
      fCachePath    : string;
      fLastMap      : string;
      fLastSuit     : integer;
      fLastZoom     : TZoomLevel;
      fLastRotation : TRotation;
      procedure ReloadMap;
      procedure Save;
      function  Load(const MapName : string) : boolean;
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
      procedure ObjectSelected(which : integer; i, j : integer);
      procedure RegionChanged(Sender : TObject; const Origin, Size : TPoint);
      procedure AreaSelected(imin, jmin, imax, jmax : integer; value : single);
    private
      fLastVClass : integer;
      fMapChanged : boolean;
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
    end;

  var
    MapEditorMainForm: TMapEditorMainForm;

implementation

  uses
    Warnings, ShutDown, LocalCacheManager, CircuitsHandler, Protocol,
    SoundLib, Threads, VoyagerUIEvents, AccidentImageViewerControl,
    IniFiles, MasterURLHandler;

  {$R *.DFM}

  // Utilities

  function ConfigFileName(const executablename : string) : string;
    begin
      Result := executablename;
      setlength(Result, length(Result) - 3);
      Result := Result + 'ini';
    end;

  // Useful constants and types

  type
    TLocalMapControl = class(TMapControl);

  const
    cThinIsometricDelta  = 5;
    cThickIsometricDelta = 20;
    cIsometricZoomDelta  = 10;
    //cIsometricZoomDelta  = 15;

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

  // TMapEditorMainForm

  procedure TMapEditorMainForm.FormCreate(Sender: TObject);
    begin
      SetBounds(0, 0, Screen.Width, Screen.Height);
      InitSoundLibrary(Handle);
    end;

  procedure TMapEditorMainForm.FormShow(Sender: TObject);
    var
      Informant : IWarningInformant;
      msg       : TGetMapperMsg;

    procedure LoadConfiguration;
      var
        ConfigFile : TIniFile;
      begin
        ConfigFile := TIniFile.Create(ConfigFileName(ParamStr(0)));
        try
          fCachePath := ConfigFile.ReadString('General', 'CachePath', ParamStr(1));
          fLastMap := ConfigFile.ReadString('General', 'LastMap', ParamStr(2));
          fLastZoom := ConfigFile.ReadInteger('General', 'Zoom', ord(zr32x64));
          fLastRotation := TRotation(ConfigFile.ReadInteger('General', 'Rotation', ord(drNorth)));
          fLastSuit := ConfigFile.ReadInteger('General', 'Suit', 1);
        finally
          ConfigFile.Free;
        end;
      end;

    begin
      fControl := TMapControl.Create(Self);
      fURLHandler := IMetaURLHandler(fControl).Instantiate;
      fMasterUrlHandler := TTestMasterUrlHandler.Create;
      LoadConfiguration;
      with fControl do
        begin
          Align     := alClient;
          ZoomLevel := 3;   // ord(zr32x64);
          LoadCache(fCachePath);
          LoadMap(fLastMap);
          Parent    := MainPanel;
          OnFacilityBuild      := FacilityBuild;
          OnFacilityBuildAbort := FacilityBuildAbort;
          OnCircuitBuild       := CircuitBuild;
          OnCircuitBuildAbort  := CircuitBuildAbort;
          OnObjectSelected     := ObjectSelected;
          OnRegionChange       := RegionChanged;
          OnAreaSelected       := AreaSelected;
        end;
      fURLHandler.setMasterURLHandler(fMasterUrlHandler);
      // IsometricMap
      fSmallMap := TFiveIsometricMap.Create(Self);
      fSmallMap.Align    := alClient;
      fSmallMap.Parent   := MapPanel;
      fSmallMap.OnSelect := SmallMapSelect;
      msg.id := msgGetMapper;
      fControl.Focus.Dispatch(msg);
      fSmallMap.Mapper := msg.Mapper;
      Informant := fControl.Focus.GetInformant;
      if Informant <> nil
        then Informant.AttachTarget(fSmallMap);
      //fControl.Zoom := fLastZoom;
      //fControl.Rotation := fLastRotation;
      fControl.ImageSuit := fLastSuit;
      case fLastSuit of
        0:
          Winter.Checked := true;
        1:
          Spring.Checked := true;
        2:
          Summer.Checked := true;
        3:
          Fall.Checked := true;
      end;
      InitAccidentList;
    end;

  destructor TMapEditorMainForm.Destroy;
    var
      Informant : IWarningInformant;

    procedure SaveConfiguration;
      var
        ConfigFile : TIniFile;
      begin
        ConfigFile := TIniFile.Create(ConfigFileName(ParamStr(0)));
        try
          ConfigFile.WriteString('General', 'CachePath', fCachePath);
          ConfigFile.WriteString('General', 'LastMap', fLastMap);
          ConfigFile.WriteInteger('General', 'Zoom', fControl.ZoomLevel);
          ConfigFile.WriteInteger('General', 'Rotation', ord(fControl.Rotation));
          ConfigFile.WriteInteger('General', 'Suit', fControl.ImageSuit);
        finally
          ConfigFile.Free;
        end;
      end;

    begin
      SaveConfiguration;
      Height := 10;
      Informant := fControl.Focus.GetInformant;
      if Informant <> nil
        then Informant.DetachTarget(fSmallMap);
      fURLHandler := nil;
      DoShutDown;
      AxlDebug.Console := nil;
      fControl.Parent := nil;
      fControl.Owner.RemoveComponent(fControl);
      inherited;
    end;

  procedure TMapEditorMainForm.SmallMapSelect(Sender : TObject; i, j : integer);
    begin
      fControl.MoveTo(i, j);
    end;

  procedure TMapEditorMainForm.ObjectSelected(which : integer; i, j : integer);
    begin
      if fSmallMap <> nil
        then fSmallMap.Selection := Point(j, i);
    end;

  procedure TMapEditorMainForm.RegionChanged(Sender : TObject; const Origin, Size : TPoint);
    begin
      if fSmallMap <> nil
        then fSmallMap.SetArea(Origin, Size, 2 shl TLocalMapControl(fControl).GetZoomLevel);
    end;

  procedure TMapEditorMainForm.AreaSelected(imin, jmin, imax, jmax : integer; value : single);
    begin
      if value = 0
        then
          begin
            if fControl.DeleteBuildingsInArea(imin, jmin, imax, jmax)
              then
                begin
                  fControl.Refresh;
                  fMapChanged := true;
                end;
          end;
    end;

  procedure TMapEditorMainForm.KeyPress(var key : char);
    var
      LastChar : char;
    begin
      inherited;
      LastChar := fLastChar;
      fLastChar := upcase(key);
      case upcase(key) of
        'B':
          if fLastVClass <> 0
            then fControl.StartFacilityBuild('', fLastVClass);
        'C':
          fControl.StartCircuitBuild(cirRoads, 20000000);
        'R':
          fControl.StartCircuitBuild(cirRailroads, 20000000);
        #27:
          case LastChar of
            'B':
              fControl.AbortFacilityBuild;
            'C', 'R':
              fControl.AbortCircuitBuild;
            'A':
              fControl.AbortAreaSelection;
          end
        else fLastChar := LastChar;
      end;
    end;

  procedure TMapEditorMainForm.KeyDown(var key : Word; shift : TShiftState);
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
          if ssShift in shift
            then fControl.RotateCW
            else fControl.RotateCCW;
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

  procedure TMapEditorMainForm.KeyUp(var key : Word; Shift : TShiftState);
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

  procedure TMapEditorMainForm.ReloadMap;
    begin
      if fMapChanged
        then
          begin
            Load(fLastMap);
            fMapChanged := false;
          end;
    end;

  procedure TMapEditorMainForm.Save;
    begin
      if fMapChanged
        then
          begin
            fControl.SaveMap;
            fMapChanged := false;
          end;
    end;

  function TMapEditorMainForm.Load(const MapName : string) : boolean;
    begin
      Result := fControl.LoadMap(MapName);
      if Result
        then fControl.Refresh;
    end;

  procedure TMapEditorMainForm.FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
    begin
      fControl.Cursor := crDefault;
      if fControl.CreateBuilding(i, j, visclass)
        then
          begin
            fControl.Refresh;
            fMapChanged := true;
          end;
    end;

  procedure TMapEditorMainForm.FacilityBuildAbort;
    begin
      fControl.Cursor := crDefault;
    end;

  procedure TMapEditorMainForm.CircuitBuild(const CircuitSegments : TSegmentReport; CKind, Cost : integer);
    {
    var
      SegIdx     : integer;
      ErrorCode  : TErrorCode;
      ClientView : IClientView;
    }
    begin
      fControl.Cursor := crDefault;
      {
      fMasterUrlHandler.HandleEvent(evnAnswerClientView, ClientView);
      with CircuitSegments do
        for SegIdx := 0 to SegmentCount - 1 do
          begin
            ClientView.CreateCircuitSeg(CKind, 0, Segments[SegIdx].x1, Segments[SegIdx].y1, Segments[SegIdx].x2, Segments[SegIdx].y2, Cost, ErrorCode);
            if ErrorCode = NOERROR
              then fControl.Focus.QueryUpdate(false);
          end;
      }
    end;

  procedure TMapEditorMainForm.CircuitBuildAbort;
    begin
      fControl.Cursor := crDefault;
    end;

  procedure TMapEditorMainForm.MainPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

  procedure TMapEditorMainForm.TimerTimer(Sender: TObject);
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

  procedure TMapEditorMainForm.FormDestroy(Sender: TObject);
    begin
      DoneSoundLibrary;
    end;

  procedure TMapEditorMainForm.InitAccidentList;
    var
      i            : integer;
      acccount     : integer;
      accinfo      : TLandAccidentInfo;
      Panel        : TPanel;
      AccImgViewer : TAccidentImageViewerControl;
      y            : integer;
    begin
      fControl.InitAccidentList;
      fAccidentImgViewers := TList.Create;
      acccount := fControl.GetAccidentCount;
      y := 0;
      for i := 0 to pred(acccount) do
        begin
          fControl.GetAccidentInfo(i, accinfo);
          Panel := TPanel.Create(AccidentImagesScrollBox);
          AccImgViewer := TAccidentImageViewerControl.Create(AccidentImagesScrollBox);
          AccImgViewer.Parent := Panel;
          AccImgViewer.Align := alClient;
          AccImgViewer.VClass := accinfo.vclass;
          AccImgViewer.Image := accinfo.img;
          AccImgViewer.OnClick := ImgViewerClicked;
          fAccidentImgViewers.Add(AccImgViewer);
          Panel.Left := 0;
          Panel.Top := y;
          Panel.Width := AccidentImagesScrollBox.Width;
          Panel.Height := AccidentImagesScrollBox.VertScrollBar.Increment;
          Panel.Parent := AccidentImagesScrollBox;
          inc(y, AccidentImagesScrollBox.VertScrollBar.Increment);
        end;
    end;

  procedure TMapEditorMainForm.RefreshAccidentListImages;
    var
      i            : integer;
      acccount     : integer;
      accinfo      : TLandAccidentInfo;
      AccImgViewer : TAccidentImageViewerControl;
    begin
      fControl.RefreshAccidentImages;
      acccount := fControl.GetAccidentCount;
      for i := 0 to pred(acccount) do
        begin
          fControl.GetAccidentInfo(i, accinfo);
          AccImgViewer := TAccidentImageViewerControl(fAccidentImgViewers[i]);
          AccImgViewer.Image := accinfo.img;
          AccImgViewer.Refresh;
        end;
    end;

  procedure TMapEditorMainForm.ImgViewerClicked(Sender : TObject);
    begin
      fControl.StartFacilityBuild('', TAccidentImageViewerControl(Sender).VClass);
      fLastVClass := TAccidentImageViewerControl(Sender).VClass;
      fLastChar := 'B';
    end;

  procedure TMapEditorMainForm.SaveMapClick(Sender: TObject);
    begin
      Save;
    end;

  procedure TMapEditorMainForm.LoadMapClick(Sender: TObject);

    function ExtractMapNameAndPath(const FullPath : string) : string;
      var
        extlen : integer;
      begin
        if pos('.', FullPath) <> 0
          then
            begin
              extlen := 0;
              Result := FullPath;
              while Result[length(Result) - extlen] <> '.' do
                inc(extlen);
              inc(extlen);
              setlength(Result, length(Result) - extlen);
            end
          else Result := FullPath;
      end;

    var
      mapname : string;
    begin
      OpenFileDialog.InitialDir := fCachePath;
      if OpenFileDialog.Execute
        then
          begin
            mapname := ExtractMapNameAndPath(OpenFileDialog.FileName);
            if Load(mapname)
              then fLastMap := mapname
              else MessageDlg('Could not load map!', mtCustom, [mbOK], 0);
          end;
    end;

  procedure TMapEditorMainForm.SmalMapZoomInClick(Sender: TObject);
    begin
      fSmallMap.ZoomIn;
    end;

  procedure TMapEditorMainForm.SmallMapZoomOutClick(Sender: TObject);
    begin
      fSmallMap.ZoomOut;
    end;

  procedure TMapEditorMainForm.ZoomInClick(Sender: TObject);
    begin
      fControl.ZoomIn;
    end;

  procedure TMapEditorMainForm.ZoomOutClick(Sender: TObject);
    begin
      fControl.ZoomOut;
    end;

  procedure TMapEditorMainForm.RotateCWClick(Sender: TObject);
    begin
      fControl.RotateCW;
    end;

  procedure TMapEditorMainForm.RotateCCWClick(Sender: TObject);
    begin
      fControl.RotateCCW;
    end;

  procedure TMapEditorMainForm.DezoneClick(Sender: TObject);
    begin
      fControl.StartAreaSelection(0, clRed, []);
      fLastChar := 'A';
    end;

  procedure TMapEditorMainForm.WinterClick(Sender: TObject);
    begin
      fControl.ImageSuit := 0;
      RefreshAccidentListImages;
      Winter.Checked := true;
    end;

  procedure TMapEditorMainForm.SpringClick(Sender: TObject);
    begin
      fControl.ImageSuit := 1;
      RefreshAccidentListImages;
      Spring.Checked := true;
    end;

  procedure TMapEditorMainForm.SummerClick(Sender: TObject);
    begin
      fControl.ImageSuit := 2;
      RefreshAccidentListImages;
      Summer.Checked := true;
    end;

  procedure TMapEditorMainForm.FallClick(Sender: TObject);
    begin
      fControl.ImageSuit := 3;
      RefreshAccidentListImages;
      Fall.Checked := true;
    end;

  procedure TMapEditorMainForm.ReloadClick(Sender: TObject);
    begin
      ReloadMap;
    end;

  procedure TMapEditorMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    var
      msgdlgres : word;
    begin
      if fMapChanged
        then
          begin
            msgdlgres := MessageDlg('Map has been changed since last save. Do you want to save changes?', mtCustom, mbYesNoCancel, 0);
            if msgdlgres = mrYes
              then Save;
            CanClose := (msgdlgres = mrYes) or (msgdlgres = mrNo);
          end;
    end;

  procedure TMapEditorMainForm.PlaceAccident1Click(Sender: TObject);
    begin
      if fLastVClass <> 0
        then fControl.StartFacilityBuild('', fLastVClass);
    end;

  procedure TMapEditorMainForm.AccidentImagesScrollBoxResize(Sender: TObject);
    var
      i : integer;
    begin
      if fAccidentImgViewers <> nil
        then
          for i := 0 to pred(fAccidentImgViewers.Count) do
            TAccidentImageViewerControl(fAccidentImgViewers[i]).Width := TControl(Sender).Width;
    end;

end.
