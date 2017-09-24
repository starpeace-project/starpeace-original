unit GameControl;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
    GameTypes, LanderTypes;

  type
    TOnRegionChange = procedure (Sender : TObject; const Origin, Size : TPoint) of object;

  const
    WM_UPDATEORIGIN = WM_USER + 1024;

  type
    TCustomGameControl =
      class(TCustomControl, IGameView)
        public
          constructor Create(aOwner : TComponent);   override;
          destructor  Destroy;   override;
        protected// IUnknown
          fRefCount : integer;
          function QueryInterface(const iid : TGUID; out obj) : hresult; stdcall;
          function _AddRef : integer;   stdcall;
          function _Release : integer;   stdcall;
        protected  // IGameUpdater
          fLockCount     : integer;
          fUpdateDefered : boolean;
          function  Lock : integer;
          function  Unlock : integer;
          function  LockCount : integer;
          procedure QueryUpdate(Defer : boolean);
        protected // IGameView
          fOrigin    : TPoint;
          fDocument  : IGameDocument;
          fZoomLevel : TZoomLevel;
          fRotation  : TRotation;
          fFocus     : IGameFocus;
          fImageSuit : integer;
          function  GetOrigin : TPoint;
          procedure SetOrigin(const which : TPoint);
          function  GetDocument : IGameDocument;
          procedure SetDocument(const which : IGameDocument);
          function  GetZoomLevel : TZoomLevel;
          procedure SetZoomLevel(which : TZoomLevel);
          function  GetRotation : TRotation;
          procedure SetRotation(which : TRotation);
          procedure UpdateRegions(const which : array of TRect);
          function  GetSize  : TPoint;
          function  GetFocus : IGameFocus;
          function  ViewPtToScPt(const which : TPoint) : TPoint;
          function  ScPtToViewPt(const which : TPoint) : TPoint;
          function  GetTextDimensions(const text : string; out width, height : integer) : boolean;
          function  GetImageSuit : integer;
          procedure SetImageSuit(ImageSuit : integer);
        protected
          property Document  : IGameDocument read fDocument    write SetDocument;
          property ZoomLevel : TZoomLevel    read GetZoomLevel write SetZoomLevel;
          property Rotation  : TRotation     read GetRotation  write SetRotation;
        private
          fOnRegionChange : TOnRegionChange;
        public
          property Origin : TPoint     read GetOrigin write SetOrigin;
          property Focus  : IGameFocus read fFocus;
          property ImageSuit : integer read GetImageSuit write SetImageSuit;
          property OnRegionChange : TOnRegionChange read fOnRegionChange write fOnRegionChange;
        protected
          procedure SetParent(which : TWinControl);   override;
          procedure Loaded;  override;
          procedure Paint;   override;
        protected
          procedure StartScrolling; virtual;
          procedure ScrollTick(dx, dy : integer);
          procedure StopScrolling; virtual;
          procedure Scroll(dx, dy : integer);
        protected
          fExposed : boolean;
          fSnap    : TCanvasImage;
          procedure RenderRegions(const which : array of TRect); virtual;
          procedure DrawRegions(const which : array of TRect); virtual;
          procedure CheckExposed;   virtual;
        protected
          fMouseDown : boolean;
          fDragging  : boolean;
          fMouseX    : integer;
          fMouseY    : integer;
          procedure MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : Integer); override;
          procedure MouseMove(Shift : TShiftState; x, y : integer);   override;
          procedure MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : Integer); override;
          procedure KeyDown(var Key: Word; Shift: TShiftState); override;
        protected // internals
          procedure RenderControl;
          procedure UpdateThroughFocus;
          procedure wmEraseBkgnd(var msg : TMessage);   message WM_ERASEBKGND;
          procedure wmSize(var msg : TWMSize);   message WM_SIZE;
          procedure wmUpdateOrigin(var msg);   message WM_UPDATEORIGIN;
      end;

  type
    TGameControl =
      class(TCustomGameControl)
        published
          property Document;
          property ZoomLevel;
          property Rotation;
          property OnRegionChange;
        published
          property Align;
          property Caption;
          property Color;
          property Font;
          property ShowHint;
          property PopupMenu;
          property OnEnter;
          property OnExit;
          property OnKeyDown;
          property OnKeyPress;
          property OnKeyUp;
          property OnClick;
          property OnDblClick;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
      end;

  procedure Register;

implementation

  uses
    ShutDown, ScrollRegions, AxlDebug, FocusTypes {$IFDEF PROFILES}, Profiler, IsoProfile{$ENDIF}
    {$IFDEF RENDERREPORTS}, RenderReports{$ENDIF};

  const
    cCanvasBitCount = 16;  // >>>>

  type
    THintWindow = class(TStaticText);

  // TCustomGameControl

  constructor TCustomGameControl.Create(aOwner : TComponent);
    begin
      inherited;
      ControlStyle := ControlStyle + [csOpaque];
      Canvas.Brush.Color := clBlack;
    end;

  destructor TCustomGameControl.Destroy;
    begin
      //fFocus._Release;
      //fFocus._Release;
      inherited;
    end;

  function TCustomGameControl.QueryInterface(const iid : TGUID; out obj) : hresult;
    const
      E_NOINTERFACE = $80004002;
    begin
      if GetInterface(iid, obj)
        then Result := 0
        else Result := E_NOINTERFACE;
    end;

  function TCustomGameControl._AddRef : integer;
    begin
      inc(fRefCount);
      Result := fRefCount;
    end;

  function TCustomGameControl._Release : integer;
    begin
      dec(fRefCount);
      Result := fRefCount;
    end;

  function TCustomGameControl.Lock : integer;
    begin
      inc(fLockCount);
      Result := fLockCount;
    end;

  function TCustomGameControl.Unlock : integer;
    begin
      assert(fLockCount > 0);
      dec(fLockCount);
      if (fLockCount = 0) and fUpdateDefered
        then RenderControl;
      Result := fLockCount;
    end;

  function TCustomGameControl.LockCount : integer;
    begin
      Result := fLockCount;
    end;

  procedure TCustomGameControl.QueryUpdate(Defer : boolean);
    begin
      if (fLockCount > 0) and Defer
        then fUpdateDefered := true
        else RenderControl;
    end;

  function TCustomGameControl.GetOrigin : TPoint;
    begin
      Result := fOrigin;
    end;

  procedure TCustomGameControl.SetOrigin(const which : TPoint);
    begin
      if fFocus <> nil
        then Scroll(fOrigin.x - which.x, fOrigin.y - which.y)
        else
          begin
            fOrigin := which;
            PostMessage(Handle, WM_UPDATEORIGIN, 0, 0);
          end;
    end;

  function TCustomGameControl.GetDocument : IGameDocument;
    begin
      Result := fDocument;
    end;

  procedure TCustomGameControl.SetDocument(const which : IGameDocument);
    begin
      if fDocument <> which
        then
          begin
            fDocument := which;
            // <<>>  Change Focus
            CheckExposed;
          end;
    end;

  function TCustomGameControl.GetZoomLevel : TZoomLevel;
    begin
      Result := fZoomLevel;
    end;

  procedure TCustomGameControl.SetZoomLevel(which : TZoomLevel);
    begin
      if which <> fZoomLevel
        then fZoomLevel := which;
    end;

  function TCustomGameControl.GetRotation : TRotation;
    begin
      Result := fRotation;
    end;

  procedure TCustomGameControl.SetRotation(which : TRotation);
    begin
      if which <> fRotation
        then fRotation := which;
    end;

  procedure TCustomGameControl.UpdateRegions(const which : array of TRect);
    var
      i    : integer;
      Clip : TRect;
      R    : TRect;
    begin
      Clip := ClientRect;
      for i := low(which) to high(which) do
        begin
          IntersectRect(R, which[i], Clip);
          if not IsRectEmpty(R)
            then
              begin
                RenderRegions(R);
                DrawRegions(R);
              end;
        end;
    end;

  function TCustomGameControl.GetSize : TPoint;
    begin
      if fExposed
        then Result := ClientRect.BottomRight
        else Result := Point(0, 0);
    end;

  function TCustomGameControl.GetFocus : IGameFocus;
    begin
      Result := fFocus;
    end;

  function TCustomGameControl.ViewPtToScPt(const which : TPoint) : TPoint;
    begin
      Result := ClientToScreen(which);
    end;

  function TCustomGameControl.ScPtToViewPt(const which : TPoint) : TPoint;
    begin
      Result := ScreenToClient(which);
    end;

  function TCustomGameControl.GetTextDimensions(const text : string; out width, height : integer) : boolean;
    begin
      if fSnap <> nil
        then
          with fSnap.Canvas do
            begin
              Font.Name := 'Verdana';
              Font.Size := 8;
              Font.Style := [fsBold];
              Font.Color := clRed;
              width := TextWidth(text);
              height := TextHeight(text);
              Result := true;
            end
        else Result := false;
    end;

  function TCustomGameControl.GetImageSuit : integer;
    begin
      Result := fImageSuit;
    end;

  procedure TCustomGameControl.SetImageSuit(ImageSuit : integer);
    begin
      fDocument.SetImageSuit(ImageSuit);
      fImageSuit := ImageSuit;
      Repaint;
    end;

  procedure TCustomGameControl.SetParent(which : TWinControl);
    begin
      inherited;
      CheckExposed;
    end;

  procedure TCustomGameControl.Loaded;
    begin
      inherited;
      CheckExposed;
    end;

  procedure TCustomGameControl.Paint;
    begin
      if fExposed
        then RenderControl
        else inherited;
    end;

  procedure TCustomGameControl.StartScrolling;
    begin
      ShutDown.DoSuspend;
    end;

  procedure TCustomGameControl.ScrollTick(dx, dy : integer);
    const
      DeltaLimit = 500;
    type
      TRegionData =
        record
          Header : TRgnDataHeader;
          Rects  : array[byte] of TRect;
        end;
    var
      SnapRect  : TRect;
      {$IFNDEF RENDERSCREEN}
      Updates   : TRegionData;
      UpdateRgn : HRGN;
      lx, ly    : integer;
      {$ENDIF}
      msg       : TViewScrolledMsg;
    begin
      if fExposed
        then
          begin
            fDocument.ClipMovement(Self, dx, dy);
            if (dx <> 0) or (dy <> 0)
              then
                begin
                  dec(fOrigin.x, dx);
                  dec(fOrigin.y, dy);
                  msg.id := msgViewScrolled;
                  msg.dx := dx;
                  msg.dy := dy;
                  fFocus.Dispatch(msg);
                  SnapRect := ClientRect;
                  {$IFNDEF RENDERSCREEN}
                  lx := DeltaLimit;
                  ly := DeltaLimit;
                  if lx > 3*SnapRect.Right div 4
                    then lx := 3*SnapRect.Right div 4;
                  if ly > 3*SnapRect.Bottom div 4
                    then ly := 3*SnapRect.Bottom div 4;
                  if (abs(dx) < lx) and (abs(dy) < ly)
                    then
                      begin
                        UpdateRgn := GetScrollUpdateRegion(Handle, dx, dy);
                        GetRegionData(UpdateRgn, sizeof(Updates), @Updates);
                        DeleteObject(UpdateRgn);
                        RenderRegions(Slice(Updates.Rects, Updates.Header.nCount));
                        ScrollWindowEx(Handle, dx, dy, nil, nil, 0, nil, 0);
                        // ScrollDC(Canvas.Handle, dx, dy, SnapRect, Snaprect, 0, nil);
                        DrawRegions(Slice(Updates.Rects, Updates.Header.nCount));
                      end
                    else Invalidate;
                  {$ELSE}
                  RenderRegions(SnapRect);
                  DrawRegions(SnapRect);
                  {$ENDIF}
                end;
          end;
    end;

  procedure TCustomGameControl.StopScrolling;
    begin
      UpdateThroughFocus;
      ShutDown.DoResume;
    end;

  procedure TCustomGameControl.Scroll(dx, dy : integer);
    begin
      StartScrolling;
      try
        ScrollTick(dx, dy);
      finally
        StopScrolling;
      end;
    end;

  procedure TCustomGameControl.RenderRegions(const which : array of TRect);
    var
      i : integer;
    begin
      {$IFDEF PROFILES}
      Profiler.ProcStarted(prfKind_Main, prfId_Rendering);
      try
      {$ENDIF}
        assert(fExposed);
        for i := low(which) to high(which) do
          fDocument.RenderSnapshot(Self, which[i], fSnap);
      {$IFDEF PROFILES}
      finally
        Profiler.ProcEnded(prfKind_Main, prfId_Rendering);
      end;
      {$ENDIF}
    end;

  procedure TCustomGameControl.DrawRegions(const which : array of TRect);
    var
      i : integer;
    begin
      {$IFDEF PROFILES}
      Profiler.ProcStarted(prfKind_Main, prfId_Blitting);
      try
      {$ENDIF}
        for i := low(which) to high(which) do
          begin
            fSnap.ClipDraw(Canvas, 0, 0, which[i]);
            {$IFDEF DRAWREGIONRECTS}
            Canvas.Pen.Color := clYellow;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle(which[i].Left, which[i].Top, which[i].Right, which[i].Bottom);
            {$ENDIF}
          end;
      {$IFDEF PROFILES}
      finally
        Profiler.ProcEnded(prfKind_Main, prfId_Blitting);
      end;
      {$ENDIF}
    end;

  procedure TCustomGameControl.CheckExposed;
    var
      aux : boolean;
    begin
      aux := (Parent <> nil) and (fDocument <> nil);
      if aux <> fExposed
        then
          begin
            fExposed := aux;
            if fExposed
              then
                begin
                  assert(fSnap = nil);
                  fSnap := TCanvasImage.CreateSized(ClientWidth, ClientHeight, cCanvasBitCount);
                  assert(fFocus = nil);
                  fFocus := fDocument.CreateFocus(Self);
                end
              else
                begin
                  fFocus := nil;
                  assert(fSnap <> nil);
                  fSnap.Free;
                  fSnap := nil;
                end;
          end;
    end;

  procedure TCustomGameControl.MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : Integer);
    begin
      inherited;
      fMouseX := x;
      fMouseY := y;
      fMouseDown := true;
    end;

  procedure TCustomGameControl.MouseMove(Shift : TShiftState; x, y : integer);
    const
      cMinDrag = 8;
    var
      dx, dy : integer;
    begin
      inherited;
      if fMouseDown
        then
          begin
            dx  := x - fMouseX;
            dy  := y - fMouseY;
            if not fDragging
              then
                begin
                  fDragging := (abs(dx) + abs(dy) > cMinDrag);
                  if fDragging
                    then
                      begin
                        MouseCapture := true;
                        StartScrolling;
                      end;
                end;
            if fDragging
              then
                begin
                  ScrollTick(dx, dy);
                  fMouseX := x;
                  fMouseY := y;
                end;
          end
        else fFocus.MouseMove(x, y);
    end;

  procedure TCustomGameControl.MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : integer);
    begin
      inherited;
      if fMouseDown
        then
          begin
            fMouseDown := false;
            if fDragging
              then
                begin
                  StopScrolling;
                  MouseCapture := false;
                  fDragging := false;
                end
              else fFocus.MouseClick;
          end;
    end;

  procedure TCustomGameControl.KeyDown(var Key: Word; Shift: TShiftState);
    begin
      inherited;
      fFocus.KeyPressed(Key, Shift);
    end;
    
  procedure TCustomGameControl.RenderControl;
    begin
      {$IFDEF RENDERREPORTS}
      ResetImgCount;
      EnableReports;
      {$ENDIF}
      fUpdateDefered := false;
      UpdateRegions([ClientRect]);
      {$IFDEF RENDERREPORTS}
      WriteDebugStr(IntToStr(GetImgCount));
      DisableReports;
      {$ENDIF}
    end;

  procedure TCustomGameControl.UpdateThroughFocus;
    begin
      if fExposed
        then
          begin
            fFocus.QueryUpdate(true);
            if assigned(fOnRegionChange)
              then fOnRegionChange(Self, fOrigin, GetSize);
          end;
    end;

  procedure TCustomGameControl.wmEraseBkgnd(var msg : TMessage);
    begin
    end;

  procedure TCustomGameControl.wmSize(var msg : TWMSize);
    begin
      inherited;
      if fExposed
        then
          begin
            fSnap.NewSize(msg.Width, msg.Height, cCanvasBitCount);
            if assigned(fOnRegionChange)
              then fOnRegionChange(Self, fOrigin, GetSize);
          end;
    end;

  procedure TCustomGameControl.wmUpdateOrigin(var msg);
    begin
      UpdateThroughFocus;
    end;


  procedure Register;
    begin
      RegisterComponents('Games', [TGameControl]);
    end;

end.
