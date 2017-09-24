unit IsometricMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SpeedBmp, Threads, IsometricMapTypes;

type
  TZoomLevel = 1..100;  // Percent

type
  TCanvasImage = TSpeedBitmap;
  hfocus       = integer;

type
  TOnIsometricChange = procedure (Sender : TObject) of object;
  TOnSelect          = procedure (Sender : TObject; i, j : integer) of object;

type
  TIsometricMap =
    class(TCustomControl)
      public
        constructor Create(Owner : TComponent);   override;
        destructor  Destroy;   override;
      private
        fMapper    : IIsometricMapper;
        fZoomLevel : TZoomLevel;
        fOrigin    : TPoint;
        fQuality   : boolean;
        fOnChange  : TOnIsometricChange;
        fOnSelect  : TOnSelect;
        procedure SetMapper(which : IIsometricMapper);
        procedure SetZoomLevel(which : TZoomLevel);
        procedure SetOrigin(which : TPoint);
        procedure SetQuality(which : boolean);
      published
        property Mapper     : IIsometricMapper   read fMapper     write SetMapper;
        property ZoomLevel  : TZoomLevel         read fZoomLevel  write SetZoomLevel default low(TZoomLevel);
        property Origin     : TPoint             read fOrigin     write SetOrigin;
        property Quality    : boolean            read fQuality    write SetQuality   default false;
        property OnChange   : TOnIsometricChange read fOnChange   write fOnChange    default nil;
        property OnSelect   : TOnSelect          read fOnSelect   write fOnSelect;
      private
        fSize      : TPoint;
        fSelection : TPoint;  // Map coordinates
        fArea      : TRect;   // Screen Coordinates
        fRatio     : integer;
        procedure SetSelection(const which : TPoint);
      public
        property Size      : TPoint read fSize;
        property Selection : TPoint read fSelection write SetSelection;  // Map coordinates
        procedure SetArea(const Origin, Size : TPoint; Ratio : integer);
        procedure SynchronizeSelection;
        procedure ViewSelection;
        procedure ViewArea;
      private
        fMouseDown : boolean;
        fClicked   : boolean;
        fMouseX    : integer;
        fMouseY    : integer;
        fExposed   : boolean;
      protected
        fHardInvalidated : boolean;
        procedure SetParent(which : TWinControl);   override;
        procedure Paint;   override;
        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : integer);  override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer);   override;
        procedure MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : integer);    override;
        procedure InvalidateIsometric(hard : boolean);
      private
        fBitmap : TCanvasImage;
        nu, du  : integer;
        w, h    : integer;
        rows    : integer;
        cols    : integer;
        procedure GetArea(out which : TRect);
        procedure DrawArea;
        procedure CheckArea;
        procedure NotifySelect(x, y : integer);
        procedure UpdateIsometric;
        procedure UpdateRegion(const which : TRect);
        procedure UpdateUnits;
        procedure DraftRender;
        procedure wmSize(var msg : TWMSize);    message WM_SIZE;
        procedure cmWantSpecialKey(var msg : TCMWantSpecialKey);   message CM_WANTSPECIALKEY;
        procedure wmEraseBkGnd(var msg);   message WM_ERASEBKGND;
    end;


procedure Register;


implementation


const
  cCanvasBitCount = 16;

const
  cSelectionColor = clYellow;
  cAreaColor      = clYellow;

// Utils

type
  TScrollRegion = array[0..1] of TRect;

function GetScrollRegion(const Client : TRect; dx, dy : integer; out which : TScrollRegion) : integer;
  begin
    fillchar(which, sizeof(which), 0);
    Result := 0;
    if dx <> 0
      then
        begin
          if dx > 0
            then which[Result] := Rect(Client.Right - dx, 0, Client.Right, Client.Bottom)
            else which[Result] := Rect(Client.Left, Client.Top, Client.Left - dx, Client.Bottom);
          inc(Result);
        end;
    if dy <> 0
      then
        begin
          if dy > 0
            then which[Result] := Rect(0, Client.Bottom - dy, Client.Right, Client.Bottom)
            else which[Result] := Rect(Client.Left, Client.Top, Client.Right, Client.Top - dy);
          inc(Result);
        end;
  end;


// TIsometricMap

constructor TIsometricMap.Create(Owner : TComponent);
  begin
    inherited;
    fZoomLevel := low(fZoomLevel);
    fBitmap := TCanvasImage.CreateSized(1, 1, cCanvasBitCount);
    fHardInvalidated := true;
  end;

destructor TIsometricMap.Destroy;
  begin
    fBitmap.Free;
    inherited;
  end;

procedure TIsometricMap.SetMapper(which : IIsometricMapper);
  begin
    if fMapper <> which
      then
        begin
          fMapper := which;
          if fMapper <> nil
            then
              begin
                rows := fMapper.GetRows;
                cols := fMapper.GetCols;
              end
            else
              begin
                rows := 0;
                cols := 0;
              end;
          fExposed := (fMapper <> nil) and (Parent <> nil) and (fRatio <> 0);
          if fExposed
            then UpdateUnits;
          UpdateIsometric;
        end;
  end;

procedure TIsometricMap.SetZoomLevel(which : TZoomLevel);
  begin
    if fZoomLevel <> which
      then
        begin
          fZoomLevel := which;
          if fExposed
            then
              begin
                UpdateUnits;
                CheckArea;
              end;
        end;
  end;

procedure TIsometricMap.SetOrigin(which : TPoint);
  var
    dx, dy : integer;

  procedure Scroll;
    var
      Client  : TRect;
      Region  : TScrollRegion;
      Count   : integer;
      i       : integer;
      hCanvas : HDC;
    begin
      fBitmap.Canvas.RequiredState([csHandleValid, csPenValid]);
      hCanvas := fBitmap.Canvas.Handle;
      Client := GetClientRect;
      ScrollDC(hCanvas, -dx, -dy, Client, Client, 0, nil);
      inc(fOrigin.x, dx);
      inc(fOrigin.y, dy);
      Count := GetScrollRegion(Client, dx, dy, Region);
      for i := 0 to pred(Count) do
        UpdateRegion(Region[i]);
      Refresh;
      if assigned(fOnChange)
        then fOnChange(Self);
    end;

  begin
    if which.x < 0
      then which.x := 0
      else
        if which.x > fSize.x - w
          then which.x := fSize.x - w;
    if which.y < 0
      then which.y := 0
      else
        if which.y > fSize.y - h
          then which.y := fSize.y - h;
    if (which.x <> fOrigin.x) or (which.y <> fOrigin.y)
      then
        begin
          dx := which.x - fOrigin.x;
          dy := which.y - fOrigin.y;
          if (abs(dx) < w div 2) and (abs(dy) < h div 2)
            then Scroll
            else
              begin
                fOrigin := which;
                UpdateIsometric;
              end;
        end;
  end;

procedure TIsometricMap.SetQuality(which : boolean);
  begin
    if fQuality <> which
      then
        begin
          fQuality := which;
          if fQuality
            then UpdateIsometric;
        end;
  end;

procedure TIsometricMap.SetSelection(const which : TPoint);
  begin
    fSelection := which;
    UpdateIsometric;
  end;

procedure TIsometricMap.SetArea(const Origin, Size : TPoint; Ratio : integer);
  begin
    assert(Ratio <> 0);
    fArea.TopLeft := Origin;
    fArea.Right   := Origin.x + Size.x;
    fArea.Bottom  := Origin.y + Size.y;
    fRatio        := Ratio;
    fExposed      := (fMapper <> nil) and (Parent <> nil);
    if fExposed
      then
        begin
          UpdateUnits;
          CheckArea;
        end;
  end;

procedure TIsometricMap.SynchronizeSelection;
  var
    x, y : integer;
    i, j : integer;
  begin
    i := fSelection.y;
    j := fSelection.x;
    if (i <> 0) and (j <> 0) and fExposed
      then
        begin
          x := 2*nu*(rows - i + j) div du - fOrigin.x;
          y := nu*((rows - i) + (cols - j)) div du - fOrigin.y;
          NotifySelect(x, y);
        end;
  end;

procedure TIsometricMap.ViewSelection;
  var
    i, j : integer;
    size : TPoint;
  begin
    if fExposed
      then
        begin
          size := ClientRect.BottomRight;
          i := fSelection.y;
          j := fSelection.x;
          if (i <> 0) and (j <> 0)
            then Origin := Point(2*nu*(rows - i + j) div du - size.x div 2, nu*((rows - i) + (cols - j)) div du - size.y div 2);
        end;
  end;

procedure TIsometricMap.ViewArea;
  var
    size   : TPoint;
    x, y   : integer;
    dx, dy : integer;
    u      : double;
  begin
    if fExposed
      then
        begin
          size := ClientRect.BottomRight;
          u    := nu/fRatio/du;
          x    := trunc(u*fArea.Left);
          y    := trunc(u*fArea.Top);
          dx   := round((size.x + x - u*fArea.Right)/2);
          dy   := round((size.y + y - u*fArea.Bottom)/2);
          Origin := Point(x - dx,  y - dy);
        end;
  end;

procedure TIsometricMap.SetParent(which : TWinControl);
  begin
    inherited;
    fExposed := (which <> nil) and (fMapper <> nil) and (fRatio <> 0);
    if fExposed
      then UpdateUnits;
  end;

procedure TIsometricMap.Paint;
  begin
    inherited;
    if fExposed
      then
        begin
          if fHardInvalidated
            then
              begin
                DraftRender;
                fHardInvalidated := false;
              end;
          fBitmap.Draw(Canvas, 0, 0);
          DrawArea;
        end;
  end;

procedure TIsometricMap.MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : integer);
  begin
    inherited;
    fMouseX  := x;
    fMouseY  := y;
    if Button = mbLeft
      then
        begin
          fClicked     := true;
          MouseCapture := true;
          fMouseDown   := true;
        end;
  end;

procedure TIsometricMap.MouseMove(Shift: TShiftState; X, Y: Integer);
  const
    cClickDelta = 3;
  var
    dx, dy : integer;
    org    : TPoint;
  begin
    inherited;
    if fMouseDown
      then
        begin
          org := Origin;
          dx  := x - fMouseX;
          dy  := y - fMouseY;
          fClicked := fClicked and (abs(dx) + abs(dy) < cClickDelta);
          Origin := Point(org.x - dx, org.y - dy);
          fMouseX := x;
          fMouseY := y;
        end;

  end;

procedure TIsometricMap.MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : integer);
  begin
    inherited;
    if fMouseDown
      then
        begin
          MouseCapture := false;
          fMouseDown   := false;
          if fClicked
            then NotifySelect(x, y);
        end;
  end;

procedure TIsometricMap.InvalidateIsometric(hard : boolean);
  begin
    if hard
      then fHardInvalidated := true;
    Invalidate;
  end;

procedure TIsometricMap.GetArea(out which : TRect);
  var
    u : double;
  begin
    u := nu/fRatio/du;
    with fArea do
      begin
        which.Left   := trunc(u*Left)   - fOrigin.x;
        which.Top    := trunc(u*Top)    - fOrigin.y;
        which.Right  := trunc(u*Right)  - fOrigin.x;
        which.Bottom := trunc(u*Bottom) - fOrigin.y;
      end;
  end;

procedure TIsometricMap.DrawArea;
  var
    aux : TRect;
  begin
    GetArea(aux);
    Canvas.Pen.Color := cAreaColor;
    Canvas.FrameRect(aux);
  end;

procedure TIsometricMap.CheckArea;
  var
    aux, tmp : TRect;
  begin
    GetArea(aux);
    if not (IntersectRect(tmp, aux, ClientRect) and EqualRect(tmp, aux))
      then ViewArea
      else InvalidateIsometric(false);
  end;

procedure TIsometricMap.NotifySelect(x, y : integer);
  var
    i, j : integer;
  begin
    if assigned(fOnSelect)
      then
        begin
          inc(x, fOrigin.x);
          inc(y, fOrigin.y);
          i := (2*nu*(2*rows + cols) - du*(x + 2*y)) div (4*nu);
          j := (2*nu*cols + du*(x - 2*y)) div (4*nu);
          fMapper.TransFormCoords(i, j);
          fOnSelect(Self, i, j);
        end;
  end;

procedure TIsometricMap.UpdateIsometric;
  begin
    InvalidateIsometric(true);
    Refresh;
    if assigned(fOnChange)
      then fOnChange(Self);
  end;

procedure TIsometricMap.UpdateRegion(const which : TRect);
  var
    x, y    : integer;
    ox, oy  : integer;
    x1, x2  : integer;
    y2      : integer;
    ry, ly  : integer;
    ww, hh  : integer;
    ix, jx  : integer;
    iy, jy  : integer;
    hCanvas : HDC;
  begin
    with fBitmap, Canvas do
      begin
        Pen.Color   := clBlack;
        Brush.Color := clBlack;
        FillRect(which);
        //Rectangle(0, 0, Width, Height);
      end;
    ww := (which.Right  - which.Left);
    hh := (which.Bottom - which.Top);
    fBitmap.Canvas.RequiredState([csHandleValid, csPenValid]);
    hCanvas := fBitmap.Canvas.Handle;
    ox := fOrigin.x + which.Left;
    oy := fOrigin.y + which.Top;
    y2 := nu*(rows + cols) div du;
    if y2 >= oy + hh
      then y2 := pred(oy + hh);
    ly := nu*(cols + 1) div du;
    ry := nu*(rows + 1) div du;
    iy := 2*(nu*(2*rows + cols) - du*oy);
    jy := 2*(nu*cols - du*oy);
    for y := oy to y2 do
      begin
        if y <= ly
          then x1 := 2*(nu*(cols + 2) - du*y)
          else x1 := 2*(du*y - nu*cols);
        if x1 < ox*du
          then x1 := ox*du;
        if y <= ry
          then x2 := 2*(nu*(cols - 2) + du*y)
          else x2 := 2*(nu*(2*rows + cols) - du*y);
        if x2 >= (ox + ww)*du
          then x2 := pred(ox + ww)*du;
        ix := iy - x1;
        jx := jy + x1;
        x1 := x1 div du;
        x2 := x2 div du;
        for x := x1 to x2 do
          begin
            SetPixel(hCanvas, x - fOrigin.x, y - fOrigin.y, fMapper.GetColor(ix div (4*nu), jx div (4*nu)));
            dec(ix, du);
            inc(jx, du);
          end;
        dec(iy, 2*du);
        dec(jy, 2*du);
      end;
  end;

procedure TIsometricMap.UpdateUnits;
  begin
    assert((rows > 0) and (cols > 0));
    w := ClientWidth;
    h := ClientHeight;
    if w < 2*h
      then h := w div 2;
    w := 2*h;
    nu := (fZoomLevel - low(fZoomLevel))*(2*(rows + cols) - w) + (high(fZoomLevel) - low(fZoomLevel))*w;
    du := (2*(high(fZoomLevel) - low(fZoomLevel)))*(rows + cols);
    fSize.y := (rows + cols)*nu div du;
    fSize.x := 2*fSize.y;
    if fOrigin.x < 0
      then fOrigin.x := 0
      else
        if fOrigin.x > fSize.x - w
          then fOrigin.x := fSize.x - w;
    if fOrigin.y < 0
      then fOrigin.y := 0
      else
        if fOrigin.y > fSize.y - h
          then fOrigin.y := fSize.y - h;
    InvalidateIsometric(true);
  end;

procedure TIsometricMap.DraftRender;
  begin
    UpdateRegion(ClientRect);
  end;

procedure TIsometricMap.wmSize(var msg : TWMSize);
  begin
    inherited;
    if (msg.Width > 0) and (msg.Height > 0)
      then
        begin
          fBitmap.NewSize(msg.Width, msg.Height, cCanvasBitCount);
          UpdateIsometric;
        end;
  end;

procedure TIsometricMap.cmWantSpecialKey(var msg : TCMWantSpecialKey);
  begin
    inherited;
    if msg.CharCode in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN]
      then msg.Result := 1;
  end;

procedure TIsometricMap.wmEraseBkGnd(var msg);
  begin
  end;

procedure Register;
  begin
    RegisterComponents('Five', [TIsometricMap]);
  end;

end.
