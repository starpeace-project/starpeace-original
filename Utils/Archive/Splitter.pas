unit Splitter;

interface

uses
  Messages, Windows, Classes, Sysutils, Controls, Graphics;

{$INCLUDE Splitter.inc}

const
  cDefaultTrackWidth = 2;
  
type
  TSplitterStyle = (ssNone, ssLowered, ssRaised);

type
  TSplitter = class;
  TOnNotify = procedure (Sender : TSplitter; x, y : integer) of object;
  TSplitter =
    class(TCustomControl)
      private
        fStyle      : TSplitterStyle;
        fAutoTrack  : boolean;
        fTrackWidth : integer;
        fOnNotify   : TOnNotify;
        procedure SetStyle(which : TSplitterStyle);
      published
        property Align;
        property Color;
        property ParentShowHint;
        property ShowHint;
        property Visible;
        property OnDblClick;
        property TrackWidth : integer        read fTrackWidth write fTrackWidth default cDefaultTrackWidth;
        property Style      : TSplitterStyle read fStyle      write SetStyle    default ssNone;
        property AutoTrack  : boolean        read fAutoTrack  write fAutoTrack;
        property OnNotify   : TOnNotify      read fOnNotify   write fOnNotify;
      public
        constructor Create(aOwner : TComponent);                                         override;
      protected
        procedure Paint;                                                                 override;
        procedure SetBounds(aLeft, aTop, aWidth, aHeight : integer);                     override;
        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : integer); override;
        procedure MouseMove(Shift : TShiftState; x, y : integer);                        override;
        procedure MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : integer);   override;
        procedure DblClick;                                                              override;
      private
        fDown     : boolean;
        fTracking : boolean;
        fDblClick : boolean;
        fClient   : TRect;
        fPos      : TPoint;
        dc        : HDC;
        pen       : HPen;
        rop       : integer;
        procedure InitializeTracking(x, y : integer);
        procedure EndTracking;
        procedure Show;
        procedure Hide;
        procedure Notify;
        function  ClipPoint(x, y : integer) : TPoint;
    end;

procedure Register;


implementation


// -- TSplitter

procedure TSplitter.SetStyle(which : TSplitterStyle);
  begin
    inherited;
    if which <> fStyle
      then
        begin
          fStyle := which;
          Invalidate;
        end;
  end;

constructor TSplitter.Create(aOwner : TComponent);
  begin
    inherited;
    ControlStyle := ControlStyle + [csReplicatable];
    fStyle      := ssNone;
    fTrackWidth := cDefaultTrackWidth;
    Color       := clBtnFace;
    Width       := 50;
    Height      := 50;
  end;

procedure TSplitter.Paint;                                                                 
  var
    Color2 : TColor;
  begin
    with Canvas do
      begin
        Pen.Width := 1;
        case fStyle of
          ssRaised :
            begin
              Pen.Color := clBtnHighlight;
              Color2    := clBtnShadow;
            end;
          ssLowered :
            begin
              Pen.Color := clBtnShadow;
              Color2    := clBtnHighlight;
            end;
          else
            Pen.Color := Color;
            Color2    := Color;
        end;
        PolyLine([Point(0, Height - 1), Point(0, 0), Point(Width - 1, 0)]);
        Pen.Color := Color2;
        PolyLine([Point(Width - 1, 0), Point(Width - 1, Height - 1), Point(0, Height - 1)]);
        if (Width > 2) and (Height > 2)
          then
            begin
              Pen.Color   := Color;
              Brush.Color := Color;
              Rectangle(1, 1, Width - 2, Height - 2);
            end;
      end;
  end;

procedure TSplitter.SetBounds(aLeft, aTop, aWidth, aHeight : integer);
  begin
    inherited;
    if Width < Height
      then Cursor := crHSplit
      else Cursor := crVSplit;
  end;

procedure TSplitter.MouseDown(Button : TMouseButton; Shift : TShiftState; x, y : integer);
  begin
    if not fDblClick
      then fDown := Button = mbLeft
      else fDblClick := false;
    inherited;
  end;

procedure TSplitter.MouseMove(Shift : TShiftState; x, y : integer);
  begin
    if fDown
      then
        begin
          if not fTracking
            then InitializeTracking(x, y);
          Hide;
          fPos := ClipPoint(x, y);
          if fAutoTrack
            then Notify;
          Show;
        end;
    inherited;
  end;

procedure TSplitter.MouseUp(Button : TMouseButton; Shift : TShiftState; x, y : integer);
  begin
    if Button = mbLeft
      then
        begin
          if fTracking
            then
              begin
                EndTracking;
                Parent.Invalidate;
                Notify;
              end;
          fDown := false;
        end;
    inherited;
  end;

procedure TSplitter.DblClick;
  begin
    fDblClick := true;
    inherited;
  end;

procedure TSplitter.InitializeTracking(x, y : integer);
  const
    PenColor = $00DCDCDC;
  begin
    dc   := GetDC(0);
    pen  := SelectObject(dc, CreatePen(PS_SOLID, fTrackWidth, PenColor));
    rop  := SetROP2(dc, R2_XORPEN);
    fPos := ClientToScreen(Point(x, y));
    with Parent, fClient do
      begin
        TopLeft     := ClientOrigin;
        BottomRight := ClientToScreen(Point(ClientWidth, ClientHeight));
      end;
    Show;
    fTracking := true;
  end;

procedure TSplitter.EndTracking;
  begin
    fTracking := false;
    Hide;
    SetROP2(dc, rop);
    DeleteObject(SelectObject(dc, pen));
    ReleaseDC(0, dc);
  end;

procedure TSplitter.Show;
  var
    xi, yi : integer;
    xf, yf : integer;
  begin
    if Width > Height
      then
        begin
          xi := fClient.Left;
          yi := fPos.y;
          xf := fClient.Right;
          yf := fPos.y;
        end
      else
        begin
          xi := fPos.x;
          yi := fClient.Top;
          xf := fPos.x;
          yf := fClient.Bottom;
        end;
    MoveToEx(dc, xi, yi, nil);
    LineTo(dc, xf, yf);
  end;

procedure TSplitter.Hide;
  begin
    Show;
  end;

procedure TSplitter.Notify;
  begin
    with Parent, ScreenToClient(fPos) do
      if assigned(fOnNotify)
        then fOnNotify(Self, x, y)
        else
          if Width < Height
            then Left := x
            else Top  := y;
  end;

function TSplitter.ClipPoint(x, y : integer) : TPoint;
  begin
    result := ClientToScreen(Point(x, y));
    with result, fClient do
      begin
        if x < Left
          then x := Left;
        if x > Right
          then x := Right;
        if y < Top
          then y := Top;
        if y > Bottom
          then y := Bottom;
      end;
  end;


  procedure Register;
    begin
      RegisterComponents('Merchise', [TSplitter]);
    end;


end.

