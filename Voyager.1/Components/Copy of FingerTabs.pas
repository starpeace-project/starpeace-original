unit FingerTabs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  const
    noFinger = -1;

  type
    TFingetTabAlign  = (ftaLeft, ftaRight);
    TFingerType      = (ftFinger, ftGradient);
    TFingerTabAdjust = procedure(Sender : TObject; DeltaSize : integer) of object;

    PRects = ^TRects;
    TRects = array[0..0] of TRect;

    TFingerTabs =
      class(TCustomControl)
        public
          constructor Create(AOwner : TComponent); override;
          destructor  Destroy; override;
        private
          fLeftColor      : TColor;
          fRightColor     : TColor;
          fLineColor      : TColor;
          fFingerColor    : TColor;
          fSelTabColor    : TColor;
          fTextColor      : TColor;
          fHilTextColor   : TColor;
          fSelTextColor   : TColor;
          fTabNames       : TStrings;
          fCurrentFinger  : integer;
          fTextAlign      : TFingetTabAlign;
          fTextMargin     : integer;
          fFingerRadius   : integer;
          fFixedSize      : boolean;
          fFingerType     : TFingerType;
          fOnFingerChange : TNotifyEvent;
          fOnAdjustHeight : TFingerTabAdjust;
        private
          fBitmap     : TBitmap;
          fRects      : PRects;
          fLightedTab : integer;
          fUpdating   : boolean;
          fStdHeight  : word;
        private
          function  MessureLineHeight(const text : string) : TRect;
          function  GetTotalHeight : integer;
          procedure CalcRects;
          procedure RebuildRects;
          function  GetTextFlags : integer;
          procedure DrawText(index : integer; aCanvas : TCanvas; color : TColor; render : boolean);
          procedure RedrawTabText(tab : integer; color : TColor; toBmp : boolean);
          procedure DrawFinger(index : integer);
          procedure DrawTabs;
          procedure RenderToBmp;
          procedure RedrawAll;
        protected
          procedure Paint; override;
          procedure Loaded; override;
          procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
        private
          function  PointInTab(aX, aY : integer; var tab : integer) : boolean;
        protected
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
          procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        public
          procedure BeginUpdate;
          procedure EndUpdate;
          procedure AddFinger(name : string; Obj : TObject);
          procedure ClearFingers;
        private
          function GetObject(index : integer) : TObject;
        public
          property Objects[index : integer] : TObject read GetObject;
        private
          procedure SetLeftColor(aColor : TColor);
          procedure SetRightColor(aColor : TColor);
          procedure SetLineColor(aColor : TColor);
          procedure SetFingerColor(aColor : TColor);
          procedure SetSelTabColor(aColor : TColor);
          procedure SetTextColor(aColor : TColor);
          procedure SetHilTextColor(aColor : TColor);
          procedure SetSelTextColor(aColor : TColor);
          procedure SetTabNames(Names : TStrings);
          procedure SetCurrentFinger(index : integer);
          procedure SetTextAlign(anAlign : TFingetTabAlign);
          procedure SetTextMargin(aMargin : integer);
          procedure SetFingerRadius(aRadius : integer);
          procedure SetFixedSize(fixed : boolean);
          procedure SetFingerType(aType : TFingerType);
        published
          property LeftColor      : TColor read fLeftColor write SetLeftColor;
          property RightColor     : TColor read fRightColor write SetRightColor;
          property LineColor      : TColor read fLineColor   write SetLineColor;
          property FingerColor    : TColor read fFingerColor write SetFingerColor;
          property SelTabColor    : TColor read fSelTabColor write SetSelTabColor;
          property TextColor      : TColor read fTextColor write SetTextColor;
          property HilTextColor   : TColor read fHilTextColor write SetHilTextColor;
          property SelTextColor   : TColor read fSelTextColor write SetSelTextColor;
          property TabNames       : TStrings read fTabNames write SetTabNames;
          property CurrentFinger  : integer read fCurrentFinger write SetCurrentFinger;
          property TextAlign      : TFingetTabAlign read fTextAlign write SetTextAlign;
          property TextMargin     : integer read fTextMargin write SetTextMargin;
          property FingerRadius   : integer read fFingerRadius write SetFingerRadius;
          property FixedSize      : boolean read fFixedSize write SetFixedSize;
          property FingerType     : TFingerType read fFingerType write SetFingerType;
        published
          property OnOnFingerChange : TNotifyEvent read fOnFingerChange write fOnFingerChange;
          property OnAdjustHeight   : TFingerTabAdjust read fOnAdjustHeight write fOnAdjustHeight;
        published
          property  Align;
          property  DragCursor;
          property  DragMode;
          property  Enabled;
          property  Font;
          property  ParentFont;
          property  ParentShowHint;
          property  PopupMenu;
          property  ShowHint;
          property  Visible;
          property  OnClick;
          property  OnDragDrop;
          property  OnDragOver;
          property  OnEndDrag;
          property  OnMouseDown;
          property  OnMouseMove;
          property  OnMouseUp;
          property  OnStartDrag;
     end;

  procedure Register;

implementation

  uses
    GradientUtils;

  function max(a, b : integer) : integer;
    begin
      if a > b
        then result := a
        else result := b;
    end;

  function min(a, b : integer) : integer;
    begin
      if a > b
        then result := b
        else result := a;
    end;

  // TFingerTabs

  constructor TFingerTabs.Create(AOwner : TComponent);
    begin
      inherited;
      fBitmap     := TBitmap.Create;
      fTabNames   := TStringList.Create;
      fTextMargin := 2;
      fLightedTab := noFinger;
      fCurrentFinger := noFinger;
    end;

  destructor TFingerTabs.Destroy;
    begin
      fBitmap.Free;
      fUpdating := true;
      ClearFingers;
      fTabNames.Free;
      inherited;
    end;

  function TFingerTabs.MessureLineHeight(const text : string) : TRect;
    begin
      result.Left   := 0;
      result.Top    := 0;
      result.Right  := Width - 2*(fFingerRadius + fTextMargin);
      result.Bottom := 0;
      Windows.DrawText(fBitmap.Canvas.Handle, pchar(text), -1, result, DT_CALCRECT or DT_WORDBREAK);
    end;

  function TFingerTabs.GetTotalHeight : integer;
    var
      i : integer;
    begin
      result   := 0;
      for i := 0 to pred(fTabNames.Count) do
        inc(result, MessureLineHeight(fTabNames[i]).Bottom);
      inc(result, pred(fTabNames.Count)*fTextMargin + 2*fFingerRadius + 2*fTextMargin);
    end;

  procedure TFingerTabs.CalcRects;
    var
      i     : integer;
      crHgh : integer;
      R     : TRect;
      crTop : integer;
    begin
      if fCurrentFinger = noFinger
        then crTop := fFingerRadius
        else
          if fCurrentFinger = 0
            then crTop := 0
            else crTop := fTextMargin;
      for i := 0 to pred(fTabNames.Count) do
        begin
          R := MessureLineHeight(fTabNames[i]);
          crHgh := R.Bottom;
          // Le's align the Rect
          if fFixedSize
            then
              begin
                R.Left := fFingerRadius + fTextMargin;
                R.Right := Width - fTextMargin;
              end
            else
              begin
                case fTextAlign of
                  ftaLeft  : R.Left := fFingerRadius + fTextMargin;
                  ftaRight : R.Left := Width - R.Right - fTextMargin;
                end;
                inc(R.Right, R.Left);
              end;
          if i <> fCurrentFinger
            then
              begin
                R.Top := crTop;
                inc(crTop, crHgh);
                R.Bottom := crTop;
              end
            else
              begin
                inc(crTop, fFingerRadius);
                R.Top := crTop;
                inc(crTop, crHgh);
                R.Bottom := crTop;
                inc(crTop, fFingerRadius);
              end;
          fRects[i] := R;
          inc(crTop, fTextMargin);
        end;
    end;

  procedure TFingerTabs.RebuildRects;
    begin
      ReallocMem(fRects, fTabNames.Count*sizeof(TRect));
      CalcRects;
    end;

  function TFingerTabs.GetTextFlags : integer;
    begin
      result := DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK;
      case fTextAlign of
        ftaLeft :
          result := result or DT_LEFT;
        ftaRight :
          result := result or DT_RIGHT;
      end;
    end;

  procedure TFingerTabs.DrawText(index : integer; aCanvas : TCanvas; color : TColor; render : boolean);
    var
      R   : TRect;
      aux : pchar;
    begin
      aux := pchar(fTabNames[index]);
      R   := fRects[index];
      aCanvas.Font.Color  := color;
      aCanvas.Brush.Style := bsClear;
      Windows.DrawText(aCanvas.Handle, aux, -1, R, GetTextFlags);
      if render
        then Canvas.CopyRect(R, fBitmap.Canvas, R);
    end;

  procedure TFingerTabs.RedrawTabText(tab : integer; color : TColor; toBmp : boolean);
    begin
      if tab <> noFinger
        then DrawText(tab, fBitmap.Canvas, color, not toBmp);
    end;

  procedure TFingerTabs.DrawFinger(index : integer);
    var
      R   : TRect;
      pts : array[0..5] of TPoint;
      d   : integer;
      h   : integer;
    begin
      R := fRects[index];
      case fFingerType of
        ftFinger :
          begin
            h := fStdHeight div 2;
            d := 2*(h + fFingerRadius);

            fBitmap.Canvas.Brush.Style := bsSolid;
            fBitmap.Canvas.Brush.Color := fFingerColor;

            InflateRect(R, fTextMargin, fFingerRadius);
            dec(R.Left, fFingerRadius);

            pts[0].x := R.Left;
            pts[0].y := R.Top  + fFingerRadius + h;
            pts[1].x := R.Left + fFingerRadius + h;
            pts[1].y := R.Top;
            pts[2].x := R.Right;
            pts[2].y := R.Top;

            pts[3].x := R.Right;
            pts[3].y := R.Bottom - 1;
            pts[4].x := R.Left + fFingerRadius + h;
            pts[4].y := R.Bottom - 1;
            pts[5].x := R.Left;
            pts[5].y := R.Bottom - (fFingerRadius + h);

            fBitmap.Canvas.Pen.Color := fFingerColor;
            fBitmap.Canvas.Polygon(pts);
            fBitmap.Canvas.Ellipse(R.Left, R.Top, R.Left + d, R.Top + d);
            fBitmap.Canvas.Ellipse(R.Left, R.Bottom - d, R.Left + d, R.Bottom);
          end;
        ftGradient :
          begin
            InflateRect(R, fTextMargin, fFingerRadius);
            dec(R.Left, fFingerRadius);
            GradientUtils.Gradient(fBitmap.Canvas, fLeftColor, fFingerColor, true, R);
          end;
      end;
    end;

  procedure TFingerTabs.DrawTabs;
    var
      i : integer;
    begin
      fBitmap.Canvas.Brush.Style := bsClear;
      for i := 0 to pred(fTabNames.Count) do
        if i <> fCurrentFinger
          then RedrawTabText(i, fTextColor, true)
          else
            begin
              DrawFinger(i);
              RedrawTabText(i, fSelTextColor, true);
            end;
    end;

  procedure TFingerTabs.RenderToBmp;
    begin
      fBitmap.Canvas.Font := Font;
      fBitmap.Canvas.Brush.Color := fLeftColor;
      fBitmap.Canvas.Brush.Style := bsSolid;
      fBitmap.Canvas.FillRect(ClientRect);
      DrawTabs;
    end;

  procedure TFingerTabs.RedrawAll;
    var
      TtHght : integer;
    begin
      if not fUpdating
        then
          begin
            fStdHeight := Canvas.TextHeight('|') div 2;
            TtHght := GetTotalHeight;
            if (TtHght <> Height) and Assigned(fOnAdjustHeight)
              then fOnAdjustHeight(self, TtHght - Height);
            if TtHght > 0
              then
                begin
                  fBitmap.Height := TtHght;
                  RebuildRects;
                  Height := max(Height, TtHght);
                  RenderToBmp;
                  Refresh;
                end;
          end;
    end;

  procedure TFingerTabs.Paint;
    begin
      Canvas.CopyRect(ClientRect, fBitmap.Canvas, ClientRect);
    end;

  procedure TFingerTabs.Loaded;
    begin
      inherited;
      fFingerRadius := Canvas.TextHeight('|') div 2;
    end;

  procedure TFingerTabs.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
    begin
      inherited;
      if (AWidth > 0) and (AHeight > 0) and (AWidth <> fBitmap.Width) and (AHeight <> fBitmap.Height)
        then
          begin
            fBitmap.Width := Width;
            fBitmap.Height := Height;
            fBitmap.Canvas.FillRect(ClientRect);
            Canvas.CopyRect(ClientRect, fBitmap.Canvas, ClientRect);
          end;
    end;

  function TFingerTabs.PointInTab(aX, aY : integer; var tab : integer) : boolean;
    var
      cnt : integer;
    begin
      tab := 0;
      cnt := fTabNames.Count;
      while (tab < cnt) and not PtInRect(fRects[tab], Point(aX, aY)) do
        inc(tab);
      result := tab < cnt;
    end;

  procedure TFingerTabs.CMMouseEnter(var Message: TMessage);
    begin
    end;

  procedure TFingerTabs.CMMouseLeave(var Message: TMessage);
    begin
      RedrawTabText(fLightedTab, fTextColor, false);
      fLightedTab := noFinger;
      Cursor      := crDefault;
    end;

  procedure TFingerTabs.CMFontChanged(var Message: TMessage);
    begin
      RedrawAll;
    end;

  procedure TFingerTabs.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TFingerTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      tab : integer;
    begin
      inherited;
      if PointInTab(X, Y, tab)
        then
          begin
            fLightedTab  := noFinger;
            MouseCapture := true;
            CurrentFinger   := tab;
          end;
    end;

  procedure TFingerTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
    var
      tab : integer;
    begin
      inherited;
      if PointInTab(X, Y, tab) and (Shift = [])
        then
          begin
            Cursor := crHandPoint;
            if fLightedTab <> tab
              then
                begin
                  RedrawTabText(fLightedTab, fTextColor, false);
                  if tab <> fCurrentFinger
                    then
                      begin
                        fLightedTab := tab;
                        RedrawTabText(tab, fHilTextColor, false);
                      end
                    else
                      begin
                        fLightedTab := noFinger;
                        Cursor := crDefault;
                      end;
                end
          end
        else
          begin
            Cursor := crDefault;
            RedrawTabText(fLightedTab, fTextColor, false);
            fLightedTab := noFinger;
          end;
    end;

  procedure TFingerTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      MouseCapture := false;
    end;

  procedure TFingerTabs.BeginUpdate;
    begin
      fUpdating := true;
    end;

  procedure TFingerTabs.EndUpdate;
    begin
      fUpdating := false;
      RedrawAll;
    end;

  procedure TFingerTabs.AddFinger(name : string; Obj : TObject);
    begin
      fTabNames.AddObject(name, Obj);
      ReDrawAll;
    end;

  procedure TFingerTabs.ClearFingers;
    var
      i : integer;
    begin
      for i := 0 to pred(fTabNames.Count) do
        fTabNames.Objects[i].Free;
      fTabNames.Clear;
      fCurrentFinger := noFinger;
      ReDrawAll;
    end;

  function TFingerTabs.GetObject(index : integer) : TObject;
    begin
      result := fTabNames.Objects[index];
    end;

  procedure TFingerTabs.SetLeftColor(aColor : TColor);
    begin
      if fLeftColor <> aColor
        then
          begin
            fLeftColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetRightColor(aColor : TColor);
    begin
      if fRightColor <> aColor
        then
          begin
            fRightColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetLineColor(aColor : TColor);
    begin
      if fLineColor <> aColor
        then
          begin
            fLineColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetFingerColor(aColor : TColor);
    begin
      if fFingerColor <> aColor
        then
          begin
            fFingerColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetSelTabColor(aColor : TColor);
    begin
      if fSelTabColor <> aColor
        then
          begin
            fSelTabColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetTextColor(aColor : TColor);
    begin
      if fTextColor <> aColor
        then
          begin
            fTextColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetHilTextColor(aColor : TColor);
    begin
      if fHilTextColor <> aColor
        then
          begin
            fHilTextColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetSelTextColor(aColor : TColor);
    begin
      if fSelTextColor <> aColor
        then
          begin
            fSelTextColor := aColor;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetTabNames(Names : TStrings);
    begin
      fCurrentFinger := -1;
      fTabNames.Clear;
      fTabNames.AddStrings(Names);
      RedrawAll;
    end;

  procedure TFingerTabs.SetCurrentFinger(index : integer);
    begin
      if fCurrentFinger <> index
        then
          begin
            fCurrentFinger := index;
            RedrawAll;
            if Assigned(fOnFingerChange)
              then fOnFingerChange(self);
          end;
    end;

  procedure TFingerTabs.SetTextAlign(anAlign : TFingetTabAlign);
    begin
      if fTextAlign <> anAlign
        then
          begin
            fTextAlign := anAlign;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetTextMargin(aMargin : integer);
    begin
      if fTextMargin <> aMargin
        then
          begin
            fTextMargin := aMargin;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetFingerRadius(aRadius : integer);
    begin
      if fFingerRadius <> aRadius
        then
          begin
            fFingerRadius := aRadius;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetFixedSize(fixed : boolean);
    begin
      if fFixedSize <> fixed
        then
          begin
            fFixedSize := fixed;
            RedrawAll;
          end;
    end;

  procedure TFingerTabs.SetFingerType(aType : TFingerType);
    begin
      if fFingerType <> aType
        then
          begin
            fFingerType := aType;
            RedrawAll;
          end;
    end;

  procedure Register;
    begin
      RegisterComponents('Five', [TFingerTabs]);
    end;

end.
