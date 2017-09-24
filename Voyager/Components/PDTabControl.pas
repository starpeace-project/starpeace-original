unit PDTabControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  const
    noTab = -1;

  type
    TTabAlign = (taLeft, taCenter, taRight);

  type
    TPDTabControl =
      //class(TGraphicControl)
      class(TCustomControl)
        public
          constructor Create(AOwner : TComponent); override;
          destructor  Destroy; override;
        protected
          fTopMargin     : word;
          fBottomColor   : TColor;
          fTopColor      : TColor;
          fLineColor     : TColor;
          fTabColor      : TColor;
          fSelTabColor   : TColor;
          fHilTextColor  : TColor;
          fSelTextColor  : TColor;
          fTabNames      : TStrings;
          fCurrentTab    : integer;
          fTextAlign     : TTabAlign;
          fTextMargin    : integer;
          fTopTextMargin : integer;
          fOnTabChange   : TNotifyEvent;
        private
          fBitmap       : TBitmap;
          fTabSize      : integer;
          fTabMargin    : integer;
          fLightedTab   : integer;
          fFitSpace     : boolean;
          fDrawOffset   : integer;
          fUpdating     : boolean;
        private
          procedure AdjustBitmap;
          function  GetTextFlags : integer;
          //procedure DrawText(const text : string; aCanvas : TCanvas; origin : integer; color : TColor; render, jump : boolean);
          procedure DrawText(index : integer; aCanvas : TCanvas; origin : integer; color : TColor; render : boolean);
          procedure RedrawTabText(tab : integer; color : TColor; toBmp : boolean);
          procedure DrawTabs;
          procedure RenderToBmp;
          procedure ReDrawAll;
          procedure MessureTabs;
        public
          procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
        protected
          procedure Paint; override;
          procedure Loaded; override;
        private
          function  PointInTab(aX, aY : integer; var tab : integer) : boolean;
          function  TabOrigin(tab : integer) : integer;
        protected
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
          procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        public
          procedure AddObject(tabName : string; Obj : TObject);
          procedure BeginUpdate;
          procedure EndUpdate;
        private
          procedure SetTopMargin(aMargin : word);
          procedure SetBottomColor(aColor : TColor);
          procedure SetTopColor(aColor : TColor);
          procedure SetLineColor(aColor : TColor);
          procedure SetTabColor(aColor : TColor);
          procedure SetSelTabColor(aColor : TColor);
          procedure SetHilTextColor(aColor : TColor);
          procedure SetSelTextColor(aColor : TColor);
          procedure SetTabNames(aList : TStrings);
          procedure SetCurrentTab(aIndex : integer);
          procedure SetTextAlign(anAlign : TTabAlign);
          procedure SetTextMargin(aMargin : integer);
          procedure SetTopTextMargin(aMargin : integer);
        published
          property  TopMargin    : word     read fTopMargin write SetTopMargin;
          property  BottomColor  : TColor   read fBottomColor write SetBottomColor;
          property  TopColor     : TColor   read fTopColor write SetTopColor;
          property  LineColor    : TColor   read fLineColor write SetLineColor;
          property  TabColor     : TColor   read fTabColor write SetTabColor;
          property  SelTabColor  : TColor   read fSelTabColor write SetSelTabColor;
          property  HilTextColor : TColor   read fHilTextColor write SetHilTextColor;
          property  SelTextColor : TColor   read fSelTextColor write SetSelTextColor;
          property  TabNames     : TStrings read fTabNames write SetTabNames;
          property  CurrentTab   : integer  read fCurrentTab write SetCurrentTab;
          property  TextAlign    : TTabAlign read fTextAlign write SetTextAlign;
          property  TextMargin   : integer   read fTextMargin write SetTextMargin;
          property  TopTextMargin : integer read fTopTextMargin write SetTopTextMargin;
          property  OnTabChange  : TNotifyEvent read fOnTabChange write fOnTabChange;
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

  // TPDTabControl

  constructor TPDTabControl.Create(AOwner : TComponent);
    begin
      inherited;
      fTabNames     := TStringList.Create;
      fBitmap       := TBitmap.Create;
      fLightedTab   := noTab;
      Width         := 250;
      Height        := 20;
      fTopMargin    := 4;
      fTopColor     := clBlack;
      fBottomColor  := $00487195;
      fLineColor    := $00487195;
      fTabColor     := clBlack;
      fSelTabColor  := $00444F35;
      fHilTextColor := $003FFCF8;
      fSelTextColor := $0095FBF3;
      fCurrentTab   := noTab;
      fTextAlign    := taCenter;
      fTextMargin   := 0;
      fUpdating     := false;
    end;

  destructor TPDTabControl.Destroy;
    begin
      fBitmap.Free;
      fTabNames.Free;
      inherited;
    end;

  procedure TPDTabControl.AdjustBitmap;
    begin
      fBitmap.Width := max(Width, fTabNames.Count*(fTabSize + fTabMargin) + fTabMargin);
      fFitSpace := fBitmap.Width = Width;
    end;

  function TPDTabControl.GetTextFlags : integer;
    begin
      result := DT_SINGLELINE or DT_VCENTER;
      case fTextAlign of
        taLeft :
          result := result or DT_LEFT;
        taCenter :
          result := result or DT_CENTER;
        taRight :
          result := result or DT_RIGHT;
      end;
    end;

  procedure TPDTabControl.DrawText(index : integer; aCanvas : TCanvas; origin : integer; color : TColor; render : boolean);
    var
      len  : integer;
      R    : TRect;
      CR   : TRect;
      text : string;
    begin
      text := fTabNames[index];
      R.Bottom := pred(Height);
      if index = fCurrentTab
        then R.Top := fTopMargin + max(1, fTopTextMargin - 2)
        else R.Top := fTopMargin + fTopTextMargin;
      case fTextAlign of
        taLeft :
          begin
            R.Left  := origin + fTabMargin;
            R.Right := R.Left + fTabSize;
          end;
        taRight :
          begin
            R.Left  := origin + fTabMargin + fTextMargin;
            R.Right := R.Left + fTabSize;
          end;
        taCenter :
          begin
            R.Left  := origin + fTabMargin;
            R.Right := R.Left + fTabSize + fTextMargin;
          end
      end;
      len := length(text);
      aCanvas.Font.Color  := color;
      aCanvas.Brush.Style := bsClear;
      Windows.DrawText(aCanvas.Handle, pchar(text), len, R, GetTextFlags);
      if render
        then
          begin
            CR := R;
            OffsetRect(CR, -fDrawOffset, 0);
            Canvas.CopyRect(CR, fBitmap.Canvas, R);
          end;
    end;

  procedure TPDTabControl.RedrawTabText(tab : integer; color : TColor; toBmp : boolean);
    begin
      if tab <> noTab
        then DrawText(tab, fBitmap.Canvas, TabOrigin(tab), color, not toBmp);
    end;

  procedure TPDTabControl.DrawTabs;
    var
      i      : integer;
      x      : integer;
      curx   : integer;
      Points : array[0..3] of TPoint;

    procedure SetPoints(origin : integer);
      begin
        Points[0].x := origin;
        Points[0].y := pred(fBitmap.Height);
        Points[1].x := origin + fTabMargin - 1;
        Points[1].y := fTopMargin;
        Points[2].x := origin + fTabMargin + fTabSize;
        Points[2].y := fTopMargin;
        Points[3].x := origin + 2*fTabMargin + fTabSize - 1;
        Points[3].y := pred(fBitmap.Height);
      end;

    begin
      x    := pred(fTabNames.Count)*(fTabSize + fTabMargin);
      curx := noTab;
      fBitmap.Canvas.Pen.Color := fLineColor;
      for i := pred(fTabNames.Count) downto 0 do
        begin
          if i <> fCurrentTab
            then
              begin
                // Tab polygon
                fBitmap.Canvas.Brush.Color := fTabColor;
                fBitmap.Canvas.Brush.Style := bsSolid;
                SetPoints(x);
                fBitmap.Canvas.Polygon(Points);
                // Tab text
                DrawText(i, fBitmap.Canvas, x, Font.Color, false);
              end
            else curx := x;
          dec(x, fTabMargin + fTabSize);
        end;
      if curx >= 0
        then
          begin
            fBitmap.Canvas.Pen.Color := fLineColor;
            fBitmap.Canvas.MoveTo(0, pred(fBitmap.Height));
            fBitmap.Canvas.LineTo(pred(fBitmap.Width), pred(fBitmap.Height));
            SetPoints(curx);
            fBitmap.Canvas.Brush.Style := bsSolid;
            fBitmap.Canvas.Pen.Color := fSelTabColor;
            fBitmap.Canvas.Brush.Color := fSelTabColor;
            fBitmap.Canvas.Polygon(Points);
            fBitmap.Canvas.Pen.Color := fLineColor;
            inc(Points[3].x);
            inc(Points[3].y);
            fBitmap.Canvas.Polyline(Points);
            // Tab text
            DrawText(fCurrentTab, fBitmap.Canvas, curx, fSelTextColor, false);
          end;
    end;

  procedure TPDTabControl.RenderToBmp;
    begin
      GradientUtils.Gradient(fBitmap.Canvas, fTopColor, fBottomColor, false, ClientRect);
      fBitmap.Canvas.Font := Font;
      DrawTabs;
    end;

  procedure TPDTabControl.ReDrawAll;
    begin
      if not fUpdating
        then
          begin
            MessureTabs;
            RenderToBmp;
            Refresh;
          end;
    end;

  procedure TPDTabControl.MessureTabs;
    var
      i : integer;
    begin
      fTabSize := 0;
      fBitmap.Canvas.Font := Font;
      for i := 0 to pred(fTabNames.Count) do
        fTabSize := max(fTabSize, fTextMargin + Canvas.TextWidth(fTabNames[i]));
      AdjustBitmap;
    end;

  procedure TPDTabControl.Paint;
    var
      R : TRect;
    begin
      R := ClientRect;
      OffsetRect(R, fDrawOffset, 0);
      Canvas.CopyRect(ClientRect, fBitmap.Canvas, R);
    end;

  procedure TPDTabControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    begin
      inherited;
      if Parent <> nil
        then
          begin
            fBitmap.Width := Width;
            fBitmap.Height := Height;
            fTabMargin := Height - fTopMargin;
            RenderToBmp;
          end;
    end;

  procedure TPDTabControl.Loaded;
    begin
      inherited;
      if Parent <> nil
        then
          begin
            MessureTabs;
            RenderToBmp;
          end;
    end;

  function TPDTabControl.PointInTab(aX, aY : integer; var tab : integer) : boolean;
    var
      tabOffs : integer;
    begin
      inc(aX, fDrawOffset);
      tab     := aX div (fTabSize + fTabMargin);
      if tab > fTabNames.Count
        then tab := noTab;
      tabOffs := aX mod (fTabSize + fTabMargin);
      result  := (tab < fTabNames.Count) and (tabOffs > fTabMargin) and (aY > fTopMargin);
    end;

  function TPDTabControl.TabOrigin(tab : integer) : integer;
    begin
      result := tab*(fTabsize + fTabMargin);
    end;

  procedure TPDTabControl.CMMouseEnter(var Message: TMessage);
    begin
    end;

  procedure TPDTabControl.CMMouseLeave(var Message: TMessage);
    begin
      if fLightedTab <> noTab
        then DrawText(fLightedTab, fBitmap.Canvas, TabOrigin(fLightedTab), Font.Color, true);
      fLightedTab := noTab;
      Cursor := crDefault;
    end;

  procedure TPDTabControl.CMFontChanged(var Message: TMessage);
    begin
      inherited;
      ReDrawAll;
    end;

  procedure TPDTabControl.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TPDTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      tab : integer;
    begin
      inherited;
      if PointInTab(X, Y, tab)
        then
          begin
            fLightedTab  := noTab;
            MouseCapture := true;
            CurrentTab   := tab;
            Cursor       := crDefault;
            if Assigned(fOnTabChange)
              then fOnTabChange(self);
          end;
    end;

  procedure TPDTabControl.MouseMove(Shift: TShiftState; X, Y: Integer);
    var
      tab    : integer;
      offchg : boolean;
      oldoff : integer;
    begin
      inherited;
      oldoff := fDrawOffset;
      if not fFitSpace
        then
          begin
            if Width - X < fTabMargin
              then fDrawOffset := min(fDrawOffset + fTabSize, fBitmap.Width - Width)
              else
                if X < fTabMargin
                  then fDrawOffset := max(0, fDrawOffset - fTabSize);
          end;
      offchg := oldoff <> fDrawOffset;
      if PointInTab(X, Y, tab) and (Shift = [])
        then
          begin
            Cursor := crHandPoint;
            if fLightedTab <> tab
              then
                begin
                  RedrawTabText(fLightedTab, Font.Color, offchg);
                  if tab <> fCurrentTab
                    then
                      begin
                        fLightedTab := tab;
                        RedrawTabText(tab, fHilTextColor, offchg);
                      end
                    else
                      begin
                        fLightedTab := noTab;
                        Cursor := crDefault;
                      end;
                end
          end
        else
          begin
            Cursor := crDefault;
            if fLightedTab < fTabNames.Count
              then RedrawTabText(fLightedTab, Font.Color, offchg);
            fLightedTab := noTab;
          end;
      if offchg
        then Refresh;
    end;

  procedure TPDTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      MouseCapture := false;
    end;

  procedure TPDTabControl.AddObject(tabName : string; Obj : TObject);
    begin
      fTabNames.AddObject(tabName, Obj);
    end;

  procedure TPDTabControl.BeginUpdate;
    begin
      fUpdating := true;
    end;

  procedure TPDTabControl.EndUpdate;
    begin
      fUpdating := false;
      ReDrawAll;
    end;

  procedure TPDTabControl.SetTopMargin(aMargin : word);
    begin
      if fTopMargin <> aMargin
        then
          begin
            fTopMargin := min(Height div 2, aMargin);
            fTabMargin := Height - fTopMargin;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetBottomColor(aColor : TColor);
    begin
      if fBottomColor <> aColor
        then
          begin
            fBottomColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetTopColor(aColor : TColor);
    begin
      if fTopColor <> aColor
        then
          begin
            fTopColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetLineColor(aColor : TColor);
    begin
      if fLineColor <> aColor
        then
          begin
            fLineColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetTabColor(aColor : TColor);
    begin
      if fTabColor <> aColor
        then
          begin
            fTabColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetSelTabColor(aColor : TColor);
    begin
      if fSelTabColor <> aColor
        then
          begin
            fSelTabColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetHilTextColor(aColor : TColor);
    begin
      if fHilTextColor <> aColor
        then
          begin
            fHilTextColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetSelTextColor(aColor : TColor);
    begin
      if fSelTextColor <> aColor
        then
          begin
            fSelTextColor := aColor;
            ReDrawAll;
          end;
    end;

  procedure TPDTabControl.SetTabNames(aList : TStrings);
    begin
      fTabNames.Clear;
      fTabNames.AddStrings(aList);
      MessureTabs;
      fCurrentTab := 0;
      ReDrawAll;
    end;

  procedure TPDTabControl.SetCurrentTab(aIndex : integer);
    begin
      if aIndex <> fCurrentTab
        then
          begin
            fCurrentTab := min(aIndex, pred(fTabNames.Count));
            ReDrawAll;
            if Assigned(fOnTabChange)
              then fOnTabChange(self);
          end;
    end;

  procedure TPDTabControl.SetTextAlign(anAlign : TTabAlign);
    begin
      if fTextAlign <> anAlign
       then
         begin
           fTextAlign := anAlign;
           ReDrawAll;
         end;
    end;

  procedure TPDTabControl.SetTextMargin(aMargin : integer);
    begin
      if fTextMargin <> aMargin
       then
         begin
           fTextMargin := max(0, aMargin);
           ReDrawAll;
         end;
    end;

  procedure TPDTabControl.SetTopTextMargin(aMargin : integer);
    begin
      if fTopTextMargin <> aMargin
       then
         begin
           fTopTextMargin := max(0, aMargin);
           ReDrawAll;
         end;
    end;

  procedure Register;
    begin
      RegisterComponents('Five', [TPDTabControl]);
    end;

end.
