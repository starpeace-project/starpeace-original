unit FramedButton;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, extctrls;

  const
    fbsNormal    = 0;
    fbsMouseDown = 1;
    fbsMouseIn   = 2;
    fbsEnabled   = 4;
    fbsSelected  = 8;

  type
    TFramedButtonState = byte;
    TFramedButtonAlign = (fbaLeft, fbaCenter, fbaRight);

  type
    TFramedButton =
      class(TGraphicControl)
        public
          constructor Create(AOwner : TComponent); override;
        protected
          fTextColor     : TColor;
          fFrameColor    : TColor;
          fSelFrameColor : TColor;
          fSelTextColor  : TColor;
          fDisableColor  : TColor;
          fFrameMargin   : integer;
          fTextMargin    : integer;
          fState         : TFramedButtonState;
          fAlign         : TFramedButtonAlign;
          fFramed        : boolean;
          fHilFramed     : boolean;
          fNorGrpColor   : TColor;
          fSelGrpColor   : TColor;
          fGrpBkColor    : TColor;
          fGroupIndex    : integer;
          fAllowAllUp    : boolean;
          fTimerMouse    : TTimer;
        public
          procedure IroClick;
        private
          procedure DoPaint(R : TRect; frameColor, textColor : TColor; mouseDown : boolean);
        protected
          procedure Paint; override;
          procedure Click; override;
        public
          procedure CMTextChange(var Message: TMessage); message CM_TEXTCHANGED;
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
          procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure UnSelectGroup;
        private
          procedure SetTextColor(aColor : TColor);
          procedure SetSelTextColor(aColor : TColor);
          procedure SetFrameColor(aColor : TColor);
          procedure SetSelFrameColor(aColor : TColor);
          procedure SetDisableColor(aColor : TColor);
          procedure SetFrameMargin(aMargin : integer);
          procedure SetTextMargin(aMargin : integer);
          procedure SetAlign(anAlign : TFramedButtonAlign);
          procedure SetFramed(value : boolean);
          procedure SetHilFramed(value : boolean);
          function  GetFlags : integer;
          function  GetSelected : boolean;
          procedure SetSelected(value : boolean);
          procedure MouseUpTimer(Sender: TObject);
        published
          property  Color;
          property  TextColor     : TColor  read fTextColor     write SetTextColor;
          property  FrameColor    : TColor  read fFrameColor    write SetFrameColor;
          property  SelTextColor  : TColor  read fSelTextColor  write SetSelTextColor;
          property  DisableColor  : TColor  read fDisableColor  write SetDisableColor;
          property  SelFrameColor : TColor  read fSelFrameColor write SetSelFrameColor;
          property  FrameMargin   : integer read fFrameMargin   write SetFrameMargin default 2;
          property  TextMargin    : integer read fTextMargin    write SetTextMargin default 2;
          property  Align         : TFramedButtonAlign read fAlign write SetAlign;
          property  Framed        : boolean read fFramed write SetFramed;
          property  HilFramed     : boolean read fHilFramed write SetHilFramed;
          property  NorGrpColor   : TColor  read fNorGrpColor write fNorGrpColor;
          property  SelGrpColor   : TColor  read fSelGrpColor write fSelGrpColor;
          property  GrpBkColor    : TColor  read fGrpBkColor  write fGrpBkColor;
          property  GroupIndex    : integer read fGroupIndex  write fGroupIndex;
          property  Selected      : boolean read GetSelected  write SetSelected;
          property  AllowAllUp    : boolean read fAllowAllUp  write fAllowAllUp default false;
          property  Text;
          property  DragCursor;
          property  DragMode;
          property  Enabled;
          property  Font;
          property  ParentFont;
          property  ParentShowHint;
          property  PopupMenu;
          property  ShowHint;
          //property TabOrder;
          //property TabStop;
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


  constructor TFramedButton.Create(AOwner : TComponent);
    begin
      inherited;
      fState       := fbsEnabled;
      fFramed      := true;
      fHilFramed   := true;
      fNorGrpColor := clBlack;
      fSelGrpColor := clWhite;
      fGrpBkColor  := clBlack;
      Cursor       := crHandPoint;
      fTimerMouse  := TTimer.Create(self);
      fTimerMouse.OnTimer  := MouseUpTimer;
      fTimerMouse.Interval := 1000;
      fTimerMouse.Enabled  := false;
    end;

  procedure TFramedButton.IroClick;
    begin
      fState := fState or fbsMouseDown;
      if fGroupIndex > 0
        then
          begin
            UnSelectGroup;
            if fAllowAllUp
              then
                if fState and fbsSelected = 0
                  then fState := fState or fbsSelected
                  else fState := fState and not fbsSelected
              else fState := fState or fbsSelected;
          end;
      Refresh;
    end;
    
  procedure TFramedButton.DoPaint(R : TRect; frameColor, textColor : TColor; mouseDown : boolean);
    begin
      if not mouseDown
        then InflateRect(R, -fFrameMargin, -fFrameMargin);
      if fHilFramed
        then
          begin
            Canvas.Brush.Color := frameColor;
            Canvas.FrameRect(R);
          end;
      if not mouseDown
        then InflateRect(R, -fTextMargin, -fTextMargin)
        else InflateRect(R, -(fFrameMargin + fTextMargin), -(fFrameMargin + fTextMargin));

      if not mouseDown
        then Canvas.Font.Color  := textColor
        else Canvas.Font.Color := fframeColor;

      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, GetFlags);
    end;

  procedure TFramedButton.Paint;
    var
      R : TRect;
    begin
      R := ClientRect;
      Canvas.Font := Font;
      if (fGroupIndex > 0) and (fState and fbsSelected <> 0) and (fState and fbsEnabled <> 0)
        then
          begin
            Canvas.Brush.Color := fGrpBkColor;
            Canvas.Brush.Style := bsSolid;
            Canvas.FillRect(R);
            if fState and fbsMouseIn = 0
              then DoPaint(R, fNorGrpColor, fNorGrpColor, fState and fbsMouseDown <> 0)
              else DoPaint(R, fSelGrpColor, fSelGrpColor, fState and fbsMouseDown <> 0);
          end
        else
          if (fState and fbsMouseIn = 0) or (fState and fbsEnabled = 0)
            then
              begin
                if fState and fbsEnabled = 0
                  then DoPaint(R, fDisableColor, fDisableColor, false)
                  else DoPaint(R, fFrameColor, Font.Color, false);
              end
            else DoPaint(R, fSelFrameColor, fSelTextColor, fState and fbsMouseDown <> 0);
    end;

  procedure TFramedButton.CMTextChange(var Message: TMessage);
    begin
      Refresh;
    end;

  procedure TFramedButton.CMMouseEnter(var Message: TMessage);
    begin
      {
      if fState and fbsMouseIn = 0
        then
          begin
      }
            Cursor := crHandPoint;
            fState := fState or fbsMouseIn;
            if fState and fbsMouseDown = 0
              then Refresh;
      {
          end;
      }
    end;

  procedure TFramedButton.CMMouseLeave(var Message: TMessage);
    begin
      {
      if fState and fbsMouseIn <> 0
        then
          begin
      }
            Cursor := crDefault;
            fState := fState and not fbsMouseIn;
            if fState and fbsMouseDown = 0
              then Refresh;
      {
          end;
      }
    end;

  procedure TFramedButton.CMEnabledChanged(var Message: TMessage);
    begin
      inherited;
      if Enabled
        then fState := fState or fbsEnabled
        else fState := fState and not fbsEnabled;
    end;

  procedure TFramedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if (fState and fbsMouseDown)=0
        then
          begin
            inherited;
            IroClick;
          end;
    end;

  procedure TFramedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      {
      if PtInRect(ClientRect, Point(X, Y))
        then
          begin
            Cursor := crHandPoint;
            if fState and fbsMouseIn = 0
              then
                begin
                  //MouseCapture := true;
                  fState := fState or fbsMouseIn;
                  if (fState and fbsEnabled <> 0) and (fState and fbsMouseDown = 0)
                    then Refresh;
                end;
          end;
      }
    end;

  procedure TFramedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      inherited;
      fTimerMouse.Enabled  := true;
      // fState := fState and not fbsMouseDown;
      //Refresh;
    end;

  procedure TFramedButton.UnSelectGroup;
    var
      Ctrl : TControl;
      i    : integer;
      cnt  : integer;
    begin
      cnt := Parent.ControlCount;
      i   := 0;
      while i < cnt do
        begin
          Ctrl := Parent.Controls[i];
          if Ctrl is TFramedButton
            then
              if (Ctrl <> Self) and (TFramedButton(Ctrl).fGroupIndex = fGroupIndex)
                then
                  begin
                    if TFramedButton(Ctrl).Selected
                      then i := cnt;
                    TFramedButton(Ctrl).Selected := false;
                  end;
          inc(i);
        end;
    end;

  procedure TFramedButton.SetTextColor(aColor : TColor);
    begin
      if fTextColor <> aColor
        then
          begin
            fTextColor := aColor;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetSelTextColor(aColor : TColor);
    begin
      if fSelTextColor <> aColor
        then
          begin
            fSelTextColor := aColor;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetFrameColor(aColor : TColor);
    begin
      if fFrameColor <> aColor
        then
          begin
            fFrameColor := aColor;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetSelFrameColor(aColor : TColor);
    begin
      if fSelFrameColor <> aColor
        then
          begin
            fSelFrameColor := aColor;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetDisableColor(aColor : TColor);
    begin
      if fDisableColor <> aColor
        then
          begin
            fDisableColor := aColor;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetFrameMargin(aMargin : integer);
    begin
      if fFrameMargin <> aMargin
        then
          begin
            fFrameMargin := aMargin;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetTextMargin(aMargin : integer);
    begin
      if fTextMargin <> aMargin
        then
          begin
            fTextMargin := aMargin;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetAlign(anAlign : TFramedButtonAlign);
    begin
      if fAlign <> anAlign
        then
          begin
            fAlign := anAlign;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetFramed(value : boolean);
    begin
      if fFramed <> value
        then
          begin
            fFramed := value;
            Refresh;
          end;
    end;

  procedure TFramedButton.SetHilFramed(value : boolean);
    begin
      if fHilFramed <> value
        then
          begin
            fHilFramed := value;
            Refresh;
          end;
    end;

  function TFramedButton.GetFlags : integer;
    begin
      case fAlign of
        fbaLeft :
          result := DT_LEFT;
        fbaCenter :
          result := DT_CENTER;
        fbaRight :
          result := DT_RIGHT;
        else
          result := 0
      end;
      result := result or DT_VCENTER or DT_SINGLELINE;// or DT_WORDBREAK;
    end;

  function TFramedButton.GetSelected : boolean;
    begin
      result := fState and fbsSelected <> 0;
    end;

  procedure TFramedButton.SetSelected(value : boolean);
    begin
      if value
        then
          begin
            fState := fState or fbsSelected;
            UnSelectGroup;
          end
        else fState := fState and not fbsSelected;
      Refresh;
    end;

procedure TFramedButton.MouseUpTimer(Sender: TObject);
  begin
    fTimerMouse.Enabled  := false;
    fState := fState and not fbsMouseDown;
    Refresh;
  end;

procedure TFramedButton.Click;
  begin
     if not fTimerMouse.Enabled 
        then inherited;
  end;

  // Register
  procedure Register;
    begin
      RegisterComponents('Five', [TFramedButton]);
    end;


end.
