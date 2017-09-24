unit PercentBar;

interface

  uses
    SysUtils, Windows, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, ExtCtrls, StdCtrls;

  const
    SensitiveRadius = 2;

  const
    InitRepeatPause = 500;  { pause before repeat timer (ms) }
    RepeatPause     = 100;  { pause before hint window displays (ms)}

  type
    THorJustify = (hjCenter, hjClose);
    TVerJustify = (vjTop, vjCenter, vjBottom);
    TTextFormat = (tfNone, tfNumber, tfPercent);
    TKind       = (kdCuantitative, kdCualitative);

    TWinControlClass = class of TWinControl;

  type
    TBltBitmap = class(TBitmap)
      procedure MakeLike(ATemplate: TBitmap);
    end;

    TTimeState = set of (tbFocusRect, tbAllowTimer);

  type
    TPercentBar = class(TGraphicControl)
    private
      FAvailableArea:    word;
      FBackColor:        TColor;
      FBorderStyle:      TBorderStyle;
      FCurValue:         Longint;
      FForeColor:        TColor;
      FIncrement:        integer;
      FLastCoord:        integer;
      FLimitsVerJustify: TVerJustify;
      FLimitsHorJustify: THorJustify;
      FLimitsFont:       TFont;
      FLimitsWidth:      integer;
      FKind:             TKind;
      FMaxValue:         Longint;
      FMinValue:         Longint;
      FProgressFont:     TFont;
      FProgressJustify:  TVerJustify;
      FShowMaxValue:     TTextFormat;
      FShowMinValue:     TTextFormat;
      FShowProgress:     TTextFormat;
      FRepeatTimer:      TTimer;
      FTimeState:        TTimeState;
      FValuesList:       TStrings;
      FEndForeBar:       integer;
      FTypeLimits:       boolean;
      MouseX:            integer;
      MovingBar:         boolean;
      RightLimitFormat:  TTextFormat;
      LeftLimitFormat:   TTextFormat;
      FLeftVBoxOpen:     boolean;
      FRightVBoxOpen:    boolean;
      ValuesBox:         TWinControl;
      BoxClass:          TWinControlClass;
      Acknowlege:        boolean;
      FLimitsChangeable: boolean;
      function GetStartBackBar: integer;
      function GetEndBackBar: integer;
      function GetStartForeBar: integer;
      function GetEndForeBar: integer;
      function GetStartSecondHeader: integer;
      function GetMaxBarWidth: integer;
      function GetBackBarWidth: integer;
      function GetBarWidth: integer;
      function GetPercentDone: Longint;
      function GetProgress(const X: integer): Longint;
      function GetValuesList: TStrings;
      function GetText(const Number: Longint; const TheFormat: TTextFormat): string;
      procedure RunListBox(const X: integer; const Value: Longint);
      procedure UpDateVBox(const Val: Longint; const UpDate: boolean);
      procedure PaintBackground(AnImage: TBitmap);
      procedure PaintAsText(AnImage: TBitmap);
      procedure PaintAsBar(AnImage: TBitmap; PaintRect: TRect);
      procedure PaintHeaders(Image: TBitmap);
      procedure PutProgress(Image: TBitmap);
      procedure PutLimits(Image: TBitmap; const Left,Top,Width,Height: integer; const Number: Longint; TheFormat: TTextFormat);
      procedure PaintBorder(Image: TBitMap);
      procedure SetBorderStyle(Value: TBorderStyle);
      procedure SetForeColor(Value: TColor);
      procedure SetBackColor(Value: TColor);
      procedure SetMinValue(const Value: Longint);
      procedure SetMaxValue(const Value: Longint);
      procedure SetProgress(const Value: Longint);
      procedure SetLimitsHorJustify(Value: THorJustify);
      procedure SetLimitsVerJustify(Value: TVerJustify);
      procedure SetLimitsFont(Value: TFont);
      procedure SetShowMaxValue(Value: TTextFormat);
      procedure SetShowMinValue(Value: TTextFormat);
      procedure SetShowProgress(Value: TTextFormat);
      procedure SetProgressFont(Value: TFont);
      procedure SetProgressJustify(Value: TVerJustify);
      procedure SetLimitsWidth(const Value: integer);
      procedure SetValuesList(Value: TStrings);
      procedure SetKind(Value: TKind);
      procedure SetEndForeBar(const Value: integer);
      procedure SetLastCoord(const Value: integer);
      procedure SetTypeLimits(const Value: boolean);
      procedure SetRightVBoxOpen(const Value: boolean);
      procedure SetLeftVBoxOpen(const Value: boolean);
      procedure KeyPressVBox(Sender: TObject; var Key: Char);
      procedure TimerExpired(Sender: TObject);
      procedure RefreshFont(Sender: TObject);
      procedure ClickVBoxValue(Sender: TObject);
      procedure Exit(Sender: TObject);
      property LeftVBoxOpen: boolean read FLeftVBoxOpen write SetLeftVBoxOpen;
      property RightVBoxOpen: boolean read FRightVBoxOpen write SetRightVBoxOpen;
      property StartBackBar: integer read GetStartBackBar;
      property EndBackBar: integer read GetEndBackBar;
      property StartForeBar: integer read GetStartForeBar;
      property EndForeBar: integer read FEndForeBar write SetEndForeBar stored true; //GetEndForeBar;
      property StartSecondHeader: integer read GetStartSecondHeader;
      property MaxBarWidth: integer read GetMaxBarWidth;  {Relative to StartForeBar}
      property BackBarWidth: integer read GetBackBarWidth;
      property BarWidth: integer read GetBarWidth;
      property LastCoord: integer read FLastCoord write SetLastCoord stored true;
      property AvailableArea: word read FAvailableArea write FAvailableArea;
    protected
      procedure Paint;                                                                     override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);        override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer);                              override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);          override;
      procedure DblClick;                                                                  override;
      procedure Click;                                                                     override;
      procedure Loaded;                                                                    override;
      procedure AddProgress(const Value: Longint);
      property PercentDone: Longint read GetPercentDone;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      property TimeState: TTimeState read FTimeState write FTimeState;
    published
      property Align;
      property BackColor: TColor read FBackColor write SetBackColor default clWhite;
      property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
      property Color;
      property Enabled;
      property ForeColor: TColor read FForeColor write SetForeColor default clBlack;
      property Increment: integer read FIncrement write FIncrement;
      property MinValue: Longint read FMinValue write SetMinValue default 0;
      property MaxValue: Longint read FMaxValue write SetMaxValue default 100;
      property LimitsFont: TFont read FLimitsFont write SetLimitsFont;
      property LimitsChangeable: boolean read FLimitsChangeable write FLimitsChangeable;
      property LimitsHorJustify: THorJustify read FLimitsHorJustify write SetLimitsHorJustify;
      property LimitsVerJustify: TVerJustify read FLimitsVerJustify write SetLimitsVerJustify;
      property LimitsWidth: integer read FLimitsWidth write SetLimitsWidth;
      property Kind: TKind read FKind write SetKind;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property Progress: Longint read FCurValue write SetProgress;
      property ProgressFont: TFont read FProgressFont write SetProgressFont;
      property ProgressJustify: TVerJustify read FProgressJustify write SetProgressJustify;
      property ShowHint;
      property ShowMaxValue: TTextFormat read FShowMaxValue write SetShowMaxValue;
      property ShowMinValue: TTextFormat read FShowMinValue write SetShowMinValue;
      property ShowProgress: TTextFormat read FShowProgress write SetShowProgress;
      property TypeLimits: boolean read FTypeLimits write SetTypeLimits;
      property ValuesList: TStrings read GetValuesList write SetValuesList;
      property Visible;
    end;

  procedure Register;

implementation

  procedure TBltBitmap.MakeLike(ATemplate: TBitmap);
    begin
      Width := ATemplate.Width;
      Height := ATemplate.Height;
      Canvas.Brush.Color := clWindowFrame;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    end;

  function SolveForX(Y, Z: Longint): Integer;
    begin
      SolveForX := Trunc( Z * (Y * 0.01) );
    end;

  function SolveForY(X, Z: Longint): Integer;
    begin
      if Z = 0
        then Result := 0
        else Result := Trunc( (X * 100) / Z );
    end;

  function TPercentBar.GetProgress(const X: integer): Longint;
    begin
      Result := MinValue + Trunc(((FMaxValue - FMinValue)*(X-StartForeBar))/MaxBarWidth);
    end;

  function TPercentBar.GetPercentDone: Longint;
    begin
      GetPercentDone := SolveForY(FCurValue - FMinValue, FMaxValue - FMinValue);
    end;

  {TPercentBar}

  constructor TPercentBar.Create(AOwner: TComponent);
    begin
      inherited Create(AOwner);
      if FTypeLimits
        then BoxClass := TComboBox
        else BoxClass := TListBox;
      ValuesBox := BoxClass.Create(Self);
      ControlStyle              := ControlStyle + [csFramed, csOpaque];
      { default values }
      FMinValue                 := 0;
      FMaxValue                 := 100;
      FLimitsChangeable         := False;
      FLimitsWidth              := 40;
      FCurValue                 := 0;
      Acknowlege                := false;
      FShowMinValue             := tfNumber;
      FShowMaxValue             := tfNumber;
      RightLimitFormat          := FShowMaxValue;
      LeftLimitFormat           := FShowMinValue;
      FShowProgress             := tfPercent;
      FBorderStyle              := bsSingle;
      FForeColor                := clBtnFace;
      FBackColor                := clWhite;
      FLimitsVerJustify         := vjCenter;
      FLimitsHorJustify         := hjCenter;
      FKind                     := kdCuantitative;
      FProgressJustify          := vjCenter;
      FEndForeBar               := StartForeBar;
      FLastCoord                := StartForeBar;
      MovingBar                 := false;
      Width                     := 200;
      Height                    := 20;
      FIncrement                := 2;
      TimeState                 := [tbAllowTimer];
      AvailableArea             := 80;
      LeftVBoxOpen              := false;
      RightVBoxOpen             := false;
      // Creation and initialization of Fonts
      FLimitsFont               := TFont.Create;
      FLimitsFont.Assign(Font);
      FLimitsFont.OnChange      := RefreshFont;
      FLimitsFont.Size          := 8;
      FProgressFont             := TFont.Create;
      FProgressFont.Assign(Font);
      FProgressFont.OnChange    := RefreshFont;
      FProgressFont.Size        := 8;
      FValuesList               := TStringList.Create;
    end;

  destructor TPercentBar.Destroy;
    begin
      FLimitsFont.Free;
      FProgressFont.Free;
      FValuesList.Free;
      FRepeatTimer.Free;
      inherited Destroy;
    end;

  procedure TPercentBar.Loaded;
    begin
      inherited Loaded;
      if FKind = kdCualitative
        then
          begin
            FMinValue := 0;
            FMaxValue := FValuesList.Count - 1;
          end;
      FLastCoord := FEndForeBar;
    end;

  procedure TPercentBar.Paint;
    var
      TheImage: TBitmap;
      OverlayImage: TBltBitmap;
      PaintRect: TRect;
    begin
      with Canvas do
        begin
          TheImage := TBitmap.Create;
          try
            TheImage.Height := Height;
            TheImage.Width := Width;
            PaintBackground(TheImage);
            PaintRect := Rect(StartBackBar,0,EndBackBar+1, ClientHeight);
            if FBorderStyle = bsSingle
              then InflateRect(PaintRect, 0, -1);
            OverlayImage := TBltBitmap.Create;
            try
              OverlayImage.MakeLike(TheImage);
              PaintBackground(OverlayImage);
              PaintAsBar(OverlayImage, PaintRect);
              TheImage.Canvas.CopyMode := cmSrcInvert;
              TheImage.Canvas.Draw(0, 0, OverlayImage);
              TheImage.Canvas.CopyMode := cmSrcCopy;
              PaintHeaders(TheImage);
              if FBorderStyle = bsSingle
                then PaintBorder(TheImage);
              PaintAsText(TheImage);
            finally
              OverlayImage.Free;
            end;
            Canvas.CopyMode := cmSrcCopy;
            Canvas.Draw(0, 0, TheImage);
          finally
            TheImage.Destroy;
          end;
        end;
    end;

  procedure TPercentBar.PaintAsBar(AnImage: TBitmap; PaintRect: TRect);
    var
      H: Integer;
    begin
      with PaintRect do
        H := PaintRect.Bottom - PaintRect.Top + 1;
      with AnImage.Canvas do
        begin
          Brush.Color := BackColor;
          FillRect(PaintRect);
          Pen.Color := ForeColor;
          Pen.Width := 1;
          Brush.Color := ForeColor;
          FillRect(Rect(StartForeBar, 0, EndForeBar, H));
          with PaintRect do
            begin
              Pen.Color := clBtnShadow;  // Bevel Outer
              MoveTo(Right-1,Top);
              LineTo(Left,Top);
              LineTo(Left,Bottom-1);
              Pen.Color := clBtnHighlight;
              MoveTo(Left,Bottom-1);
              LineTo(Right-1,Bottom-1);
              LineTo(Right-1,Top);
              if EndForeBar > StartForeBar
                then
                  begin
                    Pen.Color := clBtnShadow;  // Bevel Inner
                    MoveTo(Left+1,Bottom-2);
                    LineTo(EndForeBar, Bottom-2);
                    LineTo(EndForeBar, Top+1);
                    Pen.Color := clBtnHighlight;
                    MoveTo(EndForeBar,Top+1);
                    LineTo(Left+1,Top+1);
                    LineTo(Left+1,Bottom-2);
                  end;
            end;
        end;
    end;

  procedure TPercentBar.PaintBackground(AnImage: TBitmap);
    var
      ARect: TRect;
    begin
      with AnImage.Canvas do
        begin
          CopyMode := cmBlackness;
          ARect := Rect(0, 0, Width, Height);
          CopyRect(ARect, Animage.Canvas, ARect);
          CopyMode := cmSrcCopy;
        end;
    end;

  procedure TPercentBar.PaintAsText(AnImage: TBitmap);
    begin
       if ShowMinValue <> tfNone
         then PutLimits(AnImage,0,0,LimitsWidth,ClientHeight,MinValue, ShowMinValue);
       if ShowMaxValue <> tfNone
         then PutLimits(AnImage,StartSecondHeader,0,LimitsWidth,ClientHeight,MaxValue, ShowMaxValue);
       if ShowProgress <> tfNone
         then PutProgress(AnImage);
    end;

  procedure TPercentBar.PutProgress(Image: TBitmap);
    var
      X,Y: integer;
      S: string;
    const
      margin = 4;

    begin
      S := GetText(Progress,ShowProgress);
      with Image.Canvas do
        begin
          Font := ProgressFont;
          case ProgressJustify of
            vjTop:    Y := 3;
            vjCenter: Y := (Height - TextHeight(S)) div 2;
          else // vjBottom
            Y := Height - TextHeight(S)-3;
          end;
          {
          if TextWidth(S) + margin < BarWidth
            then
              begin
                Brush.Color := ForeColor;
                X := EndForeBar - TextWidth(S) - margin;
              end
            else
              begin
                Brush.Color := BackColor;
                X := EndForeBar + margin;
              end;
          }
          if TextWidth(S) + margin < MaxBarWidth - BarWidth
            then
              begin
                Brush.Color := BackColor;
                X := EndForeBar + margin;
              end
            else
              begin
                Brush.Color := ForeColor;
                X := EndForeBar - TextWidth(S) - margin;
              end;
          TextOut(X,Y,S);
        end;
    end;

  procedure TPercentBar.PutLimits(Image: TBitmap; const Left,Top,Width,Height: integer; const Number: Longint; TheFormat: TTextFormat);
    var
      S: string;
      X,Y: integer;
      dY,dX: integer;
    begin
      with Image.Canvas do
        begin
          Font := LimitsFont;
          S := GetText(Number,TheFormat);
          case LimitsVerJustify of
            vjTop:    dY := 1;
            vjCenter: dY := (Height - TextHeight(S)) div 2;
          else  // vjBottom:
            dY := Height - TextHeight(S)-1;
          end;
          if LimitsHorJustify = hjCenter
            then dX := (Width - TextWidth(S)) div 2
            else
              if Left = 0
                then dX := LimitsWidth -  TextWidth(S + 'A')
                else dX := TextWidth('A');
          X := Left + dX;
          Y := Top + dY;
          TextOut(X,Y,S);
        end;
    end;

  procedure TPercentBar.PaintHeaders(Image: TBitmap);
    begin
      with Image.Canvas do
        begin
          Brush.Color := Color;
          Pen.Width := 1;
          FillRect(Rect(0, 0, LimitsWidth, ClientHeight));
          FillRect(Rect(StartSecondHeader, 0, ClientWidth, ClientHeight));
        end;
    end;

  procedure TPercentBar.PaintBorder(Image: TBitMap);
    begin
      with Image.Canvas do
        begin
          Pen.Color := clBtnHighlight;
          MoveTo(0,ClientHeight-1);
          LineTo(0,0);
          LineTo(ClientWidth-1,0);
          Pen.Color := clBtnShadow;
          LineTo(ClientWidth-1,ClientHeight-1);
          LineTo(0,ClientHeight-1);
        end;
    end;

  procedure TPercentBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if ((X > StartBackBar) and LeftVBoxOpen) or ((X < EndBackBar) and RightVBoxOpen)
        then
          begin
            RightVBoxOpen := false;
            LeftVBoxOpen := false;
            Acknowlege := true;
          end
        else
          if Button = mbLeft
            then
              if (X > EndForeBar - SensitiveRadius) and (X <= EndForeBar + SensitiveRadius)
                then MovingBar:= True
                else
                  if tbAllowTimer in FTimeState
                    then
                      begin
                        if FRepeatTimer = nil
                          then FRepeatTimer := TTimer.Create(Self);
                        FRepeatTimer.OnTimer := TimerExpired;
                        FRepeatTimer.Interval := InitRepeatPause;
                        FRepeatTimer.Enabled  := True;
                      end;
      inherited;
    end;

  procedure TPercentBar.MouseMove(Shift: TShiftState; X, Y: Integer);
    begin
      MouseX := X;
      if MovingBar
        then
          if (X > - AvailableArea) and (X < Width + AvailableArea) and
             (Y > - AvailableArea) and (Y < Height + AvailableArea)
               then
                 begin
                   EndForeBar := X;
                   Progress := GetProgress(X);
                 end
               else
                 begin
                   EndForeBar := LastCoord;
                   Progress := GetProgress(EndForeBar);
                 end
        else
          begin
            if (X > EndForeBar - Sensitiveradius) and (X <= EndForeBar + SensitiveRadius)
                and not (LeftVBoxOpen or RightVBoxOpen)
              then Cursor := crSizeWE
              else Cursor := crDefault;
          end;
      inherited;
    end;

  procedure TPercentBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if (X > -AvailableArea) and (X < Width + AvailableArea) and
         (Y > 0) and (Y < Height)
        then LastCoord := EndForeBar
        else
          begin
            EndForeBar := LastCoord;
            Progress := GetProgress(EndForeBar);
          end;
      if MovingBar
        then
          begin
            MovingBar := false;
            Cursor := crDefault;
          end;
      if FRepeatTimer <> nil
        then FRepeatTimer.Enabled  := False;
      inherited;
    end;

  procedure TPercentBar.SetBorderStyle(Value: TBorderStyle);
    begin
      if Value <> FBorderStyle
        then
          begin
            FBorderStyle := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetForeColor(Value: TColor);
    begin
      if Value <> FForeColor
        then
          begin
            FForeColor := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetBackColor(Value: TColor);
    begin
      if Value <> FBackColor
        then
          begin
            FBackColor := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetMinValue(const Value: Longint);
    begin
      if (Value <> FMinValue) and (Value < FMaxValue)
        then
          begin
            if (Kind = kdCualitative) and (Value < 0)
              then FMinValue := 0
              else FMinValue := Value;
            if FCurValue < FMinValue
              then FCurValue := FMinValue;
            FEndForeBar := GetEndForeBar;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetMaxValue(const Value: Longint);
    begin
      if (Value <> FMaxValue) and (Value > FMinValue)
        then
          begin
            if (Kind = kdCualitative) and (Value >= FValuesList.Count)
              then FMaxValue := pred(FValuesList.Count)
              else FMaxValue := Value;
            if FCurValue > FMaxValue
              then FCurValue := FMaxValue;
            FEndForeBar := GetEndForeBar;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetLastCoord(const Value: integer);
    begin
      if Value <> FLastCoord
        then
          begin
            FLastCoord := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetProgress(const Value: Longint);
    var
      ActValue: Longint;
    begin
      if Value > FMaxValue
        then ActValue := FMaxValue
        else
          if Value < MinValue
            then ActValue := FMinValue
            else ActValue := Value;
      if FCurValue <> ActValue
        then
          begin
            FCurValue := ActValue;
            if (csDesigning in ComponentState) or (csLoading in ComponentState) or not MovingBar
              then EndForeBar := GetEndForebar;
          end;
      Invalidate;
    end;

  procedure TPercentBar.SetLimitsHorJustify(Value: THorJustify);
    begin
      if Value <> FLimitsHorJustify
        then
          begin
            FLimitsHorJustify := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetLimitsVerJustify(Value: TVerJustify);
    begin
      if Value <> FLimitsVerJustify
        then
          begin
            FLimitsVerJustify := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetLimitsFont(Value: TFont);
    begin
      if Value <> FLimitsFont
        then
          begin
            FLimitsFont.Assign(Value);
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetProgressJustify(Value: TVerJustify);
    begin
      if Value <> FProgressJustify
        then
          begin
            FProgressJustify := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetShowMaxValue(Value: TTextFormat);
    begin
      if Value <> FShowMaxValue
        then
          begin
            FShowMaxValue := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetShowMinValue(Value: TTextFormat);
    begin
      if Value <> FShowMinValue
        then
          begin
            FShowMinValue := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetShowProgress(Value: TTextFormat);
    begin
      if Value <> FShowProgress
        then
          begin
            FShowProgress := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetLimitsWidth(const Value: integer);
    begin
      if Value <> FLimitsWidth
        then
          begin
            FLimitsWidth := Value;
            EndForeBar := GetEndForeBar;
            Invalidate;
          end;
    end;

  procedure TPercentBar.SetProgressFont(Value: TFont);
    begin
      if Value <> FProgressFont
        then
          begin
            FProgressFont := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.AddProgress(const Value: Longint);
    begin
      Progress := FCurValue + Value;
      EndForeBar := GetEndForeBar;
      if ((Value > 0) and (EndForeBar > MouseX)) or
         ((Value < 0) and (EndForeBar < MouseX))
        then
          begin
            EndForeBar := MouseX;
            Progress := GetProgress(EndForeBar);
          end;
      Invalidate;
    end;

  function TPercentBar.GetStartBackBar: integer;
    begin
      Result := LimitsWidth;
    end;

  function TPercentBar.GetEndBackBar: integer;
    begin
      Result := Width - LimitsWidth - 1;
    end;

  function TPercentBar.GetStartForeBar: integer;
    begin
      Result := LimitsWidth + 1;
    end;

  function TPercentBar.GetEndForeBar: integer;
    begin
      Result := SolveForX(PercentDone, pred(MaxBarWidth));
      if Result > pred(MaxBarWidth)
        then Result := pred(MaxBarWidth);
      Result := Result + StartForeBar;
    end;

  function TPercentBar.GetStartSecondHeader: integer;
    begin
      Result := Width - LimitsWidth;
    end;

  function TPercentBar.GetMaxBarWidth: integer;
    begin
      Result := BackBarWidth - 2;
    end;

  function TPercentBar.GetBackBarWidth: integer;
    begin
      Result := Width - 2*LimitsWidth;
    end;

  function TPercentBar.GetBarWidth: integer;
    begin
      Result := EndForeBar - StartBackBar;
      if Result < 0
        then Result := 0; 
    end;

  procedure TPercentBar.RefreshFont(Sender: TObject);
    begin
      Invalidate;
    end;

  function TPercentBar.GetText(const Number: Longint; const TheFormat: TTextFormat): string;
    begin
      if Kind = kdCualitative
        then Result := FValuesList.Strings[Number]
        else
          if TheFormat = tfNumber
            then Result := Format(' %d ', [Number])
            else Result := Format(' %d%% ', [Number]);
    end;

  function TPercentBar.GetValuesList: TStrings;
    begin
      Result := FValuesList;
    end;

  procedure TPercentBar.SetValuesList(Value: TStrings);
    begin
      if Value.Count >= 2
        then FValuesList.Assign(Value);   // Sino Error: No puede haber menos de dos
                                          //      posibles valores extremos
      Invalidate;
    end;

  procedure TPercentBar.SetRightVBoxOpen(const Value: boolean);
    begin
      if Value <> FRightVBoxOpen
        then
          begin
            FRightVBoxOpen := Value;
            if FRightVBoxOpen
              then
                begin
                  LeftVBoxOpen := false;
                  RightLimitFormat := ShowMaxValue;
                  ShowMaxValue := tfNone;
                  RunListBox(StartSecondHeader, FMaxValue);
                end
              else
                begin
                  ShowMaxValue := RightLimitFormat;
                  if ValuesBox is TComboBox
                    then (ValuesBox as TComboBox).Visible := false
                    else (ValuesBox as TListBox).Visible := false;
                end;
          end;
    end;

  procedure TPercentBar.SetLeftVBoxOpen(const Value: boolean);
    begin
      if Value <> FLeftVBoxOpen
        then
          begin
            FLeftVBoxOpen := Value;
            if FLeftVBoxOpen
              then
                begin
                  RightVBoxOpen := false;
                  LeftLimitFormat := ShowMinValue;
                  ShowMinValue := tfNone;
                  RunListBox(1, FMinValue);
                end
              else
                begin
                  ShowMinValue := LeftLimitFormat;
                  if ValuesBox is TComboBox
                    then (ValuesBox as TComboBox).Visible := false
                    else (ValuesBox as TListBox).Visible := false;
                end;
          end;
    end;

  procedure TPercentBar.SetEndForeBar(const Value: integer);
    begin
      if Value < StartForeBar
        then FEndForeBar := StartForeBar
        else
          if Value >= StartForeBar + MaxBarWidth
            then FEndForeBar := StartForeBar + MaxBarWidth - 1
            else FEndForeBar := Value;
    end;

  procedure TPercentBar.SetTypeLimits(const Value: boolean);
    begin
      if Value <> FTypeLimits
        then
          begin
            ValuesBox.Free;
            FTypeLimits := Value;
            if FTypeLimits
              then BoxClass := TComboBox
              else BoxClass := TListBox;
            ValuesBox := BoxClass.Create(Self);
          end;
    end;

  procedure TPercentBar.SetKind(Value: TKind);
    begin
      if Value <> FKind
        then
          begin
            if Value = kdCualitative
              then
                begin
                  MinValue := 0;
                  MaxValue := FValuesList.Count - 1;
                  Increment := 1;
                  TypeLimits := false;
                end
              else
                begin
                  Increment := 2;
                  TypeLimits := true;
                end;
            FKind := Value;
            Invalidate;
          end;
    end;

  procedure TPercentBar.UpDateVBox(const Val: Longint; const UpDate: boolean);
    begin
      if LeftVBoxOpen
        then
          begin
            if UpDate
              then MinValue := val;
            LeftVBoxOpen := false;
          end
        else
          begin
            if UpDate
              then MaxValue := val;
            RightVBoxOpen := false;
          end;
    end;

  procedure TPercentBar.KeyPressVBox(Sender: TObject; var Key: Char);
    var
      value: Longint;
      code: integer;
    begin
      if Key = #13
        then
          begin
            val((ValuesBox as TComboBox).Text, value, code);
            if ((ValuesBox as TComboBox).Text <> '') and (code = 0)
              then UpDateVBox(value, true)
              else UpDateVBox(0,false);
          end;
    end;

  procedure TPercentBar.ClickVBoxValue(Sender: TObject);
    var
      value: Longint;
      code: integer;
    begin
      if Kind = kdCualitative
        then UpDateVBox((ValuesBox as TListBox).ItemIndex, true)
        else
          if ValuesBox is TComboBox
            then
              with ValuesBox as TComboBox do
                begin
                  if ItemIndex <> - 1
                    then
                      begin
                        value := StrToInt(Items[ItemIndex]);
                        UpDateVBox(value, true);
                      end
                    else
                      begin
                        val(Items[ItemIndex], value, code);
                        if code = 0
                          then UpDateVBox(value, true)
                          else UpDateVBox(0, false);
                      end;
                  visible := false;
                end
            else
              with ValuesBox as TListBox do
                begin
                  if ItemIndex <> - 1
                    then
                      begin
                        value := StrToInt(Items[ItemIndex]);
                        UpDateVBox(value, true);
                      end
                    else
                      begin
                        val(Items[ItemIndex], value, code);
                        if code = 0
                          then UpDateVBox(value, true)
                          else UpDateVBox(0, false);
                      end;
                  visible := false;
                end;
    end;

  procedure TPercentBar.RunListBox(const X: integer; const Value: Longint);
    begin
      if ValuesBox.Parent = nil
        then
          // Creation and initialization of ValuesBox
          begin
            if ValuesBox is TComboBox
              then
                with (ValuesBox as TComboBox) do
                  begin
                    Parent     := Self.Parent;
                    Visible    := false;
                    OnClick    := ClickVBoxValue;
                    OnExit     := Exit;
                    OnKeyPress := KeyPressVBox;
                    Font       := LimitsFont;
                  end
              else
                with (ValuesBox as TListBox) do
                  begin
                    Parent     := Self.Parent;
                    Visible    := false;
                    Font       := LimitsFont;
                    OnClick    := ClickVBoxValue;
                    OnExit     := Exit;
                  end;
            with ValuesBox do
              begin
                Width      := LimitsWidth;
                Height     := 50;
              end;
          end;

      if ValuesBox is TComboBox
        then
          with ValuesBox as TComboBox do
            begin
              Text := IntToStr(Value);
              Items.Assign(ValuesList);
            end
        else (ValuesBox as TListBox).Items.Assign(ValuesList);

      with ValuesBox do
        begin
          Left      := Self.Left + X + LimitsWidth div 4;
          Top       := Self.Top + Self.Height div 3;
        end;

      if ValuesBox is TComboBox
        then (ValuesBox as TComboBox).Visible := true
        else (ValuesBox as TListBox).Visible := true;
      ValuesBox.SetFocus;
    end;

  procedure TPercentBar.Exit(Sender: TObject);
    begin
      if RightVBoxOpen
        then RightVBoxOpen := false;
      if LeftVBoxOpen
        then LeftVBoxOpen := false;
    end;

  procedure TPercentBar.DblClick;
    begin
      if LimitsChangeable
        then
          if (MouseX >= StartSecondHeader) and not RightVBoxOpen //and not MovingBar
            then RightVBoxOpen := true
            else
              if (MouseX < StartBackBar) and not LeftVBoxOpen //and not MovingBar
                then LeftVBoxOpen := true;
      inherited;
    end;

  procedure TPercentBar.Click;
    begin
      if Acknowlege
        then Acknowlege := false
        else
          if EndForeBar + 1 < MouseX
            then AddProgress(Increment)
            else
              if EndForeBar - 1 > MouseX
                then AddProgress(- Increment);
      inherited;
    end;

  procedure TPercentBar.TimerExpired(Sender: TObject);
    begin
      FRepeatTimer.Interval := RepeatPause;
      if MouseCapture
        then
          begin
            try
              Click;
            except
              FRepeatTimer.Enabled := False;
              raise;
            end;
          end;
    end;

  procedure Register;
    begin
      RegisterComponents('Merchise', [TPercentBar]);
    end;

end.
