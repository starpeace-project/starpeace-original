unit MarqueeCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  const
    cidxBack  = 0;
    cidxFore  = 1;
    cidxGrill = 2;
    cidxLight = 3;

  type
    TFadeDirection = (fdUp, fdDown);

  type
    TMarquee =
      class(TCustomControl)
        public
          constructor Create(AOwner: TComponent); override;
          destructor  Destroy; override;
        private
          fBitmap       : TBitmap;
          fMarqueeText  : string;
          fCacheText    : string; //TStringList;
          fMarqueeWidth : integer;
          fTextHeight   : integer;
          fOffset       : integer;
          fGap          : integer;
          fStep         : integer;
          fBackColor    : TColor;
          fForeColor    : TColor;
          fShowGrill    : boolean;
          fClickStop    : boolean;
          fStoped       : boolean;
          fLeftMargin   : integer;
          fTopBorder    : integer;
          fRaiseText    : boolean;
          fDirection    : TFadeDirection;
          fDigitalize   : boolean;
          fLastX        : integer;
          fLastY        : integer;
          fIsCaption    : boolean;
        private
          function  MatchStrings(text1, text2 : string; maxDeltaPerc : byte) : boolean;
          procedure SetBmpPalette;
          procedure DigitalizeBmp;
          procedure AcceptText(text : string);
        protected
          procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
          procedure Paint; override;
          procedure Loaded; override;
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
          procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
          procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
          procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
          procedure WMHitTest(var Message : TMessage); message WM_NCHITTEST;
        public
          procedure Tick;
        private
          procedure SetMarqueeText(text : string);
          procedure SetBackColor(Color : TColor);
          procedure SetForeColor(Color : TColor);
        published
          property  Caption     : string  read fMarqueeText write SetMarqueeText;
          property  Gap         : integer read fGap write fGap;
          property  Step        : integer read fStep write fStep;
          property  ShowGrill   : boolean read fShowGrill write fShowGrill;
          property  BackColor   : TColor  read fBackColor write SetBackColor;
          property  ForeColor   : TColor  read fForeColor write SetForeColor;
          property  ClickStop   : boolean read fClickStop write fClickStop;
          property  LeftMargin  : integer read fLeftMargin write fLeftMargin;
          property  Direction   : TFadeDirection read fDirection write fDirection;
          property  Digitalize  : boolean read fDigitalize write fDigitalize;
          property  IsCaption   : boolean read fIsCaption  write fIsCaption;
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
    MathUtils, CompStringsParser;

  // TMarquee

  constructor TMarquee.Create(AOwner: TComponent);
    begin
      inherited;
      //fCacheText := TStringList.Create;
      fBitmap    := TBitmap.Create;
      fBitmap.PixelFormat := pf8bit;
      Width      := 200;
      Height     := 50;
      fGap       := 50;
      fStep      := 2;
      fBackColor := clBlack;
      fBackColor := clWhite;
      fShowGrill := true;
    end;

  destructor TMarquee.Destroy;
    begin
      //fCacheText.Free;
      fBitmap.Free;
      inherited;
    end;

  function TMarquee.MatchStrings(text1, text2 : string; maxDeltaPerc : byte) : boolean;
    var
      maxLen : integer;
      minLen : integer;
      mrqLen : integer;
      chLen  : integer;
      aux    : string;
      p      : integer;
      wCount : integer;
      fCount : integer;
    begin
      mrqLen := length(text1);
      chLen  := length(text2);
      maxLen := max(mrqLen, chLen);
      minLen := min(mrqLen, chLen);
      if maxLen - minLen < maxLen*maxDeltaPerc/100
        then
          begin
            p := 1;
            wCount := 0;
            fCount := 0;
            repeat
              aux := CompStringsParser.GetNextStringUpTo(text1, p, ' ');
              inc(wCount);
              if pos(aux, text1) <> 0
                then inc(fCount);
            until not CompStringsParser.SkipChar(text1, p, ' ');
            result := fCount/wCount > 0.6;
          end
        else result := false;
    end;

  procedure TMarquee.SetBmpPalette;
    type
      TRGB =
        packed record
          Blue  : byte;
          Green : byte;
          Red   : byte;
          Unk   : byte;
        end;
    var
      pal  : PLogPalette;
      rgb1 : TRGB;
      rgb2 : TRGB;
      hpal : HPALETTE;
      i    : integer;
    begin
      GetMem(pal, sizeof(TLogPalette) + 3*sizeof(TPaletteEntry));
      pal.palVersion := $300;
      pal.palNumEntries := 3;

      // I hate this...
      i := cidxBack;
      pal.palPalEntry[i] := TPaletteEntry(fBackColor);
      i := cidxFore;
      pal.palPalEntry[i] := TPaletteEntry(fForeColor);

      rgb1   := TRGB(fBackColor);
      rgb2   := TRGB(fForeColor);

      rgb1.Blue := (rgb1.Blue + 2*rgb2.Blue) div 3;
      rgb1.Green := (rgb1.Green + 2*rgb2.Green) div 3;
      rgb1.Red := (rgb1.Red + 2*rgb2.Red) div 3;

      i := cidxGrill;
      pal.palPalEntry[i] := TPaletteEntry(rgb1);

      hpal := CreatePalette(pal^);
      fBitmap.Palette := hpal;
      freemem(pal);
    end;

  procedure TMarquee.DigitalizeBmp;
    var
      i, j : integer;
      line : PByteArray;
    begin
      for i := 0 to pred(fBitmap.Height) do
        begin
          line := fBitmap.ScanLine[i];
          if i mod 2 = 0
            then
              for j := 0 to pred(fBitmap.Width) do
                if (j mod 2 = 0) and (fShowGrill or (line[j] = cidxFore))
                  then line[j] := cidxGrill;
            {
            else
              for j := 0 to pred(fBitmap.Width) do
                if (j mod 2 <> 0) and (fShowGrill or (line[j] = cidxFore))
                  then line[j] := cidxGrill;
            }
        end;
    end;

  procedure TMarquee.AcceptText(text : string);
    begin
      fMarqueeText  := text;
      fMarqueeWidth := fBitmap.Canvas.TextWidth(fMarqueeText);
      fTextHeight   := fBitmap.Canvas.TextHeight(fMarqueeText);
      Hint          := text;
      fRaiseText    := false;
    end;

  procedure TMarquee.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    begin
      inherited;
      fBitmap.Width  := aWidth;
      fBitmap.Height := aHeight;
    end;

  procedure TMarquee.Paint;
    var
      R     : TRect;
      offs  : integer;
      y     : integer;
      flag  : boolean;
    begin
      if fRaiseText
        then y := fTopBorder
        else y := (Height - fTextHeight) div 2;
      R := ClientRect;
      fBitmap.Canvas.Brush.Color := fBackColor;
      fBitmap.Canvas.Brush.Style := bsSolid;
      fBitmap.Canvas.FillRect(R);
      fBitmap.Canvas.Font.Color := fForeColor;

      flag := (Width > fMarqueeWidth) or fRaiseText;
      offs := fOffset;

      repeat
        fBitmap.Canvas.TextOut(offs, y, fMarqueeText);
        inc(offs, fMarqueeWidth + fGap);
      until flag or (offs > Width);
      if fDigitalize
        then DigitalizeBmp;
      Canvas.CopyRect(R, fBitmap.Canvas, R);
    end;

  procedure TMarquee.Loaded;
    begin
      //AcceptText(fMarqueeText);
      fOffset := Width;
    end;

  procedure TMarquee.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TMarquee.CMFontChanged(var Message: TMessage);
    begin
      fBitmap.Canvas.Font := Font;
      fMarqueeWidth := Canvas.TextWidth(fMarqueeText);
      fTextHeight := Canvas.TextHeight(fMarqueeText);
    end;

  procedure TMarquee.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fLastX  := X;
      fLastY  := Y;
      fStoped := not fStoped;
    end;

  procedure TMarquee.MouseMove(Shift: TShiftState; X, Y: Integer);
    var
      deltaX : integer;
    begin
      if ssLeft in Shift
        then
          begin
            deltaX := X - fLastX;
            fLastX := X;
            if abs(deltaX) > 1
              then
                begin
                  if fMarqueeWidth > Width - fLeftMargin
                    then fOffset := min(Width, fOffset + deltaX)
                    else fOffset := max(fLeftMargin, fOffset + deltaX);
                  Refresh;
                  fStoped := true;
                end;
          end;
    end;

  procedure TMarquee.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    end;

  procedure TMarquee.WMHitTest(var Message : TMessage);
    begin
      if fIsCaption
        then Message.Result := HTTRANSPARENT
        else inherited;
    end;

  procedure TMarquee.Tick;
    begin
      if not fStoped
        then
          begin
            if fMarqueeWidth > Width - fLeftMargin
              then
                begin
                  if fMarqueeWidth + fOffset < 0
                    then fOffset := fGap;
                  dec(fOffset, fStep);
                end
              else fOffset := max(fLeftMargin, fOffset - fStep);
            if fRaiseText
              then
                begin
                  if fDirection = fdUp
                    then
                      begin
                        dec(fTopBorder, 2);
                        fRaiseText := fTopBorder + fTextHeight > 0;
                      end
                    else
                      begin
                        inc(fTopBorder, 2);
                        fRaiseText := fTopBorder < Height;
                      end;
                  if not fRaiseText
                    then
                      begin
                        AcceptText(fCacheText);
                        fOffset := Width;
                      end;
                end;
            Refresh;
          end;
    end;

  procedure TMarquee.SetMarqueeText(text : string);
    var
      match : boolean;
    begin
      if csDesigning in ComponentState
        then
          begin
            AcceptText(text);
            fOffset := fLeftMargin;
          end
        else
          begin
            if fRaiseText
              then fCacheText := text
              else
                begin
                  match := MatchStrings(fMarqueeText, text, 10);
                  if not fStoped and not match
                    then
                      begin
                        fTopBorder := (Height - fTextHeight) div 2;
                        fCacheText := text;
                        fRaiseText := true;
                      end
                    else
                      begin
                        AcceptText(text);
                        if not match and fStoped and (fOffset < fLeftMargin)
                          then fOffset := fLeftMargin;
                      end;
                end;
          end;
      Refresh;
    end;

  procedure TMarquee.SetBackColor(Color : TColor);
    begin
      fBackColor := Color;
      SetBmpPalette;
      Refresh;
    end;

  procedure TMarquee.SetForeColor(Color : TColor);
    begin
      fForeColor := Color;
      SetBmpPalette;
      Refresh;
    end;

  procedure Register;
    begin
      RegisterComponents('Five', [TMarquee]);
    end;

end.
