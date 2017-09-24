unit ImageForm;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Menus, ExtDlgs, SpeedBmp, SpriteImages, ColorTableMgr, ExtCtrls, GameTypes;

  type
    TImageTestWindow =
      class(TForm)
          MainMenu: TMainMenu;
          File1: TMenuItem;
          Effects1: TMenuItem;
          Load1: TMenuItem;
          TintImage1: TMenuItem;
          OpenDialog: TOpenDialog;
          Geometric1: TMenuItem;
          TintImage2: TMenuItem;
          Flip1: TMenuItem;
          Mirror1: TMenuItem;
          Both1: TMenuItem;
          AnimationTimer: TTimer;
          ConverttoGray1: TMenuItem;
          rag: TMenuItem;
          TestBuffer1: TMenuItem;
          procedure Load1Click(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure FormPaint(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
          procedure FormResize(Sender: TObject);
          procedure TintImage1Click(Sender: TObject);
          procedure Flip1Click(Sender: TObject);
          procedure Mirror1Click(Sender: TObject);
          procedure Both1Click(Sender: TObject);
          procedure AnimationTimerTimer(Sender: TObject);
          procedure ConverttoGray1Click(Sender: TObject);
          procedure TestBuffer1Click(Sender: TObject);
          procedure DrawTransparent(const from, dc: HDC; const x, y: integer; const color: TColor);
        private
          { Private declarations }
          fBuffer          : TSpeedBitmap;
          fBuffer1         : TSpeedBitmap;
          fImage           : TGameImage;
          fPalette         : TPaletteInfo;
          fFrame           : integer;
          fLastFrameUpdate : integer;
        public
          { Public declarations }
      end;

  var
    ImageTestWindow: TImageTestWindow;

implementation

  {$R *.DFM}

  uses
    GDI, SpriteUtils, ImageLoader, BufferDraw;

procedure TImageTestWindow.DrawTransparent(const from, dc: HDC; const x, y: integer; const color: TColor);
  var
    crOldBack, crOldText : TColor;
    dcTrans : HDC;
    pOldBitmapImage, bitmapTrans, pOldBitmapTrans : HBitmap;
  begin
    //crOldBack := SetBkColor(dc, clWhite);
    //crOldText := SetTextColor(dc, clBlack);
    // Create two memory dcs for the image and the mask
    dcTrans := windows.CreateCompatibleDC(dc);

    // Create the mask bitmap
    bitmapTrans := windows.CreateBitmap(Width, Height, 1, 1, nil);

    // Select the mask bitmap into the appropriate dc
    pOldBitmapTrans := windows.SelectObject(dcTrans, bitmapTrans);

    // Build mask based on transparent colour
     windows.SetBkColor(from, color);
     windows.SetBkColor(dcTrans, clBlack);
     windows.BitBlt(dcTrans, 0, 0, Width div 2, Height div 2, from, 0, 0, SRCCOPY);

     //windows.BitBlt(canvas.handle, 0, 0, Width div 2, Height div 2, dcTrans, 0, 0, SRCCOPY);

    // Do the work - True Mask method - cool if not actual display
     windows.BitBlt(dc, x, y, Width, Height, from, 0, 0, SRCINVERT);
     windows.BitBlt(dc, x, y, Width, Height, dcTrans, 0, 0, SRCAND);
     windows.BitBlt(dc, x, y, Width, Height, from, 0, 0, SRCINVERT);

    // Restore settings
     windows.SelectObject(dcTrans, pOldBitmapTrans);
     windows.SetBkColor(dc, crOldBack);
     windows.SetTextColor(dc, crOldText);
     windows.DeleteDc(dcTrans);
     windows.DeleteObject(bitmapTrans);
  end;

  procedure TImageTestWindow.Load1Click(Sender: TObject);
    begin
      if OpenDialog.Execute
        then
          begin
            fImage.Free;
            fImage := nil;
            fPalette.Free;
            fPalette := nil;
            fImage := LoadGameImage(OpenDialog.FileName);
            if fImage <> nil
              then
                begin
                  AnimationTimer.Enabled := fImage.FrameCount > 1;
                  fFrame := 0;
                  fLastFrameUpdate := GetTickCount;
                  Refresh;
                end
              else Application.MessageBox('Can''t open image file', 'Image Test', MB_OK);
          end;
    end;

  procedure TImageTestWindow.FormCreate(Sender: TObject);
    begin
      fBuffer := TSpeedBitmap.CreateSized(ClientWidth, ClientHeight, 16);
    end;

  procedure TImageTestWindow.FormPaint(Sender: TObject);
    var
      bmp   : HBITMAP;
      MemDC : HDC;
    begin
      if fImage <> nil
        then
          begin
            fBuffer.Canvas.Brush.Color := clBlack;
            fBuffer.Canvas.FillRect(ClientRect);
            try
              fImage.Draw(0, 0, 0, fFrame, Rect(0, 0, fImage.Width, fImage.Height), fBuffer, fPalette);
            except
            end;
            //fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
          end;

      fBuffer.Canvas.Brush.Color := clWhite;
      fBuffer.Canvas.FillRect(ClientRect);
      if fBuffer1<>nil
        then
          begin
          {
              BufferCopyGDI(fBuffer1, fBuffer, 0, 0, nil);
              fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
              }
              {
              fBuffer1.DrawOnDC(fBuffer.Canvas.Handle, 2, 3, 0);
              fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
              }
               //fBuffer1.ClipDrawOnDC(fBuffer.Canvas.Handle, 2, 3, fBuffer1.ClientRect, 0);
{
            bmp := fBuffer1.CreateMask(false);
            MemDC := CreateCompatibleDC( Canvas.Handle );
            SelectObject( MemDC, bmp );

            Windows.BitBlt(Canvas.Handle, 0, 0, fBuffer1.ClientRect.Right, fBuffer1.ClientRect.Bottom,  MemDC, 0, 0, Canvas.CopyMode);
            DeleteDc(MemDC);
            DeleteObject(bmp);
 }
            //Windows.BitBlt(Canvas.Handle, 0, 0, fBuffer1.ClientRect.Right, fBuffer1.ClientRect.Bottom,  fBuffer1.Canvas.Handle, 0, 0, Canvas.CopyMode);

            //fBuffer1.Transparent := false;
            //
            fBuffer1.DrawTransparent(fBuffer.canvas.Handle, 4, 20, RECT(2, 4, 59, 59), clred);
            //DrawTransparent(fBuffer1.Canvas.Handle, fBuffer.Canvas.Handle, 0, 0, clRed);
            fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
          end
        else fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
    end;

  procedure TImageTestWindow.FormDestroy(Sender: TObject);
    begin
      fBuffer.Free;
      fBuffer := nil;
    end;

  procedure TImageTestWindow.FormResize(Sender: TObject);
    begin
      if fBuffer <> nil
        then fBuffer.NewSize(ClientWidth, ClientHeight, 16);
    end;

  function ReddenPalette(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      ReddedPalette : PRGBPalette;
      i             : integer;
    begin
      getmem(ReddedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(ReddedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          ReddedPalette[i].rgbRed := $FF;
          ReddedPalette[i].rgbGreen := Palette.RGBPalette[i].rgbGreen;
          ReddedPalette[i].rgbBlue := Palette.RGBPalette[i].rgbBlue;
        end;
      Result := ReddedPalette;
    end;

  function TintPalette1(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      TintedPalette    : PRGBPalette;
      i                : integer;
      red, green, blue : byte;
      tintweight       : extended;
    begin
      red := Data[0].VInteger;
      green := Data[1].VInteger;
      blue := Data[2].VInteger;
      tintweight := Data[3].VExtended^;
      getmem(TintedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(TintedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          TintedPalette[i].rgbRed := round(Palette.RGBPalette[i].rgbRed*(1 - tintweight) + red*tintweight);
          TintedPalette[i].rgbGreen := round(Palette.RGBPalette[i].rgbGreen*(1 - tintweight) + green*tintweight);
          TintedPalette[i].rgbBlue := round(Palette.RGBPalette[i].rgbBlue*(1 - tintweight) + blue*tintweight);
        end;
      Result := TintedPalette;
    end;

  function TintPalette2(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      TintedPalette    : PRGBPalette;
      i                : integer;
      red, green, blue : byte;
      tintweight       : extended;
    begin
      red := Data[0].VInteger;
      green := Data[1].VInteger;
      blue := Data[2].VInteger;
      tintweight := Data[3].VExtended^;
      getmem(TintedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(TintedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          if red > TintedPalette[i].rgbRed
            then TintedPalette[i].rgbRed := red
            else TintedPalette[i].rgbRed := Palette.RGBPalette[i].rgbRed;
          if green > TintedPalette[i].rgbGreen
            then TintedPalette[i].rgbGreen := green
            else TintedPalette[i].rgbGreen := Palette.RGBPalette[i].rgbGreen;
          if blue > TintedPalette[i].rgbBlue
            then TintedPalette[i].rgbBlue := blue
            else TintedPalette[i].rgbBlue := Palette.RGBPalette[i].rgbBlue;
        end;
      Result := TintedPalette;
    end;

  procedure TImageTestWindow.TintImage1Click(Sender: TObject);

    procedure ColorToRGB(color : TColor; out red, green, blue : byte);
      begin
        red := color and $FF;
        green := color shr 8 and $FF;
        blue := color shr 16 and $FF;
      end;

    var
      red, green, blue : byte;
    begin
      if fImage <> nil
        then
          begin
            ColorToRGB(clYellow, red, green, blue);
            fPalette.Free;
            //fPalette := fImage.Tables.MainTable.FilteredPalette(TintPalette1, [red, green, blue, 0.5]);
            fPalette := fImage.PaletteInfo.FilteredPalette(TintPalette1, [red, green, blue, 0.5]);
            Refresh;
          end;
    end;

  procedure TImageTestWindow.Flip1Click(Sender: TObject);
    var
      tmp : TFrameImage;
    begin
      if fImage <> nil
        then
          begin
            tmp := SpriteFlipStretch(fImage, fImage.Width, fImage.Height, ftVertical);
            //tmp.PaletteInfo := fImage.PaletteInfo;
            //fImage.Free;
            fImage := tmp;
            Refresh;
          end;
    end;

  procedure TImageTestWindow.Mirror1Click(Sender: TObject);
    var
      tmp : TFrameImage;
    begin
      if fImage <> nil
        then
          begin
            tmp := SpriteFlipStretch(fImage, fImage.Width, fImage.Height, ftHorizontal);
            //tmp.PaletteInfo := fImage.PaletteInfo;
            //fImage.Free;
            fImage := tmp;
            Refresh;
          end;
    end;

  procedure TImageTestWindow.Both1Click(Sender: TObject);
    var
      tmp : TFrameImage;
    begin
      if fImage <> nil
        then
          begin
            tmp := SpriteFlipStretch(fImage, fImage.Width, fImage.Height, ftBoth);
            //tmp.PaletteInfo := fImage.PaletteInfo;
            //fImage.Free;
            fImage := tmp;
            Refresh;
          end;
    end;

  procedure TImageTestWindow.AnimationTimerTimer(Sender: TObject);
    var
      CurTicks : integer;
      R        : TRect;
    begin
      CurTicks := GetTickCount;
      if CurTicks - fLastFrameUpdate > fImage.FrameDelay[fFrame]
        then
          begin
            if fFrame = pred(fImage.FrameCount)
              then fFrame := 0
              else inc(fFrame);
            fLastFrameUpdate := CurTicks;
            R := Rect(0, 0, fImage.Width, fImage.Height);
            InvalidateRect(Handle, @R, false);
          end;
    end;

  function ColorToGray(Palette : TPaletteInfo; Data : array of const) : PRGBPalette;
    var
      GrayPalette : PRGBPalette;
      i           : integer;
      gray        : integer;
    begin
      getmem(GrayPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(GrayPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          gray := (30*Palette.RGBPalette[i].rgbRed + 59*Palette.RGBPalette[i].rgbGreen + 11*Palette.RGBPalette[i].rgbBlue) div 100;
          GrayPalette[i].rgbRed := gray;
          GrayPalette[i].rgbGreen := gray;
          GrayPalette[i].rgbBlue := gray;
        end;
      Result := GrayPalette;
    end;

  procedure TImageTestWindow.ConverttoGray1Click(Sender: TObject);
    begin
      if fImage <> nil
        then
          begin
            fPalette.Free;
            //fPalette := fImage.Tables.MainTable.FilteredPalette(TintPalette1, [red, green, blue, 0.5]);
            fPalette := fImage.PaletteInfo.FilteredPalette(ColorToGray, [0]);
            Refresh;
          end;
    end;

procedure TImageTestWindow.TestBuffer1Click(Sender: TObject);
  begin
{    if OpenDialog.Execute
      then
        begin}
          if fBuffer1=nil
            then fBuffer1 := TSpeedBitmap.CreateSized(ClientWidth div 2, ClientHeight div 2, 16);
         // fBuffer1.LoadFromFile(OpenDialog.FileName);
          //fBuffer1.TransparentMode := tmFixed;
          fBuffer1.Canvas.Brush.Color := clRed;
          fBuffer1.Canvas.FillRect(fBuffer1.Canvas.ClipRect);
          fBuffer1.Canvas.Font.Color := clBlue;
          fBuffer1.Canvas.TextOut(1, 2, 'Testerg fhdhgs');
          fBuffer1.Canvas.Brush.Color := clYellow;
          fBuffer1.Canvas.Ellipse(10, 10, 50, 50);
          Refresh;
//        end;
  end;

end.
