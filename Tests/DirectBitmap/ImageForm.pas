unit ImageForm;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Menus, DDrawMgr, DirectStuff, DirectBmp, ExtDlgs, SpriteImages, ExtCtrls;

  type
    TImageTestWindow =
      class(TForm)
          MainMenu: TMainMenu;
          File1: TMenuItem;
          Load1: TMenuItem;
          OpenDialog: TOpenDialog;
          AnimationTimer: TTimer;
          procedure Load1Click(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
          procedure FormResize(Sender: TObject);
          procedure FormShow(Sender: TObject);
          procedure AnimationTimerTimer(Sender: TObject);
        private
          { Private declarations }
          fBuffer          : TDirectBitmap;
          fImage           : TFrameImage;
          fPrimary         : TDirectBitmap;
          fOverlay         : IDirectSurface;
          fOverlayVisible  : boolean;
          //fPalette         : TPaletteInfo;
          fFrame           : integer;
          fLastFrameUpdate : integer;
          procedure RefreshOverlay;
          procedure WMMove(var Msg : TWMMove); message WM_MOVE;
        public
          { Public declarations }
      end;

  var
    ImageTestWindow: TImageTestWindow;

implementation

  {$R *.DFM}

  uses
    SpriteLoaders, ImageLoaders, GifLoader, GDI, DDraw;

  const
    cBufferBitCount = 15;

  const
    cAlpha = 0;

  function ShowOverlay(Control : TWinControl; const Overlay, Primary : IDirectDrawSurface4) : boolean;
    var
      SrcRect  : TRect;
      DestRect : TRect;
      ScPt     : TPoint;
    begin
      SrcRect := Rect(0, 0, pred(Control.ClientWidth), pred(Control.ClientHeight));
      DestRect := SrcRect;
      ScPt := Control.ClientToScreen(Point(0, 0));
      OffsetRect(DestRect, ScPt.x, ScPt.y);
      if Overlay.UpdateOverlay(@SrcRect, Primary, @DestRect, DDOVER_SHOW, nil) <> DD_OK
        then
          begin
            MessageBeep(0);
            Result := false;
          end
        else Result := true;
    end;

  function MoveOverlay(Control : TWinControl; const Overlay : IDirectDrawSurface4) : boolean;
    var
      ScPt : TPoint;
    begin
      ScPt := Control.ClientToScreen(Point(0, 0));
      // adjust position accounting for destination alignment restrictions
      Result := Overlay.SetOverlayPosition(ScPt.x, ScPt.y) = DD_OK;
    end;
    
  procedure TImageTestWindow.Load1Click(Sender: TObject);
    var
      ImageStream : TStream;
    begin
      if OpenDialog.Execute
        then
          begin
            fImage.Free;
            fImage := nil;
            //fPalette.Free;
            //fPalette := nil;
            try
              ImageStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
              try
                LoadFrameImage(fImage, ImageStream);
                AnimationTimer.Enabled := fImage.FrameCount > 1;
                fFrame := 0;
                fLastFrameUpdate := GetTickCount;
                if not fOverlayVisible
                  then fOverlayVisible := ShowOverlay(Self, fOverlay, fPrimary.Surface);
                RefreshOverlay;
              finally
                //ImageStream.Free;
              end;
            except
              Application.MessageBox('Can''t open image file', 'Image Test', MB_OK);
            end;
          end;
    end;

  procedure TImageTestWindow.RefreshOverlay;
    var
      DDBltEfx : TDDBltFx;
    begin
      if fImage <> nil
        then
          begin
            fillchar(DDBltEfx, sizeof(DDBltEfx), 0);
            DDBltEfx.dwSize := sizeof(DDBltEfx);
            DDBltEfx.dwFillColor := RGB(0, 0, 0);
            fBuffer.BackSurface.Blt(nil, nil, nil, DDBLT_COLORFILL, @DDBltEfx);
            fImage.Draw(0, 0, fFrame, cAlpha, ClientRect, fBuffer, nil);
            if fOverlay.Flip(nil, DDFLIP_WAIT) <> DD_OK
              then MessageBeep(0);
          end;
    end;

  procedure TImageTestWindow.FormDestroy(Sender: TObject);
    begin
      fBuffer.Free;
      fBuffer := nil;
    end;

  procedure TImageTestWindow.FormResize(Sender: TObject);
    begin
      {if fBuffer <> nil
        then fBuffer.NewSize(ClientWidth, ClientHeight, 16);}
    end;

  procedure TImageTestWindow.FormShow(Sender: TObject);
    begin
      DirectDrawMgr.SetCooperativeLevel(DDSCL_NORMAL);
      fPrimary := TDirectBitmap.Create;
      fPrimary.SurfaceType := stPrimary;
      fPrimary.LockFlags := lckWait;
      fBuffer := TDirectBitmap.Create;
      fBuffer.SurfaceType := stOverlay;
      fBuffer.BackSurfaceCount := 1;
      fBuffer.NewSize(ClientWidth, ClientHeight, cBufferBitCount);
      fBuffer.LockFlags := lckNoSysLock or lckWait;
      fOverlay := fBuffer.Surface;
    end;

  procedure TImageTestWindow.WMMove(var Msg : TWMMove);
    begin
      inherited;
      if fOverlay <> nil
        then MoveOverlay(Self, fOverlay);
    end;

  procedure TImageTestWindow.AnimationTimerTimer(Sender: TObject);
    var
      CurTicks : integer;
    begin
      CurTicks := GetTickCount;
      if CurTicks - fLastFrameUpdate > fImage.FrameDelay[fFrame]
        then
          begin
            if fFrame = pred(fImage.FrameCount)
              then fFrame := 0
              else inc(fFrame);
            fLastFrameUpdate := CurTicks;
            RefreshOverlay;
          end;
    end;

initialization
  InitSprites;
  RegisterLoader(GetGifLoader, 0);
finalization
  DoneSprites;
end.
