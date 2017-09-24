unit MainWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    DDraw, Menus, ExtDlgs;

  type
    TMainForm =
      class(TForm)
          OpenPictureDialog: TOpenPictureDialog;
          MainMenu: TMainMenu;
          File1: TMenuItem;
          Load: TMenuItem;
          procedure FormShow(Sender: TObject);
          procedure FormPaint(Sender: TObject);
          procedure LoadClick(Sender: TObject);
        private
          { Private declarations }
          fDirectDraw     : IDirectDraw4;
          fPrimarySurface : IDirectDrawSurface4;
          fClipper        : IDirectDrawClipper;
          fBitmap         : TBitmap;
          fBitmapSurface  : IDirectDrawSurface4;
        public
          { Public declarations }
      end;

  var
    MainForm: TMainForm;

implementation

  {$R *.DFM}

  uses
    ComObj, LogFile;

  function CopyBitmapToSurface(Bitmap : TBitmap; const Surface : IDirectDrawSurface4; x, y : integer) : boolean;
    var
      dc    : HDC;
      memdc : HDC;
    begin
      Result := false;
      if Surface.GetDC(dc) = DD_OK
        then
          try
            memdc := CreateCompatibleDC(dc);
            if memdc <> 0
              then
                try
                  SelectObject(memdc, Bitmap.Handle);
                  if not BitBlt(dc, x, y, Bitmap.Width, Bitmap.Height, memdc, 0, 0, SRCCOPY)
                    then LogThis('BitBlt FAILED')
                    else Result := true;
                finally
                  DeleteDC(memdc);
                end
              else LogThis('CreateCompatibleDC FAILED');
          finally
            Surface.ReleaseDC(dc);
          end
        else LogThis('GetDC FAILED');
    end;

  procedure TMainForm.FormShow(Sender: TObject);
    var
      ddsd    : TDDSurfaceDesc2;
      //ddscaps : TDDSCAPS2;
      hRet    : HRESULT;
      pDD     : IDirectDraw;
    begin
      hRet := DirectDrawCreate(nil, pDD, nil);
      if hRet <> DD_OK
        then LogThis('DirectDrawCreate FAILED')
        else
          begin
            hRet := pDD.QueryInterface(IID_IDirectDraw4, fDirectDraw);
            if hRet <> DD_OK
              then LogThis('QueryInterface FAILED')
              else
                begin
                  hRet := fDirectDraw.SetCooperativeLevel(Handle, DDSCL_NORMAL);
                  if hRet <> DD_OK
                    then LogThis('SetCooperativeLevel FAILED')
                    else
                      begin
                        // Create the primary surface
                        fillchar(ddsd, sizeof(ddsd), 0);
                        ddsd.dwSize := sizeof(ddsd);
                        ddsd.dwFlags := DDSD_CAPS;
                        ddsd.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;
                        hRet := fDirectDraw.CreateSurface(ddsd, fPrimarySurface, nil);
                        if hRet <> DD_OK
                          then LogThis('CreateSurface FAILED')
                          else
                            begin
                              hRet := fDirectDraw.CreateClipper(0, fClipper, nil);
                              if hRet <> DD_OK
                                then LogThis('GetClipper FAILED')
                                else
                                  begin
                                    hRet := fPrimarySurface.SetClipper(fClipper);
                                    if hRet <> DD_OK
                                      then LogThis('SetClipper FAILED');
                                  end;
                            end;
                      end;
                end;
          end;
    end;

  procedure TMainForm.FormPaint(Sender: TObject);

    function DoBlit(const Src, Dest : IDirectDrawSurface4; x, y : integer) : boolean;
      var
        BltEfx   : TDDBltFX;
        DestRect : TRect;
        SrcRect  : TRect;
      begin
        fillchar(BltEfx, sizeof(BltEfx), 0);
        BltEfx.dwSize := sizeof(BltEfx);
        DestRect := Rect(x, y, pred(x + fBitmap.Width), pred(y + fBitmap.Height));
        SrcRect := Rect(0, 0, pred(fBitmap.Width), pred(fBitmap.Height));
        Result := Dest.Blt(@DestRect, Src, @SrcRect, DDBLT_WAIT, BltEfx) = DD_OK;
      end;

    var
      hRet : HRESULT;
      ScPt : TPoint;
    begin
      if (fBitmap <> nil) and (fClipper <> nil)
        then
          begin
            hRet := fClipper.SetHWnd(0, Handle);
            if hRet <> DD_OK
              then LogThis('SetHWnd FAILED')
              else
                begin
                  ScPt := ClientToScreen(Point(0, 0));
                  DoBlit(fBitmapSurface, fPrimarySurface, ScPt.x, ScPt.y);
                end;
          end;
    end;

  procedure TMainForm.LoadClick(Sender: TObject);

    function CreateBitmapSurface(Width, Height : integer) : IDirectDrawSurface4;
      var
        hRet : HRESULT;
        ddsd : TDDSurfaceDesc2;
      begin
        fillchar(ddsd, sizeof(ddsd), 0);
        ddsd.dwSize := sizeof(ddsd);
        ddsd.dwFlags := DDSD_CAPS or DDSD_WIDTH or DDSD_HEIGHT;
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;
        ddsd.dwWidth := Width;
        ddsd.dwHeight := Height;
        hRet := fDirectDraw.CreateSurface(ddsd, Result, nil);
        if hRet <> DD_OK
          then Result := nil;
      end;

    begin
      if OpenPictureDialog.Execute
        then
          begin
            fBitmapSurface := nil;
            fBitmap.Free;
            fBitmap := nil;
            fBitmap := TBitmap.Create;
            try
              fBitmap.LoadFromFile(OpenPictureDialog.FileName);
              fBitmapSurface := CreateBitmapSurface(fBitmap.Width, fBitmap.Height);
              if fBitmapSurface <> nil
                then CopyBitmapToSurface(fBitmap, fBitmapSurface, 0, 0)
                else
                  begin
                    fBitmap.Free;
                    fBitmap := nil;
                  end;
              Refresh;
            except
              fBitmap.Free;
              fBitmap := nil;
            end;
          end;
    end;

initialization
  SetLogFile('C:\Tmp\Windowed.log');
end.
