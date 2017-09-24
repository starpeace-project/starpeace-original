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
          Load1: TMenuItem;
          OpenDialog: TOpenDialog;
          Report1: TMenuItem;
          Rendering1: TMenuItem;
          RenderingwithAlpha1: TMenuItem;
          procedure Load1Click(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
          procedure FormResize(Sender: TObject);
          procedure Rendering1Click(Sender: TObject);
          procedure RenderingwithAlpha1Click(Sender: TObject);
          procedure FormShow(Sender: TObject);
        private
          { Private declarations }
          fBuffer : TSpeedBitmap;
          fImage  : TGameImage;
        public
          { Public declarations }
      end;

  var
    ImageTestWindow: TImageTestWindow;

implementation

  {$R *.DFM}

  uses
    GDI, SpriteUtils, ImageLoader;

  procedure TImageTestWindow.Load1Click(Sender: TObject);
    begin
      {
      if OpenDialog.Execute
        then
          begin
      }
            fImage.Free;
            fImage := nil;
            fImage := LoadGameImage({'C:\Work\Five\Release\Client\cache\landimages\1\MidGrassSpecial1.bmp'}
                                    {'C:\WORK\FIVE\RELEASE\CLIENT\Cache\BuildingImages\MapMKOHiResB64x32x0.gif'}
                                     'C:\Work\Five\Release\Client\cache\buildingimages\mapmoabmine64x32x0.gif'
                                    {OpenDialog.FileName});
            fBuffer.Canvas.Brush.Color := clBtnFace;
            fBuffer.Canvas.FillRect(ClientRect);
            fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
            if fImage = nil
              then Application.MessageBox('Can''t open image file', 'Image Test', MB_OK);
      //    end;
    end;

  procedure TImageTestWindow.FormCreate(Sender: TObject);
    begin
      fBuffer := TSpeedBitmap.CreateSized(ClientWidth, ClientHeight, 16);
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

  const
    cLoopCount = 1500;

  procedure TImageTestWindow.Rendering1Click(Sender: TObject);
    var
      i                : integer;
      initialtickcount : integer;
      elapsedticks     : integer;
      ImageRect        : TRect;
      x, y             : integer;
    begin
      if fImage <> nil
        then
          begin
            initialtickcount := GetTickCount;
            for i := 0 to pred(cLoopCount) do
              begin
                x := random(1024);
                y := random(768);
                ImageRect := Rect(x, y, x + fImage.Width, y + fImage.Height);
                IntersectRect(ImageRect, ImageRect, ClientRect);
                fImage.Draw(x, y, 0, 0, ImageRect, fBuffer, nil);
              end;
            fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
            elapsedticks := GetTickCount - initialtickcount;
            Application.MessageBox(pchar(IntToStr(cLoopCount) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.'), 'Report', MB_OK);
          end;
    end;

  procedure TImageTestWindow.RenderingwithAlpha1Click(Sender: TObject);
    var
      i                : integer;
      initialtickcount : integer;
      elapsedticks     : integer;
      ImageRect        : TRect;
      x, y             : integer;
    begin
      if fImage <> nil
        then
          begin
            initialtickcount := GetTickCount;
            for i := 0 to pred(cLoopCount) do
              begin
                x := random(1024);
                y := random(768);
                ImageRect := Rect(x, y, x + fImage.Width, y + fImage.Height);
                IntersectRect(ImageRect, ImageRect, ClientRect);
                fImage.Draw(x, y, 1, 0, ImageRect, fBuffer, nil);
              end;
            fBuffer.ClipDraw(Canvas, 0, 0, ClientRect);
            elapsedticks := GetTickCount - initialtickcount;
            Application.MessageBox(pchar(IntToStr(cLoopCount) + ' images rendered in ' + IntToStr(elapsedticks) + ' milliseconds.'), 'Report', MB_OK);
          end;
    end;

  procedure TImageTestWindow.FormShow(Sender: TObject);
    begin
      Left := 0;
      Top := 0;
      Width := 1024;
      Height := 768;
    end;

end.
