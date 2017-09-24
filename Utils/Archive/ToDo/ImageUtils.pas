unit ImageUtils;

interface

  uses
    Classes, Windows, Graphics, ExtCtrls;

  function LoadIconFromFile(Icon : TIcon; FileName : string; const Width, Height : integer) : boolean;
  procedure DrawIcon2Image(Image : TImage; Icon : TIcon; const Width, Height : integer; const BkColor : TColor);

implementation

  function LoadIconFromFile(Icon : TIcon; FileName : string; const Width, Height : integer) : boolean;
    var
      IconHandle : HICON;
    begin
      IconHandle := Windows.LoadImage(0, PCHAR(FileName), IMAGE_ICON, Width, Height, LR_LOADFROMFILE);
      if IconHandle <> 0
        then
          begin
            Icon.Handle := IconHandle;
            result := true;
          end
        else result := false;
    end;

  procedure DrawIcon2Image(Image : TImage; Icon : TIcon; const Width, Height : integer; const BkColor : TColor);
    var
      Bitmap : TBitmap;
    begin
      Bitmap := TBitmap.Create;
      if Icon <> nil
        then
          begin
            Bitmap.Width  := Width;
            Bitmap.Height := Height;
            with Bitmap.Canvas do
              begin
                Brush.Color := BkColor;
                Brush.Style := bsSolid;
                FillRect(ClipRect);
              end;
            DrawIconEx(Bitmap.Canvas.Handle, 0, 0, Icon.Handle, Width, Height, 0, 0, DI_NORMAL);
          end;
      Image.Picture.Bitmap := Bitmap;
    end;

end.
