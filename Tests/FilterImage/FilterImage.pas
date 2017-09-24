unit FilterImage;

interface

  uses
    Graphics;

  function FilterBmp(Template, Bmp : TBitmap) : TBitmap;

implementation

  function FilterBmp(Template, Bmp : TBitmap) : TBitmap;
    var
      xSize : integer;
      ySize : integer;
      x, y  : integer;
      tc    : TColor;
    begin
      xSize := Bmp.Width;
      ySize := Bmp.Height;
      result := TBitmap.Create;
      result.Width  := xSize;
      result.Height := ySize;
      result.PixelFormat := Bmp.PixelFormat;
      tc := Bmp.Canvas.Pixels[0, 0];
      for x := 0 to pred(xSize) do
        for y := 0 to pred(ySize) do
          if (Template.Canvas.Pixels[x, y] = clWhite) and (Bmp.Canvas.Pixels[x, y] <> tc)
            then result.Canvas.Pixels[x, y] := tc
            else result.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[x, y];
    end;

end.
