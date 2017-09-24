unit ConvertToIsometric;

interface

  uses
    Graphics;

  function ConvertBmpToIsometric(Template, Bmp : TBitmap) : TBitmap;

implementation

  function Xt(x, y, offset : integer) : integer;
    begin
      result := offset + x - y;
    end;

  function Yt(x, y : integer) : integer;
    begin
      result := trunc((x + y)/2);
    end;

  {
  function Yt(x, y : integer) : integer;
    begin
      result := x div 2 + y div 2;
    end;
  }

  function ConvertBmpToIsometric(Template, Bmp : TBitmap) : TBitmap;
    var
      xSize : integer;
      ySize : integer;
      x, y  : integer;
      c     : TColor;
      tc    : TColor;
    begin
      xSize  := Bmp.Width;
      ySize  := Bmp.Height;
      result := TBitmap.Create;
      result.Width  := 2*xSize;
      result.Height := ySize;
      tc := Bmp.Canvas.Pixels[0, 0];
      result.Canvas.Brush.Color := tc;
      result.Canvas.FillRect(result.Canvas.ClipRect);
      for x := 0 to pred(xSize) do
        for y := 0 to pred(ySize) do
          begin
            c := Bmp.Canvas.Pixels[x, y];
            result.Canvas.Pixels[Xt(x, y, xSize), Yt(x, y) {+ 1}] := c;
            result.Canvas.Pixels[Xt(x, y, xSize) - 1, Yt(x, y)] := c;
          end;
      for x := 0 to pred(Template.Width) do
        for y := 0 to pred(Template.Height) do
          if Template.Canvas.Pixels[x, y] = clWhite
            then result.Canvas.Pixels[x, y] := tc;
    end;

end.
