unit Gradients;

interface

  uses
    Graphics;

  procedure Gradient(Bitmap : TBitmap; horizontal : boolean);

implementation

  procedure Gradient(Bitmap : TBitmap; horizontal : boolean);
    type
      TRGBRec =
        record
          R : byte;
          B : byte;
          G : byte;
          I : byte;
        end;
    var
      ri    : single;
      gi    : single;
      bi    : single;
      fr    : single;
      fg    : single;
      fb    : single;
      Color : TColor;
      dist  : integer;

    procedure HorizontalGradient;
      var
        i : integer;
        j : integer;
      begin
        for i := 0 to pred(Width) do
          begin
            for j := 0 to pred(Height) do
              fBitmap.Canvas.Pixels[i, j] := Color;
            with TRGBRec(Color) do
              begin
                fr := fr + ri;
                R  := round(fr);
                fg := fg + gi;
                G  := round(fg);
                fb := fb + bi;
                B  := round(fb);
              end;
          end;
      end;

    procedure VerticalGradient;
      var
        i : integer;
        j : integer;
      begin
        for j := 0 to pred(Height) do
          begin
            for i := 0 to pred(Width) do
              fBitmap.Canvas.Pixels[i, j] := Color;
            with TRGBRec(Color) do
              begin
                fr := fr + ri;
                R  := round(fr);
                fg := fg + gi;
                G  := round(fg);
                fb := fb + bi;
                B  := round(fb);
              end;
          end;
      end;

    begin
      if horizontal
        then dist := fBitmap.Width
        else dist := fBitmap.Height;
      if dist > 0
        then
          begin
            ri := (TRGBRec(fToColor).R - TRGBRec(fFromColor).R)/dist;
            gi := (TRGBRec(fToColor).G - TRGBRec(fFromColor).G)/dist;
            bi := (TRGBRec(fToColor).B - TRGBRec(fFromColor).B)/dist;
            Color := fFromColor;
            with TRGBRec(Color) do
              begin
                fr := R;
                fg := G;
                fb := B;
              end;
            if horizontal
              then HorizontalGradient
              else VerticalGradient;
          end;
    end;

end.
 