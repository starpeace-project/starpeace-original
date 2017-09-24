unit GradientUtils;

interface

  uses
    Windows, Graphics;

  procedure Gradient(Canvas : TCanvas; frColor, toColor : TColor; horizontal : boolean; R : TRect);

implementation

  procedure Gradient(Canvas : TCanvas; frColor, toColor : TColor; horizontal : boolean; R : TRect);
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

    procedure VerticalGradient;
      var
        j : integer;
        D : TRect;
        S : TRect;
      begin
        for j := R.Top to pred(R.Bottom) do
          begin
            Canvas.Pixels[R.Left, j] := Color;
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
        D := R;
        D.Right := succ(D.Left);
        S := D;
        for j := succ(R.Left) to pred(R.Right) do
          begin
            OffsetRect(D, 1, 0);
            Canvas.CopyRect(D, Canvas, S);
          end;
      end;

    procedure HorizontalGradient;
      var
        i : integer;
        j : integer;
        D : TRect;
        S : TRect;
      begin
        for j := R.Left to pred(R.Right) do
          begin
            Canvas.Pixels[j, R.Top] := Color;
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
        D := R;
        D.Bottom := succ(D.Top);
        S := D;
        for i := succ(R.Top) to pred(R.Bottom) do
          begin
            OffsetRect(D, 0, 1);
            Canvas.CopyRect(D, Canvas, S);
          end;
      end;

    begin
      if horizontal
        then dist := R.Right - R.Left + 1
        else dist := R.Bottom - R.Top + 1;
      if dist > 0
        then
          begin
            ri := (TRGBRec(toColor).R - TRGBRec(frColor).R)/dist;
            gi := (TRGBRec(toColor).G - TRGBRec(frColor).G)/dist;
            bi := (TRGBRec(toColor).B - TRGBRec(frColor).B)/dist;
            Color := frColor;
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
