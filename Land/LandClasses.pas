unit LandClasses;

interface

  uses
    Windows, Land, Collection, SysUtils, Graphics, IniFiles;

  const
    TextureNames : array[TLandClass] of string =
      ( 'Grass',
        'MidGrass',
        'DryGround',
        'Water' );

  type
    TCollection    = Collection.TCollection;
    TClassTypeInfo = array[TLandType] of TCollection;

  type
    TMapTexture    = class;
    TLandClassInfo = class;

    TTextureRelation  = (trNone, trMix, trBorder, trDither);
    PTextureRelations = ^TTextureRelations;
    TTextureRelations = array[0..0] of TTextureRelation;

    TMapTexture =
      class
        public
          constructor Create(Bmp : TBitmap; txCount : integer);
          destructor  Destroy; override;
        private
          fBmp       : TBitmap;
          fRelations : PTextureRelations;
      end;

    TLandClassInfo =
      class
        public
          constructor Create(IniPath : string);
          destructor  Destroy; override;
        private
          fTemplate    : TBitmap;
          fTextures    : TCollection;
          fClassTypes  : TClassTypeInfo;
          fSourcePath  : string;
          fOutputPath  : string;
          fClassesPath : string;
          fCenterCount : integer;
          fStrghtCount : integer;
          fCornerCount : integer;
        private
          procedure LoadTextures(Ini : TIniFile);
          procedure LoadVectors(center, straight, corner : string);
          procedure SaveClass(name : string; Bmp : TBitmap; clType : TLandType; txA, txB, varIdx : integer; TraceVert : boolean);
          procedure GenCenters;
          procedure GenOtherTypes;
        public
          procedure Run;
      end;

implementation

  uses
    Classes, Forms;

  type
    TColorRec =
      packed record
        R : byte;
        G : byte;
        B : byte;
        I : byte;
      end;

  function LoadBitmap(path : string; Bmp : TBitmap) : boolean;
    var
      st : TStream;
    begin
      try
        st := TFileStream.Create(path, fmOpenRead);
        try
          Bmp.LoadFromStream(st);
        finally
          st.Free;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  function SaveBitmap(path : string; Bmp : TBitmap) : boolean;
    var
      st : TStream;
    begin
      try
        st := TFileStream.Create(path, fmCreate);
        try
          Bmp.SaveToStream(st);
        finally
          st.Free;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  procedure StraightGradient(Bmp : TBitmap);
    var
      xSize : integer;
      ySize : integer;
      tC    : TColor;
      fC    : TColor;
      i     : integer;
      y, y1 : integer;
      d     : integer;
      Step  : integer;
      Brush : integer;
      Color : TColor;
    begin
      xSize := Bmp.Width;
      ySize := Bmp.Height;
      tC    := Bmp.Canvas.Pixels[0, 0];
      fC    := Bmp.Canvas.Pixels[0, pred(ySize)];
      for i := 0 to pred(xSize) do
        begin
          y := pred(ySize);
          while (y >= 0) and (Bmp.Canvas.Pixels[i, y] = fC) do
            dec(y);
          y1 := y;
          d  := 0;
          while (y >= 0) and (Bmp.Canvas.Pixels[i, y] <> tC) do
            begin
              dec(y);
              inc(d);
            end;
          Step  := integer(lo(tC) - lo(fC)) div d;
          Brush := lo(fC);
          while y1 >= y do
            begin
              with TColorRec(Color) do
                begin
                  R := Brush;
                  G := Brush;
                  B := Brush;
                  I := 0;
                end;
              Brush := Brush + Step;
              Bmp.Canvas.Pixels[i, y1] := Color;
              Color := Bmp.Canvas.Pixels[i, y1];
              dec(y1);
            end;
          while y1 > 0 do
            begin
              if Bmp.Canvas.Pixels[i, y1] <> tC
                then Bmp.Canvas.Pixels[i, y1] := tC;
              dec(y1);
            end;
        end;
    end;

  procedure RadialGradient(Bmp : TBitmap);
    var
      xSize : integer;
      ySize : integer;
      tC    : TColor;
      fC    : TColor;
      i, j  : integer;
      x, x1 : integer;
      y, y1 : integer;
      d     : integer;
      Step  : integer;
      Brush : integer;
      Color : TColor;
      ptA   : integer;
      ptB   : integer;
    begin
      xSize := Bmp.Width;
      ySize := Bmp.Height;
      fC    := Bmp.Canvas.Pixels[1, 1];
      tC    := Bmp.Canvas.Pixels[pred(xSize)-1, pred(xSize)-1];
      j     := 0;
      // Calculate ptA
      x   := 0;
      y   := pred(ySize);
      while (x < xSize) and (Bmp.Canvas.Pixels[x, y] <> tC) do
        inc(x);
      ptA := x;

      // Calculate ptB
      x := pred(xSize);
      y := 0;
      while (y < ySize) and (Bmp.Canvas.Pixels[x, y] <> tC) do
        inc(y);
      ptB := y;

      for i := pred(ySize) downto 0 do
        begin
          x := 0;
          y := i;
          while (x <= j) and (y < ySize) and (Bmp.Canvas.Pixels[x, y] = fC) do
            begin
              inc(x);
              inc(y);
            end;
          x1 := x;
          y1 := y;
          d := 0;
          while (x <= j) and (y < ySize) and (Bmp.Canvas.Pixels[x, y] <> tC) do
            begin
              inc(x);
              inc(y);
              inc(d);
            end;
          if d > 0
            then
              begin
                //if Bmp.Canvas.Pixels[x, y] = -1
                  //then d := ySize div 2;
                if y = ySize
                  then d := d + round(sqrt(sqr(ptA - x)/2));
                Step  := integer(lo(tC) - lo(fC)) div d;
                Brush := lo(fC);
                while (x1 < x) and (y1 < y) do
                  begin
                    with TColorRec(Color) do
                      begin
                        R := Brush;
                        G := Brush;
                        B := Brush;
                        I := 0;
                      end;
                    Brush := Brush + Step;
                    Bmp.Canvas.Pixels[x1, y1] := Color;
                    inc(x1);
                    inc(y1);
                  end;
              end;
          inc(j);
        end;
      j := pred(xSize);
      for i := 0 to pred(ySize) do
        begin
          x := j;
          y := 0;
          while (x <= pred(xSize)) and (y <= i) and (Bmp.Canvas.Pixels[x, y] = fC) do
            begin
              inc(x);
              inc(y);
            end;
          x1 := x;
          y1 := y;
          d := 0;
          while (x <= pred(xSize)) and (y <= i) and (Bmp.Canvas.Pixels[x, y] <> tC) do
            begin
              inc(x);
              inc(y);
              inc(d);
            end;
          if d > 0
            then
              begin
                //if Bmp.Canvas.Pixels[x, y] = -1
                  //then d := ySize div 2;
                if x = xSize
                  then d := d + round(sqrt(sqr(ptB - y)/2));
                Step  := integer(lo(tC) - lo(fC)) div d;
                Brush := lo(fC);
                while (x1 < x) and (y1 < y) do
                  begin
                    with TColorRec(Color) do
                      begin
                        R := Brush;
                        G := Brush;
                        B := Brush;
                        I := 0;
                      end;
                    Brush := Brush + Step;
                    Bmp.Canvas.Pixels[x1, y1] := Color;
                    inc(x1);
                    inc(y1);
                  end;
              end;
          dec(j);
        end;
      {for i := 0 to pred(Bmp.Width) do
        for j := 0 to pred(Bmp.Height) do
          begin
            if (Bmp.Canvas.Pixels[i, j] <> clWhite) and (Bmp.Canvas.Pixels[i, j] <> clBlack)
              then Bmp.Canvas.Pixels[i, j] := $00808080;
          end;}
    end;

  procedure RotateBitmap(var Bmp : TBitmap);
    var
      xSize  : integer;
      ySize  : integer;
      x, y   : integer;
      TmpBmp : TBitmap;
    begin
      xSize := Bmp.Width;
      ySize := Bmp.Height;
      TmpBmp := TBitmap.Create;
      TmpBmp.Width  := xSize;
      TmpBmp.Height := ySize;
      for x := 0 to pred(xSize) do
        for y := 0 to pred(ySize) do
          TmpBmp.Canvas.Pixels[x, y] := Bmp.Canvas.Pixels[y, xSize-x-1];
      Bmp.Free;
      Bmp := TmpBmp;
    end;

  function Xt(x, y, offset : integer) : integer;
    begin
      result := offset + x - y;
    end;

  function Yt(x, y : integer) : integer;
    begin
      result := trunc((x + y)/2);
    end;

  function ConvertToIsometric(Template, Bmp : TBitmap) : TBitmap;
    var
      xSize : integer;
      ySize : integer;
      x, y  : integer;
      c     : TColor;
    begin
      xSize  := Bmp.Width;
      ySize  := Bmp.Height;
      result := TBitmap.Create;
      result.Width  := 2*xSize;
      result.Height := ySize;
      result.Canvas.Brush.Color := clBlue;
      result.Canvas.FillRect(result.Canvas.ClipRect);
      for x := 0 to pred(xSize) do
        for y := 0 to pred(ySize) do
          begin
            c := Bmp.Canvas.Pixels[x, y];
            result.Canvas.Pixels[Xt(x, y, xSize), Yt(x, y) + 1] := c;
            result.Canvas.Pixels[Xt(x, y, xSize) - 1, Yt(x, y)] := c;
          end;
      c := Bmp.Canvas.Pixels[random(xSize), random(ySize)];
      for x := 0 to pred(Template.Width) do
        for y := 0 to pred(Template.Height) do
          if Template.Canvas.Pixels[x, y] = clWhite
            then result.Canvas.Pixels[x, y] := clBlue
            else
              if result.Canvas.Pixels[x, y] = clBlue
                then result.Canvas.Pixels[x, y] := c;
    end;

  function AverageColors(Color1, Color2 : TColor; Perc1, Perc2, BrdPrc1, BrdPrc2 : single) : TColor;
    var
      ColRec1 : TColorRec absolute Color1;
      ColRec2 : TColorRec absolute Color2;
    begin
      with TColorRec(result) do
        begin
          R := round((ColRec1.R*Perc1*BrdPrc1 + ColRec2.R*Perc2*BrdPrc2));
          G := round((ColRec1.G*Perc1*BrdPrc1 + ColRec2.G*Perc2*BrdPrc2));
          B := round((ColRec1.B*Perc1*BrdPrc1 + ColRec2.B*Perc2*BrdPrc2));
          I := round((ColRec1.I*Perc1*BrdPrc1 + ColRec2.I*Perc2*BrdPrc2));
        end;
    end;

  function MixTextures(Template, TextureA, TextureB : TBitmap; PercA, PercB : byte; Dither : boolean) : TBitmap;
    var
      Templ  : TCanvas;
      xSize  : integer;
      ySize  : integer;
      xTa    : integer;
      yTa    : integer;
      xTb    : integer;
      yTb    : integer;
      i, j   : integer;
      Color  : integer;
      Transp : integer;
      aPerc  : byte;
      bPerc  : byte;
    begin
      Templ := Template.Canvas;
      with Templ.ClipRect do
        begin
          xSize := Right - Left;
          ySize := Bottom - Top;
        end;
      // Result bitmap
      result        := TBitMap.Create;
      result.Width  := xSize;
      result.Height := ySize;
      // Get the transparent color
      Transp := ColorToRGB(Templ.Pixels[0, 0]);
      // Get randomly the areas in the textures
      xTa := random(TextureA.Canvas.ClipRect.Right  - xSize);
      yTa := random(TextureA.Canvas.ClipRect.Bottom - ySize);
      xTb := random(TextureB.Canvas.ClipRect.Right  - xSize);
      yTb := random(TextureB.Canvas.ClipRect.Bottom - ySize);
      // Create the resulting texture
      for i := 0 to pred(xSize) do
        for j := 0 to pred(ySize) do
          begin
            Color := ColorToRGB(Templ.Pixels[i, j]);
            if Color <> Transp
              then
                case lo(Color) of
                  $00 :
                    result.Canvas.Pixels[i, j] := TextureB.Canvas.Pixels[xTb + i, yTb + j];
                  $FF :
                    result.Canvas.Pixels[i, j] := TextureA.Canvas.Pixels[xTa + i, yTa + j];
                  else
                    begin
                      aPerc := lo(Color);
                      bPerc := high(bPerc) - aPerc;
                      if Dither
                        then
                          begin
                            if random(256) <= aPerc
                              then result.Canvas.Pixels[i, j] := TextureA.Canvas.Pixels[xTa + i, yTa + j]
                              else result.Canvas.Pixels[i, j] := TextureB.Canvas.Pixels[xTa + i, yTa + j];
                          end
                        else
                          result.Canvas.Pixels[i, j] :=
                            AverageColors(
                              TextureA.Canvas.Pixels[xTa + i, yTa + j],
                              TextureB.Canvas.Pixels[xTb + i, yTb + j],
                              aPerc/high(aPerc),
                              bPerc/high(bPerc),
                              PercA/100,
                              PercB/100);
                    end;
                end
              else result.Canvas.Pixels[i, j] := Transp;
          end;
    end;

  (*
  function ConvertToPalettedBitmap(Bmp : TBitmap; VertTrace : boolean; var AvrColor : TColor) : TBitmap;
    begin
      result := Bmp;
    end;
  *)

  function ConvertToPalettedBitmap(Bmp : TBitmap; VertTrace : boolean; var AvrColor : TColor) : TBitmap;

    type
      PRGB = ^TRGB;
      TRGB =
        packed record
          Blue  : byte;
          Green : byte;
          Red   : byte;
          Unk   : byte;
        end;

      T256Palette = array[0..255] of TColor;

    var
      pal    : PLogPalette;
      hpal   : HPALETTE;
      line   : PByteArray;
      aux    : T256Palette;
      plIdx  : integer;
      xSize  : integer;
      ySize  : integer;
      i, j   : integer;
      Color  : TColor;

    function SameColor(var col1, col2 : TColor) : boolean;
      const
        MinDist = 10;
      var
        c1   : TRGB absolute col1;
        c2   : TRGB absolute col2;
        dist : integer;
      begin
        dist := abs(integer(c1.Red) - integer(c2.Red)) +
                abs(integer(c1.Green) - integer(c2.Green)) +
                abs(integer(c1.Blue) - integer(c2.Blue));
        result := dist <= MinDist;
      end;

    function ColorExists(Color : TColor) : boolean;
      var
        i : integer;
      begin
        Color := Color and $00FFFFFF;
        i := pred(plIdx);
        while (i >= 0) and not SameColor(Color, aux[i]) do //(Color <> aux[i]) do //((aux[i].Red <> C.R) or (aux[i].Green <> C.G) or (aux[i].Blue <> C.B)) do
          dec(i);
        result := i >= 0;
      end;

    function PaletteIndexOf(Color : TColor) : byte;
      var
        t : single;
        d : single;
        i : integer;
        C : TColorRec absolute Color;
      begin
        Color := Color and $00FFFFFF;
        if Color = aux[0]
          then result := 0
          else
            begin
              result := 1;
              d := 256;
              i := 1;
              while (i < 256) and (d > 0) do
                begin
                  with TColorRec(aux[i]) do
                    t := sqrt(sqr(C.R - R) + sqr(C.G - G) + sqr(C.B - B));
                  if d > t
                    then
                      begin
                        d := t;
                        result := i;
                      end;
                  inc(i);
                end;
            end;
      end;

    procedure PickColor(Color : TColor);
      begin
        if not ColorExists(Color)
          then
            begin
              aux[plIdx] := Color and $00FFFFFF;
              inc(plIdx);
            end;
      end;

    procedure PickVLineColors(lnIdx : integer);
      var
        i : integer;
      begin
        i := 0;
        while (i < ySize) and (plIdx < 256) do
          begin
            PickColor(Bmp.Canvas.Pixels[Xt(lnIdx, i, 32), Yt(lnIdx, i)]);
            inc(i);
          end;
      end;

    procedure PickHLineColors(lnIdx : integer);
      var
        j : integer;
      begin
        j := 0;
        while (j < xSize) and (plIdx < 256) do
          begin
            PickColor(Bmp.Canvas.Pixels[Yt(j, lnIdx), Xt(j, lnIdx, 32)]);
            inc(j);
          end;
      end;

    var
      cR, cG, cB : integer;
      cnt        : integer;
      bgColor    : TColor;

    begin
      cR := 0;
      cG := 0;
      cB := 0;
      cnt := 0;

      xSize  := Bmp.Width;
      ySize  := Bmp.Height;
      GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
      pal.palVersion := $300;
      pal.palNumEntries := 256;

      // Create the 256 colors palette
      FillChar(aux, 256*sizeof(aux[0]), 0);

      // Transparent color
      aux[0] := $000000FF; //Bmp.Canvas.Pixels[0, 0];
      plIdx  := 1;

      bgColor := Bmp.Canvas.Pixels[0, 0];

      i := 0;
      while (i < ySize) and (plIdx < 256) do
        begin
          j := 0;
          while (j < xSize) and (plIdx < 256) do
            begin
              Color := Bmp.Canvas.Pixels[j, i];
              PickColor(Color);
              if Color <> bgColor
                then
                  begin
                    with TColorRec(Color) do
                      begin
                        inc(cR, R);
                        inc(cG, G);
                        inc(cB, B);
                      end;
                    inc(cnt);
                  end;
              inc(j);
            end;
          inc(i);
        end;

      {
      while (i < pred(ySize div 2)) and (plIdx < 256) do
        begin
          PickVLineColors(pred(ySize div 2) - i);
          PickHLineColors(pred(ySize div 2) - i);
          PickVLineColors(pred(ySize) - i);
          PickHLineColors(pred(ySize) - i);
          inc(i);
        end;
       }

      // Create the new bitmap
      result := TBitmap.Create;
      result.PixelFormat := pf8bit;
      result.Width  := xSize;
      result.Height := ySize;

       // Assign the palette
      for i := 0 to 255 do
        pal.palPalEntry[i] := TPaletteEntry(aux[i]);

        {
        with TColorRec(aux[i]) do
          begin
            pal.palPalEntry[i].peRed := R;
            pal.palPalEntry[i].peGreen := G;
            pal.palPalEntry[i].peBlue := B;
          end;
        }

      hpal := CreatePalette(pal^);
      if hpal <> 0
        then result.Palette := hpal;

      // Set the pixels
      for i := 0 to pred(ySize) do
        begin
          line := result.ScanLine[i];
          for j := 0 to pred(xSize) do
            line[j] := PaletteIndexOf(Bmp.Canvas.Pixels[j, i]);
        end;

      with TColorRec(AvrColor) do
        begin
          R := round(cR/cnt);
          G := round(cG/cnt);
          B := round(cB/cnt);
          I := 0;
        end;

    end;

  (*
  function ConvertToPalettedBitmap(Bmp : TBitmap; VertTrace : boolean; var AvrColor : TColor) : TBitmap;

    type
      PRGB = ^TRGB;
      TRGB =
        packed record
          Blue  : byte;
          Green : byte;
          Red   : byte;
          Unk   : byte;
        end;

      T256Palette = array[0..255] of TColor;

    var
      pal    : PLogPalette;
      hpal   : HPALETTE;
      line   : PByteArray;
      aux    : T256Palette;
      plIdx  : integer;
      xSize  : integer;
      ySize  : integer;
      i, j   : integer;
      Color  : TColor;

    function ColorExists(Color : TColor) : boolean;
      var
        i : integer;
      begin
        Color := Color and $00FFFFFF;
        i := pred(plIdx);
        while (i >= 0) and (Color <> aux[i]) do //((aux[i].Red <> C.R) or (aux[i].Green <> C.G) or (aux[i].Blue <> C.B)) do
          dec(i);
        result := i >= 0;
      end;

    function PaletteIndexOf(Color : TColor) : byte;
      var
        t : single;
        d : single;
        i : integer;
        C : TColorRec absolute Color;
      begin
        Color := Color and $00FFFFFF;
        if Color = aux[0]
          then result := 0
          else
            begin
              result := 1;
              d := 256;
              i := 1;
              while (i < 256) and (d > 0) do
                begin
                  with TColorRec(aux[i]) do
                    t := sqrt(sqr(C.R - R) + sqr(C.G - G) + sqr(C.B - B));
                  if d > t
                    then
                      begin
                        d := t;
                        result := i;
                      end;
                  inc(i);
                end;
            end;
      end;

    procedure PickColor(Color : TColor);
      begin
        if not ColorExists(Color)
          then
            begin
              aux[plIdx] := Color and $00FFFFFF;
              inc(plIdx);
            end;
      end;

    procedure PickVLineColors(lnIdx : integer);
      var
        i : integer;
      begin
        i := 0;
        while (i < ySize) and (plIdx < 256) do
          begin
            PickColor(Bmp.Canvas.Pixels[Xt(lnIdx, i, 32), Yt(lnIdx, i)]);
            inc(i);
          end;
      end;

    procedure PickHLineColors(lnIdx : integer);
      var
        j : integer;
      begin
        j := 0;
        while (j < xSize) and (plIdx < 256) do
          begin
            PickColor(Bmp.Canvas.Pixels[Yt(j, lnIdx), Xt(j, lnIdx, 32)]);
            inc(j);
          end;
      end;

    var
      cR, cG, cB : integer;
      cnt        : integer;
      bgColor    : TColor;

    begin
      cR := 0;
      cG := 0;
      cB := 0;
      cnt := 0;

      xSize  := Bmp.Width;
      ySize  := Bmp.Height;
      GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
      pal.palVersion := $300;
      pal.palNumEntries := 256;

      // Create the 256 colors palette
      FillChar(aux, 256*sizeof(aux[0]), 0);

      // Transparent color
      aux[0] := $000000FF; //Bmp.Canvas.Pixels[0, 0];
      plIdx  := 1;

      bgColor := Bmp.Canvas.Pixels[0, 0];

      i := 0;
      while (i < ySize) and (plIdx < 256) do
        begin
          j := 0;
          while (j < xSize) and (plIdx < 256) do
            begin
              Color := Bmp.Canvas.Pixels[j, i];
              PickColor(Color);
              if Color <> bgColor
                then
                  begin
                    with TColorRec(Color) do
                      begin
                        inc(cR, R);
                        inc(cG, G);
                        inc(cB, B);
                      end;
                    inc(cnt);
                  end;
              inc(j);
            end;
          inc(i);
        end;

      {
      while (i < pred(ySize div 2)) and (plIdx < 256) do
        begin
          PickVLineColors(pred(ySize div 2) - i);
          PickHLineColors(pred(ySize div 2) - i);
          PickVLineColors(pred(ySize) - i);
          PickHLineColors(pred(ySize) - i);
          inc(i);
        end;
       }

      // Create the new bitmap
      result := TBitmap.Create;
      result.PixelFormat := pf8bit;
      result.Width  := xSize;
      result.Height := ySize;

       // Assign the palette
      for i := 0 to 255 do
        pal.palPalEntry[i] := TPaletteEntry(aux[i]);

        {
        with TColorRec(aux[i]) do
          begin
            pal.palPalEntry[i].peRed := R;
            pal.palPalEntry[i].peGreen := G;
            pal.palPalEntry[i].peBlue := B;
          end;
        }

      hpal := CreatePalette(pal^);
      if hpal <> 0
        then result.Palette := hpal;

      // Set the pixels
      for i := 0 to pred(ySize) do
        begin
          line := result.ScanLine[i];
          for j := 0 to pred(xSize) do
            line[j] := PaletteIndexOf(Bmp.Canvas.Pixels[j, i]);
        end;

      with TColorRec(AvrColor) do
        begin
          R := round(cR/cnt);
          G := round(cG/cnt);
          B := round(cB/cnt);
          I := 0;
        end;

    end;
  *)

{
  function ConvertToPalettedBitmap(Bmp : TBitmap; VertTrace : boolean) : TBitmap;

    type
      PRGB = ^TRGB;
      TRGB =
        record
          Red   : byte;
          Green : byte;
          Blue  : byte;
        end;

      T256Palette = array[0..255] of TRGB;

    var
      pal    : PLogPalette;
      hpal   : HPALETTE;
      line   : PByteArray;
      aux    : T256Palette;
      p      : PRGB;
      plIdx  : integer;
      Color  : TColor;
      xSize  : integer;
      ySize  : integer;
      i, j   : integer;

    function ColorExists(Color : TColor) : boolean;
      var
        i : integer;
        p : PRGB;
        C : TColorRec absolute Color;
      begin
        i := 0;
        p := @aux[i];
        while (i < plIdx) and ((p.Red <> C.R) or (p.Green <> C.G) or (p.Blue <> C.B)) do
          inc(i);
        result := i < plIdx;
      end;

    function PaletteIndexOf(Color : TColor) : byte;
      var
        t : integer;
        d : integer;
        i : integer;
        p : PRGB;
      begin
        result := 0;
        d := 3*256;
        i := 0;
        while (i < 256) and (d > 0) do
          begin
            p := @aux[i];
            with TColorRec(Color) do
              t := abs(p.Red - R) + abs(p.Green - G) + abs(p.Blue - B);
            if d > t
              then
                begin
                  d := t;
                  result := i;
                end;
            inc(i);
          end;
      end;

    begin
      xSize  := Bmp.Width;
      ySize  := Bmp.Height;
      GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
      pal.palVersion := $300;
      pal.palNumEntries := 256;

      // Create the 256 colors palette
      FillChar(aux, 256*sizeof(aux[0]), 0);
      plIdx := 0; // aux[0] = Black
      i := 0;
      while (i < ySize) and (plIdx < 256) do
        begin
          j := 0;
          while (j < xSize) and (plIdx < 256) do
            begin
              Color := Bmp.Canvas.Pixels[i, j];
              if not ColorExists(Color)
                then
                  begin
                    p := @aux[plIdx];
                    with TColorRec(Color) do
                      begin
                        p.Red   := R;
                        p.Green := G;
                        p.Blue  := B;
                      end;
                    inc(plIdx);
                  end;
              inc(j);
            end;
          inc(i);
        end;

      // Create the new bitmap
      result := TBitmap.Create;
      result.PixelFormat := pf8bit;
      result.Width  := xSize;
      result.Height := ySize;

       // Assign the palette
      for i := 0 to 255 do
        begin
          p := @aux[i];
          pal.palPalEntry[i].peRed := p.Red;
          pal.palPalEntry[i].peGreen := p.Green;
          pal.palPalEntry[i].peBlue := p.Blue;
        end;

      hpal := CreatePalette(pal^);
      if hpal <> 0
        then result.Palette := hpal;

      // Set the pixels
      for i := 0 to pred(ySize) do
        begin
          line := result.ScanLine[i];
          for j := 0 to pred(xSize) do
            line[j] := PaletteIndexOf(Bmp.Canvas.Pixels[j, i]);
        end;
    end;
}

  // TMapTexture

  constructor TMapTexture.Create(Bmp : TBitmap; txCount : integer);
    begin
      inherited Create;
      fBmp := Bmp;
      GetMem(fRelations, txCount*sizeof(fRelations[0]));
      FillChar(fRelations[0], txCount*sizeof(fRelations[0]), byte(trNone));
    end;

  destructor TMapTexture.Destroy;
    begin
      fBmp.Free;
      FreeMem(fRelations);
    end;

  // TLandClassInfo

  constructor TLandClassInfo.Create(IniPath : string);
    var
      i       : TLandType;
      Ini     : TIniFile;
      ExePath : string;
    begin
      Ini := TIniFile.Create(IniPath);
      try
        ExePath := ExtractFilePath(Application.ExeName);
        fSourcePath  := ExePath + Ini.ReadString('Paths', 'Source', '');
        fOutputPath  := ExePath + Ini.ReadString('Paths', 'ImgOutput', '');
        fClassesPath := ExePath + Ini.ReadString('Paths', 'ClassOutput', '');

        fCenterCount := Ini.ReadInteger('Variation', 'cntCenter', 1);
        fStrghtCount := Ini.ReadInteger('Variation', 'cntStraight', 1);
        fCornerCount := Ini.ReadInteger('Variation', 'cntCorner', 1);

        fTextures := TCollection.Create(0, rkBelonguer);
        LoadTextures(Ini);

        fTemplate := TBitmap.Create;
        LoadBitmap(fSourcePath + 'Template.bmp', fTemplate);

        for i := low(i) to high(i) do
          fClassTypes[i] := TCollection.Create(0, rkBelonguer);
        LoadVectors(
          Ini.ReadString('Source', 'Center', ''),
          Ini.ReadString('Source', 'Straight', ''),
          Ini.ReadString('Source', 'Corner', ''));
      finally
        Ini.Free;
      end;
    end;

  destructor TLandClassInfo.Destroy;
    var
      i : TLandType;
    begin
      fTemplate.Free;
      fTextures.Free;
      for i := low(i) to high(i) do
        fClassTypes[i].Free;
    end;

  procedure TLandClassInfo.LoadTextures(Ini : TIniFile);
    var
      Bmp     : TBitmap;
      Texture : TMapTexture;
      i, j    : integer;
      Count   : integer;
      txRel   : string;
    begin
      Count := Ini.ReadInteger('Textures', 'Count', 0);
      for i := 0 to pred(Count) do
        begin
          Bmp := TBitmap.Create;
          if LoadBitmap(fSourcePath + 'Texture.' + IntToStr(i) + '.bmp', Bmp)
            then
              begin
                Texture := TMapTexture.Create(Bmp, Count);
                txRel := Ini.ReadString('Textures', 'Texture' + IntToStr(i), '');
                j := 1;
                while (j <= Count) and (j <= length(txRel)) do
                  begin
                    case txRel[j] of
                      'm' :
                        Texture.fRelations[j-1] := trMix;
                      'd' :
                        Texture.fRelations[j-1] := trDither;
                      'b' :
                        Texture.fRelations[j-1] := trBorder;
                      else
                        Texture.fRelations[j-1] := trNone;
                    end;
                    inc(j);
                  end;
                fTextures.Insert(Texture);
              end
            else Bmp.Free;
        end;
    end;

  procedure TLandClassInfo.LoadVectors(center, straight, corner : string);
    var
      Bmp, Tmp  : TBitmap;
      SearchRec : TSearchRec;
      x, y      : integer;
      //St        : TStream;
    begin
      // Center
      Bmp := TBitmap.Create;
      if LoadBitmap(fSourcePath + center, Bmp)
        then
          begin
            //St  := TFileStream.Create('d:\temp\center.bmp', fmCreate);
            Tmp := ConvertToIsometric(fTemplate, Bmp);
            //Tmp.SaveToStream(St);
            //St.Free;
            fClassTypes[ldtCenter].Insert(Tmp);
          end;
      Bmp.Free;

      // Straight
      if FindFirst(fSourcePath + straight, faArchive, SearchRec) = 0
        then
          repeat
            Bmp := TBitmap.Create;
            if LoadBitmap(fSourcePath + SearchRec.Name, Bmp)
              then
                begin
                  StraightGradient(Bmp);
                  // East
                  fClassTypes[ldtE].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // South
                  RotateBitmap(Bmp);
                  fClassTypes[ldtS].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // West
                  RotateBitmap(Bmp);
                  fClassTypes[ldtW].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // North
                  RotateBitmap(Bmp);
                  fClassTypes[ldtN].Insert(ConvertToIsometric(fTemplate, Bmp));
                  Bmp.Free;
                end;
          until FindNext(SearchRec) <> 0;

      // Corner
      if FindFirst(fSourcePath + corner, faArchive, SearchRec) = 0
        then
          repeat
            Bmp    := TBitmap.Create;
            if LoadBitmap(fSourcePath + SearchRec.Name, Bmp)
              then
                begin
                  // Apply gradient
                  RadialGradient(Bmp);
                  // Northeast
                  fClassTypes[ldtNEo].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Southeast
                  RotateBitmap(Bmp);
                  fClassTypes[ldtSEo].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Southwest
                  RotateBitmap(Bmp);
                  fClassTypes[ldtSWo].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Northwest
                  RotateBitmap(Bmp);
                  fClassTypes[ldtNWo].Insert(ConvertToIsometric(fTemplate, Bmp));

                  Bmp.Free;

                  Bmp := TBitmap.Create;
                  LoadBitmap(fSourcePath + SearchRec.Name, Bmp);

                  // swap colors
                  for x := 0 to pred(Bmp.Width) do
                    for y := 0 to pred(Bmp.Height) do
                      case ColorToRGB(Bmp.Canvas.Pixels[x, y]) of
                        clBlack : Bmp.Canvas.Pixels[x, y] := clWhite;
                        clWhite : Bmp.Canvas.Pixels[x, y] := clBlack;
                      end;

                  // Apply gradient
                  RadialGradient(Bmp);
                  // Northeast
                  fClassTypes[ldtSWi].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Southeast
                  RotateBitmap(Bmp);
                  fClassTypes[ldtNWi].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Southwest
                  RotateBitmap(Bmp);
                  fClassTypes[ldtNEi].Insert(ConvertToIsometric(fTemplate, Bmp));
                  // Northwest
                  RotateBitmap(Bmp);
                  fClassTypes[ldtSEi].Insert(ConvertToIsometric(fTemplate, Bmp));
                end;
          until FindNext(SearchRec) <> 0;
    end;

  function LandTypeToStr(aType : TLandType) : string;
    begin
      case atype of
        ldtCenter : result := 'Center';
        ldtN   : result := 'N';
        ldtE   : result := 'E';
        ldtS   : result := 'S';
        ldtW   : result := 'W';
        ldtNEo : result := 'NEo';
        ldtSEo : result := 'SEo';
        ldtSWo : result := 'SWo';
        ldtNWo : result := 'NWo';
        ldtNEi : result := 'NEi';
        ldtSEi : result := 'SEi';
        ldtSWi : result := 'SWi';
        ldtNWi : result := 'NWi';
      end;
    end;

  procedure SaveIniClass(path : string; Id : byte; Color  : TColor; BmpName : string);
    var
      Ini : TextFile;
    begin
      AssignFile(Ini, path + 'land.' + IntToStr(Id) + '.ini');
      Rewrite(Ini);
      Writeln(Ini, '[General]');
      Writeln(Ini, 'Id=' + IntToStr(Id));
      Writeln(Ini, 'MapColor=' + IntToStr(integer(Color))); // >>
      Writeln(Ini, '[Images]');
      Writeln(Ini, '64x32=' + BmpName);
      CloseFile(Ini);
    end;

  procedure TLandClassInfo.SaveClass(name : string; Bmp : TBitmap; clType : TLandType; txA, txB, varIdx : integer; TraceVert : boolean);
    var
      Id : byte;
      PalBmp : TBitmap;
      Color  : TColor;
      BmpName : string;

    function VisualText : string;
      begin
        result := TextureNames[TLandClass(txA)] + LandTypeToStr(clType) + IntToStr(varIdx);
      end;

    function InvertLand( Id : TLandVisualClassId ) : TLandVisualClassId;
      var
        c : TLandClass;
        t : TLandType;
        v : byte;
      begin
        c := LandClassOf( Id );
        t := LandTypeOf( Id );
        v := LandVarOf( Id );
        case t of
          ldtNEo : t := ldtNEi;
          ldtSEo : t := ldtSEi;
          ldtSWo : t := ldtSWi;
          ldtNWo : t := ldtNWi;
          ldtNEi : t := ldtNEo;
          ldtSEi : t := ldtSEo;
          ldtSWi : t := ldtSWo;
          ldtNWi : t := ldtNWo;
        end;
        result := LandIdOf( c, t, v );
      end;

    begin
      //Id := Land.LandIdOf(TLandClass(txA), TLandClass(txB), clType, varIdx);
      Id := Land.LandIdOf(TLandClass(txA), clType, varIdx); // TLandClass(txA), TLandClass(txB), clType, varIdx);
      //Id := InvertLand( Id );
      PalBmp := ConvertToPalettedBitmap(Bmp, false, Color);
      BmpName := 'land.' + IntToStr(Id) + '.' + VisualText + '.bmp';
      SaveBitmap(fOutputPath + BmpName, PalBmp);
      SaveIniClass(fClassesPath, Id, Color, BmpName);
      // PalBmp.Free;
    end;

  procedure TLandClassInfo.GenCenters;
    var
      i      : integer;
      txIdx  : integer;
      Templ  : TBitmap;
      Bmp    : TBitmap;
    begin
      // Center
      for i := 1 to fCenterCount do
        begin
          Templ := TBitmap(fClassTypes[ldtCenter][0]);
          for txIdx := 0 to pred(fTextures.Count) do
            begin
              Bmp := MixTextures(Templ, TMapTexture(fTextures[txIdx]).fBmp, TMapTexture(fTextures[txIdx]).fBmp, 100, 100, false);
              SaveClass('center', Bmp, ldtCenter, txIdx, txIdx, i-1, true);
              Bmp.Free;
            end;
        end;
    end;

  procedure TLandClassInfo.GenOtherTypes;
    var
      txCount  : integer;
      txAIdx   : integer;
      txBIdx   : integer;
      varIdx   : integer;
      Templ    : TBitmap;
      Bmp      : TBitmap;
      ldType   : TLandType;
      TextureA : TMapTexture;
      TextureB : TMapTexture;
      BorderA  : byte;
      BorderB  : byte;
      Dither   : boolean;
    begin
      // Center
      txCount := fTextures.Count;
      for txAIdx := pred(txCount) downto 0 do
        begin
          TextureA := TMapTexture(fTextures[txAIdx]);
          for txBIdx := pred(txCount) downto 0 do
            begin
              TextureB := TMapTexture(fTextures[txBIdx]);
              if TextureA.fRelations[txBIdx] <> trNone
                then
                  begin
                    if TextureA.fRelations[txBIdx] = trBorder
                      then BorderA := 70
                      else BorderA := 100;
                    if TextureB.fRelations[txAIdx] = trBorder
                      then BorderB := 70
                      else BorderB := 100;
                    Dither := TextureA.fRelations[txBIdx] = trDither;
                    // Straights
                    for ldType := ldtN to ldtW do
                      for varIdx := 0 to pred(fStrghtCount) do
                        begin
                          Templ := TBitmap(fClassTypes[ldType][Random(fClassTypes[ldType].Count)]);
                          Bmp   := MixTextures(Templ, TextureA.fBmp, TextureB.fBmp, BorderA, BorderB, Dither);
                          SaveClass('straight', Bmp, ldType, txAIdx, txBIdx, varIdx, true);
                          Bmp.Free;
                        end;
                    // Corners
                    for ldType := ldtNEo to ldtNWi do
                      for varIdx := 0 to pred(fCornerCount) do
                        begin
                          Templ := TBitmap(fClassTypes[ldType][Random(fClassTypes[ldType].Count)]);
                          Bmp   := MixTextures(Templ, TextureA.fBmp, TextureB.fBmp, BorderA, BorderB, Dither);
                          SaveClass('corner', Bmp, ldType, txAIdx, txBIdx, varIdx, false);
                          Bmp.Free;
                        end;
                  end;
            end;
        end;
    end;


  procedure TLandClassInfo.Run;
    begin
      GenCenters;
      GenOtherTypes;
    end;

initialization

  Randomize;

end.
