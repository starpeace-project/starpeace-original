unit JPGtoBMP;

interface

  uses
    Windows, Graphics;

  procedure LoadJPGToBMP( filename : string; DestBMP : TBitmap );
  procedure CopyJPGToBMP( filename : string; DestBMP : TBitmap );
  procedure TVFilter( DestBMP : TBitmap; Color : TColor );
  procedure DarkImage( DestBMP : TBitmap; perc : integer );

implementation

  uses
    jpeg, Classes;

  procedure LoadJPGToBMP( filename : string; DestBMP : TBitmap );
    var
      jpg : TJPEGImage;
    begin
      jpg := TJPEGImage.Create;
      try
        jpg.PixelFormat := jf24Bit;
        jpg.LoadFromFile( filename );
        //DestBMP.Assign( jpg );
        DestBMP.Canvas.StretchDraw( Rect(0, 0, DestBMP.Width, DestBMP.Height), jpg );
      finally
        jpg.Free;
      end;
    end;

  procedure CopyJPGToBMP( filename : string; DestBMP : TBitmap );
    var
      jpg : TJPEGImage;
    begin
      jpg := TJPEGImage.Create;
      try
        jpg.PixelFormat := jf24Bit;
        jpg.LoadFromFile( filename );
        DestBMP.Width := jpg.Width;
        DestBMP.Height := jpg.Height;
        DestBMP.Canvas.StretchDraw( Rect(0, 0, DestBMP.Width, DestBMP.Height), jpg );
      finally
        jpg.Free;
      end;
    end;

  procedure TVFilter( DestBMP : TBitmap; Color : TColor );
    var
      i : integer;
    begin
      with DestBMP.Canvas do
        begin
          Pen.Color := Color;//$00243940;//$00343924;//clGray;
          Pen.Style := psSolid;
          Pen.Width := 1;
          for i := 0 to pred(DestBMP.Height) div 2 do
            begin
              MoveTo( 0, 2*i );
              LineTo( DestBMP.Width, 2*i );
            end;
        end;
    end;

  procedure DarkImage( DestBMP : TBitmap; perc : integer );
    type
      TRGB =
        packed record
          x, b, g, r : byte;
        end;
    var
      x, y : integer;
      c    : TRGB;
    begin
      for y := 0 to pred(DestBMP.Height) do
        for x := 0 to pred(DestBMP.Width) do
          begin
            c := TRGB(DestBMP.Canvas.Pixels[x,y]);
            c.r := perc*c.r div 100;
            c.g := perc*c.g div 100;
            c.b := perc*c.b div 100;
            c.x := perc*c.x div 100;
            DestBMP.Canvas.Pixels[x,y] := TColor(c);
          end;
    end;
    
end.


