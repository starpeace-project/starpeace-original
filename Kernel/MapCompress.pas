unit MapCompress;

interface

  uses
    Matrix, Classes, Windows;

  type
    any = 0..0;
    TMapSection = array[any] of single;
    TMapImage   = string;

    function CompressMap  ( Matrix : IMatrix; Area : TRect; out image : TMapImage; Resolution : integer ) : boolean;
    function DecompressMap( Matrix : IMatrix; const image : TMapImage ) : boolean;

implementation

  uses
    CompStringsParser, SysUtils;

  const
    LineSep = ':';
    PairSep = ',';
    DataSep = '=';

  const
    Scale = 1000;

  function CompressMap( Matrix : IMatrix; Area : TRect; out image : TMapImage; Resolution : integer ) : boolean;

    function ConvertValue( val : single ) : integer;
      begin
        result := Resolution*(round(Scale*val) div Resolution)
      end;

    var
      x, y      : integer;
      value     : integer;
      lastvalue : integer;
      count     : integer;
      line      : string;
    begin
      try
        image := IntToStr(Area.Bottom - Area.Top + 1) + LineSep + IntToStr(Area.Right - Area.Left + 1) + LineSep;
        for y := Area.Top to Area.Bottom do
          begin
            line  := '';
            lastvalue := ConvertValue( Matrix.getElement(y, Area.Left) );
            count := 0;
            for x := Area.Left to Area.Right do
              begin
                value := ConvertValue( Matrix.getElement(y, x) );
                if value <> lastvalue
                  then
                    begin
                      line      := line + IntToStr(lastvalue) + DataSep + IntToStr(count) + PairSep;
                      lastvalue := value;
                      count     := 1;
                    end
                  else inc( count );
              end;
            line  := line  + IntToStr(lastvalue) + DataSep + IntToStr(count) + PairSep;
            image := image + line + LineSep;
          end;
        result := true;
      except
        image  := '';
        result := false;
      end;
    end;

  function DecompressMap( Matrix : IMatrix; const image : TMapImage ) : boolean;
    var
      p      : integer;
      p2, p3 : integer;
      m, n   : integer;
      i, j   : integer;
      line   : string;
      pair   : string;
      val    : integer;
      count  : integer;
      error  : boolean;
    begin
      try
        p := 1;
        n := StrToInt(GetNextStringUpTo( image, p, LineSep ));
        m := StrToInt(GetNextStringUpTo( image, p, LineSep ));
        Matrix.setDimensions( n, m );
        i := 0;
        j := 0;
        error := false;
        repeat
          line := GetNextStringUpTo( image, p, LineSep );
          if line <> ''
            then
              begin
                p2 := 1;
                repeat
                  pair := GetNextStringUpTo( line, p2, PairSep );
                  if pair <> ''
                    then
                      begin
                        p3    := 1;
                        try
                          val := StrToInt( GetNextStringUpTo( pair, p3, DataSep ) );
                        except
                          val := 0;
                        end;
                        try
                          count := StrToInt( GetNextStringUpTo( pair, p3, DataSep ) );
                        except
                          count := 0;
                          error := true;
                        end;
                        while count > 0 do
                          begin
                            Matrix.setElement( i, j, val/Scale );
                            inc( j );
                            dec( count );
                          end;
                      end
                    else raise Exception.Create( 'Map image corrupted!' );
                until p2 >= length(line);
                inc( i );
                j := 0;
              end
            else raise Exception.Create( 'Map image corrupted!' );
        until p >= length(image);
        result := not error;
      except
        result := false;
      end;
    end;

end.

