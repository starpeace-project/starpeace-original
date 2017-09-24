unit RangeStrUtils;

interface

  //
  const
    rngIncluded  = 0;
    rngUnderflow = -1;
    rngOverflow  = MaxInt;

  type
    TRange =
      record
        rLow, rHigh : integer;
      end;

  type
    PRanges = ^TRanges;
    TRanges =
      record
        Count : integer;
        Items : array[0..50] of TRange;
      end;

  type
    TRangeStr =
      class
        public
          constructor Create( const aRange : string );

        protected
          fRangeStr : string;
          fRangeMin : integer;
          fRangeMax : integer;
          fRanges   : TRanges;

          procedure SetRange( const aRange : string );
          procedure ParseRangeStr;

        public
          property RangeStr : string read fRangeStr write SetRange;
          property RangeMin : integer read fRangeMin;
          property RangeMax : integer read fRangeMax;

          function  Includes( Indx : integer ) : integer;
          procedure Add( FromIndx, ToIndx : integer );

          function Next( Indx : integer ) : integer;
          function Prev( Indx : integer ) : integer;

          function Nearest( Indx : integer ) : integer;
          function NearestNext( Indx : integer ) : integer;
          function NearestPrev( Indx : integer ) : integer;
      end;

implementation

  uses
    SysUtils, StrUtils;

  function GetNextToken( const aRange : string; var aPos : integer ) : string;
    var
      i : integer;
    begin
      i := Pos( ' ', aRange, aPos + 1 );
      if i = 0
        then i := length( aRange ) + 1;
      Result := copy( aRange, aPos, i - aPos );
      aPos := i + 1;
    end;

  procedure GetTokenValues( const aRange : string; var Min, Max : integer );
    var
      i : integer;
    begin
      i := System.Pos( '-', aRange );
      if i = 0  // '18'
        then
          begin
            Min := StrToInt( aRange );
            Max := Min;
          end
        else    // '18-25'
          begin
            Min := StrToInt( LeftStr( aRange, i - 1 ) );
            Max := StrToInt( copy( aRange, i + 1, MaxInt ) );
          end;
    end;

  procedure Ranges2RangeStr( const Ranges : TRanges; var aRangeStr : string );
    var
      i : integer;
    begin
      aRangeStr := '';
      with Ranges do
        for i := 0 to Count - 1 do
          with Items[i] do
            if rLow = rHigh
              then aRangeStr := aRangeStr + IntToStr( rLow ) + ' '
              else aRangeStr := aRangeStr + IntToStr( rLow ) + '-' + IntToStr( rHigh ) + ' ';
      SetLength( aRangeStr, length( aRangeStr ) - 1 );        
    end;

  procedure AddTokenToRange( Min, Max : integer; var Ranges : TRanges );
    var
      i : integer;
    begin
      with Ranges do
        begin
          i := 0;
          while (i < Count) and (Items[i].rHigh + 1 < Min) do
            inc( i );

          if i = Count
            then
              begin // This range is higher than all previous...
                with Items[Ranges.Count] do
                  begin
                    rLow  := Min;
                    rHigh := Max;
                  end;
                inc( Count );
              end
            else
              if Min > Items[i].rHigh + 1
                then  // There is no overlapping...
                  with Items[i] do
                    begin
                      Move( Items[i], Items[i + 1], sizeof( Items[0] ) * ( Count - i + 1 ) );
                      rLow  := Min;
                      rHigh := Max;
                      inc( Count );
                    end
                else
                  with Items[i] do
                    begin
                      if rLow > Min  // Left Overlapping?
                        then rLow := Min;
                      if rHigh < Max // Right Overlapping?
                        then rHigh := Max;
                    end;
        end;
    end;

  procedure PackRanges( var Ranges : TRanges );
    var
      i      : integer;
      OldLow : integer;
    begin
      i := 0;
      with Ranges do
        while i < Count - 1 do
          if Items[i].rHigh + 1 >= Items[i + 1].rLow
            then
              begin // overlapping...
                OldLow := Items[i].rLow;
                Move( Items[i + 1], Items[i], sizeof( Items[0] ) * ( Count - i + 1 ) );
                Items[i].rLow := OldLow;
                dec( Count );
              end
            else inc( i );
    end;

  procedure RangeStr2Ranges( const aRangeStr : string; var Ranges : TRanges );
    var
      i        : integer;
      Token    : string;
      Min, Max : integer;
    begin
      Ranges.Count := 0;
      i            := 1;
      repeat
        Token := GetNextToken( aRangeStr, i );
        if Token <> ''
          then
            begin
              try
                GetTokenValues( Token, Min, Max );
                AddTokenToRange( Min, Max, Ranges );
              except
              end;
            end;
      until Token = '';
      PackRanges( Ranges );
    end;

  procedure TRangeStr.ParseRangeStr;
    begin
      RangeStr2Ranges( fRangeStr, fRanges );
      Ranges2RangeStr( fRanges, fRangeStr );
      fRangeMin := fRanges.Items[0].rLow;
      fRangeMax := fRanges.Items[fRanges.Count - 1].rHigh;
    end;

  constructor TRangeStr.Create( const aRange : string );
    begin
      RangeStr := aRange;
      inherited Create;
    end;

  procedure TRangeStr.SetRange( const aRange : string );
    begin
      fRangeStr := ReplaceStr( ReplaceStr( Trim( PackSpaces( aRange ) ), ' -', '-' ), '- ', '-' );
      ParseRangeStr;
    end;

  procedure TRangeStr.Add( FromIndx, ToIndx : integer );
    begin
      RangeStr := RangeStr + ' ' + IntToStr( FromIndx ) + '-' + IntToStr( ToIndx );
    end;

  function TRangeStr.Includes( Indx : integer ) : integer;
    var
      i : integer;
    begin
      i := Nearest( Indx );
      if i = Indx
        then Result := rngIncluded
        else Result := i;
    end;

  function TRangeStr.Next( Indx : integer ) : integer;
    begin
      Result := NearestNext( Indx + 1 );
    end;

  function TRangeStr.Prev( Indx : integer ) : integer;
    begin
      Result := NearestPrev( Indx - 1 );
    end;

  function TRangeStr.NearestNext( Indx : integer ) : integer;
    var
      i : integer;
    begin
      if (Indx < RangeMin)     // Fix needed
         or (Indx > RangeMax)  // Wrap needed
        then Result := RangeMin
        else
          with fRanges do
            begin
              i := 0;
              while Items[i].rHigh < Indx do
                inc( i );

              if ( Indx >= Items[i].rLow ) and ( Indx <= Items[i].rHigh )
                then Result := Indx
                else Result := Items[i].rLow;
            end;
    end;

  function TRangeStr.NearestPrev( Indx : integer ) : integer;
    var
      i : integer;
    begin
      if (Indx < RangeMin)     // Fix needed
         or (Indx > RangeMax)  // Wrap needed
        then Result := RangeMax
        else
          with fRanges do
            begin
              i := Count - 1;
              while Items[i].rLow > Indx do 
                dec( i );

              if ( Indx >= Items[i].rLow ) and ( Indx <= Items[i].rHigh )
                then Result := Indx
                else Result := Items[i].rHigh;
            end;
    end;

  function TRangeStr.Nearest( Indx : integer ) : integer;
    var
      NextIndx : integer;
      PrevIndx : integer;
    begin
      NextIndx := NearestNext( Indx );
      if NextIndx = Indx
        then Result := NextIndx
        else
          begin  // Too bad, Indx not in range...
            PrevIndx := NearestPrev( Indx );
            if Indx - PrevIndx > NextIndx - Indx // Get nearest of both
              then Result := PrevIndx
              else Result := NextIndx;
          end;
    end;

end.
