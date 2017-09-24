unit Rects;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows;

  function RectSize( const R : TRect) : TPoint;
  function RectFromSize( x, y : integer; const Size : TPoint) : TRect;
  function RectFromBounds( x, y : integer; Width, Height : integer ) : TRect;

  procedure MovePoint( var P : TPoint; OfsX, OfsY : integer );
  procedure MoveRect( var R : TRect; const Point : TPoint );

  function GetBoundingRect( NewPos, OldPos : TPoint; Width, Height : integer ) : TRect;

implementation

  function GetBoundingRect( NewPos, OldPos : TPoint; Width, Height : integer ) : TRect;
    begin
      with Result do
        begin
          if OldPos.X > NewPos.X
            then
              begin
                Left  := NewPos.X;
                Right := OldPos.X + Width;
              end
            else
              begin
                Left  := OldPos.X;
                Right := NewPos.X + Width;
              end;
          if OldPos.Y > NewPos.Y
            then
              begin
                Top    := NewPos.Y;
                Bottom := OldPos.Y + Height;
              end
            else
              begin
                Top    := OldPos.Y;
                Bottom := NewPos.Y + Height;
              end;
        end;
    end;

  procedure MovePoint( var P : TPoint; OfsX, OfsY : integer );
    begin
      with P do
        begin
          Inc( x, OfsX );
          Inc( y, OfsY );
        end;
    end;

  procedure MoveRect( var R : TRect; const Point : TPoint );
    begin
      OffsetRect( R, Point.X, Point.Y );
    end;

  function RectSize( const R : TRect) : TPoint;
    begin
      with Result, R do
        begin
          x := Right - Left;
          y := Bottom - Top;
        end;
    end;

  function RectFromBounds( x, y : integer; Width, Height : integer ) : TRect;
    begin
      with Result do
        begin
          Left := x;
          Right := x + Width;
          Top := y;
          Bottom := y + Height;
        end;
    end;

  function RectFromSize( x, y : integer; const Size : TPoint) : TRect;
    begin
      with Result do
        begin
          Left := x;
          Right := x + Size.X;
          Top := y;
          Bottom := y + Size.Y;
        end;
    end;

end.
