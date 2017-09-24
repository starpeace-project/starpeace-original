unit IsometricMapTypes;

interface

uses
  Windows, Graphics;

type
  hfocus = integer;

type
  IIsometricMapper =
    interface
      function  GetRows : integer;
      function  GetCols : integer;
      function  GetColor(i, j : integer) : TColor;
      procedure TransformCoords(var i, j : integer);
    end;


implementation


end.
