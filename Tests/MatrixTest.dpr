{$APPTYPE CONSOLE}

program MatrixTest;

uses
  Classes,
  LargeMatrix in '..\Utils\Misc\LargeMatrix.pas',
  Matrix in '..\Utils\Misc\Matrix.pas',
  MapCompress in '..\Kernel\MapCompress.pas';

type
  PSingleArray = ^TSingleArray;
  TSingleArray = array[0..0] of single;

type
  TTestMatrix =
    class( TInterfacedObject, IMatrix )
      private
        fRows, fCols : integer;
        fItems       : PSingleArray;
      private
        function  getCols : integer;
        function  getRows : integer;
        procedure setDimensions( n, m : integer );
        function  getElement   ( i, j : integer ) : single;
        procedure setElement   ( i, j : integer; value : single );
    end;

  function TTestMatrix.getCols : integer;
    begin
      result := fCols;
    end;

  function TTestMatrix.getRows : integer;
    begin
      result := fRows;
    end;

  procedure TTestMatrix.setDimensions( n, m : integer );
    begin
      if fItems <> nil
        then freemem( fItems, fRows*fCols*sizeof(single) );
      fCols := m;
      fRows := n;
      getmem( fItems, fRows*fCols*sizeof(single) );
    end;

  function TTestMatrix.getElement( i, j : integer ) : single;
    begin
      result := fItems[i*fCols + j];
    end;

  procedure TTestMatrix.setElement( i, j : integer; value : single );
    begin
      fItems[i*fCols + j] := value;
    end;


{$R *.RES}

const
  Rows = 32;
  Cols = 32;

var
  i, j  : integer;
  // M    : TSingleLargeMatrix;
  K     : IMatrix;
  image : TMapImage;
begin
  K := TTestMatrix.Create;
  K.setDimensions( Rows, Cols );
  for i := 0 to Rows - 1 do
    for j := 0 to Cols - 1 do
      K.setElement( i, j, i + j );
  for i := 0 to Rows - 1 do
    begin
      for j := 0 to Cols - 1 do
        write( round(K.getElement(i, j)):4, ' ' );
      writeln;
    end;
  CompressMap( K, Rect( 0, 0, Cols - 1, Rows - 1 ), image, 5 );
  DeCompressMap( K, image );
  writeln;
  writeln( 'Decompressed map:' );
  for i := 0 to Rows - 1 do
    begin
      for j := 0 to Cols - 1 do
        write( round(K.getElement(i, j)):4, ' ' );
      writeln;
    end;
  // writeln( image );
  readln;


  {
  M := TSingleLargeMatrix.Create( Rows, Cols, 20 );
  writeln( 'llenando matriz...' );
  for i := 0 to Rows - 1 do
    for j := 0 to Cols - 1 do
      M[i, j] := i + j;
  writeln( 'matriz llena!' );
  writeln;
  for i := 0 to Rows - 1 do
    begin
      for j := 0 to Cols - 1 do
        write( round(M[i, j]):4, ' ' );
      writeln;
    end;
  readln;
  M.Free;
  }
end.



