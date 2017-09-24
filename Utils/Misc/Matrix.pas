unit Matrix;

interface

  type
    IMatrix =
      interface
        function  getCols : integer;
        function  getRows : integer;
        procedure setDimensions( n, m : integer );
        function  getElement   ( i, j : integer ) : single;
        procedure setElement   ( i, j : integer; value : single );
      end;

implementation

end.


