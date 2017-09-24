unit AsymSoftKeys;

interface

  type
    T2x2Matrix   = array[0..1,0..1] of single;
    TSoftKeyType = (skt64, skt128, skt256);

  procedure GenPair(var M, IM : T2x2Matrix; res : integer);
  function  MultPair(A, B : T2x2Matrix) : T2x2Matrix;

  function GenSoftAsymKeys(var pubKey, privKey : string; kType : TSoftKeyType) : boolean;

implementation

  uses
    SysUtils;

  procedure GenPair(var M, IM : T2x2Matrix; res : integer);
    var
      det        : integer;
      a, b, c, d : integer;
    begin
      repeat
        a   := random(res); // a
        b   := random(res); // b
        c   := random(res); // c
        d   := random(res); // d
        det := a*d - c*b;
      until {(det = 8) or (det = 16) or (det = 32) or} (det = 64);
      M [0, 0] := a;
      M [0, 1] := b;
      M [1, 0] := c;
      M [1, 1] := d;
      IM[0, 0] := d/det;
      IM[0, 1] := -b/det;
      IM[1, 0] := -c/det;
      IM[1, 1] := a/det;
    end;

  function MultPair(A, B : T2x2Matrix) : T2x2Matrix;
    begin
      result[0, 0] := A[0, 0]*B[0, 0] + A[0, 1]*B[1, 0];
      result[0, 1] := A[0, 0]*B[0, 1] + A[0, 1]*B[1, 1];
      result[1, 0] := A[1, 0]*B[0, 0] + A[1, 1]*B[1, 0];
      result[1, 1] := A[1, 0]*B[0, 1] + A[1, 1]*B[1, 1];
    end;

  function T2x2MatrixToStr(M : T2x2Matrix) : string;
    begin
      result :=
        FloatToStr(M[0, 0]) + ',' +
        FloatToStr(M[0, 1]) + ',' +
        FloatToStr(M[1, 0]) + ',' +
        FloatToStr(M[1, 1]);
    end;

  function sktLength(kType : TSoftKeyType) : integer;
    begin
      case kType of
        skt64  : result := 4;
        skt128 : result := 8;
        skt256 : result := 16;
        else result := 16;
      end;
    end;

  function GenSoftAsymKeys(var pubKey, privKey : string; kType : TSoftKeyType) : boolean;
    var
      len, i : integer;
      A, B   : T2x2Matrix;
    begin
      pubKey  := '';
      privKey := '';
      len     := sktLength(kType);
      GenPair(A, B, 64);
      pubKey  := T2x2MatrixToStr(A);
      privKey := T2x2MatrixToStr(B);
      for i := 2 to len do
        begin
          GenPair(A, B, 64);
          pubKey  := pubKey  + ',' + T2x2MatrixToStr(A);
          privKey := privKey + ',' + T2x2MatrixToStr(B);
        end;
      result := true;
    end;


end.
