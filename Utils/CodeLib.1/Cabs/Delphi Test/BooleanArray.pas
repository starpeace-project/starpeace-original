unit BooleanArray;

interface

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[word] of integer;

type
  TBooleanMatrix =
    class
      public
        constructor Create(aRows, aCols : integer);
        destructor  Destroy;   override;
      private
        fRows : integer;
        fCols : integer;
        fInfo : PIntegerArray;
        function  GetItem(i, j : integer) : boolean;
        procedure SetItem(i, j : integer; which : boolean);
      public
        property Rows : integer read fRows;
        property Cols : integer read fCols;
        property Item[i, j : integer] : boolean read GetItem write SetItem;   default;
        procedure Clear;
        procedure Fill;
        function  GetAddressOf(i, j : integer; out mask : integer) : PIntegerArray;
      private
        fSize : integer;
    end;

implementation

// TBooleanMatrix.

const
  cIntegerBits = 32;

constructor TBooleanMatrix.Create(aRows, aCols : integer);
  begin
    fSize := (aRows*aCols div cIntegerBits)*sizeof(integer);
    fRows := aRows;
    fCols := aCols;
    getmem(fInfo, fSize);
    Clear;
  end;

destructor TBooleanMatrix.Destroy;
  begin
    freemem(fInfo);
    inherited;
  end;

function TBooleanMatrix.GetItem(i, j : integer) : boolean;
  var
    aux : integer;
  begin
    assert((i >= 0) and (i < fRows) and (j >= 0) and (j < fCols));
    aux := i*fCols + j;
    Result := fInfo[aux div 32] and (1 shl (aux mod 32)) <> 0;
  end;

procedure TBooleanMatrix.SetItem(i, j : integer; which : boolean);
  var
    aux : integer;
  begin
    assert((i >= 0) and (i < fRows) and (j >= 0) and (j < fCols));
    aux  := i*fCols + j;
    if which
      then fInfo[aux div 32] := fInfo[aux div 32] or (1 shl (aux mod 32))
      else fInfo[aux div 32] := fInfo[aux div 32] and not (1 shl (aux mod 32));
  end;

procedure TBooleanMatrix.Clear;
  begin
    fillchar(fInfo^, fSize, 0);
  end;

procedure TBooleanMatrix.Fill;
  begin
    fillchar(fInfo^, fSize, $FF);
  end;

function TBooleanMatrix.GetAddressOf(i, j : integer; out mask : integer) : PIntegerArray;
  var
    aux : integer;
  begin
    assert((i >= 0) and (i < fRows) and (j >= 0) and (j < fCols));
    aux    := i*fCols + j;
    mask   := 1 shl (aux mod 32);
    Result := @fInfo[aux div 32]; 
  end;

end.

