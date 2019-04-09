unit LargeMatrix;

interface

  uses
    Persistent, BackupInterfaces, SyncObjs, Matrix, Windows;

  type
    any = 0..0;
    PLargeMatrixIndex = ^TLargeMatrixIndex;
    TLargeMatrixIndex = array[any] of pointer;

  type
    TLargeMatrix =
      class( TPersistent )
        public
          constructor Create( aRows, aCols : integer; aElementSize, aResolution : integer );
          destructor  Destroy; override;
        private
          fRows        : integer;
          fCols        : integer;
          fElementSize : integer;
          fResolution  : integer;
          fIndexRows   : integer;
          fIndexCols   : integer;
        public
          property Rows : integer read fRows;
          property Cols : integer read fCols;
        protected
          function  GetElement( i, j : integer ) : pointer;
          procedure SetElement( i, j : integer; value : pointer );
        private
          fIndex : PLargeMatrixIndex;
        private
          function  GetIndex( idxI, idxJ : integer ) : pointer;
          procedure SetIndex( idxI, idxJ : integer; value : pointer );
        private
          property Index[idxI, idxJ : integer] : pointer read GetIndex write SetIndex;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          fLock : TCriticalSection;
        public
          procedure Lock;
          procedure Unlock;
        public
          procedure Fill( C : byte );
      end;

    TSingleLargeMatrix =
      class( TLargeMatrix )
        public
          constructor Create( aRows, aCols : integer; aResolution : integer );
        private
          function  GetSingleElement( i, j : integer ) : single;
          procedure SetSingleElement( i, j : integer; value : single );
        public
          property Elements[i, j : integer] : single read GetSingleElement write SetSingleElement; default;
        public
          procedure IncElement( i, j : integer; delta : single );
      end;

    TByteLargeMatrix =
      class( TLargeMatrix, IMatrix )
        public
          constructor Create( aRows, aCols : integer; aResolution : integer );
        private
          function  GetByteElement( i, j : integer ) : byte;
          procedure SetByteElement( i, j : integer; value : byte );
        public
          property Elements[i, j : integer] : byte read GetByteElement write SetByteElement; default;
        public
          procedure IncElement( i, j : integer; delta : byte );
        // IMatrix
        private
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement   ( i, j : integer ) : single;
          procedure setElement   ( i, j : integer; value : single );
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

    TIntegerLargeMatrix =
      class( TLargeMatrix, IMatrix )
        public
          constructor Create( aRows, aCols : integer; aResolution : integer );
        private
          function  GetIntegerElement( i, j : integer ) : integer;
          procedure SetIntegerElement( i, j : integer; value : integer );
        public
          property Elements[i, j : integer] : Integer read GetIntegerElement write SetIntegerElement; default;
        public
          procedure IncElement( i, j : integer; delta : integer );
        // IMatrix
        private
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement   ( i, j : integer ) : single;
          procedure setElement   ( i, j : integer; value : single );
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;


  // RegisterBackup

  procedure RegisterBackup;
  

implementation

  uses
    SysUtils, Logs;


  // TLargeMatrix

  constructor TLargeMatrix.Create( aRows, aCols : integer; aElementSize, aResolution : integer );
    begin
      inherited Create;
      fRows        := aRows;
      fCols        := aCols;
      fElementSize := aElementSize;
      fResolution  := aResolution;
      fIndexRows   := fRows div fResolution + integer(fRows mod fResolution > 0);
      fIndexCols   := fCols div fResolution + integer(fCols mod fResolution > 0);
      getmem( fIndex, fIndexRows*fIndexCols*sizeof(fIndex[0]) );
      fillchar( fIndex^, fIndexRows*fIndexCols*sizeof(fIndex[0]), 0 );
      fLock := TCriticalSection.Create;
    end;

  destructor TLargeMatrix.Destroy;
    var
      idxI, idxJ : integer;
    begin
      try
        Logs.Log( 'Demolition', DateTimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fLock.Free;
      for idxI := 0 to pred(fIndexRows) do
        for idxJ := 0 to pred(fIndexCols) do
          freemem( Index[idxI, idxJ], sqr(fResolution)*fElementSize );
      freemem( fIndex, fIndexRows*fIndexCols*sizeof(fIndex[0]) );
      inherited;
    end;

  function TLargeMatrix.GetElement( i, j : integer ) : pointer;
    var
      SubMatrix : pointer;
    begin
      SubMatrix := Index[i div fResolution, j div fResolution];
      if SubMatrix <> nil
        then
          begin
            i := i mod fResolution;
            j := j mod fResolution;
            result := @(TByteArray(SubMatrix^)[i*fResolution*fElementSize + j*fElementSize]);
          end
        else result := nil;
    end;

  procedure TLargeMatrix.SetElement( i, j : integer; value : pointer );
    var
      idxI, idxJ : integer;
      SubMatrix  : pointer;
    begin
      if (i >= 0) and (i < Rows) and
         (j >= 0) and (j < Cols)
        then
          begin
            idxI := i div fResolution;
            idxJ := j div fResolution;
            SubMatrix := Index[idxI, idxJ];
            if SubMatrix = nil
              then
                begin
                  getmem( SubMatrix, sqr(fResolution)*fElementSize );
                  //Logs.Log( 'Survival', DateTimeToStr(Now) + Format('GetMem [%d, %d] (%d)', [idxI, idxJ, sqr(fResolution)*fElementSize]));
                  fillchar( SubMatrix^, sqr(fResolution)*fElementSize, 0 );
                  Index[idxI, idxJ] := SubMatrix;
                end;
            if SubMatrix <> nil
              then
                begin
                  i := i mod fResolution;
                  j := j mod fResolution;
                  move(
                    value^,
                    TByteArray(SubMatrix^)[i*fResolution*fElementSize + j*fElementSize],
                    fElementSize );
                end;
          end
        else Logs.Log( 'Survival', DateTimeToStr(Now) + ' Matrix out of bounds: ' + IntToStr(i) + ',' + IntToStr(j) );
    end;

  function TLargeMatrix.GetIndex( idxI, idxJ : integer ) : pointer;
    begin
      result := fIndex[idxI*fIndexCols + idxJ];
    end;

  procedure TLargeMatrix.SetIndex( idxI, idxJ : integer; value : pointer );
    begin
      fIndex[idxI*fIndexCols + idxJ] := value;
    end;

  procedure TLargeMatrix.LoadFromBackup( Reader : IBackupReader );
    var
      idxI, idxJ : integer;
      SubMatrix  : pointer;
    begin
      inherited;
      fRows        := Reader.ReadInteger( 'Rows', fRows );
      fCols        := Reader.ReadInteger( 'Cols', fCols );
      fElementSize := Reader.ReadInteger( 'ElementSize', fElementSize );     
      fResolution  := Reader.ReadInteger( 'Resolution', fResolution );
      fIndexRows   := fRows div fResolution + integer(fRows mod fResolution > 0);
      fIndexCols   := fCols div fResolution + integer(fCols mod fResolution > 0);
      getmem( fIndex, fIndexRows*fIndexCols*sizeof(fIndex[0]) );

      // Load Index items
      for idxI := 0 to pred(fIndexRows) do
        for idxJ := 0 to pred(fIndexCols) do
          if Reader.ReadBoolean( 'Used', false )
            then
              begin
                getmem( SubMatrix, sqr(fResolution)*fElementSize );
                Reader.ReadBuffer( 'Data', SubMatrix^, nil, sqr(fResolution)*fElementSize );
                Index[idxI, idxJ] := SubMatrix;
              end
            else Index[idxI, idxJ] := nil;

      fLock := TCriticalSection.Create;
    end;

  procedure TLargeMatrix.StoreToBackup( Writer : IBackupWriter );
    var
      idxI, idxJ : integer;
    begin
      inherited;
      Writer.WriteInteger( 'Rows', fRows );
      Writer.WriteInteger( 'Cols', fCols );
      Writer.WriteInteger( 'ElementSize', fElementSize );
      Writer.WriteInteger( 'Resolution', fResolution );

      // Store Index items
      for idxI := 0 to pred(fIndexRows) do
        for idxJ := 0 to pred(fIndexCols) do
          if Index[idxI, idxJ] <> nil
            then
              begin
                Writer.WriteBoolean( 'Used', true );
                Writer.WriteBuffer( 'Data', Index[idxI, idxJ]^, sqr(fResolution)*fElementSize );
              end
            else Writer.WriteBoolean( 'Used', false );
    end;

  procedure TLargeMatrix.Lock;
    begin
      fLock.Enter;
    end;

  procedure TLargeMatrix.Unlock;
    begin
      fLock.Leave;
    end;

  procedure TLargeMatrix.Fill( C : byte );
    var
      SubMatrix : pointer;
      i, j      : integer;
    begin
      for i := 0 to pred(fIndexRows) do
        for j := 0 to pred(fIndexCols) do
          begin
            SubMatrix := Index[i, j];
            if SubMatrix <> nil
              then fillchar( SubMatrix^, sqr(fResolution)*fElementSize, C );
          end;
    end;
    

  // TSingleLargeMatrix

  type
    psingle = ^single;

  constructor TSingleLargeMatrix.Create( aRows, aCols : integer; aResolution : integer );
    begin
      inherited Create( aRows, aCols, sizeof(single), aResolution );
    end;

  function TSingleLargeMatrix.GetSingleElement( i, j : integer ) : single;
    var
      ptr : pointer;
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            ptr := GetElement( i, j );
            if ptr <> nil
              then result := single(ptr^)
              else result := 0;
          end
        else result := 0;
    end;

  procedure TSingleLargeMatrix.SetSingleElement( i, j : integer; value : single );
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then SetElement( i, j, @value )
    end;

  procedure TSingleLargeMatrix.IncElement( i, j : integer; delta : single );
    var
      element : psingle;
    begin
      if (delta <> 0) and (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            element := GetElement( i, j );
            if element <> nil
              then element^ := element^ + delta
              else SetSingleElement( i, j, delta );
          end;
    end;


  // TByteLargeMatrix

  type
    pbyte = ^byte;

  constructor TByteLargeMatrix.Create( aRows, aCols : integer; aResolution : integer );
    begin
      inherited Create( aRows, aCols, sizeof(byte), aResolution );
    end;

  function TByteLargeMatrix.GetByteElement( i, j : integer ) : byte;
    var
      ptr : pointer;
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            ptr := inherited GetElement( i, j );
            if ptr <> nil
              then result := byte(ptr^)
              else result := 0;
          end
        else result := 0;
    end;

  procedure TByteLargeMatrix.SetByteElement( i, j : integer; value : byte );
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then inherited SetElement( i, j, @value )
    end;                         
                                                          
  procedure TByteLargeMatrix.IncElement( i, j : integer; delta : byte );
    var
      element : pbyte;
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            element := inherited GetElement( i, j );
            if element <> nil
              then element^ := element^ + delta
              else SetByteElement( i, j, delta );
          end;
    end;

  function TByteLargeMatrix.getCols : integer;
    begin
      result := fCols;
    end;

  function TByteLargeMatrix.getRows : integer;
    begin
      result := fRows;
    end;

  procedure TByteLargeMatrix.setDimensions( n, m : integer );
    begin
    end;

  function TByteLargeMatrix.getElement( i, j : integer ) : single;
    begin
      result := Elements[i, j];
    end;

  procedure TByteLargeMatrix.setElement( i, j : integer; value : single );
    begin
    end;

  function TByteLargeMatrix.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TByteLargeMatrix._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TByteLargeMatrix._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TIntegerLargeMatrix

  type
    pInteger = ^Integer;

  constructor TIntegerLargeMatrix.Create( aRows, aCols : integer; aResolution : integer );
    begin
      inherited Create( aRows, aCols, sizeof(Integer), aResolution );
    end;

  function TIntegerLargeMatrix.GetIntegerElement( i, j : integer ) : Integer;
    var
      ptr : pointer;
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            ptr := inherited GetElement( i, j );
            if ptr <> nil
              then result := Integer(ptr^)
              else result := 0;
          end
        else result := 0;
    end;

  procedure TIntegerLargeMatrix.SetIntegerElement( i, j : integer; value : Integer );
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then inherited SetElement( i, j, @value )
    end;

  procedure TIntegerLargeMatrix.IncElement( i, j : integer; delta : Integer );
    var
      element : pInteger;
    begin
      if (i >= 0) and (j >= 0) and (i < Rows) and (j < Cols)
        then
          begin
            element := inherited GetElement( i, j );
            if element <> nil
              then element^ := element^ + delta
              else SetIntegerElement( i, j, delta );
          end;
    end;

  function TIntegerLargeMatrix.getCols : integer;
    begin
      result := fCols;
    end;

  function TIntegerLargeMatrix.getRows : integer;
    begin
      result := fRows;
    end;

  procedure TIntegerLargeMatrix.setDimensions( n, m : integer );
    begin
    end;

  function TIntegerLargeMatrix.getElement( i, j : integer ) : single;
    begin
      result := Elements[i, j];
    end;

  procedure TIntegerLargeMatrix.setElement( i, j : integer; value : single );
    begin
    end;

  function TIntegerLargeMatrix.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TIntegerLargeMatrix._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TIntegerLargeMatrix._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TSingleLargeMatrix );
      BackupInterfaces.RegisterClass( TByteLargeMatrix );
    end;


end.


