unit FIFOUtils;

interface

  uses
    Windows, FIFOIntf;

  function CreateFIFO : IFIFO;

implementation

  uses
    FIFO;

  type
    TFIFOAdapter =
      class(TInterfacedObject, IFIFO)
        public
          constructor Create;
          destructor  Destroy; override;
        private // IFIFO
          function Write(const Data; Size : integer) : hResult; stdcall;
          function Read(out Data; Size : integer; out Actual : integer) : hResult; stdcall;
          function Peek(out Data; Size : integer; out Actual : integer) : hResult; stdcall;
          function Advance(Size : integer; out Actual : integer) : hResult; stdcall;
          function Clear : hResult; stdcall;
          function GetSize(out Size : integer) : integer; stdcall;
        private
          fFIFO : TFIFO;
      end;

  constructor TFIFOAdapter.Create;
    begin
      inherited;
      fFIFO := TFIFO.Create;
    end;

  destructor TFIFOAdapter.Destroy;
    begin
      fFIFO.Free;
      inherited;
    end;

  function TFIFOAdapter.Write(const Data; Size : integer) : hResult;
    begin
      fFIFO.Write(Data, Size);
      Result := S_OK;
    end;

  function TFIFOAdapter.Read(out Data; Size : integer; out Actual : integer) : hResult;
    begin
      Actual := fFIFO.Read(Data, Size);
      Result := S_OK;
    end;

  function TFIFOAdapter.Peek(out Data; Size : integer; out Actual : integer) : hResult;
    begin
      Actual := fFIFO.Peek(Data, Size);
      Result := S_OK;
    end;

  function TFIFOAdapter.Advance(Size : integer; out Actual : integer) : hResult;
    begin
      Actual := fFIFO.Advance(Size);
      Result := S_OK;
    end;

  function TFIFOAdapter.Clear : hResult;
    begin
      fFIFO.Clear;
      Result := S_OK;
    end;

  function TFIFOAdapter.GetSize(out Size : integer) : integer;
    begin
      Size := fFIFO.Size;
      Result := S_OK;
    end;

  function CreateFIFO : IFIFO;
    begin
      Result := TFIFOAdapter.Create;
    end;

end.
 