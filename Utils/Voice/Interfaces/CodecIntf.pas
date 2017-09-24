unit CodecIntf;

interface

  uses
    Windows,
    CompressIntf;

  const
    E_ALLOCATE = hResult(-100);
    E_BADINFO  = hResult(-101);

  type
    ICodec =
      interface['{3A1075C0-652C-11d3-B638-00400566F3E8}']
        function GetInfo(lpInfo : pointer; out InfoSize : integer) : hResult; stdcall;
        function GetCompressor(lpInfo : pointer; InfoSize : integer; out Compressor : ICompressor) : hResult; stdcall;
        function GetDecompressor(lpInfo : pointer; InfoSize : integer; out Decompressor : IDecompressor) : hResult; stdcall;
      end;

  type
    TCodecLibProc = function : ICodec; stdcall; // 'GetCodec' prototype

  const
    CodecLibProcName = 'GetCodec';

  // We need to add something to decide which codec to use, and something to find
  // the codec based on the codec info.    
  type
    ICodecs =
      interface // Just Delphi interface, no IID
        procedure Initialize(const aPath : string);
        function  Count : integer;
        function  OpenCompressor(ndx : integer) : ICompressor;
        function  OpenDecompressor(ndx : integer) : IDecompressor;
      end;

  function Codecs : ICodecs;

implementation

  uses
    Classes, SysUtils, VCLUtils;

  const
    CodecWildCard = '*.vcc';

  function AddNameToPath(const Path, Name : string) : string;
    begin
      if (Path <> '') and (Path[length(Path)] <> '\')
        then Result := Path + '\' + Name
        else Result := Path + Name
    end;

  function IsCodec(const aLib : string) : boolean;
    var
      hLib : hInst;
    begin
      hLib := LoadLibrary(pchar(aLib));
      if hLib <> 0
        then
          try
            Result := GetProcAddress(hLib, CodecLibProcName) <> nil;
          finally
            FreeLibrary(hLib);
          end
        else Result := false;
    end;

  type
    TCodecs =
      class(TInterfacedObject, ICodecs)
        public
          constructor Create;
          destructor  Destroy; override;
        private // ICodecs
          procedure Initialize(const aPath : string);
          function  Count : integer;
          function  OpenCompressor(ndx : integer) : ICompressor;
          function  OpenDecompressor(ndx : integer) : IDecompressor;
        private
          fLibs : TStringList;
          function  GetCodec(ndx : integer) : ICodec;
          procedure FreeLibraries;
      end;

    constructor TCodecs.Create;
      begin
        inherited;
        fLibs := TStringList.Create;
      end;

    destructor TCodecs.Destroy;
      begin
        FreeLibraries;
        fLibs.Free;
        inherited;
      end;

    procedure TCodecs.Initialize(const aPath : string);
      var
        F        : TSearchRec;
        code     : integer;
        CodecLib : string;
      begin
        code := FindFirst(AddNameToPath(aPath, CodecWildCard), faAnyFile, F);
        try
          while code = 0 do
            begin
              CodecLib := AddNameToPath(aPath, F.FindData.cFileName);
              if IsCodec(CodecLib)
                then fLibs.Add(CodecLib);
              code := FindNext(F);
            end;
        finally
          FindClose(F);
        end;
      end;

    function TCodecs.Count : integer;
      begin
        Result := fLibs.Count; 
      end;

    function TCodecs.OpenCompressor(ndx : integer) : ICompressor;
      var
        aCodec    : ICodec;
        Dummy     : array[byte] of byte;
        DummySize : integer;
      begin
        Assert((ndx >= 0) and (ndx < fLibs.Count));
        aCodec := GetCodec(ndx);
        DummySize := sizeof(Dummy);
        aCodec.GetInfo(@Dummy, DummySize);
        aCodec.GetCompressor(@Dummy, DummySize, Result);
      end;

    function TCodecs.OpenDecompressor(ndx : integer) : IDecompressor;
      var
        aCodec    : ICodec;
        Dummy     : array[byte] of byte;
        DummySize : integer;
      begin
        Assert((ndx >= 0) and (ndx < fLibs.Count));
        aCodec := GetCodec(ndx);
        DummySize := sizeof(Dummy);
        aCodec.GetInfo(@Dummy, DummySize);
        aCodec.GetDecompressor(@Dummy, DummySize, Result);
      end;

    function TCodecs.GetCodec(ndx : integer) : ICodec;
      var
        Proc : TCodecLibProc;
      begin
        Assert((ndx >= 0) and (ndx < fLibs.Count));
        if fLibs.Objects[ndx] = nil
          then fLibs.Objects[ndx] := pointer(LoadLibrary(pchar(fLibs[ndx])));
        Proc := GetProcAddress(hInst(fLibs.Objects[ndx]), CodecLibProcName);
        if Proc <> nil
          then Result := Proc()
          else Result := nil; 
      end;

    procedure TCodecs.FreeLibraries;
      var
        lib : hInst;
        i   : integer;
      begin
        for i := 0 to pred(fLibs.Count) do
          begin
            lib := hInst(fLibs.Objects[i]);
            if lib <> 0
              then FreeLibrary(lib);
          end;
        fLibs.Clear;
      end;


  var
    gCodecs : ICodecs = nil;

  function Codecs : ICodecs;
    begin
      if gCodecs = nil
        then gCodecs := TCodecs.Create;
      Result := gCodecs;
    end;

initialization
finalization
  gCodecs := nil;
end.
 