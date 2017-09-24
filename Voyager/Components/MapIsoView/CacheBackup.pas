unit CacheBackup;

interface

  uses
    ActiveX;

  type
    TCacheBlockMethod = procedure (row, col : integer) of object;

  type
    TCacheBackup =
      class
        private
          fCacheStorage  : IStorage;
          fCurrentStream : IStream;
        public
          constructor Create;
        public
          procedure SearchBlock(row, col : integer);
          procedure EnumCacheBlocks(Method : TCacheBlockMethod);
          procedure Flush;
          procedure WriteData(var Buffer; BufSize : integer);
          procedure ReadData(var Buffer; BufSize : integer);
      end;

implementation

  uses
    Windows, SysUtils;

  const
    CacheFileName : widestring = 'cache.five';

  // TCacheBackup

  constructor TCacheBackup.Create;
    begin
      inherited Create;
      if Failed(StgOpenStorage(pwidechar(CacheFileName), nil, STGM_READWRITE or STGM_TRANSACTED, nil, 0, fCacheStorage))
        then
          if Failed(StgCreateDocFile(pwidechar(CacheFileName), STGM_CREATE or STGM_READWRITE or STGM_TRANSACTED, 0, fCacheStorage))
            then
              raise Exception.Create('Can''''t create cache backup file');
    end;

  procedure TCacheBackup.SearchBlock(row, col: integer);
    var
      BlockStreamName : widestring;
    begin
      if fCacheStorage <> nil
        then
          begin
            BlockStreamName := IntToStr(row) + '.' + IntToStr(col);
            if Failed(fCacheStorage.OpenStream(pwidechar(BlockStreamName), nil, STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, fCurrentStream))
              then
                if Failed(fCacheStorage.CreateStream(pwidechar(BlockStreamName), STGM_CREATE or STGM_READWRITE or STGM_DIRECT or STGM_SHARE_EXCLUSIVE, 0, 0, fCurrentStream))
                  then
                    raise Exception.Create('Unknown error');
          end;
    end;

   procedure TCacheBackup.EnumCacheBlocks(Method : TCacheBlockMethod);
    var
      Enumerator : IEnumStatStg;
      CurStatStg : TStatStg;
      cfetched   : longint;
      row        : integer;
      col        : integer;
      namelen    : integer;
      rowastext  : string;
      colastext  : string;
      commapos   : integer;
    begin
      if Succeeded(fCacheStorage.EnumElements(0, nil, 0, Enumerator))
        then
          begin
            while Enumerator.Next(1, CurStatStg, @cfetched) = S_OK do
              with CurStatStg do
                begin
                  rowastext := pwcsName;
                  colastext := pwcsName;
                  namelen := length(pwcsName);
                  commapos := Pos('.', pwcsName);
                  delete(rowastext, commapos, namelen - commapos + 1);
                  row := StrToInt(rowastext);
                  delete(colastext, 1, commapos);
                  col := StrToInt(colastext);
                  Method(row, col);
                  CoTaskMemFree(pwcsName);
                end;
          end;
    end;

  procedure TCacheBackup.Flush;
    begin
      fCacheStorage.Commit(STGC_DEFAULT);
    end;

  procedure TCacheBackup.ReadData(var Buffer; BufSize: integer);
    var
      BytesRead : integer;
    begin
      fCurrentStream.Read(@Buffer, BufSize, @BytesRead);
    end;

  procedure TCacheBackup.WriteData(var Buffer; BufSize: integer);
    var
      BytesWritten : integer;
    begin
      fCurrentStream.Write(@Buffer, BufSize, @BytesWritten);
    end;

end.
