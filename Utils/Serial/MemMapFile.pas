unit MemMapFile;

interface

  uses
    Windows;

  type
    TFileMap =
      class
        public
          constructor Create(const aFileName : string);
          destructor  Destroy;                                   override;
        private
          fFileHandle : THandle;
          fFileMap    : THandle;
          fAddress    : pointer;
          fSize       : integer;
        public
          property Address : pointer read fAddress;
          property Size : integer read fSize;
      end;

implementation

  uses
    SysUtils;

  constructor TFileMap.Create(const aFileName : string);
    begin
      inherited Create;

      fFileHandle := CreateFile(pchar(aFilename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0); //runerror( 7 );
      if fFileHandle = INVALID_HANDLE_VALUE
        then raise Exception.Create('Cannot open file');

      fSize := GetFileSize(fFileHandle, nil);

      try
        fFileMap := CreateFileMapping(fFileHandle, nil, PAGE_READONLY, 0, fSize, nil);
        if fFileMap = 0
         then raise Exception.Create('Cannot map file');

        try
          fAddress := MapViewOfFile(fFileMap, FILE_MAP_READ, 0, 0, 0);
          if fAddress = nil
           then raise Exception.Create('Cannot get map file pointer');
        except
          CloseHandle(fFileMap);
          raise;
        end;
      except
        CloseHandle(fFileHandle);
        raise;
      end;
    end;

  destructor TFileMap.Destroy;
    begin
      UnmapViewOfFile(fAddress);
      CloseHandle(fFileMap);
      CloseHandle(fFileHandle);
      inherited;
    end;

end.



