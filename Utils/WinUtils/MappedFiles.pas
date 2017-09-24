unit MappedFiles;

interface

  uses
    Windows;

  const
    usgReadOnly  = 0;
    usgReadWrite = 1;
    usgWriteCopy = 2;

  type
    TMemoryMappedFile =
      class
        public
          constructor Create( const aFileName : string; Usage : integer );
          destructor  Destroy;                                                            override;

        protected
          fFileHandle : THandle;
          fFileMap    : THandle;
          fAddress    : pointer;
          fLockCount  : integer;
          fViewAccess : integer;

          function GetAddress : pointer;

        public
          property Address : pointer read GetAddress;

          function  Lock( Offset : integer ) : pointer;
          procedure Unlock( Addr : pointer );
      end;

  resourcestring
    sCantOpenFile    = 'File to be mapped cannot be opened!!';
    sCantMapFile     = 'File cannot be mapped!!';
    sCantViewMapFile = 'Cannot get a view of mapped file!!';

implementation

  uses
    SysUtils;

  function TMemoryMappedFile.GetAddress : pointer;
    begin
      if fAddress = nil
        then fAddress := Lock( 0 );
      Result := fAddress;
    end;

  function TMemoryMappedFile.Lock( Offset : integer ) : pointer;
    begin
      Result := MapViewOfFile( fFileMap, FILE_MAP_READ, 0, 0, 0 );
      if Assigned( Result )
       then inc( fLockCount )
       else raise Exception.Create( sCantViewMapFile );
    end;

  procedure TMemoryMappedFile.Unlock( Addr : pointer );
    begin
      if UnmapViewOfFile( Addr )
        then dec( fLockCount );
    end;

  constructor TMemoryMappedFile.Create( const aFileName : string; Usage : integer );
    var
      xShareMode     : integer;
      xAccess        : integer;
      xMapProtection : integer;
    begin
      inherited Create;

      case Usage of
        usgReadOnly :
          begin
            xAccess        := GENERIC_READ;
            xShareMode     := FILE_SHARE_READ;
            xMapProtection := PAGE_READONLY;
            fViewAccess    := FILE_MAP_READ;
          end;
        usgReadWrite :
          begin
            xAccess        := GENERIC_READ or GENERIC_WRITE;
            xShareMode     := 0;
            xMapProtection := PAGE_READWRITE;
            fViewAccess    := FILE_MAP_WRITE;
          end;
        else // usgWriteCopy
          begin
            xAccess        := GENERIC_READ or GENERIC_WRITE;
            xShareMode     := FILE_SHARE_READ;
            xMapProtection := PAGE_WRITECOPY;
            fViewAccess    := FILE_MAP_COPY;
          end;
      end;

      try
        fFileHandle := CreateFile( pchar(aFilename), xAccess, xShareMode, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
        if fFileHandle = INVALID_HANDLE_VALUE
          then raise Exception.Create( sCantOpenFile )
          else
            begin
              fFileMap := CreateFileMapping( fFileHandle, nil, xMapProtection, 0, 0, nil );
              if fFileMap = 0
                then raise Exception.Create( sCantMapFile );
            end;
      except
        CloseHandle( fFileHandle );
        raise;
      end;
    end;

  destructor TMemoryMappedFile.Destroy;
    begin
      if Assigned( fAddress )
        then Unlock( fAddress );

      assert( fLockCount = 0, 'Unmatched Lock/Unlock calls!! in MappedFiles.TMemoryMappedFile.Destroy!!' );
        
      CloseHandle( fFileMap );
      CloseHandle( fFileHandle );
      inherited;
    end;

end.
