unit FileFolders;

interface

  uses
    Classes, SysUtils, Windows, MappedFiles;

  // Internal structure
  type
    PFileFolderHeader = ^TFileFolderHeader;
    TFileFolderHeader =
      record
        Count  : integer;
        Offset : integer;
      end;

   type
     PFileOffsetList = ^TFileOffsetList;
     TFileOffsetList = array[0..0] of integer;

  // TFileFolder
  type
    TFileFolder =
      class
        protected
          fMappedFile : TMemoryMappedFile;
          fData       : PFileFolderHeader;

          function GetFileCount : integer;
          function GetFileAddr( Value : integer ) : pointer;
          function GetFileSize( Value : integer ) : integer;

        public
          constructor Create( const aFilename : string );
          destructor  Destroy;                                                                   override;

          property FileAddr[ Indx : integer ] : pointer read GetFileAddr; default;
          property FileSize[ Indx : integer ] : integer read GetFileSize; 
          property FileCount : integer                  read GetFileCount;
      end;

  // TFileFolderCreator
  type
    TFileFolderCreator =
      class
        protected
          fFilename : string;
          fList     : TStringList;

        public
          constructor Create( const aFilename : string );
          destructor  Destroy;                                                                   override;

          function  AddFile( const aFilename : string ) : integer;                               virtual;
          procedure Save;                                                                        virtual;

          property List : TStringList read fList;
      end;

implementation

  // TFileFolder

  constructor TFileFolder.Create( const aFilename : string );
    begin
      inherited Create;

      fMappedFile := TMemoryMappedFile.Create( aFilename, usgReadOnly );
      fData       := fMappedFile.Address;
    end;

  destructor TFileFolder.Destroy;
    begin
      fMappedFile.Free;

      inherited;
    end;

  function TFileFolder.GetFileCount : integer;
    begin
      Result := fData.Count;
    end;

  function TFileFolder.GetFileSize( Value : integer ) : integer;
    begin
      with fData^ do
        if Value = FileCount - 1
          then Result := Offset - PFileOffsetList( pchar(fData) + Offset )[Value]
          else Result := PFileOffsetList( pchar(fData) + Offset )[Value + 1] - PFileOffsetList( pchar(fData) + Offset )[Value];
    end;

  function TFileFolder.GetFileAddr( Value : integer ) : pointer;
    begin
      Result := pchar(fData) + PFileOffsetList( pchar(fData) + fData.Offset )[Value];
    end;

  // TFileFolderCreator

  constructor TFileFolderCreator.Create( const aFilename : string );
    begin
      inherited Create;

      fFilename := aFilename;
      fList     := TStringList.Create;
    end;

  destructor TFileFolderCreator.Destroy;
    begin
      fList.Free;

      inherited;
    end;

  function TFileFolderCreator.AddFile( const aFilename : string ) : integer;
    begin
      Result := List.Add( aFilename );
    end;

  procedure TFileFolderCreator.Save;
    var
      Stream     : TStream;
      FileStream : TStream;
      i          : integer;
      FileOfs    : integer;
      PadCount   : integer;
      Header     : TFileFolderHeader;
    begin
      Stream := TFileStream.Create( fFilename, fmCreate or fmShareExclusive );
      with List, Stream do
        try
          // Reserve space for header
          Seek( sizeof( Header ), soFromBeginning );

          // Now save all files, aligning them to a dword boundary
          for i := 0 to Count - 1 do
            begin
              FileStream := TFileStream.Create( List[i], fmOpenRead or fmShareDenyWrite );
              try
                PadCount :=  4 - ( Stream.Position mod 4 );
                if PadCount <> 4 // Padding needed?
                  then Seek( PadCount, soFromCurrent );

                List.Objects[i] := TObject( Stream.Position );
                CopyFrom( FileStream, FileStream.Size );
              finally
                FileStream.Free;
              end;
            end;

          with Header do
            begin
              Count  := List.Count;
              Offset := Stream.Position;
            end;

          // Now save indexes  
          for i := 0 to List.Count - 1 do
            begin
              FileOfs := integer( Objects[i] );
              WriteBuffer( FileOfs, sizeof( FileOfs ) );
              Objects[i] := nil;
            end;

          // Now save header
          Seek( 0, soFromBeginning );  // Go back
          WriteBuffer( Header, sizeof( Header ) );
          
        finally
          Stream.Free;
        end;
    end;

end.
