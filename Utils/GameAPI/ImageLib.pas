unit ImageLib;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Classes,
    SpriteImages, FileFolders, MemUtils, StreamUtils, ListUtils;

  type
    TImageInfo =
      class
        Image     : TSpriteImage;
        fRefCount : integer;

        constructor Create( anImage : TSpriteImage );
        procedure   Reference;
        function    Release : integer;
      end;

  type
    TImageLibrary =
      class
        protected
          fFileFolder  : TFileFolder;
          fCurrentSize : integer;
          fMaxSize     : integer;
          fImages      : TObjectList;

          function GetImage( Indx : integer ) : TSpriteImage;

        public
          constructor Create( const Filename : string );
          destructor  Destroy;                                                             override;

          function  Load( Indx : integer ) : TSpriteImage;
          procedure Release( anImage : TSpriteImage );

          procedure Allocated( Size : integer );
          procedure Freed( Size : integer );

          procedure UncacheAll;
          procedure Uncache( Size : integer );

          property CurrentSize : integer                     read fCurrentSize;
          property Images[ Indx : integer ] : TSpriteImage   read GetImage; default;
      end;

  var
    ImageLibrary : TImageLibrary = nil;

  procedure InitImageLibrary( const Filename : string );
  procedure DoneImageLibrary;

implementation

  function Kb( Size : integer ) : integer;
    begin
      Result := (Size + 1023 ) div 1024;
    end;

  // TImageInfo

  constructor TImageInfo.Create( anImage : TSpriteImage );
    begin
      inherited Create;
      Image := anImage;
    end;

  procedure TImageInfo.Reference;
    begin
      Inc( fRefCount );
    end;

  function TImageInfo.Release : integer;
    begin
      Dec( fRefCount );
      if fRefCount = 0
        then
          begin
            Result := Image.ImageSize;
            Free;
          end
        else Result := 0;
    end;

  // TImageLibrary

  constructor TImageLibrary.Create( const Filename : string );
    begin
      inherited Create;

      fFileFolder := TFileFolder.Create( Filename );
    end;

  destructor TImageLibrary.Destroy;
    begin
      fFileFolder.Free;
      
      inherited;
    end;

  function TImageLibrary.GetImage( Indx : integer ) : TSpriteImage;
    var
      Info : TImageInfo;
    begin
      Info := TImageInfo( fImages[Indx] );
      if Info <> nil
        then Result := Info.Image
        else Result := nil;
    end;

  function TImageLibrary.Load( Indx : integer ) : TSpriteImage;
    var
      Info   : TImageInfo;
      Stream : TStream;
    begin
      Info := TImageInfo( fImages[Indx] );
      if Info <> nil
        then
          with Info do
            begin
              Reference;
              Result := Image;
            end
        else
          begin
            Stream := TPointerStream.Create( fFileFolder[ Indx ], MaxInt );
            Result := TFrameImage.LoadFromStream( Stream ); // !!
            Allocated( Result.ImageSize );
            fImages[Indx] := TImageInfo.Create( Result );
          end;
    end;

  procedure TImageLibrary.Allocated( Size : integer );
    begin
      Inc( fCurrentSize, Size );
      if CurrentSize >= fMaxSize
        then Uncache( Size );
    end;

  procedure TImageLibrary.Freed( Size : integer );
    begin
      Dec( fCurrentSize, Size );
    end;

  procedure TImageLibrary.Release( anImage : TSpriteImage );
    var
      Found : boolean;
      i     : integer;
    begin
      Found := false;
      i     := 0;
      with fImages do
        while not Found and (i < Count ) do
          with TImageInfo( Items[i] ) do
            if Image = anImage
              then
                begin
                  Found := true;
                  Release;
                  if CurrentSize > fMaxSize
                    then Uncache( CurrentSize - fMaxSize + 1 );
                end
    end;

  procedure TImageLibrary.UncacheAll;
    begin
      Uncache( CurrentSize );
    end;

  procedure TImageLibrary.Uncache( Size : integer );
    var
      i : integer;
    begin
      i := fImages.Count - 1;
      while (Size > 0 ) and (i > 0 ) do
        begin
          while (TImageInfo( fImages[i] ).fRefCount <> 0 ) and (i >= 0 ) do
            dec( i );
          if i >= 0
            then
              begin
                fImages.Delete( i );
                with TSpriteImage( Images[i]  ) do
                  Freed( ImageSize );
              end;
        end;
    end;

  procedure InitImageLibrary( const Filename : string );
    begin
      ImageLibrary := TImageLibrary.Create( Filename );
    end;

  procedure DoneImageLibrary;
    begin
      FreeObject( ImageLibrary );
    end;

end.
