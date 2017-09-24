unit ImageLoaders;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Classes, Windows, Dibs, ListUtils;

  type
    TImageData = class;
    CImageData = class of TImageData;

    CImages = class of TImages;
    TImages =
      class
        protected
          fStream               : TStream;
          fFirstHeaderStreamPos : integer;
          fWidth                : integer;
          fHeight               : integer;
          fBitCount             : integer;
          fDibHeader            : PDib;
          fImageCount           : integer;
          fImages               : TObjectList;

          function GetImageClass : CImageData;                                                                                 virtual; abstract;
          function GetImageCount : integer;                                                                                    virtual; abstract;
          function GetDibHeader : PDib;                                                                                        virtual; abstract;
          function GetImage( Indx : integer ) : TImageData;                                                                    virtual;
          function GetHeaderStreamPos( Indx : integer ) : integer;                                                             virtual;
          function GetImageStreamPos( Indx : integer ) : integer;                                                              virtual;

        public
          constructor Create;
          destructor  Destroy;                                                                                                 override;

        public
          // NOTE: It's responsibility of this method to fill the fWidth, fHeight, fBitCount & fImageCount fields
          procedure LoadFromStream( aStream : TStream );                                                                       virtual;
          procedure LoadImages;                                                                                                virtual;

        public
          property Width : integer                             read fWidth;
          property Height : integer                            read fHeight;
          property BitCount : integer                          read fBitCount;
          property DibHeader : PDib                            read GetDibHeader;
          property ImageCount : integer                        read GetImageCount;
          property Image[ Indx : integer ] : TImageData        read GetImage; default;
          property Stream : TStream                            read fStream;
          property HeaderStreamPos[ Indx : integer ] : integer read GetHeaderStreamPos;
          property ImageStreamPos[ Indx : integer ] : integer  read GetImageStreamPos;
      end;

    TImageData =
      class
        protected
          fOwner               : TImages;
          fImageStreamPos      : integer;
          fNextHeaderStreamPos : integer;
          fStream              : TStream;
          fDibHeader           : PDib;
          fWidth               : integer;
          fHeight              : integer;
          fBitCount            : integer;
          fLocalPalette        : boolean;

          function GetDibHeader : PDib;                                                                                        virtual; abstract;
          function GetOrigin : TPoint;                                                                                         virtual; abstract;
          function GetDelay : integer;                                                                                         virtual; abstract;
          function GetTransparent : integer;                                                                                   virtual; abstract;
          function GetDisposal : integer;                                                                                      virtual; abstract;

        public
          property NextHeaderStreamPos : integer read fNextHeaderStreamPos;
          property ImageStreamPos : integer      read fImageStreamPos;

        public
          procedure   Decode( Dest : pointer; DestWidth : integer );                                                           virtual; abstract;
          constructor Create( anOwner : TImages );                                                                             virtual;

        public
          property Owner : TImages          read fOwner;
          property Stream : TStream         read fStream;
          property Origin : TPoint          read GetOrigin;
          property Width : integer          read fWidth;
          property Height : integer         read fHeight;
          property BitCount : integer       read fBitCount;
          property LocalPalette : boolean   read fLocalPalette;
          property DibHeader : PDib         read GetDibHeader;
          property Delay : integer          read GetDelay;
          property Transparent : integer    read GetTransparent;
          property Disposal : integer       read GetDisposal;
      end;

  // This is the guy we all must call: =============================================

  function GetImageLoader( aStream : TStream; Info : pointer ) : TImages;

  // Registration stuff:

  type
    TGetImageLoader = function( aStream : TStream; Info : pointer ) : TImages;

  // The list of image loaders is sorted by overhead index, so that loaders with less overhead are
  // checked first. You should also register the most used image loaders with a smaller index
  //
  // Ex. If GIF is the main image format of your application you should do something like this:
  //     RegisterLoader( GetGifLoader, ovLoadSignature + 10 );
  //     RegisterLoader( GetBmpLoader, ovLoadSignature + 20 );
  //
  const // Basic overhead indices
    ovLoadSignature  = $0000; // By simple signature inspection, correct image loader can be determined
    ovLoadHeader     = $1000; // The whole header has to be read
    ovLoadFirstImage = $5000; // The first image
    ovLoadFullData   = $a000; // All data needs to be loaded

  procedure RegisterLoader( aGetImageLoader : TGetImageLoader; Overhead : integer );

implementation

  uses
    MemUtils;
    
  constructor TImages.Create;
    begin
      inherited;

      fImages     := TObjectList.Create;
      fImageCount := -1;
    end;

  destructor TImages.Destroy;
    begin
      fImages.Free;

      inherited;
    end;

  procedure TImages.LoadFromStream( aStream : TStream );
    begin
      fStream := aStream;
    end;

  procedure TImages.LoadImages; // !!! Esto hay que arreglarlo
    begin
      fImageCount := 0;
      while Image[fImageCount] <> nil do
        ;
    end;

  function TImages.GetHeaderStreamPos( Indx : integer ) : integer;
     begin
       case Indx of
         0 :
           Result := fFirstHeaderStreamPos;
         else
           Result := Image[Indx - 1].NextHeaderStreamPos;
       end;
     end;

  function TImages.GetImageStreamPos( Indx : integer ) : integer;
     begin
       Result := Image[Indx].ImageStreamPos;
     end;

  function TImages.GetImage( Indx : integer ) : TImageData;
    var
      i, j  : integer;
      Image : TImageData;
    begin
      assert( ( fImageCount < 0 ) or ( Indx <= fImageCount ), 'Indx out of range in TImages.GetImage!!' );
      if ( fImageCount <= Indx )
        then fImages.AssertCapacity( Indx + 1 );
      if fImages[Indx] = nil
        then
          begin
            i := Indx;
            while (i >= 0) and not Assigned( fImages[i] ) do
              dec( i );
            for j := i + 1 to Indx do
              begin
                fImages.AssertCapacity( fImages.Count + 1 );
                Stream.Position := HeaderStreamPos[j];
                try
                  Image := GetImageClass.Create( Self );
                except
                  Image := nil;
                end;
                if (Image <> nil) and (Image.BitCount = 0)  // !!! Esto es un parche, el constructor debia fallar...
                  then FreeObject( Image );
                fImages[j] := Image;
                if Image = nil // No more frames?
                  then
                    begin
                      fImageCount := j;
                      //fImages.Capacity := j;
                    end
                  else
                    if ImageCount <= j // A new frame was just loaded?
                      then fImageCount := j + 1;
              end;
          end;
      Result := TImageData( fImages[Indx] );
    end;

  //

  constructor TImageData.Create( anOwner : TImages );
    begin
      inherited Create;

      fOwner  := anOwner;
      fStream := anOwner.fStream;
    end;

  //

  var
    ImgLoaders : TPointerList;

  function GetImageLoader( aStream : TStream; Info : pointer ) : TImages;
    var
      i           : integer;
      StartingPos : integer;
    begin
      i           := 0;
      Result      := nil;
      StartingPos := aStream.Position;
      while ( i < ImgLoaders.Count ) and ( Result = nil ) do
        begin
          aStream.Position := StartingPos;
          Result           := TGetImageLoader( ImgLoaders[i] )( aStream, Info );
          inc(i);
        end;
    end;

  procedure RegisterLoader( aGetImageLoader : TGetImageLoader; Overhead : integer );
    begin
      ImgLoaders.Add( @aGetImageLoader );
      //
    end;

  procedure UnregisterAll;
    begin
      ImgLoaders.Free;
    end;

initialization
  ImgLoaders            := TPointerList.Create;
  ImgLoaders.ItemsOwned := false;

finalization
  UnregisterAll;

end.
