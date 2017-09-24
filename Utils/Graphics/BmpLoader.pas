unit BmpLoader;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, Classes,
    MemUtils, ListUtils, GDI, Dibs, ImageLoaders;

  type
    TBmpImage =
      class( TImages )
        protected
          fDibHeader : PDib;
          fImagePos : cardinal;

//          function  GetRandomAccess : boolean;                                                                                 override;
          function  GetImageClass : CImageData;                                                                                override;
          function  GetDibHeader : PDib;                                                                                       override;
          function  GetImageCount : integer;                                                                                   override;

        public
          destructor Destroy;                                                                                                  override;

          procedure LoadFromStream( Stream : TStream );                                                                        override;
      end;

    TBmpImageData =
      class( TImageData )
        protected
          function GetDibHeader : PDib;                                                                                        override;
          function GetOrigin : TPoint;                                                                                         override;
          function GetDelay : integer;                                                                                         override;
          function GetTransparent : integer;                                                                                   override;

        public
          procedure Decode( Dest : pointer; DestWidth : integer );                                                             override;
      end;

  // Registration, just do something like RegisterLoader( GetBmpLoader, Overhead );

  function GetBmpLoader( aStream : TStream; Info : pointer ) : TImages;

implementation

  // TBmpImage

{  function TBmpImage.GetRandomAccess : boolean;
    begin
      Result := true; // Ha, there's only one frame...
    end;}

  function TBmpImage.GetImageClass : CImageData;
     begin
       Result := TBmpImageData;
     end;

  function TBmpImage.GetImageCount : integer;
    begin
      Result := 1; // The bitmap format supports only one image
    end;

  function TBmpImage.GetDibHeader : PDib;
    begin
      assert( Assigned( fDibHeader ), 'Unassigned DibHeader in BmpLoader.TBmpImage.GetDibHeader!!' );
      Result := fDibHeader;
    end;

  destructor TBmpImage.Destroy;
    begin
      DibFree( fDibHeader );

      inherited;
    end;

  procedure TBmpImage.LoadFromStream( Stream : TStream );
    begin
      fDibHeader := DibReadHeader( Stream );
    end;

  // TBmpImageData

  procedure TBmpImageData.Decode( Dest : pointer; DestWidth : integer );
    begin
      Stream.Position := TBmpImage( Owner ).fImagePos;
      DibReadPixels( Stream, fDibHeader, Dest, false);
    end;

  function TBmpImageData.GetDibHeader : PDib;
    begin
      assert( Assigned( TBmpImage( Owner ).fDibHeader ),
                'Unassigned DibHeader in BmpLoader.TBmpImageData.GetDibHeader!!' );
      Result := TBmpImage( Owner ).fDibHeader;
    end;

  function TBmpImageData.GetOrigin : TPoint;
    begin
      Result := Point( 0, 0 );
    end;

  function TBmpImageData.GetDelay : integer;
    begin
      assert( false, 'Can''t call this, it''s undefined!!' ); // !!!!
      Result := high( Result );
    end;

  function TBmpImageData.GetTransparent : integer;
    begin
      assert( false, 'Can''t call this, it''s undefined!!' ); // !!!!
      Result := high( Result );
    end;

  // Registration

  function GetBmpLoader( aStream : TStream; Info : pointer ) : TImages;
    var
      fStartingPos : cardinal;
      BmpId        : array[0..1] of char;
    begin
      try
        fStartingPos := aStream.Position;
        aStream.Read( BmpId, sizeof( BmpId ) );
        if ( BmpId[0] = 'B' ) and ( BmpId[1] = 'M' )
          then
            begin
              aStream.Position := fStartingPos;
              Result := TBmpImage.Create;
              Result.LoadFromStream( aStream );
            end
          else Result := nil;
      except
        Result := nil;
      end;
    end;

end.
