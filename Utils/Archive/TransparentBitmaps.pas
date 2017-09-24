unit TransparentBitmaps;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, CommCtrl, SysUtils, Classes,
    Graphics, Dibs;

  const
    clrDefault = CLR_DEFAULT;
    clrNone    = CLR_NONE;

  const
    dsTransparent = ILD_TRANSPARENT;
    dsNormal      = ILD_NORMAL;
    dsMask        = ILD_MASK;
    dsImage       = ILD_IMAGE;
    dsBlend       = ILD_BLEND;          // Same as Blend50
    dsBlend25     = ILD_BLEND25;
    dsBlend50     = ILD_BLEND50;
    dsOverlayMask = ILD_OVERLAYMASK;

  type
    TTransparentBitmap =
      class
        private
          fImageList        : HIMAGELIST;
          fBitmap           : HBITMAP;
          fMask             : HBITMAP;

        private
          fBackgroundColor  : TColorRef;
          fTransparentColor : TColorRef;
          fStyle            : integer;
          fWidth            : integer;
          fHeight           : integer;
          fBitCount         : integer;

          procedure SetBackgroundColor( aColor : TColorRef );
          procedure SetTransparentColor( aColor : TColorRef );
          procedure SetStyle( NewStyle : integer );

          procedure Release;

          function  AddBitmapAndMask( ImageList : HIMAGELIST; BitmapHandle : HBITMAP ) : boolean;
          function  GetImageList( aWidth, aHeight, aBitCount : integer ) : HIMAGELIST;
          procedure FreeImageList;

        protected
          procedure SetHandle( BitmapHandle : HBITMAP );
          procedure Update;

        public
          property TransparentColor : TColorRef read fTransparentColor   write SetTransparentColor;
          property BackgroundColor : TColorRef  read fBackgroundColor    write SetBackgroundColor;
          property Width : integer              read fWidth;
          property Height : integer             read fHeight;
          property BitCount : integer           read fBitCount;
          property Style : integer              read fStyle              write SetStyle;
          property Handle : HBITMAP                                      write SetHandle;

          constructor Create;                                                                  
          destructor  Destroy;                                                                 override;

          procedure LoadFromDib( DibHeader : PDib; DibBits : pointer );                        virtual;
          procedure LoadFromStream( Stream : TStream );
          procedure LoadFromResourceName( Instance : THandle; const ResourceName : string );
          procedure LoadFromResourceID( Instance : THandle; ResourceId : Integer );
          procedure LoadFromFile( const Filename : string );

          function  Empty : boolean;

          procedure DrawOnDC( dc : HDC; x, y : integer );
          procedure Draw( Canvas : TCanvas; x, y : integer );
      end;

implementation

  // Visuals

  procedure TTransparentBitmap.DrawOnDC( dc : HDC; x, y : integer );
    begin
      assert( dc <> 0, 'NULL DC in TransparentBitmaps.TTransparentBitmap.DrawOnDC!!' );
      assert( not Empty, 'Image empty in TransparentBitmaps.TTransparentBitmap.DrawOnDC!!' );

      ImageList_Draw( fImageList, 0, dc, x, y, fStyle );
    end;

  procedure TTransparentBitmap.Draw( Canvas : TCanvas; x, y : integer );
    begin
      DrawOnDC( Canvas.Handle, x, y );
    end;

  procedure TTransparentBitmap.SetStyle( NewStyle : integer );
    begin
      fStyle := NewStyle;
    end;

  procedure TTransparentBitmap.SetTransparentColor( aColor : TColorRef );
    begin
      fTransparentColor := aColor;
      Update;
    end;

  procedure TTransparentBitmap.SetBackgroundColor( aColor : TColorRef );
    begin
      assert( not Empty, 'NULL ImageList in TransparentBitmaps.TTransparentBitmap.SetBackgroundColor!!' );
      
      fBackgroundColor := aColor;
      ImageList_SetBkColor( fImageList, fBackgroundColor );
    end;

  function TTransparentBitmap.GetImageList( aWidth, aHeight, aBitCount : integer ) : HIMAGELIST;
    begin
      Result := ImageList_Create( aWidth, aHeight, ILC_MASK or aBitCount, 1, 1 );
      if Result <> 0
        then
          begin
            fWidth    := aWidth;
            fHeight   := aHeight;
            fBitCount := aBitCount;
            ImageList_SetBkColor( fImageList, fBackgroundColor );
          end;
    end;

  function TTransparentBitmap.AddBitmapAndMask( ImageList : HIMAGELIST; BitmapHandle : HBITMAP ) : boolean;
    var
      ImageInfo : TImageInfo;
    begin
      Result := false;
      if ImageList <> 0
        then
          if ImageList_AddMasked( ImageList, BitmapHandle, TransparentColor ) <> -1
            then
              begin
                ImageList_GetImageInfo( ImageList, 0, ImageInfo );
                fBitmap := ImageInfo.hbmImage;
                fMask   := ImageInfo.hbmMask;
                Result  := true;
              end;
    end;

  procedure TTransparentBitmap.FreeImageList;
    begin
      if fImageList <> 0
        then ImageList_Destroy( fImageList );
      fImageList := 0;
    end;

  procedure TTransparentBitmap.SetHandle( BitmapHandle : HBITMAP );
    var
      BitmapInfo : Windows.TBITMAP;
    begin
      FreeImageList;
      if GetObject( BitmapHandle, sizeof(BitmapInfo), @BitmapInfo ) <> 0
        then
          with BitmapInfo do
            begin
              fImageList := GetImageList( bmWidth, bmHeight, bmBitsPixel * bmPlanes );
              if not AddBitmapAndMask( fImageList, BitmapHandle )
                then FreeImageList;
            end;
    end;

  procedure TTransparentBitmap.Update;
    var
      ilHandle  : HIMAGELIST;
      ImageInfo : TImageInfo;
    begin
      if not Empty
        then
          begin
            ImageList_GetImageInfo( fImageList, 0, ImageInfo );

            ilHandle := GetImageList( Width, Height, BitCount );
            if AddBitmapAndMask( ilHandle, ImageInfo.hbmImage )
              then
                begin
                  FreeImageList;
                  fImageList := ilHandle;
                end
              else ImageList_Destroy( ilHandle );
          end;
    end;

  function TTransparentBitmap.Empty : boolean;
    begin
      Result := not Assigned( Self) or ( fImageList = 0 );
    end;

  // Object creation/destruction

  procedure TTransparentBitmap.Release;
    begin
      FreeImageList;
    end;

  constructor TTransparentBitmap.Create;
    begin
      inherited;

      fStyle           := dsNormal;
      fBackgroundColor := clrNone;
    end;

  destructor TTransparentBitmap.Destroy;
    begin
      Release;

      inherited;
    end;

  // Bitmap loading methods -----

  procedure TTransparentBitmap.LoadFromDib( DibHeader : PDib; DibBits : pointer );
    var
      bmpHandle : HBITMAP;
    begin
      bmpHandle := SectionFromDib( DibHeader, DibBits );
      try
        SetHandle( bmpHandle );
      finally
        DeleteObject( bmpHandle );
      end;
    end;

  procedure TTransparentBitmap.LoadFromStream( Stream : TStream );
    var
      Dib : PDib;
    begin
      Dib := DibLoadFromStream( Stream, false );
      try
        LoadFromDib( Dib, DibPtr( Dib ) );
      finally
        DibFree( Dib );
      end;
    end;

  procedure TTransparentBitmap.LoadFromResourceName( Instance : THandle; const ResourceName : string );
    var
      Stream : TCustomMemoryStream;
    begin
      Stream := TResourceStream.Create( Instance, ResourceName, RT_BITMAP );
      try
        LoadFromStream( Stream )
      finally
        Stream.Free;
      end;
    end;

  procedure TTransparentBitmap.LoadFromResourceID( Instance : THandle; ResourceId : Integer );
    var
      Stream : TCustomMemoryStream;
    begin
      Stream := TResourceStream.CreateFromID( Instance, ResourceId, RT_BITMAP );
      try
        LoadFromStream( Stream )
      finally
        Stream.Free;
      end;
    end;

  procedure TTransparentBitmap.LoadFromFile( const Filename : string );
    var
      Stream : TStream;
    begin
      Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
      try
        LoadFromStream( Stream );
      finally
        Stream.Free;
      end;
    end;

end.

