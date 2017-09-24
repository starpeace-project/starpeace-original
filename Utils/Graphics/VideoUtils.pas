unit VideoUtils;

interface

  uses
    Windows, Dibs, VideoICM, VideoDraw;

  // DIBs compression I/O hooks

  procedure DibUnpackVideo( SourcePixels : pointer; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );

  procedure RegisterVideoDibIoHooks;
  procedure UnregisterVideoDibIoHooks;

  // Compression/Decompression

  const
    COMP_USERCHOSEN = $52455355; // 'USER': Ask user which compressor and settings to use..
    COMP_DEFAULT    = $00000000; // Use default compressor codec

  function CompressImage( DibHeader : PDib; DibPixels : pointer; RealTime : boolean;
                          Codec : dword; lQuality : longint ) : PDib;
  function DecompressImage( DibHeader : PDib; DibPixels : pointer; RealTime : boolean ) : PDib;

  // Compressed Drawing

  function  DrawCompBegin( dc : HDC; DibHeader : PDib; Flags : dword ) : HDRAWDIB;
  procedure DrawCompEnd( hdd : HDRAWDIB );

  procedure DrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer;
                           x, y : integer; Flags : dword );
  procedure StretchDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer;
                                  x, y : integer; dx, dy : integer; Flags : dword );
  procedure ClipDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer;
                               x, y : integer; const ClippingRect : TRect; Flags : dword );
  procedure ClipStretchDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer;
                                      const Rect, ClippingRect : TRect; Flags : dword );

  // Helpers

  const
    fccTypeVIDEO = ICTYPE_VIDEO;

  function GetCompressor( fccHandler : dword; RealTime : boolean ) : HIC;
  function GetDecompressor( fccHandler : dword; RealTime : boolean ) : HIC;
  function LocateCompressor( DibHeader : PBitmapInfoHeader; RealTime : boolean ) : HIC;
  function LocateDecompressor( DibHeader : PBitmapInfoHeader; RealTime : boolean ) : HIC;
  function GetUserChosenCompressor( DibHeader : PBitmapInfoHeader; var lQuality : longint;
                                    WinHandle : HWND; const Title : string ) : HIC;

  // Codec enumeration: ----------------------------------------------------------------

  type
    TVideoCodecEnumProc = procedure ( const Info : TICInfo; UserData : dword );

  procedure EnumerateCompressors( DibHeader : PBitmapInfoHeader; UserData : dword;
                                  CodecFound, CodecCanHandleThis : TVideoCodecEnumProc );
  procedure EnumerateDecompressors( DibHeader : PBitmapInfoHeader; UserData : dword;
                                    CodecFound, CodecCanHandleThis : TVideoCodecEnumProc );

  type
    TLogProc = procedure ( Message : string );

  var
    LogProc : TLogProc = nil;

  // Quick hack: Output codecs
  // You need to set LogProc in order to use these procedures
  procedure DebugEnumerateCompressors( DibHeader : PBitmapInfoHeader );
  procedure DebugEnumerateDecompressors( DibHeader : PBitmapInfoHeader );

implementation

  uses
    Rects;

  // DIBs compression I/O hooks

  procedure DibUnpackVideo( SourcePixels : pointer; DibHeader : PDib; DibPixels : pointer; ForceTopDown : boolean );
    var
      UnpackedDib    : PDib;
      UnpackedPixels : pointer;
    begin
      UnpackedDib    := DecompressImage( DibHeader, SourcePixels, false );
      UnpackedPixels := DibPtr( UnpackedDib );

      with DibHeader^ do
        try
          if ( biHeight > 0 ) and ForceTopDown
            then biHeight := -abs( biHeight );
          if UnpackedDib.biHeight * biHeight > 0 // Same orientation?
            then Move( UnpackedPixels^, DibPixels^, DibSizeImage( UnpackedDib ) )
            else DibCopyFlippedVert( UnpackedDib, UnpackedPixels, DibPixels );

        finally
          DibFree( UnpackedDib );
        end;
    end;

  procedure RegisterVideoDibIoHooks;
    begin
      //RegisterDibIOHooks( , DibUnpackVideo );
    end;

  procedure UnregisterVideoDibIoHooks;
    begin
      //UnregisterDibIOHooks(  );
    end;

  // Compression/Decompression

  function CompressImage( DibHeader : PDib; DibPixels : pointer; RealTime : boolean;
                          Codec : dword; lQuality : longint ) : PDib;
    var
      ic        : HIC;
      tmpHeader : PDib;
      icOk      : boolean;
    begin
      if Codec = COMP_USERCHOSEN
        then
          begin
            getmem( tmpHeader, sizeof( tmpHeader^ ) );            // This stuff is to handle top-down DIBs, 
            move( DibHeader^, tmpHeader^, sizeof( tmpHeader^ ) ); // they are made bottom-up in ICCompressImage
            if tmpHeader.biHeight < 0
              then tmpHeader.biHeight := -tmpHeader.biHeight;

            ic   := GetUserChosenCompressor( tmpHeader, lQuality, 0, '' );
            icOk := ( ic <> 0 );
          end
        else
          if Codec = COMP_DEFAULT
            then
              begin
                ic   := 0;
                icOk := true;
              end
            else
              begin
                ic   := GetCompressor( Codec, RealTime );
                icOk := ( ic <> 0 );
              end;
      if icOk
        then
          begin
            Result := ICCompressImage( ic, 0, DibHeader, DibPixels, nil, lQuality );
            if ic <> 0
              then ICClose( ic );
          end
        else Result := nil;
    end;

  function DecompressImage( DibHeader : PDib; DibPixels : pointer; RealTime : boolean ) : PDib;
    var
      ic : HIC;
    begin
      if ( ( DibHeader.biCompression <> BI_RGB ) and ( DibHeader.biCompression <> BI_BITFIELDS ) ) or
         ( DibPixels <> DibPtr( DibHeader ) )
        then
          begin
            ic := LocateDecompressor( DibHeader, RealTime );
            Result := ICDecompressImage( ic, 0, DibHeader, DibPixels, nil );
            ICClose( ic );
          end
        else Result := DibHeader; // Image is already unpacked and OK
    end;

  // Compressed Drawing

  function DrawCompBegin( dc : HDC; DibHeader : PDib; Flags : dword ) : HDRAWDIB;
    begin
      Result := DrawDibOpen;
      with DibHeader^ do
        DrawDibBegin( Result, dc, biWidth, biHeight, DibHeader, biWidth, biHeight, Flags );
    end;

  procedure DrawCompEnd( hdd : HDRAWDIB );
    begin
      DrawDibEnd( hdd );
    end;

  procedure DrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer; x, y : integer; Flags : dword );
    begin
      with DibHeader^ do
        DrawDibDraw( hdd, dc, x, y, biWidth, biHeight, DibHeader, DibPixels, 0, 0, biWidth, biHeight, Flags );
    end;

  procedure StretchDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer; x, y : integer; dx, dy : integer; Flags : dword );
    begin
      with DibHeader^ do
        DrawDibDraw( hdd, dc, x, y, dx, dy, DibHeader, DibPixels, 0, 0, biWidth, biHeight, Flags );
    end;

  procedure ClipDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer; x, y : integer; const ClippingRect : TRect; Flags : dword );
    var
      SrcOfs  : TPoint;
      Rect    : TRect;
      DstRect : TRect;
      DstSize : TPoint;
    begin
      with DibHeader^ do
        begin
          Rect := RectFromBounds( x, y, biWidth, biHeight );
          if IntersectRect( DstRect, Rect, ClippingRect )
            then
              begin
                DstSize := RectSize( DstRect );

                if Rect.Left < DstRect.Left
                  then SrcOfs.x := DstRect.Left - Rect.Left
                  else SrcOfs.x := 0;

                if Rect.Top < DstRect.Top
                  then SrcOfs.y := DstRect.Top - Rect.Top
                  else SrcOfs.y := 0;

                DrawDibDraw( hdd, dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, DibHeader, DibPixels, SrcOfs.x, SrcOfs.y, DstSize.x, DstSize.y, Flags );
              end;
        end;
    end;

  procedure ClipStretchDrawCompImage( hdd : HDRAWDIB; dc : HDC; DibHeader : PDib; DibPixels : pointer; const Rect, ClippingRect : TRect; Flags : dword );
    var
      SrcOfs  : TPoint;
      SrcSize : TPoint;
      DstRect : TRect;
      DstSize : TPoint;
    begin
      with DibHeader^ do
        if IntersectRect( DstRect, ClippingRect, Rect )
          then
            begin
              DstSize := RectSize( DstRect );

              if Rect.Left < DstRect.Left
                then SrcOfs.x := (DstRect.Left - Rect.Left) * biWidth div (Rect.Right - Rect.Left)
                else SrcOfs.x := 0;
              SrcSize.x := DstSize.x * biWidth div (Rect.Right - Rect.Left);

              if Rect.Top < DstRect.Top
                then SrcOfs.y := (DstRect.Top - Rect.Top) * biHeight div (Rect.Bottom - Rect.Top)
                else SrcOfs.y := 0;
              SrcSize.y := DstSize.y * biHeight div (Rect.Bottom - Rect.Top);

              DrawDibDraw( hdd, dc, DstRect.Left, DstRect.Top, DstSize.x, DstSize.y, DibHeader, DibPixels, SrcOfs.x, SrcOfs.y, SrcSize.x, SrcSize.y, Flags );
            end;
    end;

  // Helpers

  function GetUserChosenCompressor( DibHeader : PBitmapInfoHeader; var lQuality : longint;
                                    WinHandle : HWND; const Title : string ) : HIC;
    var
      Info : TCompVars;
    begin
      Info.cbSize  := sizeof( TCompVars );
      Info.dwFlags := 0;
      if ICCompressorChoose( WinHandle, 0, DibHeader, nil, Info, pchar( Title ) )
        then
          begin
            lQuality := Info.lQ;
            Result   := Info.ic;
          end
        else Result := 0;
    end;

  function GetCompressor( fccHandler : dword; RealTime : boolean ) : HIC;
    var
      wMode : word;
    begin
      if RealTime
        then wMode := ICMODE_FASTCOMPRESS
        else wMode := ICMODE_COMPRESS;
      Result := ICOpen( fccTypeVIDEO, fccHandler, wMode );
    end;

  function GetDecompressor( fccHandler : dword; RealTime : boolean ) : HIC;
    var
      wMode : word;
    begin
      if RealTime
        then wMode := ICMODE_FASTDECOMPRESS
        else wMode := ICMODE_DECOMPRESS;
      Result := ICOpen( fccTypeVIDEO, fccHandler, wMode );
    end;

  function LocateCompressor( DibHeader : PBitmapInfoHeader; RealTime : boolean ) : HIC;
    var
      wMode : word;
    begin
      if RealTime
        then wMode := ICMODE_FASTCOMPRESS
        else wMode := ICMODE_COMPRESS;
      Result := ICLocate( fccTypeVIDEO, 0, DibHeader, nil, wMode );
    end;

  function LocateDecompressor( DibHeader : PBitmapInfoHeader; RealTime : boolean ) : HIC;
    var
      wMode : word;
    begin
      if RealTime
        then wMode := ICMODE_FASTDECOMPRESS
        else wMode := ICMODE_DECOMPRESS;
      Result := ICLocate( fccTypeVIDEO, 0, DibHeader, nil, wMode );
    end;

  // Codec enumeration: ----------------------------------------------------------------

  procedure EnumerateCompressors( DibHeader : PBitmapInfoHeader; UserData : dword;
                                  CodecFound, CodecCanHandleThis : TVideoCodecEnumProc );
    var
      Info : TICInfo;
      ic   : HIC;
      i    : integer;
    begin
      i := 0;
      with Info do
        while ICInfo( fccTypeVIDEO, i, Info ) do
          begin
            inc( i );
            CodecFound( Info, UserData );
            ic := ICOpen( Info.fccType, Info.fccHandler, ICMODE_QUERY );
            if ic <> 0
              then
                begin
                  // Skip this compressor if it can't handle the specified format.
                  if ICCompressQuery( ic, DibHeader, nil) = ICERR_OK
                    then
                      begin
                        // Find out the compressor name.
                        ICGetInfo( ic, Info, sizeof( Info ) );
                        CodecCanHandleThis( Info, UserData );
                      end
                    else ICClose( ic );
                end;
          end;
    end;

  procedure EnumerateDecompressors( DibHeader : PBitmapInfoHeader; UserData : dword;
                                    CodecFound, CodecCanHandleThis : TVideoCodecEnumProc );
    var
      Info : TICInfo;
      ic   : HIC;
      i    : integer;
    begin
      i := 0;
      with Info do
        while ICInfo( fccTypeVIDEO, i, Info ) do
          begin
            inc( i );
            CodecFound( Info, UserData );
            ic := ICOpen( Info.fccType, Info.fccHandler, ICMODE_QUERY );
            if ic <> 0
              then
                begin
                  // Skip this compressor if it can't handle the specified format.
                  if ICDecompressQuery( ic, DibHeader, nil) = ICERR_OK
                    then
                      begin
                        // Find out the compressor name.
                        ICGetInfo( ic, Info, sizeof( Info ) );
                        CodecCanHandleThis( Info, UserData );
                      end
                    else ICClose( ic );
                end;
          end;
    end;

  procedure DebugCodecFound( const Info : TICInfo; UserData : dword );
    begin
      assert( Assigned( LogProc ), 'Unassigned LogProc in VideoUtils.DebugCodecFound' );
      with Info do
        if boolean(UserData)
          then LogProc( '>>> Found a compressor (' + FourCCToStr( Info.fccHandler ) + ')' )
          else LogProc( '>>> Found a decompressor (' + FourCCToStr( Info.fccHandler ) + ')' );
    end;

  procedure DebugCodecCanHandleThis( const Info : TICInfo; UserData : dword );
    begin
      assert( Assigned( LogProc ), 'Unassigned LogProc in VideoUtils.DebugCodecCanHandleThis' );
      with Info do
        LogProc( 'Named ''' + WideCharToString( Info.szName ) +
                 ''', description ''' + WideCharToString( Info.szDescription ) +
                 ''', found in driver ''' + WideCharToString( Info.szDriver ) + '''...' );
    end;

  procedure DebugEnumerateCompressors( DibHeader : PBitmapInfoHeader );
    begin
      EnumerateCompressors( DibHeader, dword(true), DebugCodecFound, DebugCodecCanHandleThis );
    end;

  procedure DebugEnumerateDecompressors( DibHeader : PBitmapInfoHeader );
    begin
      EnumerateDecompressors( DibHeader, dword(false), DebugCodecFound, DebugCodecCanHandleThis );
    end;
    
end.
