unit DDrawManager;

interface

  uses
    Windows, Forms, Classes,
    DDraw, GDI, Dibs, DirectStuff;

  //

  function AreOverlaysSupported : boolean;

  type
    PVideoMode = ^TVideoMode;
    TVideoMode =
      record
        vmWidth    : dword;
        vmHeight   : dword;
        vmBitCount : dword;
        vmDepth    : dword;
        vmEnabled  : boolean;
      end;

  type
    PVideoModes = ^TVideoModes;
    TVideoModes =
      record
        Count : dword;
        Modes : array[0..50] of TVideoMode;
      end;

  type
    TDirectDrawManager =
      class
        private
          fDirectDraw : IDirectDrawX;

        public
          constructor Create;
          destructor  Destroy; override;

          function CreatePrimarySurface(BackBufferCount : dword ; Support3D : boolean) : IDirectSurface;
          function CreateOverlaySurface(Width, Height : dword; BitCount : dword; BackBufferCount : dword; Support3D : boolean) : IDirectSurface;
          function CreateOffscreenSurface( Width, Height : dword; BitCount : dword; Support3D : boolean ) : IDirectSurface;
          function CreateTextureSurface(Width, Height : dword; BitCount : dword; managed : boolean) : IDirectSurface;
          function CreateSurfaceEx( SurfaceDesc : TDirectSurfaceDesc ) : IDirectSurface;

          function CreatePalette(Count : dword; LogEntries : pointer; dwCaps : dword) : IDirectPalette;
          function CreateClipper : IDirectClipper;

          function GetDriverCaps(out Caps : TDirectDrawCaps) : HRESULT;

          procedure InitDirectDraw;
          procedure FreeDirectDraw;
          procedure InitSurfaces;
          procedure FreeSurfaces;

          procedure SetCooperativeLevel(Flags : dword);

        protected
          fUseHardware : boolean;
          //fDeviceDesc  : TD3DDeviceDesc;
          //fDeviceGUID  : TGUID;

          fCooperativeFlags : dword;
          fVideoMemory      : dword;
          fVideoMode        : dword;
          fVideoModes       : PVideoModes;

          fOverlayMinStretch    : dword;
          fOverlaySrcSizeAlign  : dword;
          fOverlayDestSizeAlign : dword;
          fOverlayXPosAlign     : dword;

          procedure SetCurrentMode(Value : dword);
          procedure GetVideoModes;
          function  GetCurrentModeCaps : TVideoMode;

          function  GetVideoModeCaps(Value : dword) : TVideoMode;

        public
          property DirectDraw      : IDirectDraw  read fDirectDraw;
          property CurrentModeIndx : dword        read fVideoMode          write SetCurrentMode;
          property CurrentModeCaps : TVideoMode   read GetCurrentModeCaps;

          property OverlayMinStretch    : dword read fOverlayMinStretch;
          property OverlaySrcSizeAlign  : dword read fOverlaySrcSizeAlign;
          property OverlayDestSizeAlign : dword read fOverlayDestSizeAlign;

          property OverlayXPosAlign     : dword read fOverlayXPosAlign;

          property AvailableVideoModes : PVideoModes read fVideoModes;

          procedure SetVideoMode( Width, Height, BitCount : dword; RequiresExactMatch : boolean );
          procedure SetupVideoMode( Width, Height, BitCount : dword );

          function  SearchVideoMode( Width, Height, BitCount : dword; RequiresExactMatch : boolean ) : dword;
          function  BestFitVideoMode( Width, Height, BitCount : dword ) : dword;
      end;

  // Cooperative level flags
  const
    clFullScreen         = DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN or DDSCL_ALLOWMODEX or DDSCL_ALLOWREBOOT;
    clFullScreenNoReboot = DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN or DDSCL_ALLOWMODEX;
    clNormal             = DDSCL_NORMAL;

  //
  var
    DirectDrawMgr : TDirectDrawManager;

  procedure InitDirectDrawMgr;

  //
  type
    TSurfaceType = (stPlain, stPrimary, stOverlay, stTexture);

implementation

  uses
    Dialogs,
    SysUtils, NumUtils,
    BitBlt, LogFile;

  // DirectDrawMgr

  procedure InitDirectDrawMgr;
    begin
      if DirectDrawMgr = nil
        then DirectDrawMgr := TDirectDrawManager.Create;
    end;

  const
    idxNotFound = $FFFFFFFF;

  function AreOverlaysSupported : boolean;
    var
      capsDrv : TDirectDrawCaps;
      ddrval  : HRESULT ;
    begin
      Result := false;
      InitDirectDrawMgr;
      ddrval := DirectDrawMgr.GetDriverCaps( capsDrv );
      // Does the driver support overlays in the current mode?
      // ( Currently the DirectDraw emulation layer does not support overlays.
      // Overlay related APIs will fail without hardware support ).
      if ( ddrval = DD_OK ) and ( capsDrv.dwCaps and DDCAPS_OVERLAY <> 0 )
        then Result := true;
    end;

  // DirectDraw devices enumeration callback
  function DDEnumCallback( lpGUID : PGUID; lpDriverDesc: pchar; lpDriverName : pchar; lpContext : pointer ): BOOL;
    var
      DD         : IDirectDraw;
      DriverCaps : TDirectDrawCaps;
      HELCaps    : TDirectDrawCaps;
      DDrawMgr   : TDirectDrawManager absolute lpContext;
    begin
      Result := BOOL( DDENUMRET_OK );
      if DDR( DirectDrawCreate( lpGUID, DD, nil ) ) = DD_OK
        then
          begin
            DDrawMgr.fDirectDraw := DD as IDirectDrawX;
            InitRecord( DriverCaps, sizeof( DriverCaps ) );
            InitRecord( HELCaps, sizeof( HELCaps ) );
            if DDR( DDrawMgr.fDirectDraw.GetCaps( @DriverCaps, @HELCaps ) ) = DD_OK
              then
                begin
                  if ( DriverCaps.dwCaps and DDCAPS_3D ) <> 0
                    then Result := BOOL( DDENUMRET_CANCEL );
                end;
          end;
    end;
(*
  // Direct3D devices enumeration callback
  function D3DEnumDeviceCallBack( var lpGuid: TGUID; lpDeviceDescription: PAnsiChar; lpDeviceName: PAnsiChar;
                                  var lpD3DHWDeviceDesc: TD3DDeviceDesc; var lpD3DHELDeviceDesc: TD3DDeviceDesc;
                                  lpContext : pointer ) : HRESULT; stdcall;
    var
      DDrawMgr : TDirectDrawManager absolute lpContext;
    begin
      Result  := DDENUMRET_OK;

      if ( integer( lpD3DHWDeviceDesc.dcmColorModel ) and integer( D3DCOLOR_RGB ) ) <> 0
        then
          begin
            // Make sure the driver has ZBuffering capabilities
            if ( ( ( lpD3DHWDeviceDesc.dwDeviceZBufferBitDepth and DDBD_16 ) <> 0 ) or
                ( ( lpD3DHWDeviceDesc.dwDeviceZBufferBitDepth and DDBD_24 ) <> 0 ) or
                ( ( lpD3DHWDeviceDesc.dwDeviceZBufferBitDepth and DDBD_32 ) <> 0 ) )
              then
                begin
                  // Record the HAL description
                  DDrawMgr.fDeviceDesc := lpD3DHWDeviceDesc;;
                  DDrawMgr.fDeviceGUID  := lpGuid;
                  DDrawMgr.fUseHardware := true;
                  Result := DDENUMRET_CANCEL;
                end;
          end
        else
          if ( integer( lpD3DHELDeviceDesc.dcmColorModel ) and integer( D3DCOLOR_MONO ) ) <> 0
            then
              begin
                DDrawMgr.fDeviceDesc := lpD3DHWDeviceDesc;;
                DDrawMgr.fDeviceGUID := lpGuid;
              end;
    end;
*)
  // TDirectDrawManager

  constructor TDirectDrawManager.Create;
    begin
      inherited;

      InitDirectDraw;
    end;

  destructor TDirectDrawManager.Destroy;
    begin
      FreeSurfaces;
      FreeDirectDraw;
    end;

  function TDirectDrawManager.GetDriverCaps( out Caps : TDirectDrawCaps ) : HRESULT;
    begin
      // Get driver capabilities
      InitRecord( Caps, sizeof( Caps ) );
      Result := fDirectDraw.GetCaps( @Caps, nil );
    end;

  function TDirectDrawManager.CreateClipper : IDirectClipper;
    begin
      if DDR( fDirectDraw.CreateClipper( 0, Result, nil ) ) <> DD_OK
        then // TODO 1 -oJRG : DD CreateClipper Error
    end;

  function TDirectDrawManager.CreatePalette( Count : dword; LogEntries : pointer; dwCaps : dword ) : IDirectPalette;
    begin
      assert( Count <> 0, 'Invalid Count specified!!' );
      case Count of
        1..2 :
          dwCaps := dwCaps or DDPCAPS_1BIT;
        3..4 :
          dwCaps := dwCaps or DDPCAPS_2BIT;
        5..16 :
          dwCaps := dwCaps or DDPCAPS_4BIT;
        else // 17..256 :
          dwCaps := dwCaps or DDPCAPS_8BIT;
      end;
      if DDR( fDirectDraw.CreatePalette( dwCaps, LogEntries, Result, nil ) ) <> DD_OK
        then // TODO 1 -oJRG : DD CreatePalette Error
    end;

  procedure SetPixFormat(BitCount : dword; var Dest : TDDPixelFormat );
    begin
      with Dest do
        begin
          // TODO 1 -oJRG -cGraphics : Tengo que añadir soporte para el formato BGR!!
          dwSize        := sizeof( TDDPixelFormat );
          dwFourCC      := 0;
          dwRGBBitCount := BitCount;
          case BitCount of
            1 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED1;
            2 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED2;
            4 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED4;
            8 :
              dwFlags := DDPF_RGB or DDPF_PALETTEINDEXED8;
            15 :
              begin
                dwFlags           := DDPF_RGB;
                dwRGBBitCount     := 16;
                dwRBitMask        := $7C00;
                dwGBitMask        := $03E0;
                dwBBitMask        := $001F;
                dwRGBAlphaBitMask := 0;
              end;
            16 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $F800;
                dwGBitMask        := $07E0;
                dwBBitMask        := $001F;
                dwRGBAlphaBitMask := 0;
              end;
            24 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $FF0000;
                dwGBitMask        := $00FF00;
                dwBBitMask        := $0000FF;
                dwRGBAlphaBitMask := 0;
              end;
//            24 :
//              begin
//                dwFlags           := DDPF_RGB;
//                dwRBitMask        := $0000FF;
//                dwGBitMask        := $00FF00;
//                dwBBitMask        := $FF0000;
//                dwRGBAlphaBitMask := 0;
//              end;
            32 :
              begin
                dwFlags           := DDPF_RGB;
                dwRBitMask        := $FF0000;
                dwGBitMask        := $00FF00;
                dwBBitMask        := $0000FF;
                dwRGBAlphaBitMask := 0;
              end;
//            32 :
//              begin
//                dwFlags           := DDPF_RGB;
//                dwRBitMask        := $0000FF;
//                dwGBitMask        := $00FF00;
//                dwBBitMask        := $FF0000;
//                dwRGBAlphaBitMask := 0;
//              end;
          end;
        end;
    end;

  function TDirectDrawManager.CreateSurfaceEx(SurfaceDesc : TDirectSurfaceDesc) : IDirectSurface;
    begin
      if DDR(fDirectDraw.CreateSurface(SurfaceDesc, Result, nil)) <> DD_OK
        then Result := nil;
    end;

  function TDirectDrawManager.CreateOffscreenSurface(Width, Height, BitCount : dword; Support3D : boolean) : IDirectSurface;
    var
      ddsd  : TDirectSurfaceDesc;
    begin
      InitRecord( ddsd, sizeof( ddsd ) );
      with ddsd do
        begin
          dwFlags        := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
          ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN;// or DDSCAPS_SYSTEMMEMORY;
          dwWidth        := Width;
          dwHeight       := Height;
          if Support3D
            then ddsCaps.dwCaps := ddsCaps.dwCaps or DDSCAPS_3DDEVICE;

          SetPixFormat( BitCount, ddpfPixelFormat );
        end;
      if DDR( fDirectDraw.CreateSurface( ddsd, Result, nil ) ) <> DD_OK
        then raise Exception.Create( 'Could not create plain surface!!' ); //EDirectDrawSurface.Create;;
    end;

  function TDirectDrawManager.CreateTextureSurface(Width, Height, BitCount : dword) : IDirectSurface;
    var
      ddsd : TDirectSurfaceDesc;
    begin
      // TODO 1 -oJRG -cGraphics : Unfinished!!
      with ddsd do
        begin
          dwFlags         := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT
                              or DDSD_TEXTURESTAGE;
          ddsCaps.dwCaps  := DDSCAPS_TEXTURE or DDSCAPS_SYSTEMMEMORY;
          ddsCaps.dwCaps2 := 0;
          dwWidth         := Width;
          dwHeight        := Height;
          SetPixFormat( BitCount, ddpfPixelFormat );
        end;
    end;

  function TDirectDrawManager.CreatePrimarySurface(BackBufferCount : dword; Support3D : boolean) : IDirectSurface;
    var
      ddsd : TDirectSurfaceDesc;
    begin
      InitRecord( ddsd, sizeof( ddsd ) );
      with ddsd do
        begin
          if BackBufferCount > 0
            then
              begin
                dwFlags           := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
                dwBackBufferCount := BackBufferCount;
                ddsCaps.dwCaps    := DDSCAPS_PRIMARYSURFACE or DDSCAPS_FLIP or DDSCAPS_COMPLEX;
              end
            else
              begin
                dwFlags        := DDSD_CAPS;
                ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;
              end;
          if Support3D
            then ddsCaps.dwCaps := ddsCaps.dwCaps or DDSCAPS_3DDEVICE;
        end;

      if DDR( fDirectDraw.CreateSurface( ddsd, Result, nil ) ) <> DD_OK
        then raise Exception.Create( 'Could not create primary surface!!' ); //EDirectDrawSurface.Create;;
    end;

  function TDirectDrawManager.CreateOverlaySurface(Width, Height, BitCount, BackBufferCount : dword; Support3D : boolean) : IDirectSurface;
    var
      ddsd : TDirectSurfaceDesc;
      retcode : integer;
    begin
      InitRecord( ddsd, sizeof( ddsd ) );
      with ddsd do
        begin
          if BackBufferCount > 0
            then
              begin
                dwFlags           := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT or DDSD_BACKBUFFERCOUNT;
                dwBackBufferCount := BackBufferCount;
                ddsCaps.dwCaps    := DDSCAPS_OVERLAY or DDSCAPS_FLIP or DDSCAPS_COMPLEX or DDSCAPS_VIDEOMEMORY;
              end
            else
              begin
                dwFlags        := DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or DDSD_PIXELFORMAT;
                ddsCaps.dwCaps := DDSCAPS_OVERLAY or DDSCAPS_VIDEOMEMORY;
              end;
          if Support3D
            then ddsCaps.dwCaps := ddsCaps.dwCaps or DDSCAPS_3DDEVICE;

          SetPixFormat( BitCount, ddpfPixelFormat );
          dwWidth  := Width;
          dwHeight := Height;
        end;
      retcode := {DDR( }fDirectDraw.CreateSurface( ddsd, Result, nil ){)};
      LogThis(ErrorString(retcode));
      if retcode <> DD_OK
        then raise Exception.Create( 'Could not create overlay surface!! ' + ' Error : ' + ErrorString(retcode)); //EDirectDrawSurface.Create;;
    end;

(*
  procedure TDirectDrawManager.CreateSurfaces;
    var
      CurMode : TVideoMode;
      ddsd    : TDDSURFACEDESC;
      ddscaps : TDDSCAPS;
      Palette : IDIRECTPALETTE;
      i       : integer;
    begin
      Result  := false;
      CurMode := fModes[fVideoMode];

      InitRecord( ddsd, sizeof( ddsd ) );
      ddsd.dwFlags           := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
      ddsd.ddsCaps.dwCaps    := DDSCAPS_PRIMARYSURFACE or DDSCAPS_FLIP or DDSCAPS_COMPLEX or DDSCAPS_3DDEVICE;
      ddsd.dwBackBufferCount := 1;

      if DDR( fDirectDraw.CreateSurface( ddsd, fPrimaryBuffer, nil ) ) = DD_OK
        then
          begin
            InitRecord( ddscaps, sizeof( ddscaps ) );
            ddscaps.dwCaps := DDSCAPS_BACKBUFFER;

            if DDR( fPrimaryBuffer.GetAttachedSurface( ddscaps, fBackBuffer ) ) = DD_OK
              then
                begin
                  if fZBufferBitDepth > 0
                    then
                      begin
                        InitRecord( ddsd, sizeof( ddsd ) );
                        ddsd.dwFlags            := DDSD_WIDTH or DDSD_HEIGHT or DDSD_CAPS or DDSD_ZBUFFERBITDEPTH;;
                        ddsd.dwWidth            := CurMode.Width;
                        ddsd.dwHeight           := CurMode.Height;
                        ddsd.ddsCaps.dwCaps     := DDSCAPS_ZBUFFER or fZBufferMemType;
                        ddsd.dwZBufferBitDepth  := fZBufferBitDepth;

                        if DDR( fDirectDraw.CreateSurface( ddsd, fZBuffer, nil ) ) = DD_OK
                          then fBackBuffer.AddAttachedSurface( fZBuffer );
                      end;

                  if DDR( fPrimaryBuffer.GetCaps( ddscaps ) ) = DD_OK
                    then
                      begin
                        if ( CurMode.BPP = 8 ) and ( ( ddscaps.dwCaps and DDCAPS_PALETTE ) <> 0 )
                          then
                            begin
                              for i := 0 to 255 do
                                begin
                                  fPaletteEntries[i].peRed   := i;//random( 256 );
                                  fPaletteEntries[i].peGreen := i;//random( 256 );
                                  fPaletteEntries[i].peBlue  := i;//random( 256 );
                                  fPaletteEntries[i].peFlags := D3DPAL_FREE; //D3DPAL_READONLY;
                                end;

                              if DDR( fDirectDraw.CreatePalette( DDPCAPS_8BIT or DDPCAPS_INITIALIZE, @fPaletteEntries, Palette, nil ) ) = DD_OK
                                then
                                  begin
                                    fBackBuffer.SetPalette( Palette );
                                    fPrimaryBuffer.SetPalette( Palette );
                                  end;
                            end;
                      end;

                  // Create device
                  Result := DDR( fDirect3D.CreateDevice( fDeviceGUID, fBackBuffer, fDevice ) ) = DD_OK;
                end;
          end;

      if not Result
        then DoneSurfaces;
    end;
*)
  procedure TDirectDrawManager.InitDirectDraw;
    var
      DirectDraw : IDirectDraw;
      ddsd       : TDirectSurfaceDesc;
    begin
      DirectDrawCreate( nil, DirectDraw, nil );

      fDirectDraw := DirectDraw as IDirectDrawX;
      GetVideoModes;

      ddsd.dwSize := sizeof(ddsd);
      if DDR( fDirectDraw.GetDisplayMode( ddsd ) ) = DD_OK
        then
          with ddsd do
            fVideoMode := BestFitVideoMode( dwWidth, dwHeight, ddpfPixelFormat.dwRGBBitCount );
    end;

  procedure TDirectDrawManager.FreeDirectDraw;
    begin
      fDirectDraw := nil;
    end;

  procedure TDirectDrawManager.InitSurfaces;
    begin
      // TODO 1 -oJRG : Finish InitSurfaces
    end;

  procedure TDirectDrawManager.FreeSurfaces;
    begin
      // TODO 1 -oJRG : Finish FreeSurfaces
    end;

  procedure ReallocVideoModes( var List : PVideoModes );
    begin
      reallocmem( List, sizeof( TVideoModes ) + ( List.Count - 1 ) * sizeof( TVideoMode ) );
    end;

  procedure DeleteVideoMode( const List : PVideoModes; Indx : integer );
    var
      i : integer;
    begin
      with List^ do
        if Count > 0
          then
            begin
              dec( Count );
              if Indx > 0
                then
                  for i := Indx to Count - 1 do
                    Modes[i] := Modes[i + 1];
            end;
    end;

  procedure InsertVideoMode( const List : PVideoModes; Indx : integer; const Value : TVideoMode );
    var
      i : integer;
    begin
      with List^ do
        begin
          inc( Count );
          if Indx = -1
            then Indx := Count - 1
            else
              for i := Count - 1 downto Indx + 1 do
                Modes[i] := Modes[i - 1];
          Modes[Indx] := Value;
        end;
    end;

  procedure AddVideoMode( const List : PVideoModes; const Value : TVideoMode );
    begin
      InsertVideoMode( List, -1, Value );
    end;

  procedure AddVideoModeEx( const List : PVideoModes; Width, Height, BitCount, Depth : dword );
    var
      VideoMode : TVideoMode;
    begin
      with VideoMode do
        begin
          vmWidth    := Width;
          vmHeight   := Height;
          vmBitCount := BitCount;
          vmDepth    := Depth;
        end;

      InsertVideoMode( List, -1, VideoMode );
    end;

  function CopyVideoModes( const List : PVideoModes ) : PVideoModes;
    var
      ListSize : dword;
    begin
      with List^ do
        ListSize := sizeof( TVideoModes ) + ( List.Count - 1 ) * sizeof( TVideoMode );
      getmem( Result, ListSize );
      move( List^, Result^, ListSize );
    end;

  procedure PackVideoModes( const List : PVideoModes );
    var
      i, j : integer;
    begin
      with List^ do
        if Count > 0
          then
            for i := Count - 1 downto 0 do
              if not Modes[i].vmEnabled
                then
                  begin
                    dec( Count );
                    if Count > 0
                      then
                        for j := i to Count - 1 do
                          Modes[j] := Modes[j + 1];
                  end;
    end;

  // Screen modes enumeration callback
  function DDEnumDisplayModesCallback( const ddsd : TDirectSurfaceDesc; lpContext : pointer ) : HRESULT; stdcall;
    const
      Bits : array[0..3] of dword = ( DDBD_8, DDBD_16, DDBD_24, DDBD_32 );
    var
      DDrawMgr             : TDirectDrawManager absolute lpContext;
      Depth                : dword;
      BitDepthMultiplier   : dword;
      VidRamNeeded         : dword;
    begin
      BitDepthMultiplier := ddsd.ddpfPixelFormat.dwRGBBitCount div 8;
      Depth              := Bits[BitDepthMultiplier - 1];

      with DDrawMgr do
        begin
          if fUseHardware
            then
              begin
                VidRamNeeded := ( ( ddsd.dwWidth * ddsd.dwHeight ) * BitDepthMultiplier ) * 3; // TODO 1 -oJRG: Por que * 3??
                if ( VidRamNeeded <= DDrawMgr.fVideoMemory )
                   //and ( ( fDeviceDesc.dwDeviceRenderBitDepth and Depth ) <> 0 )
                  then AddVideoModeEx( fVideoModes, ddsd.dwWidth, ddsd.dwHeight, BitDepthMultiplier * 8, Depth );
              end
            else
              AddVideoModeEx( fVideoModes, ddsd.dwWidth, ddsd.dwHeight, BitDepthMultiplier * 8, Depth );
        end;

      Result := DDENUMRET_OK;
    end;

  procedure TDirectDrawManager.GetVideoModes;
    begin
      getmem( fVideoModes, sizeof( TVideoModes ) + 255 * sizeof( TVideoMode ) ); // TODO 5 -oJRG: Use MAX_VIDEOMODES

      fVideoModes.Count := 0;
      fDirectDraw.EnumDisplayModes( 0, nil, Self, DDEnumDisplayModesCallback );

      ReallocVideoModes( fVideoModes );
    end;

  function TDirectDrawManager.GetVideoModeCaps( Value : dword ) : TVideoMode;
    begin
      assert( Value < fVideoModes.Count, 'Invalid video mode specified!!' );
      Result := fVideoModes.Modes[Value];
    end;

  function TDirectDrawManager.GetCurrentModeCaps : TVideoMode;
    begin
      Result := fVideoModes.Modes[fVideoMode];
    end;

  procedure TDirectDrawManager.SetCurrentMode( Value : dword );
    var
      Caps : TDirectDrawCaps;
    begin
      assert( fCooperativeFlags <> 0, 'You must first set cooperative level!!' );
      fVideoMode := Value;

      FreeSurfaces;

      with fVideoModes.Modes[Value] do
        if DDR( fDirectDraw.SetDisplayMode( vmWidth, vmHeight, vmBitCount, 0, 0 ) ) = DD_OK
          then
            begin
              GetDriverCaps( Caps );
              fVideoMemory := Caps.dwVidMemFree;
              InitSurfaces;

              // Check the minimum stretch and set the variable accordingly
              if Caps.dwCaps and DDCAPS_OVERLAYSTRETCH <> 0
                then
                  if Caps.dwMinOverlayStretch>1000
                    then fOverlayMinStretch := Caps.dwMinOverlayStretch
                    else fOverlayMinStretch := 1000
                else fOverlayMinStretch := 1000;

              // Grab any alignment restrictions and set the variables acordingly
              if Caps.dwCaps and DDCAPS_ALIGNSIZESRC <> 0
                then
                  begin
                    fOverlaySrcSizeAlign  := Caps.dwAlignSizeSrc;
                    fOverlayDestSizeAlign := Caps.dwAlignSizeDest;
                  end
                else
                  begin
                    fOverlaySrcSizeAlign  := 0;
                    fOverlayDestSizeAlign := 0;
                  end;
              // Set the "destination position alignment" global so we won't have to
              // keep calling GetCaps() every time we move the overlay surface.
              if Caps.dwCaps and DDCAPS_ALIGNBOUNDARYDEST <> 0
                then fOverlayXPosAlign := Caps.dwAlignBoundaryDest
                else fOverlayXPosAlign := 0;
            end
          else raise Exception.Create( 'Could not set video mode!!' ); //EDirectDrawVidMode.Create;
    end;

  function TDirectDrawManager.BestFitVideoMode( Width, Height, BitCount : dword ) : dword;
    var
      TmpList : PVideoModes;
      i       : dword;
    begin
      Result := SearchVideoMode( Width, Height, BitCount, true );
      if Result = idxNotFound // The exact match for this mode wasn't found
        then
          begin
            TmpList := CopyVideoModes( fVideoModes );
            with TmpList^ do
              begin
                for i := 0 to Count - 1 do
                  with Modes[i] do
                    if (vmWidth < Width) or (vmHeight < Height) or (vmBitCount < BitCount)
                      then vmEnabled := false;
                PackVideoModes( TmpList );
                if Count > 0
                  then
                    with Modes[0] do
                      Result := SearchVideoMode( vmWidth, vmHeight, vmBitCount, true )
                  else Result := idxNotFound
              end;
          end;
    end;

  function TDirectDrawManager.SearchVideoMode( Width, Height, BitCount : dword; RequiresExactMatch : boolean ) : dword;
    var
      i     : dword;
      Found : boolean;
    begin
      if RequiresExactMatch
        then
          with fVideoModes^ do
            begin
              i := 0;
              repeat
                with Modes[i] do
                  Found := ( vmWidth = Width ) and ( vmHeight = Height ) and ( vmBitCount = BitCount );
                  if not Found
                    then inc( i );
              until Found or ( i >= Count );
              if not Found
                then i := idxNotFound;
            end
        else i := BestFitVideoMode(Width, Height, BitCount);
      Result := i;
    end;

  procedure TDirectDrawManager.SetVideoMode( Width, Height, BitCount : dword; RequiresExactMatch : boolean );
    begin
      SetCurrentMode(SearchVideoMode(Width, Height, BitCount, RequiresExactMatch));
    end;

  procedure TDirectDrawManager.SetupVideoMode( Width, Height, BitCount : dword );
    begin
      SetCooperativeLevel(clFullScreen);
      SetVideoMode(Width, Height, BitCount, true);
    end;

  procedure TDirectDrawManager.SetCooperativeLevel( Flags : dword );
    begin
      if fCooperativeFlags <> Flags
        then
          if DDR( fDirectDraw.SetCooperativeLevel( Application.Handle, Flags ) ) = DD_OK
            then fCooperativeFlags := Flags
            else raise Exception.Create( 'Could not set cooperative level' ); //EDirectDrawCoopLevel.Create;
    end;

end.

