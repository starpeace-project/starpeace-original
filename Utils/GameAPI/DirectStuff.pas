unit DirectStuff;

interface

  uses
    Windows,
    DirectDraw, Direct3D;

  {$IFOPT C+}
  function DDR( Value : HRESULT ) : HRESULT;
  var
    ddRes : HRESULT;
  {$ENDIF}

  // DirectX Version Specific Type Setup
  type
    IDirectDraw    = IDirectDraw7;
    IDirectSurface = IDirectDrawSurface7;
    IDirectPalette = IDirectDrawPalette;
    IDirectClipper = IDirectDrawClipper;

  type
    TDirectSurfaceDesc = TDDSurfaceDesc2;
    TDirectSurfaceCaps = TDDSCaps2;
    TDirectDrawCaps    = TDDCaps;
    TDirectBltFX       = TDDBLTFX;
    TDirectOverlayFX   = TDDOverlayFX;

  //

  procedure InitRecord( var aRecord; Size : cardinal );
  function ClearSurface( Surface : IDirectSurface; Color : dword ) : boolean;

implementation

  uses
    SysUtils;

  // Misc Stuff

  function ClearSurface( Surface : IDirectSurface; Color : dword ) : boolean;
    var
      BltFx : TDirectBltFX;
    begin
      assert( Surface <> nil, 'Invalid surface!!' );
      InitRecord( BltFx, sizeof(BltFx) );
      BltFx.dwFillColor := Color;
      Result := DDR( Surface.Blt( nil, nil, nil, DDBLT_COLORFILL or DDBLT_WAIT, @BltFx ) ) = DD_OK;
    end;

  procedure InitRecord( var aRecord; Size : cardinal );
    var
      dwSize : dword absolute aRecord;
    begin
      ZeroMemory( @aRecord, Size );
      dwSize := Size;
    end;

{$IFOPT C+}
  function DDR( Value : HRESULT ) : HRESULT;
    begin
      Result := Value;
      if Value <> DD_OK
        then
          begin
            OutputDebugString( pchar( 'DDError: ' + IntToStr( Result ) ) );
            raise Exception.Create( ErrorString( Result ) );
          end;
    end;
{$ELSE}
  function DDR( Value : HRESULT ) : HRESULT;
    begin
      Result := Value;
    end;
{$ENDIF}

end.
