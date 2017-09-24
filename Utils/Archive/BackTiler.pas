unit BackTiler;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Buffer, WinUtils, VCLUtils;

  type
    TBackgroundTiler =
      class( TComponent )
        protected
          fTile : TSpeedBitmap;

          procedure SetTile( aTile : TSpeedBitmap );                         virtual;

        published
          property Tile : TSpeedBitmap read fTile write SetTile;

        public
          procedure AddControl( aControl : TWinControl );                    virtual;
      end;

  procedure Register;

implementation

  type
    PWndProcInfo = ^TWndProcInfo;
    TWndProcInfo =
      record
        Control       : TBackgroundTiler;
        FormOrigin    : TPoint;
        TileStart     : TPoint;
        TileBitmap    : TSpeedBitmap;
        HookedControl : TWinControl;
        Rect          : TRect;
        Handle        : HWND;
        WndProcData   : integer;
      end;

  var
    fWindows : TList;

  function EraseWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    var
      i         : integer;
      CtlOrigin : TPoint;
      Info      : TWndProcInfo;
    begin
      try
        i := 0;
        while ( i < fWindows.Count ) and ( PWndProcInfo( fWindows.Items[i] ).Handle <> WinHandle ) do
          inc( i );

        Info := PWndProcInfo( fWindows.Items[i] )^;
        with PWndProcInfo( fWindows.Items[i] )^ do
          case Msg of
            WM_ERASEBKGND :
              begin
                // Tile background
                TileBitmap.TileOnDC( wPar, TileStart.x, TileStart.y, Rect, cmSrcCopy );
                Result := 1;
              end;
            WM_MOVE :
              begin
                Result := CallWindowProc( PrevWndProc( WndProcData ), WinHandle, Msg, wPar, lPar );

                // Update TileStart
                CtlOrigin := HookedControl.ClientOrigin;
                with TileStart do
                  begin
                    x := FormOrigin.x - CtlOrigin.x;
                    y := FormOrigin.y - CtlOrigin.y;
                  end;
              end;
            else
              Result := CallWindowProc( PrevWndProc( WndProcData ), WinHandle, Msg, wPar, lPar );
          end;
      except
        Result := 0;
      end;
    end;

  var
    fFormOrigin : TPoint;

  procedure AddControlHook( aControl : TBackgroundTiler; aHookedControl : TWinControl );
    var
      Info      : PWndProcInfo;
      CtlOrigin : TPoint;
    begin
      with fWindows do
        begin
          new( Info );
          with Info^ do
            begin
              Control    := aControl;
              TileBitmap := aControl.fTile;
              FormOrigin := fFormOrigin;

              CtlOrigin  := aHookedControl.ClientOrigin;
              Rect       := aHookedControl.ClientRect;
              with TileStart do
                begin
                  x := FormOrigin.x - CtlOrigin.x;
                  y := FormOrigin.y - CtlOrigin.y;
                end;

              HookedControl := aHookedControl;
              Handle        := HookedControl.Handle;
              WndProcData   := ChangeWndProc( Handle, @EraseWndProc );
            end;
          fWindows.Add( Info );
        end;
    end;

  // Hook all children of 'aParent'
  procedure HookControl( aControl : TBackgroundTiler; aParent: TWinControl );
    var
      i : integer;
    begin
      try
        fFormOrigin := GetParentForm( aParent ).ClientOrigin;
        for i := 0 to aParent.ControlCount - 1 do
          if TWinControl( aParent.Controls[i] ).Visible
            then AddControlHook( aControl, aParent );
      except
      end;
    end;

  procedure ClearControlHooks( aControl : TBackgroundTiler );
    var
      i : integer;
    begin
      with fWindows do
        begin
          i := Count - 1;
          while i >= 0 do
            with PWndProcInfo( Items[i] )^ do
              begin
                if Control = aControl
                  then
                    begin
                      RestoreWndProc( Handle, WndProcData );
                      dispose( Items[i] );
                      Delete( i );
                    end;
                dec( i );
              end;
        end;
    end;

  // TBackgroundTiler

  procedure TBackgroundTiler.SetTile( aTile : TSpeedBitmap );
    begin
      fTile := aTile;
      fTile.IgnorePalette := true;
    end;

  procedure TBackgroundTiler.AddControl( aControl : TWinControl );
    begin
      fFormOrigin := GetParentForm( aControl ).ClientOrigin;
      AddControlHook( Self, aControl );
    end;

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TBackgroundTiler] );
    end;

initialization
  fWindows := TList.Create;

finalization
  fWindows.Free;

end.
