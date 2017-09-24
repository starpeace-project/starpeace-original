unit TransparentWindows;

interface

  uses
    Windows, Messages, Controls, Classes,
    WinUtils, Rects, VCLUtils;

  // High level routines

  procedure ParentChanged( aControl : TWinControl );

  // Low level routines

  procedure ClearControlHooks( aControl : TWinControl );
  procedure AddControlHook( aControl, aHookedControl : TWinControl );

implementation

  type
    PWndProcInfo = ^TWndProcInfo;
    TWndProcInfo =
      record
        Control       : TWinControl;
        HookedControl : TWinControl;
        Handle        : HWND;
        WndProcData   : integer;
      end;

  var
    fWindows : TList;

  // Hook all children of 'aParent' (except 'aControl') that intersect with ClipRect..
  procedure HookControl( aControl, aParent: TWinControl; const ClipRect : TRect );
    var
      i    : integer;
      Rect : TRect;
      Ctl  : TWinControl;
    begin
      AddControlHook( aControl, aParent );
      try
        for i := 0 to aParent.ControlCount - 1 do
          begin
            Ctl := TWinControl( aParent.Controls[i] );
            if (Ctl is TWinControl) and (Ctl <> aControl) and Ctl.Visible
               and IntersectRect( Rect, ScreenRect( Ctl ), ClipRect )
              then HookControl( aControl, Ctl, ClipRect );
          end;
      except
      end;
    end;

  procedure ParentChanged( aControl : TWinControl );
    var
      Rect : TRect;
    begin
      ClearControlHooks( aControl );
      if aControl.Parent <> nil
        then
          begin
            Rect := ScreenRect( aControl );
            HookControl( aControl, aControl.Parent, Rect );
          end;
    end;

  function PaintWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    var
      WinRect : TRect;
      i       : integer;
    begin
      try
        i := 0;
        while ( i < fWindows.Count ) and ( PWndProcInfo(fWindows.Items[i]).Handle <> WinHandle ) do
          inc( i );

        with PWndProcInfo( fWindows.Items[i] )^ do
          begin
            if ( Msg = WM_PAINT ) and GetUpdateRect( WinHandle, WinRect, false )
              then
                begin
                  OffsetRect( WinRect, HookedControl.Left - Control.Left +1, HookedControl.Top - Control.Top +1 );
                  InvalidateRect( Control.Handle, @WinRect, false );
                end;
            Result := CallWindowProc( PrevWndProc( WndProcData ), WinHandle, Msg, wPar, lPar );
          end;
      except
        Result := 0;
      end;
    end;

  procedure ClearControlHooks( aControl : TWinControl );
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

  procedure AddControlHook( aControl, aHookedControl : TWinControl );
    var
      Info : PWndProcInfo;
    begin
      with fWindows do
        begin
          new( Info );
          with Info^ do
            begin
              Control       := aControl;
              HookedControl := aHookedControl;

              Handle        := HookedControl.Handle;
              WndProcData   := ChangeWndProc( Handle, @PaintWndProc );
            end;
          fWindows.Add( Info );
        end;
    end;

initialization
  fWindows := TList.Create;

finalization
  fWindows.Free;

end.
