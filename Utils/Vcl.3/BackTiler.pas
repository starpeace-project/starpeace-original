unit BackTiler;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Gdi, WinUtils, VclUtils, Buffer, SpeedBmp;

  type
    TTiler =
      class( TComponent )
        protected
          fTile : TSpeedBitmap;

          procedure SetTile( aTile : TSpeedBitmap );                                                                           virtual;

        public
          procedure   AddControl( aControl : TWinControl );                                                                    virtual;
          constructor Create( aOwner : TComponent );                                                                           override;
          destructor  Destroy;                                                                                                 override;

        published
          property Tile : TSpeedBitmap read fTile write SetTile;
      end;

  procedure Register;

implementation

  {$R *.dcr}
  
  type
    PWndProcInfo = ^TWndProcInfo;
    TWndProcInfo =
      record
        Control            : TTiler;
        FormOrigin         : TPoint;
        TileStart          : TPoint;
        TileBitmap         : TSpeedBitmap;
        HookedControl      : TWinControl;
        ControlWndProcData : integer;
        ControlHandle      : HWND;
        ParentWndProcData  : integer;
        ParentHandle       : HWND;
        Rect               : TRect;
      end;

  var
    fWindows     : TList;
    fHollowBrush : HBRUSH;

  // Here we take care of the WM_CTLCOLOR messages the hooked control's parent
  function CtlColorWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    var
      i : integer;
    begin
      try
        i := 0;
        while ( i < fWindows.Count ) and ( PWndProcInfo( fWindows.Items[i] ).ParentHandle <> WinHandle ) do
          inc( i );
        with PWndProcInfo( fWindows.Items[i] )^ do
          case Msg of
            WM_CTLCOLOREDIT, WM_CTLCOLORLISTBOX, WM_CTLCOLORBTN,
            WM_CTLCOLORSCROLLBAR, WM_CTLCOLORSTATIC,
            CN_CTLCOLOREDIT, CN_CTLCOLORLISTBOX, CN_CTLCOLORBTN,
            CN_CTLCOLORSCROLLBAR, CN_CTLCOLORSTATIC :
              begin
                // Make sure we're dealing with the right control
                while ( i < fWindows.Count ) and
                  ( ( PWndProcInfo( fWindows.Items[i] ).ParentHandle <> WinHandle ) or
                    ( PWndProcInfo( fWindows.Items[i] ).ControlHandle <> lPar ) ) do
                  inc( i );
                if i < fWindows.Count
                  then
                    begin
                      SetBkMode( wPar, TRANSPARENT );
                      SetTextColor( wPar, clWindowText );
                      Result := GetStockObject(HOLLOW_BRUSH); //fHollowBrush;
                    end
                  else Result := CallWindowProc( PrevWndProc( ParentWndProcData ), WinHandle, Msg, wPar, lPar );
              end;
            else
              Result := CallWindowProc( PrevWndProc( ParentWndProcData ), WinHandle, Msg, wPar, lPar );
          end;
      except
        Result := 0;
      end;
    end;

  // Here we take care of WM_ERASEBKGND, WM_MOVE & WM_CTLCOLORDLG messages for the hooked control
  function EraseWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    var
      i         : integer;
      CtlOrigin : TPoint;
      Info      : TWndProcInfo;
    begin
      try
        i := 0;
        while ( i < fWindows.Count ) and ( PWndProcInfo( fWindows.Items[i] ).ControlHandle <> WinHandle ) do
          inc( i );

        Info := PWndProcInfo( fWindows.Items[i] )^;
        with PWndProcInfo( fWindows.Items[i] )^ do
          case Msg of
            WM_CTLCOLORDLG, CN_CTLCOLORDLG :
              begin
                SetBkMode( wPar, TRANSPARENT );
                SetTextColor( wPar, clWindowText );
                Result := fHollowBrush;
              end;
            WM_ERASEBKGND :
              begin
                // Tile background
                if not TileBitmap.Empty
                  then TileBitmap.TileOnDC( wPar, TileStart.x, TileStart.y, Rect, cmSrcCopy );
                Result := 1;
              end;
            WM_MOVE :
              begin
                Result := CallWindowProc( PrevWndProc( ControlWndProcData ), WinHandle, Msg, wPar, lPar );

                // Update TileStart
                CtlOrigin := HookedControl.ClientOrigin; 
                with TileStart do
                  begin
                    x := FormOrigin.x - CtlOrigin.x;
                    y := FormOrigin.y - CtlOrigin.y;
                  end;
              end;
            else
              Result := CallWindowProc( PrevWndProc( ControlWndProcData ), WinHandle, Msg, wPar, lPar );
          end;
      except
        Result := 0;
      end;
    end;

  var
    fFormOrigin : TPoint;

  procedure AddControlHook( aControl : TTiler; aHookedControl : TWinControl );
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

              // Hook control  
              HookedControl      := aHookedControl;
              ControlHandle      := HookedControl.Handle;
              ControlWndProcData := ChangeWndProc( ControlHandle, @EraseWndProc );

              // Hook control's parent
              ParentHandle      := GetParent( HookedControl.Handle );
              if ParentHandle <> 0
                then ParentWndProcData := ChangeWndProc( ParentHandle, @CtlColorWndProc );
            end;
          fWindows.Add( Info );
        end;
    end;

  // Hook all children of 'aParent'
  procedure HookControl( aControl : TTiler; aParent: TWinControl );
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

  procedure ClearControlHooks( aControl : TTiler );
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
                      if ParentHandle <> 0
                        then RestoreWndProc( ParentHandle, ParentWndProcData );
                      RestoreWndProc( ControlHandle, ControlWndProcData );
                      dispose( Items[i] );
                      Delete( i );
                    end;
                dec( i );
              end;
        end;
    end;

  // TTiler

  constructor TTiler.Create( aOwner : TComponent );
    begin
      fTile := TSpeedBitmap.Create;
      
      inherited;
    end;

  destructor TTiler.Destroy;
    begin
      fTile.Free;
      inherited;
    end;

  procedure TTiler.SetTile( aTile : TSpeedBitmap );
    begin
      fTile.Assign( aTile );
      // Process here
      fTile.IgnorePalette := true;
    end;

  procedure TTiler.AddControl( aControl : TWinControl );
    begin
      fFormOrigin := GetParentForm( aControl ).ClientOrigin;
      AddControlHook( Self, aControl );
    end;

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TTiler] );
    end;

initialization
  fHollowBrush := GetStockObject( HOLLOW_BRUSH );
  fWindows     := TList.Create;

finalization
  DeleteObject( fHollowBrush );
  fWindows.Free;

end.
