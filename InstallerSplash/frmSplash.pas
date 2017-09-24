unit frmSplash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MultiBMPButton, StdCtrls, ExtCtrls;

type
  TSPSplash =
    class(TForm)
        Image1: TImage;
        btnInstall: TMultiBMPButton;
        btnDX: TMultiBMPButton;
        btnWMP: TMultiBMPButton;
        btnCancel: TMultiBMPButton;
        btnIE: TMultiBMPButton;
        lblStatus: TLabel;
        procedure MultiBMPButton1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure btnInstallMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
        procedure btnCancelClick(Sender: TObject);
    procedure btnWMPClick(Sender: TObject);
    procedure btnDXClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
      private
        fDXCaption  : string;
        fIECaption  : string;
        fWMPCaption : string;
        fUsingNT    : boolean;
        function getDXVersion( out Desc : string; out UsingNT : boolean ) : integer;
        function getIEVersion( out Desc : string ) : integer;
        function getWMPVersion( out Desc : string ) : integer;

        procedure UpdateVersions;
   end;

var
  SPSplash: TSPSplash;

implementation

  uses
    DirectDraw, DirectInput, Registry, ShellApi, AppPathUtils;

{$R *.DFM}

  procedure TSPSplash.MultiBMPButton1Click(Sender: TObject);
    begin
      Close;
    end;

  function TSPSplash.getDXVersion( out Desc : string; out UsingNT : boolean ) : integer;
    const
      IID_DD2  : TGUID = '{B3A6F3E0-2B43-11CF-A2DE-00AA00B93356}';
      IID_DDS3 : TGUID = '{DA044E00-69B2-11D0-A1D5-00AA00B8DFBB}';
      IID_DDS4 : TGUID = '{0B2B8630-AD35-11D0-8EA6-00609797EA5B}';
      IID_DD7  : TGUID = '{15e65ec0-3b9c-11d2-b92f-00609797ea5b}';
    type
      TDirectDrawCreate   = function (lpGUID: PGUID; out lplpDD: IDirectDraw; pUnkOuter: IUnknown) : HResult; stdcall;
      TDirectDrawCreateEx = function (lpGUID: PGUID; out lplpDD: IDirectDraw7; const iid: TGUID; pUnkOuter: IUnknown) : HResult; stdcall;
      TDirectInputCreate  = function (hinst: THandle; dwVersion: DWORD; out ppDI: IDirectInput; punkOuter: IUnknown) : HResult; stdcall;
    var
      hr      : HRESULT;
      DDHinst : THandle;
      DIHinst : THandle;
      pDDraw  : IDIRECTDRAW;
      pDDraw2 : IDIRECTDRAW2;
      fnDirectDrawCreate   : TDirectDrawCreate;
      fnDirectDrawCreateEx : TDirectDrawCreateEx;
      fnDirectInputCreate  : TDirectInputCreate;
      osVer                : TOSVERSIONINFO;
      pSurf                : IDIRECTDRAWSURFACE;
      pSurf3               : IDIRECTDRAWSURFACE3;
      pSurf4               : IDIRECTDRAWSURFACE4;
      continue             : boolean;
      ddsd                 : TDDSURFACEDESC;
      pDD7                 : IDIRECTDRAW7;
    begin
      result             := 0;
      DDHinst            := 0;
      DIHinst            := 0;
      continue           := true;
      UsingNT            := false;
      // First get the windows platform
      osVer.dwOSVersionInfoSize := sizeof(osVer);
      if GetVersionEx( osVer )
        then
          begin
            if osVer.dwPlatformId = VER_PLATFORM_WIN32_NT
              then
                begin
                  UsingNT := true;
                  // NT is easy... NT 4.0 is DX2, 4.0 SP3 is DX3, 5.0 is DX5
                  // and no DX on earlier versions.
                  if osVer.dwMajorVersion < 4
                    then continue := false //outa here
                    else
                      if osVer.dwMajorVersion = 4
                        then
                          begin
                            // NT4 up to SP2 is DX2, and SP3 onwards is DX3, so we are at least DX2
                            result := 2;

                            // We're not supposed to be able to tell which SP we're on, so check for dinput
                            DIHinst := LoadLibrary( 'DINPUT.DLL' );

                            if DIHinst = 0
                              then
                                begin
                                  // No DInput... must be DX2 on NT 4 pre-SP3
                                  continue := false;
                                end
                              else
                                begin
                                  fnDirectInputCreate := GetProcAddress( DIHinst, 'DirectInputCreateA' );
                                  FreeLibrary( DIHinst );

                                  if not Assigned(fnDirectInputCreate)
                                    then continue := false // No DInput... must be pre-SP3 DX2
                                    else
                                      begin
                                        // It must be NT4, DX2
                                        result   := 3;  // DX3 on NT4 SP3 or higher
                                        continue := false;
                                      end;
                                end;
                          end
                      // Else it's NT5 or higher, and it's DX5a or higher: Drop through to
                      // Win9x tests for a test of DDraw (DX6 or higher)
                end;
            if continue //Win9x or higher version of NT
              then
                begin
                  // Now we know we are in Windows 9x (or maybe 3.1), so anything's possible.
                  // First see if DDRAW.DLL even exists.
                  DDHinst := LoadLibrary( 'DDRAW.DLL' );
                  if DDHinst = 0
                    then
                      begin
                        result := 0;
                        FreeLibrary( DDHinst );
                      end
                    else
                      begin
                        // See if we can create the DirectDraw object.
                        fnDirectDrawCreate := GetProcAddress( DDHinst, 'DirectDrawCreate' );
                        if not Assigned(fnDirectDrawCreate)
                          then
                            begin
                              result := 0;
                              FreeLibrary( DDHinst );
                            end
                          else
                            begin
                              hr := DirectDrawCreate( nil, pDDraw, nil );
                              if FAILED( hr )
                                then
                                  begin
                                    result := 0;
                                    FreeLibrary( DDHinst );
                                  end
                                else
                                  begin
                                    // So DirectDraw exists.  We are at least DX1.
                                    result := 1;

                                    // Let's see if IID_IDirectDraw2 exists.
                                    hr := pDDraw.QueryInterface( IID_DD2, pDDraw2 );
                                    if FAILED( hr )
                                      then FreeLibrary( DDHinst )
                                      else
                                        begin
                                          // IDirectDraw2 exists. We must be at least DX2
                                          result := 2;


                                          ///////////////////////////////////////////////////////////////////////////
                                          // DirectX 3.0 Checks
                                          ///////////////////////////////////////////////////////////////////////////

                                          // DirectInput was added for DX3
                                          DIHinst := LoadLibrary( 'DINPUT.DLL' );
                                          if DIHinst = 0
                                            then FreeLibrary( DDHinst ) // No DInput... must not be DX3
                                            else
                                              begin
                                                fnDirectInputCreate := GetProcAddress( DIHinst, 'DirectInputCreateA' );
                                                if not Assigned(fnDirectInputCreate)
                                                  then
                                                    begin
                                                      // No DInput... must be DX2
                                                      FreeLibrary( DIHinst );
                                                      FreeLibrary( DDHinst );
                                                    end
                                                  else
                                                    begin
                                                      // DirectInputCreate exists. We are at least DX3
                                                      result := 3;

                                                      FreeLibrary( DIHinst );

                                                      ///////////////////////////////////////////////////////////////////////////
                                                      // DirectX 5.0 Checks
                                                      ///////////////////////////////////////////////////////////////////////////

                                                      // We can tell if DX5 is present by checking for the existence of
                                                      // IDirectDrawSurface3. First, we need a surface to QI off of.

                                                      ZeroMemory( @ddsd, sizeof(ddsd) );
                                                      ddsd.dwSize         := sizeof(ddsd);
                                                      ddsd.dwFlags        := DDSD_CAPS;
                                                      ddsd.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE;

                                                      hr := pDDraw.SetCooperativeLevel( 0, DDSCL_NORMAL );
                                                      if FAILED( hr )
                                                        then
                                                          begin
                                                            // Failure. This means DDraw isn't properly installed.
                                                            FreeLibrary( DDHinst );
                                                            result := 0;
                                                          end
                                                        else
                                                          begin
                                                            hr := pDDraw.CreateSurface( ddsd, pSurf, nil );
                                                            if FAILED( hr )
                                                              then
                                                                begin
                                                                  // Failure. This means DDraw isn't properly installed.
                                                                  FreeLibrary( DDHinst );
                                                                  result := 0;
                                                                end
                                                              else
                                                                begin
                                                                  // Query for the IDirectDrawSurface3 interface
                                                                  if FAILED( pSurf.QueryInterface( IID_DDS3, pSurf3 ) )
                                                                    then FreeLibrary( DDHinst )
                                                                    else
                                                                      begin
                                                                        // QI for IDirectDrawSurface3 succeeded. We must be at least DX5
                                                                        result := 5;


                                                                        ///////////////////////////////////////////////////////////////////////////
                                                                        // DirectX 6.0 Checks
                                                                        ///////////////////////////////////////////////////////////////////////////

                                                                        // The IDirectDrawSurface4 interface was introduced with DX 6.0
                                                                        if FAILED( pSurf.QueryInterface( IID_DDS4, pSurf4 ))
                                                                          then FreeLibrary( DDHinst )
                                                                          else
                                                                            begin
                                                                              // IDirectDrawSurface4 was create successfully. We must be at least DX6
                                                                              result := 6;

                                                                              ///////////////////////////////////////////////////////////////////////////
                                                                              // DirectX 7.0 Checks
                                                                              ///////////////////////////////////////////////////////////////////////////

                                                                              // Check for DirectX 7 by creating a DDraw7 object
                                                                              fnDirectDrawCreateEx := GetProcAddress( DDHinst, 'DirectDrawCreateEx' );
                                                                              if not Assigned(fnDirectDrawCreateEx)
                                                                                then FreeLibrary( DDHinst )
                                                                                else
                                                                                  begin
                                                                                    if FAILED( DirectDrawCreateEx( nil, pDD7, IID_DD7, nil ) )
                                                                                      then FreeLibrary( DDHinst )
                                                                                      else
                                                                                        begin
                                                                                          // DDraw7 was created successfully. We must be at least DX7.0
                                                                                          result := 7;
                                                                                          FreeLibrary( DDHinst );
                                                                                        end;
                                                                                  end;
                                                                            end;
                                                                      end;
                                                                end;
                                                          end;
                                                    end;
                                              end;
                                        end;
                                  end;
                            end;
                      end;
                end;
          end;
      if result <> 0
        then Desc := 'DirectX' + inttostr(result)
        else Desc := 'No versions of DirectX found';
    end;

  function TSPSplash.getIEVersion( out Desc : string ) : integer;
    var
      Registry : TRegistry;
    begin
      Registry := TRegistry.Create;
      try
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        Registry.OpenKey( 'Software\Microsoft\Internet Explorer', false );
        Desc := Registry.ReadString( 'Version' );
        if Desc <> ''
          then
            try
              result := strtoint(Desc[1])
            except
              result := 0;
            end
          else result := 0;
      finally
        Registry.Free;
      end;
    end;

  function TSPSplash.getWMPVersion( out Desc : string ) : integer;
    var
      Registry : TRegistry;
    begin
      Registry := TRegistry.Create;
      try
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        Registry.OpenKey( 'Software\Microsoft\MediaPlayer\PlayerUpgrade', false );
        Desc := Registry.ReadString( 'PlayerVersion' );
        if Desc <> ''
          then
            try
              result := strtoint(Desc[1])
            except
              result := 0;
            end
          else result := 0;
      finally
        Registry.Free;
      end;
    end;

  procedure TSPSplash.UpdateVersions;
    const
      RequiredDXVersion  = 3;
      ProvidedDXVersion  = 7;

      RequiredIEVersion  = 4;
      ProvidedIEVersion  = 5;

      RequiredWMPVersion = 6;
      ProvidedWMPVersion = 6;
    var
      ver     : integer;
      aux     : string;
    begin
      try
        ver := getDXVersion( aux, fUsingNT );

        if ver < RequiredDXVersion
          then
            begin
              if not fUsingNT
                then fDXCaption := 'You should install DirectX '
                else fDXCaption := 'As you are using Windows NT you are not able to install DirectX';
            end
          else
            if (ver >= RequiredDXVersion) and (ver < ProvidedDXVersion)
              then
                begin
                  if not fUsingNT
                    then fDXCaption := 'Your DirectX version allows you to run the game, but you can upgrade to DirectX7'
                    else fDXCaption := 'Your DirectX version allows you to run the game';
                end
              else fDXCaption := 'Your are using the same or a newer version of the provided DirectX';
      except
        fDXCaption := 'You should install DirectX';
      end;

      try
        ver := getIEVersion( aux );
        if ver < RequiredIEVersion
          then fIECaption := 'You should install Internet Explorer'
          else
            if (ver >= RequiredIEVersion) and (ver < ProvidedIEVersion)
              then fIECaption := 'Your Internet Explorer version allows you to run the game, but you can upgrade to Internet Explorer 5.0'
              else fIECaption := 'Your are using the same or a newer version of the provided Internet Explorer';
      except
        fIECaption := 'You should install Internet Explorer';
      end;

      try

        ver := getWMPVersion( aux );
        if ver < RequiredWMPVersion
          then fWMPCaption := 'You should install Windows Media Player'
          else
            if (ver >= RequiredWMPVersion) and (ver < ProvidedWMPVersion)
              then fWMPCaption := 'Your Windows Media Player version allows you to run the game, but you can upgrade to Windows Media Player 6.4'
              else fWMPCaption := 'Your are using the same or a newer version of the provided Windows Media Player';
      except
        fWMPCaption := 'You should install Windows Media Player';
      end;

      btnInstall.Hint := 'Click here to install Star Peace on your computer';
      btnDX.Hint      := fDXCaption;
      btnIE.Hint      := fIECaption;
      btnWMP.Hint     := fWMPCaption;
      btnCancel.Hint  := 'Cancel the instalation';
    end;

  procedure TSPSplash.FormCreate(Sender: TObject);
    begin
      UpdateVersions;
    end;

  procedure TSPSplash.btnInstallMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      lblStatus.Caption := TControl(Sender).Hint;
    end;

  procedure TSPSplash.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      lblStatus.Caption := '';
    end;

  procedure TSPSplash.btnCancelClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TSPSplash.btnWMPClick(Sender: TObject);
    const
      WMPInstall = 'WMPSetup\mpfull.exe';
      WMPParams  = '';
    var
      Registry : TRegistry;
    begin
      Registry := TRegistry.Create;
      try
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        Registry.OpenKey( 'Software\Microsoft\MediaPlayer\Setup', true );
        Registry.WriteString( 'URLAtCompletion', 'No' );
      finally
        Registry.Free;
      end;
      ShellExecute( Handle, nil, pchar(AppPath + WMPInstall), pchar(WMPParams), nil, SW_SHOW );
    end;

  procedure TSPSplash.btnDXClick(Sender: TObject);
    const
      DX7Install = 'DX7Setup\dx7aeng.exe';
      DX7Params  = '';
    begin
      ShellExecute( Handle, nil, pchar(AppPath + DX7Install), pchar(DX7Params), nil, SW_SHOW );
    end;

  procedure TSPSplash.btnInstallClick(Sender: TObject);
    const
      Install = 'Setup\Setup.exe';
      Params  = '';
    begin
      ShellExecute( Handle, nil, pchar(AppPath + Install), pchar(Params), nil, SW_SHOW );
    end;

end.
