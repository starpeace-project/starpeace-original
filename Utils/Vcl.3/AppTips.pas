unit AppTips;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, RangeStrUtils;

  type
    TApplicationTips =
      class( TForm )
          imHintIcon           : TImage;
          llWelcome            : TLabel;
          llAppName            : TLabel;
          btSeeTour            : TButton;
          btWhatsNew           : TButton;
          btOnlineRegistration : TButton;
          btNextTip            : TButton;
          bvButtonSep          : TBevel;
          btClose              : TButton;
          paHintText           : TPanel;
          llDidYouKnow         : TLabel;
          meHintText           : TMemo;
          cbShowWelcome        : TCheckBox;
          llcbAppName          : TLabel;

          procedure FormCreate( Sender : TObject );

          procedure btNextTipClick( Sender : TObject );
          procedure btSeeTourClick( Sender : TObject );
          procedure btWhatsNewClick( Sender : TObject );
          procedure btOnlineRegistrationClick( Sender : TObject );

        protected
          fTipRange   : TRangeStr;
          fTipCurrent : integer;
          fTipsKey    : HKEY;

          procedure GetNextTip;

        protected
          fOnSeeTour   : TNotifyEvent;
          fOnWhatsNew  : TNotifyEvent;
          fOnOnlineReg : TNotifyEvent;

        public
          procedure ShowWelcome;
          procedure ShowTips;

        published
          property OnSeeTour   : TNotifyEvent read fOnSeeTour   write fOnSeeTour;
          property OnWhatsNew  : TNotifyEvent read fOnWhatsNew  write fOnWhatsNew;
          property OnOnlineReg : TNotifyEvent read fOnOnlineReg write fOnOnlineReg;
      end;

  var
    ApplicationTips : TApplicationTips;

  var
    AppRegistryPath : string;
    AppName         : string;

  procedure RegisterTip( Indx : integer; const TipText : string );
  procedure TipRangeRegister( const TipsName : string; FromIndx, ToIndx : integer );
  function  TipRangeRegistered( const TipsName : string ) : boolean;

implementation

  {$R *.DFM}

  uses
    Registry, RegUtils, WinUtils;

  procedure RegisterTip( Indx : integer; const TipText : string );
    begin
      SetRegValue( HKEY_LOCAL_MACHINE, AppRegistryPath + '\Tips\' + IntToStr( Indx ), TipText );
    end;

  function TipRangeRegistered( const TipsName : string ) : boolean;
    begin
      Result := GetRegValue( HKEY_LOCAL_MACHINE, AppRegistryPath + '\Tips\' + TipsName ) = '1';
    end;

  procedure TipRangeRegister( const TipsName : string; FromIndx, ToIndx : integer );
    var
      TipsKey  : HKEY;
      TipRange : TRangeStr;
    begin
      TipsKey := GetWritableKey( HKEY_LOCAL_MACHINE, pchar( AppRegistryPath + '\Tips' ) ); //RegOpenKeyEx( HKEY_LOCAL_MACHINE, pchar( AppRegistryPath + '\Tips' ), 0, KEY_WRITE, TipsKey );

      TipRange := TRangeStr.Create( GetRegValue( TipsKey, 'TipRange' ) );
      TipRange.Add( FromIndx, ToIndx );

      SetRegValue( TipsKey, 'TipRange', TipRange.RangeStr );
      SetRegValue( TipsKey, TipsName, '1' );
      RegCloseKey( TipsKey );

      TipRange.Free;
    end;

  procedure TApplicationTips.ShowWelcome;
    var
      OldCloseTop : integer;
      OldHeight   : integer;
    begin
      btNextTip.Visible := false;
      cbShowWelcome.Visible := false;
      llcbAppName.Visible := false;
      bvButtonSep.Visible := false;
      OldCloseTop := btClose.Top;
      btClose.Top := bvButtonSep.Top;
      OldHeight := Height;
      Height := btClose.Top + btClose.Height + paHintText.Top; // Bottom of btClose + Margin...

      ShowTips;

      btNextTip.Visible := true;
      cbShowWelcome.Visible := true;
      bvButtonSep.Visible := true;
      llcbAppName.Visible := true;
      btClose.Top := OldCloseTop;
      Height := OldHeight;
    end;

  // This might be a little messy, but since it's not that important I'm not fixing it
  // The problem is I'm using a TRegIniFile for accessing HKEY_CURRENT_USER keys, and
  // my own RegUtils for accessing HKEY_LOCAL_MACHINE keys. I should have standarized
  // on one of them! Knowing that, you'll should have no trouble finding in which branch
  // the registry information is stored.
  procedure TApplicationTips.ShowTips;
    var
      HinterIni : TRegIniFile;
      ShowMsg   : boolean;
    begin
      HinterIni := TRegIniFile.Create( AppRegistryPath );
      try
        with HinterIni do
          begin
            ShowMsg := ReadBool( 'Tips', 'ShowWelcome', true );                                   // HKCU
            if ShowMsg
              then
                begin
                  fTipsKey := GetWritableKey( HKEY_LOCAL_MACHINE, AppRegistryPath + '\Tips' );    // HKLM
                  try
                    if ( fTipsKey <> 0 ) and
                       ( GetRegValue( fTipsKey, 'LastRead' ) <> DateTimeToStr( Date ) )           // HKLM
                      then
                        begin
                          cbShowWelcome.Checked := true;
                          llcbAppName.Caption   := AppName;
                          llAppName.Caption     := AppName;

                          // Read registry keys
                          fTipRange   := TRangeStr.Create( GetRegValue( fTipsKey, 'TipRange' ) ); // HKLM
                          SetRegValue( fTipsKey, 'TipRange', fTipRange.RangeStr );                // HKLM
                          fTipCurrent := ReadInteger( 'Tips', 'TipCurrent', 0 );                  // HKCU

                          GetNextTip;

                          Position := poScreenCenter;
                          ShowModal;

                          fTipRange.Free;

                          WriteInteger( 'Tips', 'TipCurrent', fTipCurrent );                      // HKCU
                          SetRegValue( fTipsKey, 'LastRead', DateTimeToStr( Date ) );             // HKLM
                          if not cbShowWelcome.Checked
                            then WriteBool( 'Tips', 'ShowWelcome', false );                       // HKCU
                        end;
                  finally
                    RegCloseKey( fTipsKey );
                  end;

                end;
          end;
      finally
        HinterIni.Free;
      end;
    end;

  procedure TApplicationTips.FormCreate( Sender : TObject );
    begin
      Icon := Application.Icon;
      SetWindowSizeable( Handle, false );
    end;

  procedure TApplicationTips.GetNextTip;
    begin
      meHintText.Text := GetRegValue( fTipsKey, IntToStr( fTipCurrent ) );
      fTipCurrent := fTipRange.Next( fTipCurrent );
    end;

  procedure TApplicationTips.btNextTipClick( Sender : TObject );
    begin
      GetNextTip;
    end;

  procedure TApplicationTips.btSeeTourClick( Sender : TObject );
    begin
      if Assigned( OnSeeTour )
        then OnSeeTour( Self );
    end;

  procedure TApplicationTips.btWhatsNewClick( Sender : TObject );
    begin
      if Assigned( OnWhatsNew )
        then OnWhatsNew( Self );
    end;

  procedure TApplicationTips.btOnlineRegistrationClick( Sender : TObject );
    begin
      if Assigned( OnOnlineReg )
        then OnOnlineReg( Self );
    end;

end.
