unit AppHints;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, RegUtils;

  type
    TApplicationHints =
      class( TForm )
          btOK            : TButton;
          cbAlwaysShowMsg : TCheckBox;
          paHintText      : TPanel;
          meHintText      : TMemo;
          imHintIcon      : TImage;
          llHintTitle     : TLabel;

          procedure FormCreate( Sender: TObject );

        public
          procedure ShowHintMessageRes( HintTitleRes, HintMessageRes : integer );
          procedure ShowHintMessage( HintMessageId : integer; const HintTitle, HintMessage : string );
      end;

  // AppRegistryPath: Set it to the path where you want to store the information of whether showing this
  // hint or not (It's stored in the user branch of registry, so it's different for each one)
  //
  // ShowHintMessage:
  //    HintMessageId:   The id that will be used in the registry to access this hint state
  //    HintTitle:       The hint title
  //    HintMessage:     The hint text
  //
  // ShowHintMessageRes: Use it for resource strings
  //    HintTitleRes:    The string resource number of the hint title, also as the HintMessageId
  //    HintMessageRes:  The string resource number of the hint text
  //
  // Usage example:
  // -------------
  //    // Assuming that AppRegistryPath is 'Software\Merchise\TestApp\Current Version',
  //    // hints now will be saved in 'HKCU\Software\Merchise\TestApp\Current Version\Hints';
  //
  //    // These two calls will show dialogs only if they are in the 'Always show this message' state:
  //    ApplicationHints.ShowHintMessage( idLogNet, 'Logging on to network', 'Now you should select a valid(...)' );
  //    ApplicationHints.ShowHintMessageRes( sLogToNet, sLogToNetText );

  var
    ApplicationHints : TApplicationHints;

implementation

  {$R *.DFM}

  uses
    Registry, WinUtils;

  procedure TApplicationHints.ShowHintMessageRes( HintTitleRes, HintMessageRes : integer );
    begin
      ShowHintMessage( HintTitleRes, LoadStr( HintTitleRes ), LoadStr( HintMessageRes ) );
    end;

  procedure TApplicationHints.ShowHintMessage( HintMessageId : integer; const HintTitle, HintMessage : string );
    var
      HinterIni : TRegIniFile;
      ShowMsg   : boolean;
    begin
      HinterIni := TRegIniFile.Create( AppRegistryPath );
      try
        with HinterIni do
          begin
            ShowMsg := ReadBool( 'Hints', IntToStr( HintMessageId ), true );
            if ShowMsg
              then
                begin
                  cbAlwaysShowMsg.Checked := true;
                  llHintTitle.Caption     := HintTitle;
                  meHintText.Text         := HintMessage;

                  ShowModal;
                  WriteBool( 'Hints', IntToStr( HintMessageId ), cbAlwaysShowMsg.Checked );
                end;
          end;
      finally
        HinterIni.Free;
      end;
    end;

  procedure TApplicationHints.FormCreate( Sender : TObject );
    begin
      Icon := Application.Icon;
      SetWindowSizeable( Handle, false ); // Avoid window resizing
    end;

end.
