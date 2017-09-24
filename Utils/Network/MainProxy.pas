unit MainProxy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ProxyCfgFrame, ExtCtrls, Buttons;

type
  TProxySettingsMainForm =
    class(TForm)
        TProxyConfig1: TProxyConfig;
        procedure FormShow(Sender: TObject);
        procedure TProxyConfig1Button2Click(Sender: TObject);
        procedure TProxyConfig1Button1Click(Sender: TObject);
      private
        { Private declarations }
    end;

var
  ProxySettingsMainForm: TProxySettingsMainForm;

implementation

{$R *.DFM}

{ TForm3 }
procedure TProxySettingsMainForm.FormShow(Sender: TObject);
  begin
//    TProxyConfig1.ExtractInternetExplorerConfig;
  end;

procedure TProxySettingsMainForm.TProxyConfig1Button2Click(Sender: TObject);
  begin
    TProxyConfig1.SpeedButton2Click(Sender);
  end;

procedure TProxySettingsMainForm.TProxyConfig1Button1Click(Sender: TObject);
  begin
    close;
  end;

end.
