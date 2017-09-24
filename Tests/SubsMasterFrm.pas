unit SubsMasterFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMainForm = class(TForm)
    eAlias: TEdit;
    btnCheck: TButton;
    mProp: TMemo;
    btnUnsubs: TButton;
    eServer: TEdit;
    ePort: TEdit;
    rbAlias: TRadioButton;
    rbSubs: TRadioButton;
    RadioButton3: TRadioButton;
    procedure btnCheckClick(Sender: TObject);
    procedure btnUnsubsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetProxy(var Proxy : OleVariant) : boolean;
  private
    fAlias   : string;
    fSubsId  : integer;
    fChgDate : TDateTime;
  end;

var
  MainForm: TMainForm;

implementation

  uses
    RDOInterfaces, RDOObjectProxy, WinSockRDOConnection, ComObj,
    DirectoryServerProtocol;

{$R *.DFM}

  function TMainForm.GetProxy(var Proxy : OleVariant) : boolean;
    var
      DSCnnt  : IRDOConnectionInit;
      DSProxy : OleVariant;
      sId     : integer;
    begin
      Proxy  := Unassigned;
      DSCnnt := TWinSockRDOConnection.Create('Main');
      DSCnnt.Server := eServer.Text;
      DSCnnt.Port := StrToInt(ePort.Text);
      if DSCnnt.Connect(20*1000)
        then
          begin
            DSProxy := TRDOObjectProxy.Create as IDispatch;
            DSProxy.SetConnection(DSCnnt);
            DSProxy.TimeOut := 20*1000;
            if DSProxy.BindTo('DirectoryServer')
              then
                begin
                  sId := DSProxy.RDOOpenSession;
                  if (sId <> 0) and DSProxy.BindTo(sId)
                    then
                      begin
                        result := true;
                        Proxy  := DSProxy;
                      end
                    else result := false;
                end
              else result := false;
          end
        else result := false;
    end;

  procedure TMainForm.btnCheckClick(Sender: TObject);
    var
      Proxy   : OleVariant;
      dt      : TDateTime;
      usrInfo : string;
    begin
      try
        mProp.Clear;
        usrInfo := trim(eAlias.Text);
        if GetProxy(Proxy) and (usrInfo <> '')
          then
            begin
              if rbAlias.Checked
                then fAlias := usrInfo
                else
                  if rbSubs.Checked
                    then
                      begin
                        if Proxy.RDOSetCurrentKey('root/subcriptions/' + usrInfo)
                          then fAlias := Proxy.RDOReadString('alias')
                          else fAlias := usrInfo;
                      end
                    else
                      begin
                        if Proxy.RDOSetCurrentKey('root/transactions/' + usrInfo)
                          then fAlias := Proxy.RDOReadString('alias')
                          else fAlias := usrInfo;
                      end;
              if Proxy.RDOSetCurrentKey(GetUserPath(fAlias))
                then
                  begin
                    mProp.Lines.Add('Alias: ' + fAlias);
                    if Proxy.RDOReadInteger('AccountStatus') = '0'
                      then mProp.Lines.Add('Status: SUBSCRIBED')
                      else mProp.Lines.Add('Status: TRIAL');
                    mProp.Lines.Add('Real Name: ' + Proxy.RDOReadString('name'));
                    mProp.Lines.Add('Password: ' + Proxy.RDOReadString('password'));
                    mProp.Lines.Add('Email: ' + Proxy.RDOReadString('email'));
                    fSubsId := Proxy.RDOReadInteger('SubscriptionId');
                    mProp.Lines.Add('SubscriptionId: ' + IntToStr(fSubsId));
                    dt := Proxy.RDOReadDate('created');
                    mProp.Lines.Add('Created: ' + DateTimeToStr(dt));
                    dt := Proxy.RDOReadDate('trialexpires');
                    mProp.Lines.Add('Trial Expires: ' + DateTimeToStr(dt));
                    fChgDate := Proxy.RDOReadDate('chargedate');
                    if fChgDate < dt
                      then fChgDate := dt;
                    mProp.Lines.Add('Charge Date: ' + DateTimeToStr(fChgDate));
                    mProp.Lines.Add('Serial No: ' + Proxy.RDOReadString('accountid'));
                    btnUnsubs.Enabled := true;
                  end
                else beep;
            end
          else beep;
      except
        btnUnsubs.Enabled := false;
      end;
    end;

  procedure TMainForm.btnUnsubsClick(Sender: TObject);
    var
      Proxy   : OleVariant;
    begin
      try
        if GetProxy(Proxy) and (fAlias <> '') and (Proxy.RDOUnsubscribe(fAlias, IntToStr(fSubsId)) = 0)
          then
            begin
              btnUnsubs.Enabled := false;
              if Proxy.RDOSetCurrentKey(GetUserPath(fAlias))
                then Proxy.RDOWriteDate('TrialExpires', fChgDate);
            end
          else beep;
      except
        ShowMessage(Format('Could not Unsubscribe %s %d', [fAlias, fSubsId]));
      end;
    end;

  procedure TMainForm.FormShow(Sender: TObject);
    begin
      eAlias.SetFocus;
      eAlias.SelectAll;
    end;

end.

