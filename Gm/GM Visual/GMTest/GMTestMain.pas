unit GMTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GMCostumer, GameMaster, GMServer, GMIntServer,
  CustomerChat, GMChat, StdCtrls;

type
  TMainTestForm =
    class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
      private
        fIServer  : TGMInterfaceServer;
        fGMServer : TGMServer;
    end;

var
  MainTestForm: TMainTestForm;

implementation

{$R *.DFM}

  procedure TMainTestForm.FormCreate(Sender: TObject);
    begin
      fGMServer := TGMServer.Create;
      fIServer  := TGMInterfaceServer.Create;
      fIServer.setGMServer( fGMServer );
    end;

procedure TMainTestForm.Button1Click(Sender: TObject);
var
  Customer   : TGMCustomer;
  ChatClient : TClientChat;
begin
  if Edit1.Text <> ''
    then
      begin
        Customer   := TGMCustomer.Create( Edit1.Text );
        Customer.setIntServer( fIServer );
        fIServer.RegisterCustomer( Customer.Id );
        ChatClient := TClientChat.Create( self );
        ChatClient.setCostumer( Customer );
        ChatClient.Show;
        Edit1.Text := '';
      end;
end;

  procedure TMainTestForm.Button2Click(Sender: TObject);
    var
      GameMaster : TGameMaster;
      GMView     : TGMView;
    begin
      GameMaster := TGameMaster.Create;
      GameMaster.setGMConnection( fGMServer );
      GMView := TGMView.Create( self );
      GMView.setGameMaster( GameMaster );
      GMView.Show;
    end;

end.

