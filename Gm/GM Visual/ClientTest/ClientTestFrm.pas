unit ClientTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, GMKernel, GMIntServer, GMCostumer, ExtCtrls, GMList;

type
  TGMClientMain =
    class(TForm)
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        ServerAddr: TEdit;
        Label3: TLabel;
        ServerPort: TEdit;
        Label4: TLabel;
        Button1: TButton;
        TabSheet2: TTabSheet;
        UserAlias: TEdit;
        Label1: TLabel;
        Button2: TButton;
        ConnectionMethd: TTabSheet;
        Button3: TButton;
        GameMasterList: TListBox;
        procedure Button2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
    end;

var
  GMClientMain: TGMClientMain;

implementation

  uses
    CustomerChat, GMIServerRDOMger, Threads, ConnectOptionsFrm;

{$R *.DFM}

  // TForm1

  procedure TGMClientMain.Button2Click(Sender: TObject);
    var
      Customer   : TGMCustomer;
      ChatClient : TClientChat;
    begin
      if UserAlias.Text <> ''
        then
          begin
            Customer   := TGMCustomer.Create( UserAlias.Text );
            Customer.setIntServer( TheIServerRDOMger.IntServer );
            TheIServerRDOMger.IntServer.RegisterCustomer( integer(IGMCustomer(Customer)), Customer.Id );

            ChatClient := TClientChat.Create( self );
            ChatClient.setCostumer( Customer );
            ChatClient.Show;
            UserAlias.Text := '';
          end;
    end;

  procedure TGMClientMain.FormCreate(Sender: TObject);
    var
      Idx : integer;
      str : string;
    begin
      InitIServerRDOMger;
      BeginThreads;
    end;

  procedure TGMClientMain.Button1Click(Sender: TObject);
    begin
      TheIServerRDOMger.SetupRDO( ServerAddr.Text, StrToInt(ServerPort.Text) );
    end;

  procedure TGMClientMain.Button3Click(Sender: TObject);
    var
      GMConnOptions : TGMConnOptions;
    begin
      GMConnOptions := TGMConnOptions.Create( self );
      //GMConnOptions.setGameMasterList( fGameMasterList );
      GMConnOptions.ShowModal;
    end;

end.
