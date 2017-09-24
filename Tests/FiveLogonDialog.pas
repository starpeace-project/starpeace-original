unit FiveLogonDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFiveLogonDlg = class(TForm)
    Notebook1: TNotebook;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ServerAddress: TEdit;
    ServerPort: TEdit;
    UserName: TEdit;
    Password: TEdit;
    Button1: TButton;
    Button2: TButton;
    ClientIP: TEdit;
    Label6: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FiveLogonDlg: TFiveLogonDlg;

implementation

{$R *.DFM}

  procedure TFiveLogonDlg.FormCreate(Sender: TObject);
    begin
      Top := 0;
      Left := 0;
      Width := Screen.Width;
      Height := Screen.Height;
    end;

end.
