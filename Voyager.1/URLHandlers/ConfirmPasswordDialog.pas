unit ConfirmPasswordDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InternationalizerComponent;

type
  TConfirmPasswordDlg = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    Label4: TLabel;
    ConfirmPassword: TEdit;
    Button1: TButton;
    Button2: TButton;
    InternationalizerComponent1: TInternationalizerComponent;
  end;

implementation

{$R *.DFM}


end.
