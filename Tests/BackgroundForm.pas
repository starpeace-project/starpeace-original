unit BackgroundForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TBackForm = class(TForm)
    StartLog: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BackForm: TBackForm;

implementation

{$R *.DFM}

  procedure TBackForm.FormCreate(Sender: TObject);
    begin
      Top := 0;
      Left := 0;
      Width := Screen.Width;
      Height := Screen.Height;
    end;



end.
