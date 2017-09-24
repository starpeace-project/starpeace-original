unit BlackBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TLogForm = class(TForm)
    mLogs: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    // >>
  end;

var
  LogForm: TLogForm;

implementation

  uses
    MainForm;

{$R *.DFM}

  procedure TLogForm.Label1Click(Sender: TObject);
    begin
      BlackBoxForm.lbBoxesDblClick(Sender);
    end;

end.
