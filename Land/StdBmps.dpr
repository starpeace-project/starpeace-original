program StdBmps;

uses
  Forms,
  StdBmpForm in 'StdBmpForm.pas' {StdBmpViewer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TStdBmpViewer, StdBmpViewer);
  Application.Run;
end.
