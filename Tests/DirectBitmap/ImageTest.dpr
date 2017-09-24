program ImageTest;

uses
  Forms,
  ImageForm in 'ImageForm.pas' {ImageTestWindow};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TImageTestWindow, ImageTestWindow);
  Application.Run;
end.
