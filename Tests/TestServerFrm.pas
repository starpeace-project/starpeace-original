unit TestServerFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WSObjectCacher;

type
  TForm1 = class(TForm)
    CloseButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  uses
    CacheObjects;

{$R *.DFM}

  procedure TForm1.CloseButtonClick(Sender: TObject);
    begin
      Close;
    end;

initialization

  SetCacheBasePath('c:\work\test');

end.
