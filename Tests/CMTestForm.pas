unit CMTestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Open: TButton;
    ObjName: TEdit;
    procedure OpenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  uses
    ComObj;

{$R *.DFM}

procedure TForm1.OpenClick(Sender: TObject);
  var
    O : OleVariant;
  begin
    O := CreateOleObject('CacheManager.CachedObject');
    if O.SetObjectOfWorld(33, 33, 'Tuguria')
      then ShowMessage('Pincha!!!');
  end;

end.
