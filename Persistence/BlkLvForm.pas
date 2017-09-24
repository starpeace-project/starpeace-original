unit BlkLvForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Create: TButton;
    Target: TEdit;
    procedure CreateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  uses
    BlockLevels;

{$R *.DFM}

  procedure TForm1.CreateClick(Sender: TObject);
    var
      BlkLv  : TLevelManager;
      Stream : TStream;
    begin
      BlkLv := TLevelManager.Create;
      Stream := TFileStream.Create(Target.Text, fmOpenRead);
      try
        if BlkLv.CreateLevels(Stream, ExtractFilePath(Target.Text))
          then ShowMessage('Pincha!!')
          else ShowMessage('NIET!!')
      finally
        Stream.Free;
        BlkLv.Free;
      end;
    end;

end.
