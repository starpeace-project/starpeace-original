unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, InventionSets;

type
  TForm1 = class(TForm)
    Add: TButton;
    seNum: TSpinEdit;
    Test: TButton;
    Remove: TButton;
    procedure AddClick(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
  private
    fSet : TInventionSet;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  procedure TForm1.AddClick(Sender: TObject);
    begin
      if fSet = nil
        then fSet := TInventionSet.Create(0);
      fSet.Include(seNum.Value);
    end;

  procedure TForm1.TestClick(Sender: TObject);
    begin
      if fSet.Included(seNum.Value)
        then ShowMessage('YES!')
        else ShowMessage('NOPE!');
    end;

  procedure TForm1.RemoveClick(Sender: TObject);
    begin
      fSet.Exclude(seNum.Value);
    end;

end.
