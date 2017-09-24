unit Mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin;

type
  TForm3 = 
    class(TForm)
        ToolBar1: TToolBar;
        TreeView1: TTreeView;
        StatusBar1: TStatusBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        ToolButton5: TToolButton;
        procedure ToolButton4Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure ToolButton5Click(Sender: TObject);
      private
        kk : pointer;
      public
        aa: array of integer;
    end;

var
  Form3: TForm3;

implementation

{$R *.DFM}

type
  TCaca =
    class
      fcaca    : string;
      fNumCaca : integer;
    end;
    
procedure TForm3.ToolButton4Click(Sender: TObject);
  begin
    GetMem(kk, 2991);
  end;

procedure TForm3.ToolButton2Click(Sender: TObject);
  begin
    setlength(aa, 1000);
  end;

procedure TForm3.ToolButton5Click(Sender: TObject);
  begin
    TCaca.create;
  end;

end.
