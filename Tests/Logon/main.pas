unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PDTabControl, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    PDTabControl1: TPDTabControl;
    Notebook: TNotebook;
    MapPanel: TPanel;
    Map: TImage;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormShow(Sender: TObject);
begin
  Top := 0;
  Left := 0;
  Width := Screen.Width;
  Height := Screen.Height;
  Map.Left := (MapPanel.Width - Map.Width) div 2;
  Map.Top := (MapPanel.Height - Map.Height) div 2;
end;


end.
