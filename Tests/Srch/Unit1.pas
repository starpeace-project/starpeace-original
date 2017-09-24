unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, OutputSearch,
  StdCtrls, CacheCommon, AutoLog;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fSearch : TOutputSearchResult;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
  begin
    fSearch := TOutputSearchResult.Create(100, 0, 0, 1, [rolNeutral, rolProducer, rolDistributer, rolBuyer, rolImporter]);
  end;

procedure TForm1.Button2Click(Sender: TObject);
  begin
    fSearch.Add('D104}247}9}19120000}5}Hiroshima}Warehouse}Universal General Storage 8}58480460,');
    fSearch.Add('D110}247}11}15680000}5}Hiroshima}Warehouse}Universal General Storage 7}236296172,');
    fSearch.Add('D110}247}11}15680000}5}Hiroshima}Warehouse}Universal General Storage 7}58480460,');
  end;

initialization

  AutoLog.InitLogs;

end.
