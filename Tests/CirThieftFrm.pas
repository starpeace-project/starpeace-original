unit CirThieftFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    ePath: TEdit;
    Label1: TLabel;
    eStartMark: TEdit;
    eEndMark: TEdit;
    btnGO: TButton;
    procedure btnGOClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnGOClick(Sender: TObject);
  var
    flName  : string;
    aux     : string;
    tmp     : string;
    TxtFile : TextFile;
    done    : boolean;
    Lines   : TStringList;
    len, p  : integer;
  begin
    flName := ePath.Text;
    AssignFile(TxtFile, flName);
    Reset(TxtFile);
    done := false;

    Lines := TStringList.Create;

    while not done and not EOF(TxtFile) do
      begin
        ReadLn(TxtFile, aux);
        p := pos('CreateCircuitSeg:', aux);
        if (p <> 0) and (pos('OK', aux) = 0)
          then
            begin
              len := Length(aux);
              tmp := System.Copy(aux, p, len - p + 1);
              Lines.Add(tmp);
            end;
        p := pos('WipingCircuit:', aux);
        if (p <> 0) and (pos('OK', aux) = 0)
          then
            begin
              len := Length(aux);
              tmp := System.Copy(aux, p, len - p + 1);
              Lines.Add(tmp);
            end;
      end;

    CloseFile(TxtFile);

    Lines.SaveToFile(ExtractFilePath(flName) + 'CircuitsOps.txt');
    Lines.Free;
  end;

end.
