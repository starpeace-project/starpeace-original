unit MLKitFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CompStringsParser;

{$R *.DFM}

procedure ProcessFile(path : string);
  var
    L : TStringList;
    i : integer;
    s : string;
    p : integer;
  begin
    L := TStringList.Create;
    try
      L.LoadFromFile(path);
      for i := 0 to pred(L.Count) do
        begin
          p := 1;
          s := CompStringsParser.GetNextStringUpTo(L[i], p, #9);
          inc(p);
          CompStringsParser.GetNextStringUpTo(L[i], p, #9);
          inc(p);
          s := s + '=' + CompStringsParser.GetNextStringUpTo(L[i], p, #9);
          L[i] := s;
        end;
      L.SaveToFile(path);
    finally
      L.Free;
    end;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  var
    path : string;
    src  : TSearchrec;
  begin
    if OpenDialog1.Execute
      then
        begin
          path := ExtractFilePath(OpenDialog1.FileName);
          if FindFirst(path + '*.tln', faArchive, src) = 0
            then
              repeat
                ProcessFile(path + src.Name);
              until FindNext(src) <> 0
        end;
  end;

end.
