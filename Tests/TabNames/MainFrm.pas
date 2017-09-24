unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    ePath: TEdit;
    Button1: TButton;
    mTags: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadIniFile(path : string);
  private
    fTabs : TStringList;
  end;

var
  Form1: TForm1;

implementation

  uses
    IniFiles;

{$R *.DFM}

procedure TForm1.LoadIniFile(path : string);
  var
    iniFile : TIniFile;
    tabs, i : integer;
    aux     : string;
  begin
    iniFile := TIniFile.Create(path);
    try
      tabs := iniFile.ReadInteger('InspectorInfo', 'TabCount', 0);
      for i := 0 to pred(tabs) do
        begin
          aux := iniFile.ReadString('InspectorInfo', 'TabName' + IntToStr(i), '');
          fTabs.Add(aux);
        end;
    finally
      iniFile.Free;
    end;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  var
    SrcRec : TSearchRec;
    path   : string;
    found  : boolean;
  begin
    mTags.Clear;
    path  := ePath.Text;
    found := FindFirst(path + '\*.ini', faArchive, SrcRec) = 0;
    while found do
      begin
        LoadIniFile(path + '\' + SrcRec.Name);
        found := FindNext(SrcRec) = 0;
      end;
    mTags.Lines := fTabs;
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
    fTabs := TStringList.Create;
    fTabs.Sorted := true;
    fTabs.Duplicates := dupIgnore;
  end;

end.
