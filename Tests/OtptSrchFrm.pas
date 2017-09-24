unit OtptSrchFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OutputSearch, CacheObjects, Spin, SpecialChars;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    Make: TButton;
    Search: TButton;
    X: TSpinEdit;
    Y: TSpinEdit;
    Count: TSpinEdit;
    Town: TEdit;
    Company: TEdit;
    Path: TEdit;
    procedure MakeClick(Sender: TObject);
    procedure SearchClick(Sender: TObject);
  private
    fSearch : TOutputSearchResult;
  private
    procedure CreateFile(const name : string);
  public
    ThePath : string;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  procedure TForm1.CreateFile(const name : string);
    var
      TF : TextFile;
    begin
      system.assign(TF, ThePath + name);
      system.rewrite(TF);
      system.close(TF);
    end;

  function GenName(size : integer) : string;
    var
      i : integer;
    begin
      SetLength(Result, size);
      for i := 0 to pred(size) do
        pchar(result)[i] := char(byte('A') + random(byte('Z') - byte('A')));
    end;

  procedure TForm1.MakeClick(Sender: TObject);
    var
      i, j : integer;
    begin
      Randomize;
      ThePath := Path.Text;
      for i := 51 to Count.Value do
        for j := 51 to Count.Value do
          begin
            CreateFile(IntToStr(i) + NameSeparator + IntToStr(j) + NameSeparator + IntToStr(Random(100)) + NameSeparator + IntToStr(Random(100)) + NameSeparator + GenName(5) + NameSeparator + GenName(5) + NameSeparator + GenName(5));
            //Application.ProcessMessages;
          end;
    end;

  procedure TForm1.SearchClick(Sender: TObject);
    var
      Os : TOuputSearch;
      FI : TFolderIterator;
      i  : integer;
      tm : TDateTime;
    begin
      tm := Now;
      Os := TOuputSearch.Create('e:\test\files\', Town.Text, Company.Text, Count.Value, X.Value, Y.Value);
      //FI := TFolderIterator.Create('e:\test\files\', '*,*,*' + Town.Text + '.' + Company.Text + '.five', ioBoth);
      (*
      if not FI.Empty
        then
          repeat
            fSearch.Add(FI.Current);
          until not FI.Next;
      *)
      tm := Now - tm;
      Caption := TimeToStr(tm);
      Memo.Clear;
      (*
      for i := 0 to pred(fSearch.Count) do
        Memo.Lines.Add(fSearch[i].Name)
      *)
      for i := 0 to pred(Os.Result.Count) do
        Memo.Lines.Add(Os.Result[i].Utility);
      Os.Free;
    end;

end.
