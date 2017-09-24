unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    ePath: TEdit;
    Count: TButton;
    lbNames: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lbValues: TLabel;
    Label3: TLabel;
    lbFolders: TLabel;
    Label5: TLabel;
    lbNamesSize: TLabel;
    Label4: TLabel;
    lbValuesSize: TLabel;
    Label6: TLabel;
    lbLarger: TLabel;
    Label7: TLabel;
    lbTotal: TLabel;
    Names: TListBox;
    procedure CountClick(Sender: TObject);
  private
    fNames   : TStringList;
    fValues  : TStringList;
    fFolders : integer;
    fLarger  : integer;
    fLargerName : string;
    fTotal      : integer;
  private
    procedure CountName(List : TStringList; name : string);
    function  ListSize(List : TStringList) : integer;
    procedure CountList(List : TStringList);
    procedure CountFolder(path : string);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  function  TForm1.ListSize(List : TStringList) : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(List.Count) do
        inc(result, length(List[i]) + 4);
    end;

  procedure TForm1.CountName(List : TStringList; name : string);

    var
      n : string;
      i : integer;
      l : integer;
      p : integer;

    procedure DoCount;
      var
        idx : integer;
        frq : integer;
      begin
        if length(n) < 4 then exit;
        idx := List.IndexOf(n);
        if idx <> -1
          then
            begin
              frq := integer(List.Objects[idx]);
              inc(frq);
              List.Objects[idx] := TObject(frq);
            end
          else List.AddObject(n, TObject($00000001));
      end;
    begin
      l := length(name);
      if l > 1
        then
          begin
            i := 1;
            repeat
              p := i;
              inc(i);
              while (i <= l) and not (name[i] in ['A'..'Z']) and not (name[i] in ['0'..'9']) do
                inc(i);
              n := copy(name, p, i - p);
              while (i <= l) and (name[i] in ['0'..'9']) do
                inc(i);
              DoCount;
            until i >= l;
          end
        else
          begin
            n := name;
            DoCount;
          end;
    end;

  procedure TForm1.CountList(List : TStringList);
    var
      i : integer;
      s : string;
      n : string;
      v : string;
      p : integer;
    begin
      for i := 0 to pred(List.Count) do
        begin
          s := List[i];
          p := pos('=', s);
          if p = 0
            then n := s
            else
              begin
                n := copy(s, 1, p - 1);
                v := copy(s, p + 1, length(s) - p);
                CountName(fNames, n);
                fValues.Add(v);
              end;
        end;
    end;

  procedure TForm1.CountFolder(path : string);
    var
      Search : TSearchRec;
      List   : TStringList;
    begin
      inc(fFolders);
      if fFolders mod 5 = 0
        then
          begin
            lbFolders.Caption := IntToStr(fFolders);
            lbFolders.Refresh;
          end;
      if FindFirst(path + '\*.*', faDirectory + faArchive, Search) = 0
        then
          repeat
            if Search.Attr and faArchive <> 0
              then
                begin
                  inc(fTotal, Search.Size);
                  if Search.Size > fLarger
                    then
                      begin
                        fLarger := Search.Size;
                        fLargerName := path + '\' + Search.Name;
                      end;
                  List := TStringList.Create;
                  List.LoadFromFile(path + '\' + Search.Name);
                  CountList(List);
                  List.Free;
                end
              else
                if (Search.Name <> '.') and (Search.Name <> '..')
                  then CountFolder(path + '\' + Search.Name);
          until FindNext(Search) <> 0;
    end;

  procedure TForm1.CountClick(Sender: TObject);
    var
      i, j : integer;
      tmp : string;
      obj : TObject;
    begin
      fTotal      := 0;
      fLargerName := '';
      fLarger  := 0;
      fFolders := 0;
      fNames := TStringList.Create;
      fNames.Sorted := true;
      fNames.Duplicates := dupIgnore;
      fValues := TStringList.Create;
      fValues.Sorted := true;
      fValues.Duplicates := dupIgnore;
      CountFolder(ePath.Text);
      lbNames.Caption := IntToStr(fNames.Count);
      lbValues.Caption := IntToStr(fValues.Count);
      lbFolders.Caption := IntToStr(fFolders);
      lbNamesSize.Caption := IntToStr(ListSize(fNames));
      lbValuesSize.Caption := IntToStr(ListSize(fValues));
      lbLarger.Caption := IntToStr(fLarger);
      lbLarger.Hint := fLargerName;
      lbTotal.Caption := IntToStr(fTotal);
      fNames.Sorted := false;
      for i := 0 to pred(pred(fNames.Count)) do
        for j := succ(i) to pred(fNames.Count) do
          if integer(fNames.Objects[i]) < integer(fNames.Objects[j])
            then
              begin
                obj := fNames.Objects[i];
                tmp := fNames[i];
                fNames.Objects[i] := fNames.Objects[j];
                fNames[i] := fNames[j];
                fNames[j] := tmp;
                fNames.Objects[j] := obj;
              end;
      for i := 0 to pred(fNames.Count) do
        Names.Items.Add(fNames[i] + ' ' + IntToStr(integer(fNames.Objects[i])));
      fNames.Free;
      fValues.Free;
    end;




end.
