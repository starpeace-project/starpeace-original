unit PepeUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    eNodes: TEdit;
    Label1: TLabel;
    eValues: TEdit;
    Label2: TLabel;
    pbProgress: TProgressBar;
    SaveDialog: TSaveDialog;
    btnSave: TButton;
    procedure btnLoadClick(Sender: TObject);
    procedure eNodesKeyPress(Sender: TObject; var Key: Char);
    procedure btnSaveClick(Sender: TObject);
  private
    fNodes    : TStringList;
    fSolution : TStringList;
  private
    procedure AddLine(timeLine, maxsLine : string);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  uses
    CompStringsParser;


  procedure TForm1.btnLoadClick(Sender: TObject);
    var
      FileLines : TStringList;
      DataLines : TStringList;
      lidx      : integer;
      prgInit   : integer;

      timeLine  : string;
      maxsLine  : string;

    procedure ScapeLines(token : string; collect : boolean);
      var
        line : string;
      begin
        while (lidx < FileLines.Count) and (pos(token, FileLines[lidx]) = 0) do
          begin
            inc(lidx);
            if collect
              then DataLines.Add(FileLines[lidx]);
          end;
      end;

    begin
      // OUTPUT SET
      if OpenDialog.Execute
        then
          begin
            Cursor := crHourGlass;
            fNodes := TStringList.Create;
            FileLines := TStringList.Create;
            DataLines := TStringList.Create;
            try
              pbProgress.Visible := true;
              FileLines.LoadFromFile(OpenDialog.FileName);
              lidx := 0;
              ScapeLines('OUTPUT SET', false);
              if lidx > 0
                then dec(lidx);
              prgInit := lidx;
              pbProgress.Max := FileLines.Count - prgInit;
              while lidx < FileLines.Count do
                begin
                  pbProgress.Position := lidx - prgInit;
                  Application.ProcessMessages;
                  ScapeLines('OUTPUT SET', false);
                  inc(lidx, 3);
                  if lidx < FileLines.Count
                    then
                      begin
                        timeLine := FileLines[lidx]; //DataLines.Add(FileLines[lidx]);
                        inc(lidx);
                        ScapeLines('MAXIMUM ABSOLUTE VALUES', false);
                        inc(lidx, 2);
                        if lidx < FileLines.Count
                          then
                            begin
                              maxsLine := FileLines[lidx]; //DataLines.Add(FileLines[lidx]);
                              AddLine(timeLine, maxsLine);
                            end;
                      end;
                end;
              //mFemData.Lines := fNodes;
            finally
              pbProgress.Visible := false;
              FileLines.Free;
              DataLines.Free;
              Cursor := crDefault;
            end;
          end;
    end;

  procedure TForm1.AddLine(timeLine, maxsLine : string);
    var
      auxNodes  : TStringList;
      auxValues : TStringList;
      p, len    : integer;
      aux       : string;
    begin
      auxNodes   := TStringList.Create;
      auxValues := TStringList.Create;
      try
        // Nodes
        len := Length(timeLine);
        p := 1;
        SkipChars(timeLine, p, ['A'..'z', ' ']);    // Skip until 4
        while p <= len do
          begin
            aux := GetNextStringUpTo(timeLine, p, '-'); // Get the nodeId
            auxNodes.Add(aux);
            aux := GetNextStringUpTo(timeLine, p, ')'); // Copy until )
            inc(p);
            SkipChars(timeLine, p, [' ']);
          end;

        // Maxs
        len := Length(maxsLine);
        p := 1;
        SkipChars(maxsLine, p, ['A'..'z', ' ']);    // Skip until 4
        while p <= len do
          begin
            aux := GetNextStringUpTo(maxsLine, p, ' ');
            auxValues.Add(aux);
            inc(p);
            SkipChars(maxsLine, p, [' ']);
          end;

        if auxNodes.Count = auxValues.Count
          then
            begin
              for p := 0 to pred(auxNodes.Count) do
              fNodes.Values[auxNodes[p]] := auxValues[p];
            end;
      finally
        auxNodes.Free;
        auxValues.Free;
      end;
    end;

  procedure TForm1.eNodesKeyPress(Sender: TObject; var Key: Char);
    var
      inStr  : string;
      node   : string;
      p, len : integer;
      aux    : string;
    begin
      if Key = #13
        then
          begin
            btnSave.Enabled := true;
            fSolution.Free;
            fSolution := TStringList.Create;
            fSolution.Add('NODE' + #9 + 'TEMPERATURE');
            fSolution.Add('-------------------------------------');

            eValues.Text := '';
            inStr := eNodes.Text;
            p := 1;
            len := Length(inStr);
            node := GetNextStringUpTo(inStr, p, ',');
            while node <> '' do
              begin
                inc(p);
                aux := fNodes.Values[trim(node)];
                if aux = ''
                  then aux := 'NULL';
                if eValues.Text = ''
                  then eValues.Text := aux
                  else eValues.Text := eValues.Text + ', ' + aux;
                fSolution.Add(trim(node) + #9 + trim(aux));
                node := GetNextStringUpTo(inStr, p, ',');
              end;
          end;
    end;

  procedure TForm1.btnSaveClick(Sender: TObject);
    begin
      if SaveDialog.Execute
        then fSolution.SaveToFile(SaveDialog.FileName);
    end;

end.
