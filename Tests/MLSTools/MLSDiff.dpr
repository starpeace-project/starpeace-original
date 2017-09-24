{$APPTYPE CONSOLE}
program MLSDiff;

uses
  Classes, SysUtils;

  procedure Main;

    function CompareFiles( Source, Check : string; Anomalies : TStringList ) : TStringList;
      var
        SrcList  : TStringList;
        ChkList  : TStringList;
        i        : integer;
        id, vaue : string;
      begin
        writeln( ExtractFileName(Check) );
        result := TStringList.Create;
        if FileExists( Check )
          then
            begin
              SrcList := TStringList.Create;
              try
                try
                  ChkList := TStringList.Create;
                  SrcList.LoadFromFile( Source );
                  ChkList.LoadFromFile( Check );
                  for i := 0 to pred(SrcList.Count) do
                    begin
                      id := SrcList.Names[i];
                      if ChkList.Values[id] = ''
                        then result.Values[id] := SrcList.Values[id];
                      if (i + 1) mod 10 = 0
                        then write( #13, 100*(i + 1) div SrcList.Count, '% complete. ', result.Count, ' omissions found...              ' );
                    end;
                finally
                  ChkList.Free;
                end;
              finally
                SrcList.Free;
              end;
            end
          else Anomalies.Add( 'File Missing: ' + Check );
        writeln;
        writeln;
      end;

    var
      SearchRec : TSearchRec;
      SourceDir : string;
      CheckDir  : string;
      found     : integer;
      Diff      : TStringList;
      Anomalies : TStringList;
    begin
      SourceDir := paramstr(1);
      CheckDir  := paramstr(2);
      Anomalies := TStringList.Create;
      found := FindFirst( SourceDir + '\*.lang', faArchive, SearchRec );
      try
        while found = 0 do
          begin
            Diff := CompareFiles( SourceDir + '\' + SearchRec.Name, CheckDir + '\' + SearchRec.Name, Anomalies );
            try
              if Diff.Count > 0
                then Diff.SaveToFile( CheckDir + '\' + SearchRec.Name + '.diff' );
            finally
              Diff.Free;
            end;
            found := FindNext( SearchRec );
          end;
      finally
        FindClose( SearchRec );
      end;
      Anomalies.SaveToFile( ExtractFilePath(paramstr(0)) + 'Anomalies.dat' );
      Anomalies.Free;
    end;

  procedure ShowHelp;
    begin
      writeln( 'MLSDiff Version 1.0. Copyright (C) 2000 Oceanus Communications.' );
      writeln;
      writeln( ' This program will generate .DIFF files with the differences found between' );
      writeln( ' two sets of language tables.' );
      writeln;
      writeln( 'Usage: MLSDiff.exe <SourceDir> <DirToBeChecked>' );
      writeln;
      writeln( '  SourceDir: Folder containing a correct set of language tables.' );
      writeln( '  DirToBeChecked: Folder containing the tables to be checked for errors.' );
      writeln;
      writeln( 'Press ENTER to continue...' );
      readln;
    end;

{$R *.RES}

begin
  if paramcount = 2
    then Main
    else ShowHelp;
end.
