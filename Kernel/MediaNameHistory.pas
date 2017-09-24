unit MediaNameHistory;

interface

  uses
    Classes, SysUtils, Persistent, BackupInterfaces;

  type
    TMediaNameHistory =
      class(TPersistent)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fHistory : TStringList;
        public
          procedure Add(name : string);
          function  AddNumber(name, sep : string) : string;
        private
          function IncAt(index : integer) : integer;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  // TMediaNameHistory

  constructor TMediaNameHistory.Create;
    begin
      inherited;
      fHistory := TStringList.Create;
    end;

  destructor TMediaNameHistory.Destroy;
    begin
      fHistory.Free;
      inherited;
    end;

  procedure TMediaNameHistory.Add(name : string);
    var
      idx : integer;
      aux : string;
    begin
      aux := LowerCase(name);
      idx := fHistory.IndexOf(aux);
      if idx < 0
        then fHistory.Add(aux)
        else IncAt(idx);
    end;

  function TMediaNameHistory.AddNumber(name, sep : string) : string;
    var
      idx : integer;
      aux : string;
    begin
      aux := LowerCase(name);
      idx := fHistory.IndexOf(aux);
      if idx < 0
        then
          begin
            fHistory.Add(aux);
            result := name;
          end
        else result := name + sep + IntToStr(IncAt(idx));
    end;

  function TMediaNameHistory.IncAt(index : integer) : integer;
    begin
      result := integer(fHistory.Objects[index]);
      inc(result);
      fHistory.Objects[index] := TObject(result);
    end;

  procedure TMediaNameHistory.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fHistory := TStringList.Create;
    end;

  procedure TMediaNameHistory.StoreToBackup(Writer : IBackupWriter);
    var
      i : integer;
    begin
      inherited;
      Writer.WriteInteger('cnt', fHistory.Count);
      for i := 0 to pred(fHistory.Count) do
        begin
          Writer.WriteString('fn', fHistory[i]);
          Writer.WriteInteger('val', integer(fHistory.Objects[i]));
        end;
    end;


  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TMediaNameHistory );
    end;


end.
