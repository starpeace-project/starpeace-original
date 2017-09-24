unit VCLBackup;

interface

  uses
    BackupInterfaces, Classes, SysUtils;

  procedure RegisterBackup;

  type
    TStringListBackupAgent =
      class( TBackupAgent )
        public
          class procedure Write( Stream : IBackupWriter; Obj : TObject ); override;
          class procedure Read ( Stream : IBackupReader; Obj : TObject ); override;
      end;


implementation

  class procedure TStringListBackupAgent.Write( Stream : IBackupWriter; Obj : TObject );
    var
      List : TStringList;
      i    : integer;
    begin
      List := TStringList(Obj);
      Stream.WriteInteger( 'Count', List.Count );
      for i := 0 to pred(List.Count) do
        Stream.WriteString( 'Item.' + IntToStr(i), List[i] );
    end;

  class procedure TStringListBackupAgent.Read( Stream : IBackupReader; Obj : TObject );
    var
      count : integer;
      List  : TStringList;
      i     : integer;                        
      str   : string;
    begin
      count := Stream.ReadInteger( 'Count', 0 );
      List  := TStringList(Obj);
      for i := 0 to pred(count) do
        begin
          str := Stream.ReadString( 'Item.' + IntToStr(i), '' );
          List.Add( str );
        end;
    end;

  procedure RegisterBackup;
    begin
      TStringListBackupAgent.Register( [TStringList] );
    end;

end.



