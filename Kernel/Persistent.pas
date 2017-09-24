unit Persistent;

interface

  uses
    BackupInterfaces;            

  {$M+}
  type
    TPersistent =
      class
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
      end;
  {$M-}

  procedure RegisterBackup;

implementation


  // TPersistent

  procedure TPersistent.LoadFromBackup( Reader : IBackupReader );
    begin
    end;

  procedure TPersistent.StoreToBackup( Writer : IBackupWriter );
    begin
    end;


  // Backup Agent

  type
    TPersistentBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

    class procedure TPersistentBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
      begin
        //try
        TPersistent(Obj).StoreToBackup(Stream);
        //except
        //end;
      end;

    class procedure TPersistentBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
      begin
        try
          TPersistent(Obj).LoadFromBackup(Stream);
        except
        end;
      end;

  // RegisterBackup

  procedure RegisterBackup;
    begin
      TPersistentBackupAgent.Register([TPersistent]);
    end;


end.

