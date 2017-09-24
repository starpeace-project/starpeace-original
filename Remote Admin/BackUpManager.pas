unit BackUpManager;

interface

  uses
    ComObj, BackUpMgr_TLB, Classes;

  type
    TBackUpManager =
      class(TAutoObject, IBackUpManager)
        protected
          function  Get_ServersDir : widestring; safecall;
          procedure Set_ServersDir(const Value : widestring); safecall;
          function  GetBackupName(i : integer) : widestring; safecall;
          procedure EnumBackups; safecall;
          function  SetCurrentBackup(idx: integer): WordBool; safecall;
          function  Get_WorldName : widestring; safecall;
          procedure Set_WorldName(const Value : widestring); safecall;
          function  Get_BackupCount: integer; safecall;
          function  GetBackupDate(i: integer): widestring; safecall;
          function  GetBackupSize(i: integer): widestring; safecall;
        private
          fServersDir : string;
          fWorldName  : string;
          fBackups    : TList;
      end;

implementation

  uses
    ComServ, SysUtils;

  type
    PBackupData = ^TBackupData;
    TBackupData =
      record
        name : string;
        time : TDateTime;
        size : single;
      end;

  function CompareBackups(Item1, Item2 : pointer) : integer;
    begin
      if PBackupData(Item1).time < PBackupData(Item2).time
        then Result := 1
        else
          if PBackupData(Item1).time > PBackupData(Item2).time
            then Result := -1
            else Result := 0;
    end;

  function TBackUpManager.Get_ServersDir : widestring;
    begin
      Result := fServersDir;
    end;

  procedure TBackUpManager.Set_ServersDir(const Value : widestring);
    begin
      fServersDir := Value;
      if fServersDir[length(fServersDir)] <> '\'
        then fServersDir := fServersDir + '\';
    end;

  function TBackUpManager.GetBackupName(i : integer): widestring;
    begin
      if (fBackups <> nil) and (fBackups.Count > 0)
        then Result := PBackupData(fBackups[i]).name
        else Result := '';
    end;

  procedure TBackUpManager.EnumBackups;
    var
      info       : TSearchRec;
      ok         : boolean;
      path       : string;
      backupdata : PBackupData;
    begin
      if fBackups <> nil
        then fBackups.Clear
        else fBackups := TList.Create;
      path := fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + '*.back';
      ok := FindFirst(path, faAnyFile, info) = 0;
      try
        while ok do
          begin
            new(backupdata);
            backupdata.name := info.Name;
            backupdata.time := FileDateToDateTime(info.Time);
            backupdata.size := info.Size/1024;
            fBackups.Add(backupdata);
            ok := FindNext(info) = 0;
          end;
        fBackups.Sort(CompareBackups);
      finally
        FindClose(info);
      end;
    end;

  function TBackUpManager.SetCurrentBackup(idx: integer): WordBool;
    const
      cCurrentBackupExt : string = '.world';
      cOldBackupExt     : string = '.world.old';
    begin
      if (fBackups <> nil) and (idx >= 0) and (idx < fBackups.Count)
        then Result := (not FileExists(fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cOldBackupExt) or DeleteFile(fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cOldBackupExt)) and
                       (not FileExists(fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cCurrentBackupExt) or RenameFile(fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cCurrentBackupExt, fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cOldBackupExt)) and
                       RenameFile(fServersDir + 'Data\Worlds\' + fWorldName + '\' + PBackupData(fBackups[idx]).name, fServersDir + 'Data\Worlds\' + fWorldName + '\' + fWorldName + cCurrentBackupExt)
        else Result := false;
    end;

  function TBackUpManager.Get_WorldName: widestring;
    begin
      Result := fWorldName;
    end;

  procedure TBackUpManager.Set_WorldName(const Value: widestring);
    begin
      fWorldName := Value;
    end;

  function TBackUpManager.Get_BackupCount: integer;
    begin
      if fBackups <> nil
        then Result := fBackups.Count
        else Result := 0;
    end;

  function TBackUpManager.GetBackupDate(i: integer): widestring;
    begin
      Result := DateTimeToStr(PBackupData(fBackups[i]).time);
    end;

  function TBackUpManager.GetBackupSize(i: integer): widestring;
    begin
      Result := FloatToStrF(PBackupData(fBackups[i]).size, ffFixed, 7, 4) + 'Kb';
    end;

initialization
  TAutoObjectFactory.Create(ComServer, TBackUpManager, Class_BackUpManager, ciMultiInstance);
end.
