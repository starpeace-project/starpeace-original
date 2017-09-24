unit InformativeTask;

interface

  uses
    Tasks, BackupInterfaces;

  type
   {$M+}
    TInformativeTask =
      class(TAtomicTask)
        private
          fChecked : boolean;
        published
          procedure RDOClose(useless : integer);
          procedure RDONextStep(useless : integer);
          procedure RDOPrevStep(useless : integer);
        public
          function Execute : TTaskResult; override;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
      end;
    {$M-}

  procedure RegisterBackup;

implementation


  uses
    ModelServerCache;

  // TInformativeTask

  procedure TInformativeTask.RDOClose;
    begin
      fChecked := true;
      SuperTask.SubTaskFinalize;
    end;

  procedure TInformativeTask.RDONextStep;
    begin
      Stage := Stage + 1;
      if Stage < MetaTask.StageCount
        then
          begin
            UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
            NotifyTycoon('');
          end
        else SuperTask.SubTaskFinalize;
    end;

  procedure TInformativeTask.RDOPrevStep;
    begin
      if Stage > 0
        then
          begin
            Stage := Stage - 1;
            UpdateObjectCache(Context.getContext(tcIdx_Tycoon), -1, -1);
            NotifyTycoon('');
          end;
    end;

  function TInformativeTask.Execute : TTaskResult;
    begin
      if not fChecked
        then result := trContinue
        else result := trFinished;
    end;

  procedure TInformativeTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fChecked := Reader.ReadBoolean( 'Checked', false );
    end;

  procedure TInformativeTask.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteBoolean( 'Checked', fChecked );
    end;

    
  procedure RegisterBackup;
    begin
      RegisterClass( TInformativeTask );
    end;

end.





