unit BackupThread;

interface

  uses
    Classes, SyncObjs, Collection, BackupInterfaces, Kernel, World;

  type
    TBackupThread =
      class(TThread)
        public
          constructor Create(Writer : IBackupWriter; Coll : TCollection; Lock : TCriticalSection; prior : TThreadPriority);
          destructor  Destroy; override;
        private
          fWriter : IBackupWriter;
          fColl   : TCollection;
          fLock   : TCriticalSection;
        public
          procedure Execute; override;
      end;

implementation

  uses
    SysUtils;
    
  // TBackupThread

  constructor TBackupThread.Create(Writer : IBackupWriter; Coll : TCollection; Lock : TCriticalSection; prior : TThreadPriority);
    begin
      inherited Create(true);
      fWriter := Writer;
      fColl   := Coll;
      fLock   := Lock;
      FreeOnTerminate := true;
      Priority := prior;
      Resume;
    end;

  destructor TBackupThread.Destroy;
    begin
      fColl.Free;
      inherited;
    end;

  procedure TBackupThread.Execute;
    var
      i   : integer;
      cnt : integer;
    begin
      i   := 0;
      cnt := fColl.Count;
      fWriter.WriteInteger('Count', cnt);
      while not Terminated and (i < cnt) do
        try
          fLock.Enter;
          try
            fWriter.WriteObject(IntToStr(i), fColl[i]);
          finally
            inc(i);
            fLock.Leave;
          end;
        except
        end;
    end;


end.
