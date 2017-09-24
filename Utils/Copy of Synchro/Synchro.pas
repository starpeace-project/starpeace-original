unit Synchro;

interface

  uses
    CabUtils, URL2File, Windows, Classes;

  type
    TSyncErrorCode = integer;
    TSyncEventId   = integer;

  const
    SYNC_NOERROR                   = 0;
    SYNC_ERROR_Unknown             = 1;
    SYNC_ERROR_InvalidDestFile     = 2;
    SYNC_ERROR_InvalidSourceFile   = 3;
    SYNC_ERROR_DecompressionFailed = 4;
    SYNC_ERROR_BadIndexFile        = 5;
    SYNC_ERROR_DownloadFailed      = 6;

  const
    syncEvnDone         = 1;
    syncEvnDownloading  = 2;
    syncEvnDecompresing = 3;
    syncEvnFileDone     = 4;

  type
    TSyncTask = class;

    TOnSyncNotify =
      procedure(
        SyncTask : TSyncTask;
        EventId  : TSyncEventId;
        TaskDesc : string;
        Progress, OverallProgress : integer; out Cancel : boolean ) of object;

    TSyncTask =
      class
        private
          fSource    : string;
          fDest      : string;
          fBusy      : boolean;
          fErrorCode : TSyncErrorCode;
          fErrorInfo : string;
          fNotify    : TOnSyncNotify;
          fPriority  : integer;
          fMaxSize   : integer;
          fCurrSize  : integer;
          fFileSize  : integer;
          fStartTime : TDateTime;
          fEstHours  : integer;
          fEstMins   : integer;
        private
          procedure SetPriority( aPriority : integer );
        public
          property Busy      : boolean        read fBusy;
          property ErrorCode : TSyncErrorCode read fErrorCode;
          property ErrorInfo : string         read fErrorInfo;
          property Priority  : integer        read fPriority write SetPriority;
          property MaxSize   : integer        read fMaxSize;
          property CurrSize  : integer        read fCurrSize;
          property EstHours  : integer        read fEstHours;
          property EstMins   : integer        read fEstMins;
        private
          procedure Act; virtual;
          procedure Synchronize;
          procedure Notify( EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean ); virtual;
          function  Terminated : boolean; virtual;
        private
          procedure DownloadNotify( Progress, ProgressMax : integer; Status : TDownloadStatus; StatusText: string; out Cancel : boolean );
          function  DecompressNotify( command : TDescompressNotifyCommand; text : string; value : integer ) : boolean;
      end;

    TSyncThread =
      class( TThread )
        private
          fTask : TSyncTask;
        protected
          procedure Execute; override;
      end;

    TThreadedTask =
      class( TSyncTask )
        private
          constructor Create;
          destructor  Destroy; override;
        private
          fThread : TSyncThread;
        public
          property Thread : TSyncThread read fThread;
        private
          procedure Act; override;
        private
          vclEventId         : TSyncEventId;
          vclTaskDesc        : string;
          vclProgress        : integer;
          vclOverallProgress : integer;
          procedure Notify( EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean ); override;
          procedure vclNotify;
          function  Terminated : boolean; override;
      end;

  procedure InitSynchro( MaxParallelRequests : integer );
  procedure DoneSynchro;

  function Synchronize     ( Source, Dest : string; OnNotify : TOnSyncNotify ) : TSyncErrorCode;
  function AsyncSynchronize( Source, Dest : string; OnNotify : TOnSyncNotify; Priority : integer ) : TThreadedTask;

  procedure RegisterException( filename : string );

  const
    syncMasterFile = 'index.midx';
    syncIndexFile  = 'index.sync';

implementation

  uses
    Collection, SyncObjs, UrlMon, SysUtils, StrUtils,
    ShellAPI, SyncIndex, FileCtrl;

  type
    TPendingTasks =
      class( TSortedCollection )
        private
          function CompareTasks( Task1, Task2 : TObject ) : integer;
      end;

    function TPendingTasks.CompareTasks( Task1, Task2 : TObject ) : integer;
      begin
        if TSyncTask(Task1).fPriority > TSyncTask(Task2).fPriority
          then result := 1
          else
            if TSyncTask(Task1).fPriority < TSyncTask(Task2).fPriority
              then result := -1
              else result := 0;
      end;


  var
    Current        : TCollection      = nil;
    Pending        : TPendingTasks    = nil;
    MaxParallelReq : integer          = 0;
    SyncLock       : TCriticalSection = nil;
    Exceptions     : TStringList      = nil;

  procedure Lock;                            forward;
  procedure Unlock;                          forward;
  procedure ExecuteTask( Task : TSyncTask ); forward;
  procedure SyncDone   ( Task : TSyncTask ); forward;

  function RemoveFullPath(const Path : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Path);
      // If Path is a folder the last '\' must be removed.
      if Path[length(Path)] = '\'
        then tmp[length(Path)-1] := #0;
      with FileOp do
        begin
          wFunc  := FO_DELETE;
          Wnd    := 0;
          pFrom  := tmp;
          pTo    := nil;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          hNameMappings := nil;
        end;
      result := SHFileOperation( FileOp ) = 0;
    end;

  function ExtractCacheFileName( Cache, filename : string ) : string;
    begin
      if system.pos( Cache, filename ) > 0
        then
          begin
            delete( filename, 1, length(Cache) );
            result := filename
          end
        else result := filename;
    end;


  // TSyncTask

  procedure TSyncTask.SetPriority( aPriority : integer );
    begin
      Lock;
      try
        fPriority := aPriority;
        Pending.Sort( Pending.CompareTasks );
      finally
        Unlock;
      end;
    end;

  procedure TSyncTask.Act;
    begin
      Synchronize;
    end;

  procedure TSyncTask.Synchronize;

    function SynchronizeFiles( source, dest : string ) : TSyncErrorCode;
      begin
        if DownloadURLToFile( fSource, fDest, DownloadNotify ) = DWNL_NOERROR
          then result := SYNC_NOERROR
          else result := SYNC_ERROR_DownloadFailed;
      end;

    function SynchronizeFolders( source, dest : string ) : TSyncErrorCode;

      function DeleteFiles( names : TStringList ) : boolean;
        var
          i : integer;
        begin
          try
            for i := 0 to pred(names.count) do
              if Exceptions.IndexOf( uppercase(names[i]) ) = NoIndex 
                then DeleteFile( dest + names[i] );
            result := true;
          except
            result := false;
          end;
        end;

      function DeleteDirs( names : TStringList ) : boolean;
        var
          i : integer;
        begin
          try
            for i := 0 to pred(names.count) do
              RemoveFullPath( dest + names[i] );
            result := true;
          except
            result := false;
          end;
        end;

      function GetIndexFile : TSyncErrorCode;
        begin
          if DownloadURLToFile( source + syncMasterFile, dest + syncMasterFile, DownloadNotify ) = DWNL_NOERROR
            then result := SYNC_NOERROR
            else
              if DownloadURLToFile( source + syncIndexFile, dest + syncIndexFile, DownloadNotify ) = DWNL_NOERROR
                then result := SYNC_NOERROR
                else result := SYNC_ERROR_DownloadFailed
        end;

      procedure DeleteIndexFile;
        begin
          DeleteFile( dest + syncIndexFile );
          DeleteFile( dest + syncMasterFile );
        end;

      function FileIsCompressed( filename : string ) : boolean;
        begin
          result := uppercase(ExtractFileExt( filename )) = '.CAB';
        end;

      function GetFile( filename : string; out ErrorInfo : string ) : TSyncErrorCode;
        const
          MaxAttempts = 3;
        var
          attempt : integer;
          RelPath : string;
          cancel  : boolean;
        begin
          attempt := 0;
          RelPath := ExtractFilePath( filename );
          repeat
            fFileSize := 0;
            if DownloadURLToFile( source + ChangePath( filename, '\', '/' ), dest + filename, DownloadNotify ) = DWNL_NOERROR
              then
                if FileIsCompressed( dest + filename )
                  then
                    if DecompressFile( dest + filename, dest + RelPath, DecompressNotify )
                      then
                        begin
                          DeleteFile( dest + filename );
                          result := SYNC_NOERROR;
                        end
                      else
                        begin
                          result := SYNC_ERROR_DecompressionFailed;
                          ErrorInfo := dest + filename;
                        end
                  else result := SYNC_NOERROR
              else
                begin
                  result := SYNC_ERROR_DownloadFailed;
                  ErrorInfo := source + ChangePath( filename, '\', '/' );
                end;
            inc( attempt );
            if result <> SYNC_NOERROR
              then dec( fCurrSize, fFileSize );
          until (result = SYNC_NOERROR) or (attempt = MaxAttempts) or Terminated;
          if result = SYNC_NOERROR
            then
              begin
                cancel := false;
                Notify( syncEvnFileDone, dest + filename, 0, 0, cancel );
                ErrorInfo := '';
              end;
        end;

      function GetFiles( names : TStringList; out ErrorInfo : string ) : TSyncErrorCode;
        var
          i : integer;
        begin
          i := 0;
          result := SYNC_NOERROR;
          while (i < names.Count) and (result = SYNC_NOERROR) and not Terminated do
            begin
              result := GetFile( names[i], ErrorInfo );
              inc( i );
            end;
        end;

      function GetDirs( names : TStringList ) : TSyncErrorCode;
        var
          i : integer;
        begin
          i := 0;
          result := SYNC_NOERROR;
          while (i < names.Count) and (result = SYNC_NOERROR) and not Terminated do
            begin
              result := SynchronizeFolders( source + names[i] + '/', dest + names[i] + '\' );
              inc( i );
            end;
        end;

      procedure CreateDirs( names : TStringList );
        var
          i : integer;
        begin
          try
            for i := 0 to pred(names.Count) do
              CreateDir( dest + names[i] );
          except
          end;
        end;

      var
        NewFiles   : TStringList;
        OldFiles   : TStringList;
        SyncDirs   : TStringList;
        NewDirs    : TStringList;
        OldDirs    : TStringList;
        MasterSync : boolean;

      procedure CreateLists;
        begin
          NewFiles := TStringList.Create;
          //NewFiles.Sorted := true;
          NewFiles.Duplicates := dupIgnore;
          OldFiles := TStringList.Create;
          OldFiles.Sorted := true;
          OldFiles.Duplicates := dupIgnore;
          SyncDirs := TStringList.Create;
          SyncDirs.Sorted := true;
          SyncDirs.Duplicates := dupIgnore;
          OldDirs  := TStringList.Create;
          OldDirs.Sorted := true;
          OldDirs.Duplicates := dupIgnore;
          NewDirs  := TStringList.Create;
          NewDirs.Sorted := true;
          NewDirs.Duplicates := dupIgnore;
        end;

      procedure FreeLists;
        begin
          NewFiles.Free;
          OldFiles.Free;
          SyncDirs.Free;
          OldDirs.Free;
          NewDirs.Free;
        end;

      begin
        //CreateDir( dest );
        ForceDirectories( dest );
        result := GetIndexFile;
        if result = SYNC_NOERROR
          then
            begin
              CreateLists;
              try
                MasterSync := CompareFolder( syncMasterFile, dest, NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs, fMaxSize );
                if MasterSync or CompareFolder( syncIndexFile, dest, NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs, fMaxSize )
                  then
                    begin
                      DeleteFiles( OldFiles );
                      DeleteDirs( OldDirs );
                      CreateDirs( NewDirs );
                      fCurrSize  := 0;                                               
                      fFileSize  := 0;
                      fStartTime := Now;
                      result := GetFiles( NewFiles, fErrorInfo );
                      if not MasterSync and (result = SYNC_NOERROR)
                        then result := GetDirs( SyncDirs );
                      DeleteIndexFile;
                    end
                  else result := SYNC_ERROR_Unknown;
              finally
                FreeLists;
              end;
            end;
      end;

    function IsFolder( dest : string ) : boolean;
      begin
        result := uppercase( ExtractFilePath( dest )) = uppercase( dest );
      end;

    var
      cancel : boolean;
    begin
      fBusy  := true;
      if IsFolder( fDest )
        then fErrorCode := SynchronizeFolders( fSource, fDest )
        else fErrorCode := SynchronizeFiles( fSource, fDest );
      fBusy := false;
      SyncDone( self );
      Notify( syncEvnDone, 'Done', 0, 100, cancel );
    end;

  procedure TSyncTask.Notify( EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    begin
      Cancel := false;
      if assigned(fNotify)
        then fNotify( self, EventId, TaskDesc, Progress, OverallProgress, Cancel );
    end;

  function TSyncTask.Terminated : boolean;
    begin
      result := false;
    end;

  procedure TSyncTask.DownloadNotify( Progress, ProgressMax : integer; Status : TDownloadStatus; StatusText: string; out Cancel : boolean );
    var
      Desc        : string;
      Perc        : integer;
      OverallPerc : integer;
      h, m, s, ms : word;
      secs, esecs : integer;
      delta       : integer;
      FileName    : string;
      UseCanceled : boolean;
    begin
      FileName := system.Copy( StatusText, BackPos( '/', StatusText, length(StatusText) ) + 1, length(StatusText) );
      case Status of                        
        dlstConnecting :
            Desc := 'Finding Resource: ' + FileName;
        dlstDownloading :
            Desc := 'Downloading: ' + FileName;
        else
            Desc := 'Please wait...';
      end;
      delta     := Progress - fFileSize;
      fFileSize := Progress;
      fCurrSize := fCurrSize + delta;
      DecodeTime( Now - fStartTime, h, m, s, ms );
      secs := 60*60*h + 60*m + s;
      if fCurrSize > 0
        then
          begin
            esecs := ((fMaxSize - fCurrSize)*secs) div fCurrSize;
            if esecs >= 60*60
              then
                begin
                  fEstHours := esecs div (60*60);
                  fEstMins  := (esecs mod (60*60)) div 60;
                end
              else
                begin
                  fEstHours := 0;
                  fEstMins  := esecs div 60;
                end;
          end;
      if ProgressMax > 0
        then Perc := 100*Progress div ProgressMax
        else Perc := 0;
      if fMaxSize > 0
        then OverallPerc := 100*fCurrSize div fMaxSize
        else OverallPerc := 0;
      Notify( syncEvnDownloading, Desc, Perc, OverallPerc, UseCanceled );
      Cancel := Terminated or UseCanceled;
    end;

  function TSyncTask.DecompressNotify( command : TDescompressNotifyCommand; text : string; value : integer ) : boolean;
    var
      cancel : boolean;
    begin
      Notify( syncEvnDecompresing, text, value, -1, cancel );
      result := not Terminated and not cancel;
    end;


  // TSyncThread

  procedure TSyncThread.Execute;
    begin
      fTask.Synchronize;
    end;


  // TThreadedTask

  constructor TThreadedTask.Create;
    begin
      inherited;
      fThread := TSyncThread.Create( true );
      fThread.fTask := self;
      fThread.Priority := tpLower;
    end;

  destructor TThreadedTask.Destroy;
    begin
      if not fThread.Terminated
        then
          begin
            fThread.Terminate;
            WaitForSingleObject( fThread.Handle, INFINITE );
          end;
      //fThread.Terminate;
      inherited;
    end;

  procedure TThreadedTask.Act;
    begin
      fThread.Resume;
    end;

  procedure TThreadedTask.Notify( EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    begin
      if not Terminated
        then
          begin
            vclEventId  := EventId;
            vclTaskDesc := TaskDesc;
            vclProgress := Progress;
            if OverallProgress <> -1
              then vclOverallProgress := OverallProgress;
            Thread.Synchronize( vclNotify );
            cancel := false;
          end;
    end;

  procedure TThreadedTask.vclNotify;
    var
      cancel : boolean;
    begin
      inherited Notify( vclEventId, vclTaskDesc, vclProgress, vclOverallProgress, cancel );
    end;

  function TThreadedTask.Terminated : boolean;
    begin
      result := fThread.Terminated;
    end;


  // Main functs

  procedure InitSynchro( MaxParallelRequests : integer );
    begin
      SyncLock       := TCriticalSection.Create;
      Current        := TCollection.Create( 0, rkUse );
      Pending        := TPendingTasks.Create( 0, rkUse, Pending.CompareTasks );
      MaxParallelReq := MaxParallelRequests;
      Exceptions     := TStringList.Create;
      InitDownloader( nil );
    end;

  procedure DoneSynchro;
    begin
      DoneDownloader;
      Exceptions.Free;
      Current.Free;
      Pending.Free;
      SyncLock.Free;
    end;

  function Synchronize( Source, Dest : string; OnNotify : TOnSyncNotify ) : TSyncErrorCode;
    var
      Task : TSyncTask;
    begin
      Task           := TSyncTask.Create;
      Task.fSource   := Source;
      Task.fDest     := Dest;
      Task.fNotify   := OnNotify;                            
      Task.Synchronize;
      result := Task.ErrorCode;
    end;

  function AsyncSynchronize( Source, Dest : string; OnNotify : TOnSyncNotify; Priority : integer ) : TThreadedTask;
    begin
      Lock;
      try
        result := TThreadedTask.Create;
        result.fSource   := Source;
        result.fDest     := Dest;
        result.fPriority := Priority;
        result.fNotify   := OnNotify;
        result.fBusy     := true;
        if Current.Count < MaxParallelReq
          then ExecuteTask( result )
          else Pending.Insert( result );
      finally
        Unlock;
      end;
    end;

  procedure RegisterException( filename : string );
    begin
      Exceptions.Add( uppercase(filename) );
    end;

  procedure Lock;
    begin
      SyncLock.Enter;
    end;

  procedure Unlock;
    begin
      SyncLock.Leave;
    end;

  procedure ExecuteTask( Task : TSyncTask );
    begin
      Lock;
      try
        Current.Insert( Task );
        Task.Act;
      finally
        Unlock;
      end;
    end;

  procedure SyncDone( Task : TSyncTask );
    var
      NewTask : TSyncTask;
    begin
      Lock;
      try
        Current.Delete( Task );
        while (Current.Count < MaxParallelReq) and (Pending.Count > 0) do
          begin
            NewTask := TSyncTask(Pending[0]);
            Pending.AtExtract( 0 );
            ExecuteTask( NewTask );
          end;
      finally
        Unlock;
      end;
    end;
    
end.



