unit Tasks;

interface

  uses
    SysUtils, Classes, MetaInstances, Collection, CacheAgent, ClassStorage,
    BackupInterfaces, Persistent, FacIds;

  const
    tidClassFamily_Tasks = 'Tasks';
    tidCachePath_Inputs  = 'Task\';
    tidURL_Tasks         = 'Visual/Voyager/NewTycoon/Tasks/';
    tidSilentTaskId      = 'Silent';

  const
    tcIdx_Tycoon  = 0;
    tcIdx_Company = 1;
    tcIdx_Town    = 2;

  const
    tprIgnoreTask   = -2;
    tprAccomplished = -1;
    tprLowest       = 0;
    tprLow          = 10;
    tprNormal       = 100;
    tprHigh         = 1000;
    tprHighest      = 10000;

  const
    nopTutorial_OFF  = 0;
    nopTutorial_ON   = 2;
    nopTutorial_SHOW = 4;

  const
    tckCanceled = 'canceled';
    tckFinished = 'finished';

  type
    TTaskResult = (trError, trContinue, trFinished);

  type
    ITaskContext =
      interface
        function  getContext(id : integer) : TObject;
        procedure setContext(id : integer; Obj : TObject);
      end;

  type
    TMetaTask   = class;
    TTask       = class;
    TSuperTask  = class;
    TAtomicTask = class;

    CTask       = class of TTask;

    TMetaTask =
      class(TMetaInstance)
        public
          constructor Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask); overload;
          constructor Create(srcTaskId, newId, newSuperTaskId : string); overload;
          destructor  Destroy; override;
        private
          fId           : string;
          fName         : string;
          fDesc         : string;
          fPeriod       : integer;
          fPriority     : integer;
          fTaskClass    : CTask;
          fSuperTask    : TMetaTask;
          fSubTasks     : TCollection;
          fStageCount   : byte;
          fKeepsTrack   : boolean;
          fNotTitle     : string;
          fNotOptions   : integer;
          fKindId       : string;
          //fSilent     : boolean;
          fSource       : TMetaTask;
          fAutoComplete : boolean;
          fWeight       : single;
        private
          procedure Add(aTask : TMetaTask);
        public
          function  Instantiate(SuperTask : TSuperTask; Context : ITaskContext) : TTask; virtual;
        private
          function  GetSubTaskCount : integer;
          function  GetSubTask(index : integer) : TMetaTask;
        public
          property Id         : string     read fId;
          property Name       : string     read fName;
          property Desc       : string     read fDesc;
          property Period     : integer    read fPeriod;
          property Priority   : integer    read fPriority   write fPriority;
          property StageCount : byte       read fStageCount write fStageCount;
          property TaskClass  : CTask      read fTaskClass;
          property SuperTask  : TMetaTask  read fSuperTask;
          property KeepsTrack : boolean    read fKeepsTrack write fKeepsTrack;
          property SubTaskCount : integer read GetSubTaskCount;
          property SubTasks[index : integer] : TMetaTask read GetSubTask;
          property NotTitle   : string  read fNotTitle   write fNotTitle;
          property NotOptions : integer read fNotOptions write fNotOptions;
          property KindId     : string  read fKindId     write fKindId;
          //property Silent     : boolean read fSilent     write fSilent;
          property Source     : TMetaTask read fSource;
          property AutoComplete : boolean read fAutoComplete write fAutoComplete;
          property Weight     : single read fWeight write fWeight;
        public
          procedure MakeSubTaskOf(id : string);
      end;

    TTask =
      class(TPersistent)
        public
          constructor Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext); virtual;
          destructor  Destroy; override;
        private
          fMetaTask  : TMetaTask;
          fContext   : ITaskContext;
          fSuperTask : TSuperTask;
          fCookies   : TStringList;
          fStage     : integer;
          fURL       : string;
          fCompleted : boolean;
          fFinished  : boolean;
        protected
          class function  GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; virtual;
          class procedure SetCompletionCookies(MetaTask : TMetaTask; SuperTask : TTask; canceled : boolean); virtual;
          function GetPeriod   : integer; virtual; abstract;
          function GetProgress : integer; virtual;
        private
          function  GetCookie(name : string) : string; virtual;
          procedure SetCookie(name, value : string); virtual;
        public
          procedure AddCookie(name, value : string);
          function  GetActive : boolean; virtual;
          procedure SetActive(Value : boolean); virtual;
          procedure SetStage(Value : integer);
          function GetTaskNumber : integer; virtual;
          function GetNotTycoon : boolean; virtual;
          procedure SetNotTycoon(value : boolean); virtual;
        public
          property Active     : boolean      read GetActive write SetActive;
          property MetaTask   : TMetaTask    read fMetaTask;
          property Context    : ITaskContext read fContext;
          property SuperTask  : TSuperTask   read fSuperTask;
          property Stage      : integer      read fStage write SetStage;
          property Period     : integer      read GetPeriod;
          property Progress   : integer      read GetProgress;
          property CookieList : TStringList  read fCookies;
          property Cookies[name : string] : string read GetCookie write SetCookie;
          property Finished  : boolean read fFinished write fFinished;
          property TaskNumber : integer read GetTaskNumber;
          property NotTycoon : boolean read GetNotTycoon write SetNotTycoon;
        private
          procedure SetCompleted(value : boolean);
        published
          property Completed : boolean read fCompleted write SetCompleted;
        protected
          function  CheckCookie(const name, value : string) : boolean; virtual;
          procedure SendMessage(var Msg); virtual;
          procedure DefineTarget; virtual;
          procedure ShowTutorialButton;
          procedure Activate; virtual;
          procedure Initialize; virtual;
          procedure Finalize; virtual;
          procedure ChangeStage; virtual;
        public
          procedure NotifyTycoon(ExtraInfo : string); virtual;
          function  TimeToSimulate(PeriodCount : integer) : boolean; virtual; abstract;
          function  IsLeaveTask : boolean; virtual; abstract;
          function  Execute : TTaskResult; virtual; abstract;
          procedure Cancel; virtual;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); virtual;
          procedure Loaded(Context   : ITaskContext); virtual;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        private
          function  GetBaseURL(Lang : string) : string;
        public
          procedure HideTaskButton;
      end;

    TSuperTask =
      class(TTask)
        public
          constructor Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext); override;
          destructor  Destroy; override;
        private
          fSubTask : TTask;
        protected
          function  SelectWinnerTask(Context : ITaskContext) : TMetaTask; virtual;
          function  GetPeriod : integer; override;
        public
          procedure DefaultHandler(var Message); override;
        public
          function  TimeToSimulate(PeriodCount : integer) : boolean; override;
          function  Execute : TTaskResult; override;
          procedure Cancel; override;
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
          procedure Loaded(Context   : ITaskContext); override;
          procedure SubTaskFinalize;
          procedure NotifyTycoon(ExtraInfo : string); override;
          function  IsLeaveTask : boolean; override;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
      end;

    TAtomicTask =
      class(TTask)
        private
          fPeriodCount : integer;
        protected
          function  GetPeriod : integer; override;
          procedure Activate; override;
        public
          procedure StoreToCache(Prefix : string; Cache : TObjectCache); override;
        public
          function TimeToSimulate(PeriodCount : integer) : boolean; override;
          function IsLeaveTask : boolean; override;
      end;

    TRandomTask =
      class(TSuperTask)
        private
          fPickedTask : boolean;
        protected
          function SelectWinnerTask(Context : ITaskContext) : TMetaTask; override;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    Kernel, Protocol, ModelServerCache, TaskUtils, Logs;

  // TMetaTask

  constructor TMetaTask.Create(srcTaskId, newId, newSuperTaskId : string);
    begin
      fSource := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, srcTaskId]);
      if fSource <> nil
        then
          begin
            inherited Create(newId);
            fId           := newId;
            fName         := fSource.Name;
            fDesc         := fSource.Desc;
            fPeriod       := fSource.Period;
            fTaskClass    := fSource.TaskClass;
            fStageCount   := 1;
            fPriority     := tprNormal;
            fKeepsTrack   := true;
            fKindId       := fSource.KindId;
            fNotOptions   := fSource.fNotOptions;
            fAutoComplete := fSource.fAutoComplete;
            //fSilent     := false;
            fWeight       := fSource.Weight;
            if newSuperTaskId <> ''
              then
                begin
                  fSuperTask := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, newSuperTaskId]);
                  fSuperTask.Add(self);
                end
              else fSuperTask := nil;
          end;
    end;

  constructor TMetaTask.Create(anId, aName, aDesc, aSuperTaskId : string; aPeriod : integer; aTaskClass : CTask);
    begin
      inherited Create(anId);
      fId           := anId;
      fName         := aName;
      fDesc         := aDesc;
      fPeriod       := aPeriod;
      fTaskClass    := aTaskClass;
      fStageCount   := 1;
      fPriority     := tprNormal;
      fKeepsTrack   := true;
      fKindId       := fId;
      fNotOptions   := nopTutorial_SHOW;
      fAutoComplete := false;
      //fSilent     := false;
      fWeight       := 1;
      if aSuperTaskId <> ''
        then
          begin
            fSuperTask := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, aSuperTaskId]);
            if fSuperTask <> nil
              then fSuperTask.Add(self);
          end
        else fSuperTask := nil;
    end;

  destructor TMetaTask.Destroy;
    begin
      fSubTasks.Free;
      inherited;
    end;

  procedure TMetaTask.Add(aTask : TMetaTask);
    begin
      aTask.fSuperTask := self;
      if fSubTasks = nil
        then fSubTasks := TCollection.Create(0, rkBelonguer);
      fSubTasks.Insert(aTask);
    end;

  function TMetaTask.Instantiate(SuperTask : TSuperTask; Context : ITaskContext) : TTask;
    begin
      result := fTaskClass.Create(self, SuperTask, Context);
    end;

  function TMetaTask.GetSubTaskCount : integer;
    begin
      if fSubTasks <> nil
        then result := fSubTasks.Count
        else result := 0;
    end;

  function TMetaTask.GetSubTask(index : integer) : TMetaTask;
    begin
      result := TMetaTask(fSubTasks[index]);
    end;

  procedure TMetaTask.MakeSubTaskOf(id : string);
    var
      Task : TMetaTask;
    begin
      Task := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, Id]);
      if fSuperTask = nil
        then fSuperTask := Task;
      Task.Add(self);
    end;


  // TTask

  constructor TTask.Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext);
    begin
      inherited Create;
      fMetaTask  := aMetaTask;
      fContext   := aContext;
      fSuperTask := aSuperTask;
      fCompleted := aMetaTask.AutoComplete;
      Initialize;
      DefineTarget;
    end;

  destructor TTask.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      if fSuperTask = nil
        then fCookies.Free;
      inherited;
    end;

  class function TTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      stateCookie : string;
    begin
      stateCookie := SuperTask.Cookies[MetaTask.Id];
      if (stateCookie = tckFinished) or (stateCookie = tckCanceled)
        then result := tprIgnoreTask
        else result := MetaTask.Priority;
    end;

  class procedure TTask.SetCompletionCookies(MetaTask : TMetaTask; SuperTask : TTask; canceled : boolean);
    begin
      if (SuperTask <> nil) and MetaTask.KeepsTrack
        then
          if canceled
            then SuperTask.AddCookie(MetaTask.Id, tckCanceled)
            else SuperTask.AddCookie(MetaTask.Id, tckFinished);
    end;

  function TTask.GetProgress : integer;
    begin
      if MetaTask.StageCount > 0
        then result := round(100*fStage/MetaTask.StageCount)
        else result := 100;
    end;

  function TTask.GetCookie(name : string) : string;
    begin
      if fCookies <> nil
        then result := fCookies.Values[name]
        else result := '';
      if (result = '') and (fSuperTask <> nil)
        then result := fSuperTask.GetCookie(name);
    end;

  procedure TTask.SetCookie(name, value : string);
    begin
      if fSuperTask <> nil
        then fSuperTask.AddCookie(name, value)
        else AddCookie(name, value);
    end;

  procedure TTask.AddCookie(name, value : string);
    begin
      if fCookies = nil
        then fCookies := TStringList.Create;
      fCookies.Values[name] := value;
    end;

  function TTask.GetActive : boolean;
    begin
      result := true;
    end;

  procedure TTask.SetActive(Value : boolean);
    begin
    end;

  procedure TTask.SetStage(Value : integer);
    begin
      fStage := Value;
      ChangeStage;
    end;

  procedure TTask.SetCompleted(value : boolean);
    begin
      fCompleted := value;
    end;

  function TTask.GetTaskNumber : integer;
    begin
      result := 0;
    end;

  function TTask.GetNotTycoon : boolean;
    begin
      if fSuperTask <> nil
        then result := fSuperTask.GetNotTycoon
        else result := false;
    end;

  procedure TTask.SetNotTycoon(value : boolean);
    begin
      if fSuperTask <> nil
        then fSuperTask.SetNotTycoon(value);
    end;

  function TTask.CheckCookie(const name, value : string) : boolean;
    begin
      result := Cookies[name] = value;
    end;

  procedure TTask.NotifyTycoon(ExtraInfo : string);
    var
      Tycoon : TTycoon;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon <> nil
        then
          begin
            fURL := GetBaseURL(Tycoon.Language) + ExtraInfo;
            UpdateObjectCache(Tycoon, noKind, noInfo);
            if (fURL <> '') and Tycoon.IsOnline
              then
                begin
                  Tycoon.SendNotification(ntkURLFrame, MetaTask.NotTitle, fURL, MetaTask.NotOptions); // ntkURLFrame
                  NotTycoon := true;
                end;
          end;
    end;

  procedure TTask.ShowTutorialButton;
    var
      Tycoon : TTycoon;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon <> nil
        then
          begin
            fURL := GetBaseURL(Tycoon.Language);
            if (fURL <> '') and Tycoon.IsOnline
              then Tycoon.SendNotification(ntkURLFrame, MetaTask.NotTitle, fURL, nopTutorial_SHOW); // ntkURLFrame
          end;
    end;

  procedure TTask.Activate;
    begin
    end;

  procedure TTask.DefineTarget;
    begin
    end;

  procedure TTask.Initialize;
    begin
    end;

  procedure TTask.Finalize;
    begin
    end;

  procedure TTask.ChangeStage; 
    begin
    end;

  procedure TTask.SendMessage(var Msg);
    begin
      if fSuperTask <> nil
        then fSuperTask.SendMessage(Msg);
    end;

  procedure TTask.Cancel;
    begin
      SetCompletionCookies(MetaTask, fSuperTask, true);
    end;

  procedure TTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    var
      Company : TCompany;
      Town    : TTown;
    begin
      Cache.WriteInteger(Prefix + 'ObjId', integer(self));
      Cache.WriteString (Prefix + 'Id', MetaTask.KindId);
      {if not MetaTask.Silent
        then Cache.WriteString (Prefix + 'Id', MetaTask.KindId)
        else Cache.WriteString (Prefix + 'Id', tidSilentTaskId);}
      Cache.WriteString (Prefix + 'Name', MetaTask.Name);
      Cache.WriteInteger(Prefix + 'Progress', GetProgress);
      Cache.WriteInteger(Prefix + 'Stage', fStage);
      Cache.WriteString (Prefix + 'URL', fURL);
      Company := TCompany(Context.getContext(tcIdx_Company));
      if Company <> nil
        then
          begin
            Cache.WriteString(Prefix  + 'Company', Company.Name);
            Cache.WriteString(Prefix  + 'Cluster', Company.Cluster.Id);
          end;
      Town := TTown(Context.getContext(tcIdx_Town));
      if Town <> nil
        then
          begin
            Cache.WriteString(Prefix + 'Town', Town.Name);
            Cache.WriteString(Prefix + 'TownCluster', Town.Cluster.Id);
          end;
      Cache.WriteBoolean(Prefix + 'TaskDone', fFinished);
    end;

  procedure TTask.Loaded(Context : ITaskContext);
    begin
      fContext := Context;
    end;

  procedure TSuperTask.SubTaskFinalize;
    begin
      try
        fSubTask.SetCompletionCookies(fSubTask.MetaTask, self, false);
        fSubTask.Finalize;
      finally
        fSubTask.Free;
        fSubTask := nil;
      end;
      if (Execute = trFinished) and (fSuperTask <> nil)
        then fSuperTask.SubTaskFinalize;
    end;

  procedure TSuperTask.NotifyTycoon(ExtraInfo : string);
    begin
      if fSubTask <> nil
        then fSubTask.NotifyTycoon('')
        else inherited NotifyTycoon('');
    end;

  function TSuperTask.IsLeaveTask : boolean;
    begin
      result := false;
    end;

  procedure TTask.LoadFromBackup(Reader : IBackupReader);
    var
      i     : integer;
      TskId : string;
    begin
      inherited;
      TskId := Reader.ReadString('Id', '');
      fMetaTask := TMetaTask(TheClassStorage.ClassById[tidClassFamily_Tasks, TskId]);
      Reader.ReadObject('', fSuperTask, nil);
      if Reader.ReadBoolean('', false)
        then
          begin
            fCookies := TStringList.Create;
            for i := 0 to pred(Reader.ReadInteger('', 0)) do
              fCookies.Add(Reader.ReadString('', ''));
          end;
      fStage := Reader.ReadInteger('', 0);
      fCompleted := (fMetaTask <> nil) and fMetaTask.AutoComplete;
    end;

  procedure TTask.StoreToBackup(Writer : IBackupWriter);
    var
      i : integer;
    begin
      inherited;
      Writer.WriteString('Id', MetaTask.Id);
      Writer.WriteObjectRef('SuperTask', fSuperTask);
      Writer.WriteBoolean('hasCookies', fCookies <> nil);
      if fCookies <> nil
        then
          begin
            Writer.WriteInteger('CkCount', fCookies.Count);
            for i := 0 to pred(fCookies.Count) do
              Writer.WriteString('Cookie' + IntToStr(i), fCookies[i]);
          end;
      Writer.WriteInteger('Stage', fStage);
    end;

  function TTask.GetBaseURL(Lang : string) : string;
    var
      Tycoon  : TTycoon;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      if Tycoon <> nil
        then
          result :=
            Tycoon.WorldLocator.GetWorldURL + '/' + Lang + '/' +
            tidURL_Tasks +
            MetaTask.KindId + '/' +
            IntToStr(Stage) + '/default.asp?Tycoon=' + Tycoon.Name +
            '&WorldName=' + Tycoon.WorldLocator.GetWorldName
        else result := '';
    end;

  procedure TTask.HideTaskButton;
    var
      Tycoon : TTycoon;
    begin
      Tycoon := TTycoon(Context.getContext(tcIdx_Tycoon));
      fURL   := GetBaseURL(Tycoon.Language);
      if (Tycoon <> nil) and Tycoon.IsOnline
        then Tycoon.SendNotification(ntkURLFrame, '', fURL, nopTutorial_OFF);
    end;

 // TSuperTask

  constructor TSuperTask.Create(aMetaTask : TMetaTask; aSuperTask : TSuperTask; aContext : ITaskContext);
    begin
      inherited;
      fCookies := TStringList.Create;
    end;

  destructor TSuperTask.Destroy;
    begin
      fSubTask.Free;
      inherited;
    end;

  function TSuperTask.SelectWinnerTask(Context : ITaskContext) : TMetaTask;
    var
      i     : integer;
      Task  : TMetaTask;
      prior : integer;
      tpr   : integer;
    begin
      result := nil;
      prior  := -1;
      for i := 0 to pred(MetaTask.SubTaskCount) do
        begin
          Task := MetaTask.SubTasks[i];
          tpr  := Task.TaskClass.GetPriority(Task, self, Context);
          if tpr = tprAccomplished
            then Task.TaskClass.SetCompletionCookies(Task, self, false);
          if tpr > prior
            then
              begin
                prior  := tpr;
                result := Task;
              end;
        end;
    end;

  function TSuperTask.GetPeriod : integer;
    begin
      if fSubTask <> nil
        then result := fSubTask.Period
        else result := MetaTask.Period;
    end;

  function TSuperTask.TimeToSimulate(PeriodCount : integer) : boolean;
    begin
      result := (fSubTask = nil) or (fSubTask.TimeToSimulate(PeriodCount));
    end;

  procedure TSuperTask.DefaultHandler(var Message);
    begin
      if fSubTask <> nil
        then fSubtask.Dispatch(Message);
    end;

  function TSuperTask.Execute : TTaskResult;

    procedure GetWinnerTask;
      var
        Winner : TMetaTask;
      begin
        Winner := SelectWinnerTask(Context);
        if Winner <> nil
          then
            begin
              fSubTask := Winner.Instantiate(self, Context);
              fSubTask.fSuperTask := self;
              fSubTask.Activate;
            end
          else fSubTask := nil;
      end;

    begin
      try
        // This is because the 1st time the Subtask is empty
        if fSubTask = nil
          then GetWinnerTask;
        if fSubTask = nil
          then result := trFinished
          else
            begin
              // check if finised
              if not fSubTask.Finished
                then
                  if fSubTask.Execute = trFinished
                    then
                      begin
                        fSubTask.Finished := true;
                        if fSubTask.IsLeaveTask
                          then fSubTask.NotifyTycoon('');
                      end;
              // check if completed
              if fSubTask.Finished and (not fSubTask.IsLeaveTask or fSubTask.Completed)
                then
                  begin
                    try
                      fSubTask.SetCompletionCookies(fSubTask.MetaTask, self, false);
                      fSubTask.Finalize;
                    finally
                      fSubTask.Free;
                      fSubTask := nil; // [Iroel 10/16/2002]
                    end;
                    try
                      GetWinnerTask;
                      if fSubTask <> nil
                        then result := trContinue
                        else result := trFinished;
                    except
                      result   := trError;
                      fSubTask := nil;
                    end;
                  end
                else result := trContinue;
            end;
        if result = trFinished
          then
            begin
              SetCompletionCookies(MetaTask, fSuperTask, false);
              Finalize;
            end;
      except
        result := trError;
      end;
    end;

  procedure TSuperTask.Cancel;
    begin
      if fSubTask <> nil
        then
          begin
            fSubTask.Cancel;
            fSubTask.Free;
            fSubTask := nil;
          end;
      inherited Cancel;
    end;

  procedure TSuperTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
      if fSubTask <> nil
        then fSubTask.StoreToCache(Prefix, Cache);
    end;

  procedure TSuperTask.Loaded(Context : ITaskContext);
    begin
      inherited;
      if fSubTask <> nil
        then fSubTask.Loaded(Context);
    end;

  procedure TSuperTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      Reader.ReadObject('', fSubTask, nil);
    end;

  procedure TSuperTask.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteObject('SubTask', fSubTask);
    end;

  // TAtomicTask

  procedure TAtomicTask.StoreToCache(Prefix : string; Cache : TObjectCache);
    begin
      inherited;
    end;

  function TAtomicTask.TimeToSimulate(PeriodCount : integer) : boolean;
    begin
      inc(fPeriodCount, PeriodCount);
      if fPeriodCount >= Period
        then
          begin
            fPeriodCount := 0;
            result := true;
          end
        else result := false;
    end;

  function TAtomicTask.IsLeaveTask : boolean;
    begin
      result := true;
    end;

  function TAtomicTask.GetPeriod : integer;
    begin
      result := MetaTask.Period;
    end;

  procedure TAtomicTask.Activate;
    begin
      NotifyTycoon('');
    end;


  // TRandomTask

  function TRandomTask.SelectWinnerTask(Context : ITaskContext) : TMetaTask;
    var
     idx : integer;
    begin
     if not fPickedTask
       then
         begin
           // To improve the way the random task is picked..
           fPickedTask := true;
           idx := random(MetaTask.SubTaskCount);
           result := MetaTask.SubTasks[idx];
         end
       else result := nil;
    end;

  procedure TRandomTask.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fPickedTask := Reader.ReadBoolean('Picked', false);
    end;

  procedure TRandomTask.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteBoolean('Picked', fPickedTask);
    end;

{
  // TRootTaskCacheAgent

  class function TRootTaskCacheAgent.GetPath ( Obj : TObject ) : string;
    begin
      result := GetObjectPath( TRootTask(Obj).Owner ) + tidCachePath_Inputs + 'object.five';
    end;

  class function TRootTaskCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      TRootTask(Obj).StoreToCache(result);
    end;


  // RegisterCachers

  procedure RegisterCachers;
    begin
      RegisterCacher( TRootTask.ClassName, TRootTaskCacheAgent );
    end;
}

  procedure RegisterBackup;
    begin
      RegisterClass(TSuperTask);
      RegisterClass(TAtomicTask);
      RegisterClass(TRandomTask);
    end;

end.
