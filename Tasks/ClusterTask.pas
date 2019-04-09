unit ClusterTask;

interface

  uses
    Tasks, Variants;

  type
    TMetaClusterTask =
      class(TMetaTask)
        private
          fClusterName : string;
        public
          property ClusterName : string read fClusterName write fClusterName;
      end;

    TClusterTask =
      class(TSuperTask)
        public
          class function GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer; override;
      end;


  procedure RegisterBackup;

implementation

  uses
    Kernel, BackupInterfaces;

  class function TClusterTask.GetPriority(MetaTask : TMetaTask; SuperTask : TTask; Context : ITaskContext) : integer;
    var
      Company : TCompany;
    begin
      result := inherited GetPriority(MetaTask, SuperTask, Context);
      if result <> tprIgnoreTask
        then
          begin
            Company := TCompany(Context.getContext(tcIdx_Company));
            if (Company <> nil) and (Company.Cluster.Id = TMetaClusterTask(MetaTask).fClusterName)
              then result := tprHigh
              else result := tprIgnoreTask;
          end;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TClusterTask);
    end;

end.
