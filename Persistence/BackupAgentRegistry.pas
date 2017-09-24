unit BackupAgentRegistry;

interface

  procedure RegisterBackupAgent(aClass, anAgent : TClass);
  procedure GetClassAgent(const ClassName : string; var TheAgent, TheClass : TClass);

implementation

  uses
    Classes, DataRegistry;

  // Aux class

  type
    TBackupRegistryEntry =
      class
        public
          constructor Create(aClass, anAgent : TClass);
        private
          fClass : TClass;
          fAgent : TClass;
        public
          property TheClass : TClass read fClass;
          property TheAgent : TClass read fAgent;
      end;

  constructor TBackupRegistryEntry.Create(aClass, anAgent : TClass);
    begin
      inherited Create;
      fClass := aClass;
      fAgent := anAgent;
    end;

  // Backup agents

  var
    RegClasses : TDataRegistry = nil;
    ClassCount : integer       = 0;
    AgentInquires : integer    = 0;

  procedure RegisterBackupAgent(aClass, anAgent : TClass);
    begin
      RegClasses.Add(aClass.ClassName, TBackupRegistryEntry.Create(aClass, anAgent));
      inc(ClassCount);
    end;

  procedure GetClassAgent(const ClassName : string; var TheAgent, TheClass : TClass);
    var
      aux : TBackupRegistryEntry;
    begin
      inc(AgentInquires);
      aux := TBackupRegistryEntry(RegClasses[ClassName]);
      if aux <> nil
        then
          begin
            TheAgent := aux.TheAgent;
            TheClass := aux.TheClass;
          end
        else
          begin
            TheAgent := nil;
            TheClass := nil;
          end;
    end;


initialization

  RegClasses := TDataRegistry.Create;

finalization

  RegClasses.Free;

end.
