library BackupRegistry;

uses
  ShareMem,
  SysUtils,
  Classes,
  BackupAgentRegistry in 'BackupAgentRegistry.pas',
  MapStringToObject in '..\Utils\Misc\MapStringToObject.pas';

exports
  RegisterBackupAgent,
  GetClassAgent;

begin                  
  IsMultiThread := true;    
end.
