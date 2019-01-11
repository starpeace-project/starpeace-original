unit FileLogger;

interface

uses
  SysUtils, Forms;

  function LogString(msg : string) : variant;
implementation
function LogString(msg : string) : variant;
var
  Filename: string;
  LogFile: TextFile;
begin
  // prepares log file
  Filename := ChangeFileExt(Application.Exename, '.log');
  AssignFile (LogFile, Filename);
  if FileExists (FileName) then
    Append (LogFile) // open existing file
  else
    Rewrite (LogFile); // create a new one
  try
    // write to the file and show error
    Writeln (LogFile, DateTimeToStr (Now) + ':' + msg);
  finally
    // close the file
    CloseFile (LogFile);
  end;
end;


end.