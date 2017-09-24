unit SPService;
//monitorear una app
//la app si termina ok crea el fichero
//     sino esta corriendo y esta el fichero lanzo la app
//     si esta corriendo la dejo corriendo
// si el fichero no esta y no esta corriendo pongo en la traza
// y envio un email a una address

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs;

type
  TSMSWatcher = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  SMSWatcher: TSMSWatcher;

implementation
uses DoTask, inifiles ;
{$R *.DFM}

var
  MyDoTask : TDoTask ;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SMSWatcher.Controller(CtrlCode);
end;

function TSMSWatcher.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSMSWatcher.ServiceAfterInstall(Sender: TService);
begin
  LogMessage('MSWatcher Service has just installed', EVENTLOG_INFORMATION_TYPE     , 0, 0);
end;

procedure TSMSWatcher.ServiceAfterUninstall(Sender: TService);
begin
  LogMessage('MSWatcher Service has just uninstalled', EVENTLOG_INFORMATION_TYPE     , 0, 0);
end;

procedure TSMSWatcher.ServiceStart(Sender: TService; var Started: Boolean);
var
  AppName : string ;
  CheckPeriod : integer ;
  CheckPeriodS : string ;
  FileToCheck : string ;
  i : integer ;
  S : string ;
  WinDir : array [1..150] of char ;
  TIF : TIniFile ;
  MailServer, EmailToNotify : string ;
begin
// main stuff for the service goes here
 AppName := '' ;
 CheckPeriod := 2 ;
 FileToCheck := '' ;
 try
   //Str ( ParamCount, s ) ;
   //LogMessage ( 'MSWatcher: paramcount: ' + s, EVENTLOG_INFORMATION_TYPE     , 0, 0);
   //LogMessage ( 'MSWatcher: param 1 : '+ Param[0] ) ;
   GetCurrentDirectory ( 150, PChar(@WinDir[1]) ) ;
   S := StrPas ( PChar(@WinDir[1]) ) ;
   S := S + '\MSWatcher.ini' ;
   TIF := TIniFile.Create ( S ) ;
   AppName := TIF.ReadString ( 'MSWatcher params', 'Program to Watch', '' ) ;
   FileToCheck := TIF.ReadString ( 'MSWatcher params', 'File to Check', '' ) ;
   CheckPeriodS := TIF.ReadString ( 'MSWatcher params', 'CheckPeriod','' ) ;
   MailServer := TIF.ReadString ( 'MSWatcher params', 'Server','' ) ;
   EmailToNotify := TIF.ReadString ( 'MSWatcher params', 'email','' ) ;
   TIF.Free ;
   if CheckPeriodS <>  ''
     then
      Val ( CheckPeriodS, CheckPeriod, i ) ;
//   AppName := 'C:\winnt\system32\calc.exe' ;
//   FileToCheck := 'C:\file.txt' ;
   LogMessage ( 'MSWatcher: service params AppName:'+AppName+' FileToCheck:'+FileToCheck+
                 ' CheckPeriod:'+CheckPeriodS+ ' MailServer:'+MailServer+
                 ' Email:'+ EmailToNotify+' inifile:'+S ,
                        EVENTLOG_INFORMATION_TYPE , 0, 0 ) ;
   if (AppName = '') or (FileToCheck = '')
     then
       begin
         LogMessage ( 'MSWatcher: service do not start, you must supply Exe to run and File to check' ,
                        EVENTLOG_ERROR_TYPE, 0, 0 ) ;
         Started := False ;
       end ;
   MyDoTask := TDoTask.Create ( AppName, FileToCheck, CheckPeriod, SMSWatcher,
                                      MailServer, EmailToNotify ) ;
   Started := True;
 except //rudimentary error handling
   on e:Exception do begin
     LogMessage('MSWatcher: an error occurred in MSWatcher. The error was ' + e.Message,
                 EVENTLOG_ERROR_TYPE, 0, 0);
     Started := False;
   end;
 end;
end;



procedure TSMSWatcher.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  //is there is any thread then call its terminate
  if MyDoTask <> nil
    then
      begin
        MyDoTask.Terminate ;
        Stopped := True;
      end ;
end;

procedure TSMSWatcher.ServicePause(Sender: TService; var Paused: Boolean);
begin
//if there is any thread then suspend it
  if MyDoTask <> nil
    then
      begin
        MyDoTask.Suspend ;
        Paused := True;
      end ;
end;


procedure TSMSWatcher.ServiceContinue(Sender: TService;
  var Continued: Boolean);
begin
//if there is any thread then resume it
  if MyDoTask <> nil
    then
      begin
        MyDoTask.Resume ;
        Continued := true ;
      end ;
end;

end.
