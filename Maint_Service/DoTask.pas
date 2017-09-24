unit DoTask;

interface

uses
  Classes, svcmgr;

type
  TDoTask = class(TThread)
  private
    SAppName : string ;
    SFileName : string ;
    SServerName : string ;
    SEmailAddress : string ;
    dCheckPeriod : integer ;
    servLauncher : TService ;
    { Private declarations }
  protected
    procedure Execute; override;
    function ExistFile  : boolean ;
    function AppRunning  : boolean ;
    function LaunchApp  : boolean ;
    procedure WriteTextFile ;
    procedure SendMail (address: string);
    procedure SendMailSocket ( address : string ) ;
    procedure SendMailMAPI ( address : string ) ;
    //
  public
    constructor Create ( AppName, FileName: string; CheckPeriod: integer; Launcher: TService;
                          ServerName, EmailAddress : string );
  end;

implementation
uses windows, sysutils, tlhelp32, shellapi, mapi, ScktComp ;
{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure DoTask.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ DoTask }



constructor TDoTask.Create( AppName, FileName: string; CheckPeriod : integer;
                             Launcher : TService; ServerName, EmailAddress : string );
begin
  inherited Create ( false ) ;
  SAppName := AppName ;
  SFileName := FileName ;
  dCheckPeriod := CheckPeriod ;
  servLauncher := Launcher ;
  FreeOnTerminate := true ;
  SServerName := ServerName ;
  SEmailAddress := EmailAddress ;
end;

procedure TDoTask.WriteTextFile;
const
 i : integer = 0 ;
var
 fs : TFileStream;
 TempStr : string;
 j : integer ;
begin
{ j := WinExec ( 'C:\winnt\system32\calc.exe', SW_SHOW ) ;
 Inc ( i ) ;
 Str ( j, TempStr ) ;
 TempStr := 'executing '+TempStr;
 fs := TFileStream.Create('c:\gazfiles.txt', fmCreate or fmShareDenyWrite);
 try
   fs.Write(PChar(TempStr)^, Length(TempStr));
 finally
   fs.Free;
 end;}
end;


procedure TDoTask.Execute;
var
  S : string ;
begin
  { Place thread code here }
  while not Terminated do
    begin
      //// Processing
      if (not AppRunning) and (ExistFile ) then
        begin
          {WriteTextFile ;}
{          MessageBox ( 0, 'execute', 'execute', mb_Ok ) ;}
          LaunchApp ;
        end
      else
        if (not AppRunning) and (not ExistFile) then
          begin
            if (SEmailAddress <>'') and (SServerName<>'') then
              SendMail ( SEmailAddress ) ;
          end ;
      Sleep ( dCheckPeriod * 60 * 500{1000} ) ;// 2 minutos sera configurable
    end ;
end;


procedure TDoTask.SendMail ( address: string ) ;
begin
  SendMailSocket ( address ) ;
end ;

procedure TDoTask.SendMailSocket ( address : string ) ;
var
  s: string;
  i, x : integer;
  buff: array[1..1024] of char;
  ReplyCode, ReplyString : string ;
  TCS : TClientSocket ;

procedure ReceiveText ;
begin
  repeat
    //Application.ProcessMessages;
    servLauncher.ServiceThread.ProcessRequests ( false ) ;

    x := TCS.Socket.ReceiveBuf( buff, 1024);

    s := copy( buff, 1, x);
    i := pos( #13#10, s);
    while (i > 0) and (length( s) > i+2) do
    begin
      s := copy( s, i+2, length(s)-i-1);
      i := pos( #13#10, s);
    end;
    ReplyCode := trim( copy( s, 1, 3));
    ReplyString := trim( copy( s, 4, 64));
    //result := s;
    //Application.ProcessMessages;
    servLauncher.ServiceThread.ProcessRequests ( false ) ;
    if length( s) < 4 then s := '----';
  until s[4] = ' ';
end ;

begin
//
  try
   TCS := TClientSocket.Create ( Application ) ;
   TCS.ClientType := ctNonBlocking ;
   TCS.Host := SServerName ;//'mail.starpeace.net' ; //esto sera configurable
   TCS.Address := '' ;
   TCS.Port := 25 ;
   TCS.Open ;
   //Application.ProcessMessages ;
   servLauncher.ServiceThread.ProcessRequests ( false ) ;
   ReceiveText ;
   s := 'HELO ['+TCS.Host+']'+#13#10 ;
   TCS.Socket.SendBuf( pointer(s)^, length( s));
   ReceiveText ;
   s := 'MAIL FROM:' + '<' + 'supervisor@starpeace.net' + '>' + #13#10;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   s := 'RCPT TO:' + '<' + address + '>' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   s := 'DATA' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   s := 'Subject: MSWatcher: problems running application'+#13#10#13#10+
   'Application is not running properly.' ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   s := #13#10+'.'+#13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   s := 'QUIT' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   TCS.Close ;
  finally
   TCS.Free ;
  end ;
end ;

procedure TDoTask.SendMailMAPI (address: string);
var
  MapiMessage : TMapiMessage;
  Recip : array [0..1] of TMapiRecipDesc;
  MError, cnt  : Cardinal;
  ll : TStringList;
  mName,
  MailTo,
// MailCC,
  Subject : string;
  hSession : LHANDLE;
begin
  // Escoger un nombre descriptivo del recipiente a donde se envia el mensaje
  mName  := 'Servers Supervisor';
  // Formar la direccion electronica del recipiente a donde se envia el mensaje
  MailTo := 'SMTP:' + address;
  cnt := 1; // Un solo mensaje
  with Recip[0] do
    begin
      ulReserved   := 0;
      ulRecipClass := MAPI_TO; // MAPI_TO, MAPI_CC, MAPI_BCC, MAPI_ORIG
      lpszName     := PChar(mName);  // Nombre del recipiente
      lpszAddress  := PChar(MailTo); // Direccion del recipiente (opcional)
      ulEIDSize    := 0;       // Tamaño en byte de pEntryID
     lpEntryID    := nil;
    end;
{
  // Incluir este descriptor de recipiente, si hay envio de copia del mensaje
  if mailCCAddr <> '' then
  begin
    Inc(cnt);
    MailCC := 'SMTP:' + '';//Poner aqui la direccion del recipiente que recibira la copia;
    with Recip[1] do
    begin
      ulReserved   := 0;
      ulRecipClass := MAPI_CC; // MAPI_TO, MAPI_CC, MAPI_BCC, MAPI_ORIG
      lpszName     := PChar(mName);
      lpszAddress  := PChar(MailCC);
      ulEIDSize    := 0;
      lpEntryID    := nil;
    end;
  end;
}
  // Crear y llenar el contenido del mensaje
  ll := TStringList.Create;
  try
    Subject := 'MSWatcher: there is some problem with '+SAppName+
       ' it is not running and I have not found '+sFileName;
    ll.Add(''); //Cuerpo del mensaje
    ll.Add('');
    ll.Add('');

    with MapiMessage do
      begin
        ulReserved   := 0;
        lpszSubject  := PChar(Subject); // Asunto del mensaje
        lpszNoteText := PChar(ll.Text); // Cuerpo del mensaje
        lpszMessageType    := nil;
        lpszDateReceived   := nil;
        lpszConversationID := nil;
        flFlags := 0;
        lpOriginator := nil;
        nRecipCount  := cnt;  //1 - Numero de recipientes de mensajes
        lpRecips     := @Recip; // Arreglo de recipientes de mensajes
        nFileCount   := 0;   // Contador de ficheros adjuntos
       lpFiles      := nil; // Arreglo a los descriptores de los ficheros adjuntos
      end;

    MError := MAPILogon(0, PChar(0), PChar(0), MAPI_LOGON_UI or MAPI_NEW_SESSION or MAPI_EXTENDED, 0, @hSession);
    if MError = SUCCESS_SUCCESS then
      MError := MapiSendMail(hSession,    // Sesión de Simple MAPI session o cero
                             0,
                             MapiMessage, // estructura MapiMessage
                             0,
                             0);
  finally
    ll.Free;
  end;

//ver con Iroel si muestro msgbox
{  if MError = SUCCESS_SUCCESS
    then
      MessageDlg( 'Mensaje entregado al depósito de correo.', mtInformation, [mbOK], 0)
    else
      MessageDlg( 'Error enviando el mensaje.' + #13#10 + 'Verifique que existe un cliente de correo.', mtError, [mbOK], 0);}
end;

function TDoTask.ExistFile: boolean;
begin
//
Result := FileExists ( Sfilename ) ;
end;

function TDoTask.LaunchApp: boolean;
var
 StartupInfo: TStartupInfo;
 ProcessInfo: TProcessInformation;
begin
 ZeroMemory(@StartupInfo,SizeOf(TStartupInfo));
 StartupInfo.cb := SizeOf(TStartupInfo);
 if(CreateProcess(PChar(SAppName), Nil, Nil, Nil,
                  False, NORMAL_PRIORITY_CLASS,
                  Nil, Nil, StartupInfo, ProcessInfo))  then
   begin
     // we must close the handles returned in ProcessInfo.hProcess
     // we can close the handle at any time, might as well close it now
     CloseHandle(ProcessInfo.hProcess);
     CloseHandle(ProcessInfo.hThread);
   end
 else
   begin
   end ;
end;

function TDoTask.AppRunning: boolean;
// Result := FindWindow(myClassName, nil) > 0 ; {the application is running}
var
  IsRunning, ContinueTest: Boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  IsRunning := False;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueTest := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueTest do begin
    IsRunning :=
      UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExtractFileName(SAppname));
    if IsRunning then
      ContinueTest := False
    else
      ContinueTest := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
  Result := IsRunning ;
end;

end.
