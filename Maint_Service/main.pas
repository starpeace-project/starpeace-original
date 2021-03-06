unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ScktComp;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    TCS: TClientSocket;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure TCSConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure TCSRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure TCSError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses tlhelp32, inifiles, mapi ;
{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    NotepadExe:  String;
begin
    // notepad is in the windows directory,
    SetLength(NotepadExe, MAX_PATH);
    GetWindowsDirectory(Pchar(NotepadExe), MAX_PATH);
    StrCat(Pchar(NotepadExe),'\notepad.exe');


    ZeroMemory(@StartupInfo,SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);

    if(CreateProcess(Pchar(NotepadExe), Nil, Nil, Nil,
                     False, NORMAL_PRIORITY_CLASS,
                     Nil, Nil, StartupInfo, ProcessInfo))  then
    begin
        // we must close the handles returned in ProcessInfo.hProcess
        // we can close the handle at any time, might as well close it now
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
    end
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    NotepadExe:  String;
begin
    // notepad is in the windows directory,
    SetLength(NotepadExe, MAX_PATH);
    GetWindowsDirectory(Pchar(NotepadExe), MAX_PATH);
    StrCat(Pchar(NotepadExe),'\notepad.exe c:\autoexec.bat');


    ZeroMemory(@StartupInfo,SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);

    if(CreateProcess(Nil,Pchar(NotepadExe), Nil, Nil,
                     False, NORMAL_PRIORITY_CLASS,
                     Nil, Nil, StartupInfo, ProcessInfo))  then
    begin
        // we must close the handles returned in ProcessInfo.hProcess
        // we can close the handle at any time, might as well close it now
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
    end

end;

procedure TForm1.Button3Click(Sender: TObject);
var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    NotepadExe:  String;
    StartCount:  Integer;
begin
    // notepad is in the windows directory,
    SetLength(NotepadExe, MAX_PATH);
    GetWindowsDirectory(Pchar(NotepadExe), MAX_PATH);
    StrCat(Pchar(NotepadExe),'\notepad.exe');


    ZeroMemory(@StartupInfo,SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);

    if(CreateProcess(Pchar(NotepadExe), Nil, Nil, Nil,
                     False, NORMAL_PRIORITY_CLASS,
                     Nil, Nil, StartupInfo, ProcessInfo))  then
    begin
        StartCount := GetTickCount();
        WaitForInputIdle(ProcessInfo.hProcess, 2000);
        Label1.Caption := IntToStr(GetTickCount() - StartCount);

        // we must close the handles returned in ProcessInfo.hProcess
        // we can close the handle at any time, might as well close it now
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
    end
end;

procedure TForm1.Button4Click(Sender: TObject);
var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    NotepadExe:  String;
begin
    // notepad is in the windows directory,
    SetLength(NotepadExe, MAX_PATH);
    GetWindowsDirectory(Pchar(NotepadExe), MAX_PATH);
    StrCat(Pchar(NotepadExe),'\notepad.exe');


    ZeroMemory(@StartupInfo,SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.dwX := 0;
    StartupInfo.dwY := 0;
    StartupInfo.dwXSize := Screen.Width DIV 3;
    StartupInfo.dwYSize := Screen.Height DIV 3;
    Startupinfo.dwFlags := STARTF_USEPOSITION or STARTF_USESIZE;

    if(CreateProcess(Pchar(NotepadExe), Nil, Nil, Nil,
                     False, NORMAL_PRIORITY_CLASS,
                     Nil, Nil, StartupInfo, ProcessInfo))  then
    begin
        // we must close the handles returned in ProcessInfo.hProcess
        // we can close the handle at any time, might as well close it now
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
    end
end;

procedure TForm1.Button5Click(Sender: TObject);
// Result := FindWindow(myClassName, nil) > 0 ; {the application is running}
var
  IsRunning, ContinueTest: Boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  S : string ;
  WinDir : array[1..150] of char ;
  TIF : TIniFile ;
  Progra, FileW, Period : string ;
begin
  GetCurrentDirectory ( 150, PChar(@WinDir[1]) ) ;
  S := StrPas ( PChar(@WinDir[1]) ) ;
  S := S + '\prueba.ini' ;
  TIF := TIniFile.Create ( S ) ;
  Progra := TIF.ReadString ( 'MSWatcher params', 'Program to Watch', '' ) ;
  FileW := TIF.ReadString ( 'MSWatcher params', 'File to Check', '' ) ;
  Period := TIF.ReadString ( 'MSWatcher params', 'CheckPeriod','' ) ;
  TIF.Free ;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueTest := Process32First(FSnapshotHandle, FProcessEntry32);
  while ContinueTest do begin
    IsRunning :=
      UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExtractFileName('C:\winnt\system32\calc.exe'));
    if IsRunning then
      ContinueTest := False
    else
      ContinueTest := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
  //Result := IsRunning ;
end;

procedure TForm1.Button6Click(Sender: TObject);
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
  MailTo := 'SMTP:' + 'orelbigm@starpeace.net';
  cnt := 1; // Un solo mensaje
  with Recip[0] do
    begin
      ulReserved   := 0;
      ulRecipClass := MAPI_TO; // MAPI_TO, MAPI_CC, MAPI_BCC, MAPI_ORIG
      lpszName     := PChar(mName);  // Nombre del recipiente
      lpszAddress  := PChar(MailTo); // Direccion del recipiente (opcional)
      ulEIDSize    := 0;       // Tama�o en byte de pEntryID
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
    Subject := 'MSWatcher: there is some problem with appname'+
       ' it is not running and I have not found filename';
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
{  if MError = SUCCESS_SUCCESS
    then
       servLauncher.LogMessage('mapi logon',  EVENTLOG_ERROR_TYPE, 0, 0)
    else
       servLauncher.LogMessage('mapi not logon',  EVENTLOG_ERROR_TYPE, 0, 0);}
    if MError = SUCCESS_SUCCESS then
      MError := MapiSendMail(hSession,    // Sesi�n de Simple MAPI session o cero
                             0,
                             MapiMessage, // estructura MapiMessage
                             0,
                             0);
  finally
    ll.Free;
  end;

{  if MError = SUCCESS_SUCCESS
    then
       servLauncher.LogMessage('message sent',  EVENTLOG_ERROR_TYPE, 0, 0)
    else
       servLauncher.LogMessage('message not sent',  EVENTLOG_ERROR_TYPE, 0, 0)}
//ver con Iroel si muestro msgbox
  if MError = SUCCESS_SUCCESS
    then
      MessageDlg( 'Mensaje entregado al dep�sito de correo.', mtInformation, [mbOK], 0)
    else
      MessageDlg( 'Error enviando el mensaje.' + #13#10 + 'Verifique que existe un cliente de correo.', mtError, [mbOK], 0);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  s: string;
  i, x : integer;
  buff: array[1..1024] of char;
  ReplyCode, ReplyString : string ;

procedure ReceiveText ;
begin
  repeat
    Application.ProcessMessages;

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
    Application.ProcessMessages;
    if length( s) < 4 then s := '----';
  until s[4] = ' ';
end ;

begin
//
  try
   TCS := TClientSocket.Create ( Application ) ;
   TCS.Host := 'mail.starpeace.net' ;
   TCS.Address := '' ;
   TCS.Port := 25 ;
   TCS.Open ;
   Application.ProcessMessages ;
   ReceiveText ;
   // if ReplyCode <> '220' then hay error
   s := 'HELO ['+TCS.Host+']'+#13#10 ;
   TCS.Socket.SendBuf( pointer(s)^, length( s));
   ReceiveText ;
   // if ReplyCode <> '250' then hay error
   s := 'MAIL FROM:' + '<' + 'supervisor@starpeace.net' + '>' + #13#10;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   // if ReplyCode <> '250' then hay error
   s := 'RCPT TO:' + '<' + 'orelbigm@starpeace.net' + '>' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   // if ReplyCode <> '250' then hay error
   s := 'DATA' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   // if ReplyCode <> '354' then hay error
   s := 'Subject: asunto'+#13#10#13#10+ 'este es el cuerpo del mensaje' ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   s := #13#10+'.'+#13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   // if ReplyCode <> '250' then hay error
   s := 'QUIT' + #13#10 ;
   TCS.Socket.SendBuf ( pointer(s)^, length ( s ) ) ;
   ReceiveText ;
   // if ReplyCode <> '221' then hay error

{   TCS.Socket.SendText ( 'HELO'+#13#10+'MAIL FROM:<supervisor@starpeace.net>'+
                          #13#10+'RCPT TO:<orelbigm@starpeace.net>'+#13#10+
                          'DATA'+#13#10+'Subject:aqui va el subject'+#13#10#13#10+{RFC 822}
                          {'Este es el cuerpo del mensaje'+#13#10+'.'+#13#10) ;}
   TCS.Close ;
  finally
   TCS.Free ;
  end ;
end;

procedure TForm1.TCSConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
 Form1.Caption := 'ClientConnect' ;
end;

procedure TForm1.TCSRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
 S : string ;
begin
 S := TCS.Socket.ReceiveText ;
end;

procedure TForm1.TCSError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
 Form1.Caption := 'Client Erro' ;
 TCS.Close ;
end;

end.
