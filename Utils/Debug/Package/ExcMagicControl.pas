unit ExcMagicControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExcMagic;

type
  TExcMagicControl = class(TComponent)
  private
    function  GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetMaxCallStack: Integer;
    procedure SetMaxCallStack(const Value: Integer);
    function GetCustomTab: String;
    function GetLogEnabled: Boolean;
    function GetLogFile: String;
    function GetOptions: TExcMagicOptions;
    procedure SetCustomTab(const Value: String);
    procedure SetLogEnabled(const Value: Boolean);
    procedure SetLogFile(const Value: String);
    procedure SetOptions(const Value: TExcMagicOptions);
    function GetOnCustomInfo: TExcMagicCustomInfoProc;
    function GetOnExceptionLog: TExcMagicLogProc;
    function GetOnExceptionMsg: TExcMagicMsgProc;
    function GetOnExceptionShow: TExcMagicShowProc;
    procedure SetOnCustomInfo(const Value: TExcMagicCustomInfoProc);
    procedure SetOnExceptionLog(const Value: TExcMagicLogProc);
    procedure SetOnExceptionMsg(const Value: TExcMagicMsgProc);
    procedure SetOnExceptionShow(const Value: TExcMagicShowProc);
    function GetLogHandled: Boolean;
    procedure SetLogHandled(const Value: Boolean);
    function GetSuppressRecursion: Boolean;
    procedure SetSuppressRecursion(const Value: Boolean);
    function GetOnTerminate: TExcMagicTerminateProc;
    procedure SetOnTerminate(const Value: TExcMagicTerminateProc);
  published
    property  CustomTab: String read GetCustomTab write SetCustomTab;
    property  Enabled: Boolean read GetEnabled write SetEnabled;
    property  LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;
    property  LogHandled: Boolean read GetLogHandled write SetLogHandled;
    property  LogFile: String read GetLogFile write SetLogFile;
    property  MaxCallStack: Integer read GetMaxCallStack write SetMaxCallStack;
    property  SuppressRecursion: Boolean read GetSuppressRecursion write SetSuppressRecursion;
    property  Options: TExcMagicOptions read GetOptions write SetOptions;

    property  OnExceptionMsg:  TExcMagicMsgProc  read GetOnExceptionMsg  write SetOnExceptionMsg;
    property  OnExceptionShow: TExcMagicShowProc read GetOnExceptionShow write SetOnExceptionShow;
    property  OnExceptionLog:  TExcMagicLogProc  read GetOnExceptionLog  write SetOnExceptionLog;
    property  OnCustomInfo:    TExcMagicCustomInfoProc read GetOnCustomInfo write SetOnCustomInfo;
    property  OnTerminate:     TExcMagicTerminateProc  read GetOnTerminate write SetOnTerminate;
  end;

procedure Register;

implementation

{$R *.DCR}

procedure Register;
begin
  RegisterComponents('ExcMagic', [TExcMagicControl]);
end;

{ TExcMagicControl }

function TExcMagicControl.GetEnabled: Boolean;
begin
  Result := ExceptionHook.Enabled;
end;

procedure TExcMagicControl.SetEnabled(const Value: Boolean);
begin
  ExceptionHook.Enabled := Value;
end;

function TExcMagicControl.GetMaxCallStack: Integer;
begin
  Result := ExceptionHook.MaxCallStack;
end;

procedure TExcMagicControl.SetMaxCallStack(const Value: Integer);
begin
  ExceptionHook.MaxCallStack := Value;
end;

function TExcMagicControl.GetCustomTab: String;
begin
  Result := ExceptionHook.CustomTab;
end;

function TExcMagicControl.GetLogEnabled: Boolean;
begin
  Result := ExceptionHook.LogEnabled;
end;

function TExcMagicControl.GetLogFile: String;
begin
  Result := ExceptionHook.LogFile;
end;

function TExcMagicControl.GetOptions: TExcMagicOptions;
begin
  Result := ExceptionHook.Options;
end;

procedure TExcMagicControl.SetCustomTab(const Value: String);
begin
  ExceptionHook.CustomTab := Value;
end;

procedure TExcMagicControl.SetLogEnabled(const Value: Boolean);
begin
  ExceptionHook.LogEnabled := Value;
end;

procedure TExcMagicControl.SetLogFile(const Value: String);
begin
  ExceptionHook.LogFile := Value;
end;

procedure TExcMagicControl.SetOptions(const Value: TExcMagicOptions);
begin
  ExceptionHook.Options := Value;
end;

function TExcMagicControl.GetOnCustomInfo: TExcMagicCustomInfoProc;
begin
  Result := ExceptionHook.OnCustomInfo;
end;

function TExcMagicControl.GetOnExceptionLog: TExcMagicLogProc;
begin
  Result := ExceptionHook.OnExceptionLog;
end;

function TExcMagicControl.GetOnExceptionMsg: TExcMagicMsgProc;
begin
  Result := ExceptionHook.OnExceptionMsg;
end;

function TExcMagicControl.GetOnExceptionShow: TExcMagicShowProc;
begin
  Result := ExceptionHook.OnExceptionShow;
end;

procedure TExcMagicControl.SetOnCustomInfo( const Value: TExcMagicCustomInfoProc);
begin
  ExceptionHook.OnCustomInfo := Value;
end;

procedure TExcMagicControl.SetOnExceptionLog( const Value: TExcMagicLogProc);
begin
  ExceptionHook.OnExceptionLog := Value;
end;

procedure TExcMagicControl.SetOnExceptionMsg( const Value: TExcMagicMsgProc);
begin
  ExceptionHook.OnExceptionMsg := Value;
end;

procedure TExcMagicControl.SetOnExceptionShow( const Value: TExcMagicShowProc);
begin
  ExceptionHook.OnExceptionShow := Value;
end;

function TExcMagicControl.GetOnTerminate: TExcMagicTerminateProc;
begin
  Result := ExceptionHook.OnTerminate;
end;

procedure TExcMagicControl.SetOnTerminate( const Value: TExcMagicTerminateProc);
begin
  ExceptionHook.OnTerminate := Value;
end;


function TExcMagicControl.GetLogHandled: Boolean;
begin
  Result := ExceptionHook.LogHandled;
end;

procedure TExcMagicControl.SetLogHandled(const Value: Boolean);
begin
  ExceptionHook.LogHandled := Value;
end;

function TExcMagicControl.GetSuppressRecursion: Boolean;
begin
  Result := ExceptionHook.SuppressRecursion;
end;

procedure TExcMagicControl.SetSuppressRecursion(const Value: Boolean);
begin
  ExceptionHook.SuppressRecursion := Value;
end;


end.
