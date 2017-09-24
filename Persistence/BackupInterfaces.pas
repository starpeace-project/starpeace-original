unit BackupInterfaces;

interface

  uses
    SysUtils, Classes, SyncObjs;

  const
    NULLPROC : TMethod =
      (
        Code : nil;
        Data : nil
      );

  const
    IgnoredBufferSize = -1;

  type
    IBackupObject =
      interface
        procedure Lock;
        procedure Unlock;
        function  GetPosition : integer;
        procedure Seek(offset : integer);
        function  GetPath : string;

        property Path : string read GetPath;
      end;

    IBackupWriter =
      interface(IBackupObject) ['{56DFE440-9E79-11D1-A1A8-0080C817C099}']
        procedure WriteString(Name : string; Value : string);             stdcall;
        procedure WriteClass(Name : string; Value : string);              stdcall;
        procedure WriteChar(Name : string; Value : char);                 stdcall;
        procedure WriteBoolean(Name : string; Value : boolean);           stdcall;
        procedure WriteByte(Name : string; Value : byte);                 stdcall;
        procedure WriteWord(Name : string; Value : word);                 stdcall;
        procedure WriteInteger(Name : string; Value : integer);           stdcall;
        procedure WriteInt64(Name : string; Value : int64);               stdcall;
        procedure WriteSingle(Name : string; Value : single);             stdcall;
        procedure WriteDouble(Name : string; Value : double);             stdcall;
        procedure WriteCurrency(Name : string; Value : currency);         stdcall;
        procedure WriteObject(Name : string; Value : TObject);            stdcall;
        procedure WriteObjectRef(Name : string; Value : TObject);         stdcall;
        procedure WriteLooseObject(Name : string; Value : TObject);       stdcall;
        procedure WriteMethod(Name : string; Method : TMethod);           stdcall;
        procedure WriteBuffer(Name : string; var buffer; size : integer); stdcall;
        procedure WriteUnsafeObject(Name : string; Value : TObject);      stdcall;
        procedure WriteUnsafeLooseObject(Name : string; Value : TObject); stdcall;
        procedure WriteUnsafeObjectRef(Name : string; Value : TObject);   stdcall;
      end;

    IBackupReader =
      interface ['{56DFE441-9E79-11D1-A1A8-0080C817C099}']
        function  ReadString(Name : string; DefVal : string) : string;                stdcall;
        function  ReadClass(Name : string; DefVal : string) : string;                 stdcall;
        function  ReadChar(Name : string; DefVal : char) : char;                      stdcall;
        function  ReadBoolean(Name : string; DefVal : boolean) : boolean;             stdcall;
        function  ReadByte(Name : string; DefVal : byte) : byte;                      stdcall;
        function  ReadWord(Name : string; DefVal : word) : word;                      stdcall;
        function  ReadInteger(Name : string; DefVal : integer) : integer;             stdcall;
        function  ReadInt64(Name : string; DefVal : int64) : int64;                   stdcall;
        function  ReadSingle(Name : string; DefVal : single) : single;                stdcall;
        function  ReadDouble(Name : string; DefVal : double) : double;                stdcall;
        function  ReadCurrency(Name : string; DefVal : currency) : currency;          stdcall;
        function  ReadObject(Name : string; var O; DefVal : TObject) : string;        stdcall;
        function  ReadLooseObject(Name : string; var O; DefVal : TObject) : string;   stdcall;
        procedure ReadMethod(Name : string; var Method : TMethod; DefVal : TMethod);  stdcall;
        procedure ReadBuffer(Name : string; var buffer; DefVal :  pointer; size : integer); stdcall;
        function  GetFixups : TObject; stdcall;
        procedure SetFixups(Obj : TObject); stdcall;
      end;


  // Backup Agent root class

  type
    CBackupAgent = class of TBackupAgent;
    TBackupAgent =
      class
        public
          class procedure Register(const Classes : array of TClass);
        public
          class function  CreateObject(aClass : TClass) : TObject;      virtual;
          class procedure Write(Stream : IBackupWriter; Obj : TObject); virtual; abstract;
          class procedure Read (Stream : IBackupReader; Obj : TObject); virtual; abstract;
      end;

    EInternalBackupError = class(Exception);


  // Registration util functions

  procedure RegisterClass(aClass : TClass);
  function  GetBackupAgent(ClassName : string; var Agent : CBackupAgent; var ClassType : TClass) : boolean;

implementation

  // External routines

  procedure RegisterBackupAgent(aClass, anAgent : TClass); external 'BackupRegistry.dll';
  procedure GetClassAgent(const ClassName : string; var TheAgent, TheClass : TClass); external 'BackupRegistry.dll';

  // TBackupAgent

  class procedure TBackupAgent.Register(const Classes : array of TClass);
    var
      i : integer;
    begin
      try
        for i := low(Classes) to high(Classes) do
          RegisterBackupAgent(Classes[i], Self);
      except
        raise EInternalBackupError.Create('Class already registered');
      end;
    end;

  class function TBackupAgent.CreateObject(aClass : TClass) : TObject;
    begin
      result := aClass.Create;
    end;


  // Utility functions

  procedure RegisterClass(aClass : TClass);
    var
      Agent : CBackupAgent;
      aux   : TClass;
      Cls   : TClass;
    begin
      Agent := nil;
      aux   := aClass;
      while (aux <> nil) and (Agent = nil) do
        begin
          GetBackupAgent(aux.ClassName, Agent, Cls);
          aux := aux.ClassParent;
        end;
      if Agent <> nil
        then RegisterBackupAgent(aClass, Agent)
        else raise EInternalBackupError.Create('No class parent has been registered yet for "' + aClass.ClassName + '"');
    end;

  function GetBackupAgent(ClassName : string; var Agent : CBackupAgent; var ClassType : TClass) : boolean;
    begin
      GetClassAgent(ClassName, TClass(Agent), ClassType);
      result := (Agent <> nil) and (ClassType <> nil);
    end;

end.
