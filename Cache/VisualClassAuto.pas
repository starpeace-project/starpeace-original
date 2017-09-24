unit VisualClassAuto;

interface

uses
  Windows, ComObj, Visual_TLB, VisualClassManager, StdVcl;//IniClasses;

type
  TVisualClass = class(TAutoObject, IVisualClass)
  protected
    function ReadString(const Section, Name, DefValue: widestring): widestring; safecall;
    function ReadInteger(const Section, Name: widestring; DefValue: Integer): Integer; safecall;
    function ReadBool(const Section, Name: widestring; DefValue: WordBool): WordBool; safecall;
    function Get_Masked: WordBool; safecall;
    procedure Set_Masked(Value: WordBool); safecall;
    function Open(ClassId: Integer): WordBool; safecall;
    function Get_UserContext: WideString; safecall;
    procedure RefreshClasses; safecall;
    function  Get_ErrorCode: Integer; safecall;
    function  Get_FilePath: WideString; safecall;
  private
    //fIniClass     : TIniClass;
    fVisualClass   : VisualClassManager.TVisualClass;
    fMasked        : WordBool;
    fErrorCode     : integer;
    fClassFilePath : string;
  public
    destructor Destroy; override;
  private
    function OpenClasses : boolean;
  end;

implementation

  uses
    SysUtils, Classes, ActiveX, ComServ, VisualClassesData, SyncObjs;

  const
    erNone         = 0;
    erFileNotFound = 1;
    erUnknown      = 2;

  var
    ClassManager : TClassManager;
    Lock         : TCriticalSection;

  // TVisualClass

  function TVisualClass.OpenClasses : boolean;
    var
      Stream : TStream;
    begin
      try
        fClassFilePath := GetVisualRootPath + '\classes.bin';
        Stream := TFileStream.Create(fClassFilePath, fmOpenRead);
        try
          ClassManager := TClassManager.Load(Stream);
        finally
          Stream.Free;
        end;
        result := true;
      except
        result := false;
        fErrorCode := erFileNotFound;
      end;
    end;

  function TVisualClass.Open(ClassId: Integer): WordBool;
    begin
      try
        Lock.Enter;
        try
          if (ClassManager <> nil) or OpenClasses
            then
              begin
                fVisualClass := ClassManager.ClassById[ClassId];
                result := true;
              end
            else
              begin
                result := false;
                fVisualClass := nil;
              end;
        finally
          Lock.Leave;
        end;
      except
        fErrorCode := erUnknown;
        fVisualClass := nil;
        result := false;
      end;
    end;

  function TVisualClass.ReadString(const Section, Name, DefValue: widestring): widestring;
    begin
      try
        //result := fIniClass.ReadString(Section, Name, DefValue);
        if fVisualClass <> nil
          then result := fVisualClass.ReadString(Section, Name, DefValue)
          else result := DefValue;
      except
        result := DefValue;
      end;
    end;

  function TVisualClass.ReadInteger(const Section, Name: widestring; DefValue: Integer): Integer;
    begin
      try
        //result := fIniClass.ReadInteger(Section, Name, DefValue);
        if fVisualClass <> nil
          then result := fVisualClass.ReadInteger(Section, Name, DefValue)
          else result := DefValue;
      except
        result := DefValue;
      end;
    end;

  function TVisualClass.ReadBool(const Section, Name: widestring; DefValue: WordBool): WordBool;
    begin
      try
        //result := fIniClass.ReadBool(Section, Name, DefValue);
        if fVisualClass <> nil
          then result := fVisualClass.ReadBool(Section, Name, DefValue)
          else result := DefValue;
      except
        result := DefValue;
      end;
    end;

  destructor TVisualClass.Destroy;
    begin
      //fIniClass.Free;
      inherited;
    end;

  function TVisualClass.Get_Masked: WordBool;
    begin
      result := fMasked;
    end;

  procedure TVisualClass.Set_Masked(Value: WordBool);
    begin
      fMasked := Value;
    end;

  function TVisualClass.Get_UserContext: WideString;
    var
      nm : array[0..1000] of char;
      ln : cardinal;
    begin
      FillChar(nm, sizeof(nm), 0);
      Windows.GetUserName(nm, ln);
      result := nm;
    end;

  procedure TVisualClass.RefreshClasses;
    begin
      Lock.Enter;
      try
        ClassManager.Free;
        ClassManager := nil;
      finally
        Lock.Leave;
      end;
    end;

  function TVisualClass.Get_ErrorCode: Integer;
    begin
      result := fErrorCode;
    end;

  function TVisualClass.Get_FilePath: WideString;
    begin
      result := fClassFilePath;
    end;

initialization

  Lock := TCriticalSection.Create;

  TAutoObjectFactory.Create(ComServer, TVisualClass, Class_VisualClass, ciMultiInstance, tmApartment);

finalization

  if Lock <> nil
    then
      begin
        Lock.Free;
        Lock := nil;
      end;

end.


