unit MailBrowserAuto;

interface

uses
  Classes, SysUtils, ComObj, MailBrowser_TLB, DirIterator;

type
  TMailBrowser = class(TAutoObject, IMailBrowser)
  protected
    function Get_Account: WideString; safecall;
    function Get_Folder: WideString; safecall;
    procedure Set_Account(const Value: WideString); safecall;
    procedure Set_Folder(const Value: WideString); safecall;
    function Next: WordBool; safecall;
    procedure Reset; safecall;
    function Get_Empty: WordBool; safecall;
    function Get_World: WideString; safecall;
    procedure Set_World(const Value: WideString); safecall;
    function Get_Header(const Name: WideString): WideString; safecall;
    function DeleteMessage(const MsgPath: WideString): WordBool; safecall;
    function FullPath: WideString; safecall;
  private
    fWorld    : string;
    fAccount  : string;
    fFolder   : string;
    fIterator : TFolderIterator;
    fHeaders  : TStringList;
  public
    destructor Destroy; override;
    procedure  Initialize; override;
    procedure  ReadHeaders;
  end;

implementation

  uses
    ComServ, MailUtils, MailConsts;

  function TMailBrowser.Get_Account: WideString;
    begin
      result := fAccount;
    end;

  function TMailBrowser.Get_Folder: WideString;
    begin
      result := fFolder;
    end;

  procedure TMailBrowser.Set_Account(const Value: WideString);
    begin
      fAccount := Value;
    end;

  procedure TMailBrowser.Set_Folder(const Value: WideString);
    begin
      fFolder := Value;
    end;

  function TMailBrowser.Next: WordBool;
    begin
      if fIterator <> nil
        then
          if fIterator.Next
            then
              try
                result := true;
                ReadHeaders;
              except
              end
            else result := false
        else result := false;
    end;

  procedure TMailBrowser.Reset;
    begin
      try
        fIterator.Free;
        fIterator := TFolderIterator.Create(GetAccountPath(fWorld, fAccount) + fFolder, '*.*', faDirectory);
        if not fIterator.Empty
          then ReadHeaders;
      except
      end;
    end;

  function TMailBrowser.Get_Empty: WordBool;
    begin
      if fIterator <> nil
        then
          try
            result := fIterator.Empty
          except
            result := true;
          end
        else result := true;
    end;

  function TMailBrowser.Get_Header(const Name: WideString): WideString;
    begin
      try
        result := fHeaders.Values[Name];
      except
        result :=  '';
      end;
    end;

  function TMailBrowser.Get_World: WideString;
    begin
      result := fWorld;
    end;

  procedure TMailBrowser.Set_World(const Value: WideString);
    begin
      fWorld := Value;
    end;

  destructor TMailBrowser.Destroy;
    begin
      fHeaders.Free;
      fIterator.Free;
      inherited;
    end;

  procedure TMailBrowser.Initialize;
    begin
      inherited;
      fHeaders := TStringList.Create;
    end;

  procedure TMailBrowser.ReadHeaders;
    var
      aux : string;
    begin
      try
        fHeaders.Clear;
        aux := fIterator.FullPath;
        if aux[length(aux)] = '\'
          then fHeaders.LoadFromFile(fIterator.FullPath + tidMessage_Header)
          else fHeaders.LoadFromFile(fIterator.FullPath + '\' + tidMessage_Header);
      except
      end;
    end;

  function TMailBrowser.DeleteMessage(const MsgPath: WideString): WordBool;
    begin
      try
        result := RemoveFullPath(GetAccountPath(fWorld, fAccount) + fFolder + '\' + MsgPath);
      except
        result := false;
      end;
    end;

  function TMailBrowser.FullPath: WideString;
    begin
      result := GetAccountPath(fWorld, fAccount) + fFolder + '\';
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TMailBrowser, Class_MailBrowser, ciMultiInstance, tmApartment);

end.
