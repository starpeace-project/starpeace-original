unit MailMessageAuto;

interface

uses
  ComObj, MailMessage_TLB, Classes, StdVcl;

type
  TFiveMessage = class(TAutoObject, IFiveMessage)
  protected
    function Delete: WordBool; safecall;
    function Get_Attachment(const prop: WideString): WideString; safecall;
    function Get_AttachmentCount: Integer; safecall;
    function Get_CurrentAttachment: Integer; safecall;
    function Get_Header(const Name: WideString): WideString; safecall;
    function Get_Lines(index: Integer): WideString; safecall;
    function SetMessage(const aWorld, aAccount, aFolder, aMessage: WideString): WordBool; safecall;
    procedure Set_CurrentAttachment(Value: Integer); safecall;
    function Get_LineCount: Integer; safecall;
  private
    procedure UpdateAttachments;
    procedure UpdateBody;
    procedure UpdateHeader;
    procedure LoadAttachments;
    procedure LoadBody;
    procedure LoadHeader;
    procedure UnloadAttachments;
    procedure UnloadBody;
    procedure UnloadHeader;
  private
    fMsgPath    : string;
    fHeaders    : TStringList;
    fBody       : TStringList;
    fAttachs    : TStringList;
    fCurrAttach : integer;
  public
    procedure  Initialize; override;
    destructor Destroy; override;
  end;

implementation

  uses
    SysUtils, ComServ, MailUtils, FileCtrl, DirIterator, Attachments,
    MailData, MailConsts;

  function TFiveMessage.Get_AttachmentCount: Integer;
    begin
      if fAttachs <> nil
        then
          try
            result := fAttachs.Count;
          except
            result := 0;
          end
        else result := 0;
    end;

  function TFiveMessage.Get_CurrentAttachment: Integer;
    begin
      result := fCurrAttach;
    end;

  procedure TFiveMessage.Set_CurrentAttachment(Value: Integer);
    begin
      try
        UpdateAttachments;
        if Value > Get_AttachmentCount
          then fCurrAttach := Value;
      except
      end;
    end;

  function TFiveMessage.SetMessage(const aWorld, aAccount, aFolder, aMessage: WideString): WordBool;
    begin
      try
        fMsgPath := GetAccountPath(aWorld, aAccount) + aFolder + '\' + aMessage + '\';
        result   := DirectoryExists(fMsgPath);
      except
        result := false;
      end;
    end;

  function TFiveMessage.Get_Lines(index: Integer): WideString;
    begin
      try
        UpdateBody;
        if index < fBody.Count
          then result := fBody[index]
          else result := '';
      except
        result := '';
      end;
    end;

  function TFiveMessage.Get_Attachment(const prop: WideString): WideString;
    begin
      try
        UpdateAttachments;
        if (fAttachs <> nil) and (fCurrAttach < fAttachs.Count)
          then result := TStringList(fAttachs[fCurrAttach]).Values[prop]
          else result := '';
      except
        result := '';
      end;
    end;

  function TFiveMessage.Get_Header(const name: WideString): WideString;
    begin
      try
        UpdateHeader;
        result := fHeaders.Values[name];
      except
        result := '';
      end;
    end;

  function TFiveMessage.Delete;
    begin
      try
        result := RemoveFullPath(fMsgPath);
      except
        result := false;
      end;
    end;

  procedure TFiveMessage.LoadAttachments;
    var
      Iterator : TFolderIterator;
    begin
      Iterator := TFolderIterator.Create(fMsgPath, tidAttchment_Mask, faDirectory);
      try
        fAttachs.Free;
        fAttachs := TStringList.Create;
        if not Iterator.Empty
          then
            begin
              repeat
                fAttachs.AddObject(Iterator.Current, TAttachment.Load(Iterator.FullPath));
              until not Iterator.Next;
            end;
      finally
        Iterator.Free;
      end;
    end;

  procedure TFiveMessage.LoadBody;
    begin
      fBody.Free;
      fBody := TStringList.Create;
      fBody.LoadFromFile(fMsgPath + tidMessage_Body);
    end;

  procedure TFiveMessage.LoadHeader;
    var
      path : string;
    begin
      path := fMsgPath + tidMessage_Header;
      fHeaders.Free;
      fHeaders := TStringList.Create;
      fHeaders.LoadFromFile(path);
      if fHeaders.Count > 0
        then
          begin
            fHeaders.Values[tidRead] := '1';
            fHeaders.SaveToFile(path);
          end;
    end;

  procedure TFiveMessage.UpdateAttachments;
    begin
      if fAttachs = nil
        then LoadAttachments;
    end;

  procedure TFiveMessage.UpdateBody;
    begin
      if fBody = nil
        then LoadBody;
    end;

  procedure TFiveMessage.UpdateHeader;
    begin
      if fHeaders = nil
        then LoadHeader;
    end;

  procedure TFiveMessage.UnloadAttachments;
    var
      i : integer;
    begin
      if fAttachs <> nil
        then
          begin
            for i := 0 to pred(fAttachs.Count) do
              TObject(fAttachs.Objects[i]).Free;
            fAttachs.Free;
            fAttachs := nil;
          end;
    end;

  procedure TFiveMessage.UnloadBody;
    begin
      fBody.Free;
      fBody := nil;
    end;

  procedure TFiveMessage.UnloadHeader;
    begin
      fHeaders.Free;
      fHeaders := nil;
    end;

  procedure TFiveMessage.Initialize;
    begin
      inherited;
      fCurrAttach := 0;
    end;

  destructor TFiveMessage.Destroy;
    begin
      UnloadHeader;
      UnloadBody;
      UnloadAttachments;
      inherited;
    end;

  function TFiveMessage.Get_LineCount: Integer;
    begin
      try
        UpdateBody;
        result := fBody.Count;
      except
        result := 0;
      end;
    end;

initialization          

  TAutoObjectFactory.Create(ComServer, TFiveMessage, Class_FiveMessage, ciMultiInstance, tmApartment);

end.
