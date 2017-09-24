unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ImgList, Menus;

type
  TCCSubscriberInfo = class;

  TMainForm = class(TForm)
    pctrlMain: TPageControl;
    tabMain: TTabSheet;
    TabSheet2: TTabSheet;
    lvUsers: TListView;
    Label1: TLabel;
    eServer: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    ePort: TEdit;
    ImageList1: TImageList;
    eSearchPath: TEdit;
    Label5: TLabel;
    eBillInfo: TEdit;
    Label7: TLabel;
    eAmount: TEdit;
    Label6: TLabel;
    TabSheet1: TTabSheet;
    reLog: TRichEdit;
    mCustUsers: TMemo;
    Label2: TLabel;
    btnClear: TButton;
    LogMenu: TPopupMenu;
    Clear1: TMenuItem;
    Savetofile1: TMenuItem;
    LogSaveDialog: TSaveDialog;
    SubsMenu: TPopupMenu;
    SelectExceptions1: TMenuItem;
    UnsubscribeSelected1: TMenuItem;
    eConfFilePath: TEdit;
    Label10: TLabel;
    cbProgId: TComboBox;
    Label11: TLabel;
    TabSheet3: TTabSheet;
    Label12: TLabel;
    eTotalCol: TEdit;
    btnRefresh: TButton;
    btnCollect: TButton;
    Label8: TLabel;
    Image1: TImage;
    Label9: TLabel;
    Image2: TImage;
    ePassword: TEdit;
    Label13: TLabel;
    eCertificade: TEdit;
    Label14: TLabel;
    tabTransfers: TTabSheet;
    Label15: TLabel;
    lvTransfSubs: TListView;
    btnRefreshTransfers: TButton;
    btnCheckTransf: TButton;
    btnReloadKeys: TButton;
    eCertPath: TEdit;
    Label16: TLabel;
    pbCCRefresh: TProgressBar;
    eMailerName: TEdit;
    eMailer: TEdit;
    eMailerHost: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    eMessageFile: TEdit;
    Label20: TLabel;
    cbDebugMode: TCheckBox;
    eAccStatus: TButton;
    eChargeDate: TEdit;
    Label21: TLabel;
    btnFindCC: TButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCollectClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Savetofile1Click(Sender: TObject);
    procedure SelectExceptions1Click(Sender: TObject);
    procedure UnsubscribeSelected1Click(Sender: TObject);
    procedure btnRefreshTransfersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnReloadKeysClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure eAccStatusClick(Sender: TObject);
    procedure btnFindCCClick(Sender: TObject);
  private
    function  GetProxy(var Proxy : OleVariant) : boolean;
    procedure RenderCCItem(InfoObj : TObject);
    function  GetCCSubscribedList : TStringList;
    function  GetTransfSubscribedList : TStringList;
    procedure Log(text : string);
    function  DecodeXmlResponse(xml_str : string; var pnref, resp_code, resp_msg : string) : boolean;
    function  PerformCharge(cc_no, cc_exp : string; street_addr, zip, comm1, comm2, inv_no : string) : string;
    function  Decrypt(cc_crypt : string) : string;
    function  GetInvNo(subid : integer) : string;
    function  ChargePlayer(Info : TCCSubscriberInfo; amt : currency; var inv_no, pnref, resp_code, resp_msg  : string) : boolean;
    procedure LoadKeys;
    procedure SaveKeys;
    procedure UpdateKeys;
    function  AddKey(id : integer; cphKey : string) : string;
    function  GetKey(id : integer) : string;
    function  DecypherKey(key : widestring) : string;
    function  SucceedCharge(resp_code : string) : integer;
    procedure NotifyCCUser(Info : TCCSubscriberInfo; resp_code, resp_msg : string);
  private
    fAmount : currency;
    fKeys : TStringList;
    fChargeCount : integer;
    fRefreshing : boolean;
  end;

  TCCSubscriberInfo =
    class
      private
        fName     : string;
        fAcStatus : integer;
        fTrialExp : TDateTime;
        fChrgDate : TDateTime;
        fcc_crypt : string;
        fcc_disp  : string;
        fcc_exp   : string;
        fStr_Addr : string;
        fZip      : string;
        fEmail    : string;
        {fComment1 : string;
        fComment2 : string;
        fInv_no   : string;}
        fSubID    : integer;
        fLastResp : string;
    end;

  TTransfSubscriberInfo =
    class
      private
        fName     : string;
        fAcStatus : integer;
        fTrialExp : TDateTime;
        fChrgDate : TDateTime;
        fSubID    : integer;
        fType     : string;
        fInfo     : string;
        fPeriod   : integer;
        fVerified : string;
    end;

  TKeyEntry =
    class
      public
        constructor Create(id : integer; cypherKey, plainText : string);
      public
        fId        : integer;
        fCypherKey : string;
        fPlainText : string;
    end;

var
  MainForm: TMainForm;

implementation

  uses
    RDOInterfaces, RDOObjectProxy, WinSockRDOConnection, ComObj, rc4,
    OTK_TLB, AsymSoftKeys, ASPENCRYPTLib_TLB, Registry;

{$R *.DFM}

  const
    DIR_NOERROR_StillTrial         = -1;
    DIR_NOERROR                    = 0;
    DIR_ERROR_Unknown              = 1;
    DIR_ERROR_AccountAlreadyExists = 2;
    DIR_ERROR_UnexistingAccount    = 3;
    DIR_ERROR_SerialMaxed          = 4;
    DIR_ERROR_InvalidSerial        = 5;
    DIR_ERROR_InvalidAlias         = 6;
    DIR_ERROR_InvalidPassword      = 7;
    DIR_ERROR_AccountBlocked       = 8;
    DIR_ERROR_TrialExpired         = 9;
    DIR_ERROR_SubscriberIdNotFound = 10;

  const
    USE_DBG_SERVER = true;

  const
    CHRG_OK        = 0;
    CHRG_USER_FAIL = 1;
    CHRG_SYS_ERROR = -1;

  function GetAliasId( Alias : string ) : string;
    var
      i : integer;
    begin
      result := trim(Alias);
      for i := 1 to Length( result ) do
        if result[i] = ' '
          then result[i] := '.'
          else result[i] := UpCase( result[i] );
    end;

  function GetUserPath( Alias : string ) : string;
    var
      aID : string;
    begin
      aID    := GetAliasID(Alias);
      result := 'root/users/' + aID[1] + '/' + aID
    end;

  function DecorateText(text : string) : string;
    var
      txtLen : integer;
      decLen : integer;
      i      : integer;
    begin
      result := text;
      txtLen := length(text);
      decLen := length(DecChars);
      for i := 1 to txtLen do
        insert(DecChars[1 + random(decLen)], result, random(1 + length(result)));
    end;

  function UndecorateText(text : string) : string;
    var
      i, j, len : integer;
      c : char;
    begin
      len := length(text);
      SetLength(result, len);
      j   := 0;
      for i := 1 to len do
        begin
          c := text[i];
          if pos(c, DecChars) = 0
            then
              begin
                inc(j);
                result[j] := c;
              end;
        end;
      SetLength(result, j);
    end;


  // TKeyEntry

  constructor TKeyEntry.Create(id : integer; cypherKey, plainText : string);
    begin
      inherited Create;
      fId := id;
      fCypherKey := cypherKey;
      fPlainText := plainText;
    end;


  // TMainForm

  function TMainForm.DecodeXmlResponse(xml_str : string; var pnref, resp_code, resp_msg : string) : boolean;
    var
      xmldom, resp_node : olevariant;
    begin
      try
        xmldom := CreateOleObject('MSXML.DOMDocument');
        xmldom.async := false;
        xmldom.loadXML(xml_str);
        resp_node := xmldom.documentElement.selectSingleNode('/tx/response/decoded');
        if not (VarIsNull(resp_node) or VarIsEmpty(resp_node))
          then
            begin
              pnref     := resp_node.selectSingleNode('pnref').text;
              resp_code := resp_node.selectSingleNode('res-code').text;
              resp_msg  := resp_node.selectSingleNode('res-msg').text;
              result    := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TMainForm.PerformCharge(cc_no, cc_exp : string; street_addr, zip, comm1, comm2, inv_no : string) : string;
    var
      pfObj   : OleVariant;
      cfgFile : string;
      pfPrgId : string;
      debug   : WordBool;
    begin
      cfgFile := ExtractFilePath(ParamStr(0)) + eConfFilePath.Text;
      pfPrgId := cbProgId.Text;
      debug   := cbDebugMode.Checked;
      try
        pfObj := CreateOleObject(pfPrgId);
        pfObj.OpenSession(cfgFile, debug);
        result := pfObj.SaleCC(cc_no, cc_exp, fAmount, street_addr, zip, comm1, comm2, inv_no);
      except
        result := '';
      end;
    end;

  function TMainForm.Decrypt(cc_crypt : string) : string;
    var
      RC4   : TRC4;
      ccBin : string;
      aux   : string;
      p     : integer;
      kid   : integer;
      key   : string;
    begin
      RC4 := TRC4.Create;
      try
        ccBin := RC4.toBin(cc_crypt);
        p := pos('|', ccBin);
        if p <> 0
          then
            begin
              aux := copy(ccBin, 1, p - 1);
              kid := StrToInt(aux);
              key := GetKey(kid);
              if key <> ''
                then
                  begin
                    aux := copy(ccBin, succ(p), Length(ccBin) - p);
                    RC4.Key := key;
                    result := UndecorateText(RC4.Apply(aux));
                  end
                else result := ''
            end
          else result := '';
      finally
        RC4.Free;
      end;
    end;

  function TMainForm.GetInvNo(subid : integer) : string;
    var
      y, m, d : word;
    begin
      DecodeDate(Now, y, m, d);
      result := IntToStr(subid) + '/' + IntToStr(y) + IntToStr(m) + IntToStr(d);
    end;

  function TMainForm.ChargePlayer(Info : TCCSubscriberInfo; amt : currency; var inv_no, pnref, resp_code, resp_msg  : string) : boolean;
    var
      xml_resp : string;
    begin
      try
        inv_no := GetInvNo(Info.fSubID);
        xml_resp :=
          PerformCharge(
            Decrypt(Info.fcc_crypt),
            Info.fcc_exp,
            Info.fStr_Addr,
            Info.fZip,
            'Star Peace Subscription',
            inv_no,
            inv_no);
        if xml_resp <> ''
          then result := DecodeXmlResponse(xml_resp, pnref, resp_code, resp_msg)
          else result := false;
      except
        result := false;
      end;
    end;

  procedure TMainForm.RenderCCItem(InfoObj : TObject);
    var
      Info : TCCSubscriberInfo;
    begin
      Info := TCCSubscriberInfo(InfoObj);
      with lvUsers.Items.Add do
        begin
          Data := Info;
          Caption := Info.fName;
          SubItems.Add(IntToStr(Info.fSubID)); // SubsID
          SubItems.Add(DateToStr(Info.fChrgDate));
          SubItems.Add(Info.fcc_disp);
          SubItems.Add(Info.fcc_exp);
          SubItems.Add(Info.fStr_Addr);
          SubItems.Add(Info.fZip);
          SubItems.Add(Info.fLastResp);
          ImageIndex := Info.fAcStatus;
          if Info.fAcStatus = 0
            then inc(fChargeCount);
        end;
    end;

  procedure TMainForm.btnRefreshClick(Sender: TObject);
    var
      List  : TStringList;
      //i     : integer;
      //Info  : TCCSubscriberInfo;
      rev   : currency;
    begin
      if not fRefreshing
        then
          begin
            btnRefresh.Caption := 'Stop';
            fRefreshing := true;
            Cursor := crHourGlass;
            lvUsers.Cursor := crHourGlass;
            try
              // retieve all the new keys from the Server..
              //UpdateKeys;
              fChargeCount := 0;

              lvUsers.Items.BeginUpdate;
              try
                lvUsers.Items.Clear;
              finally
                lvUsers.Items.EndUpdate;
              end;

              pctrlMain.ActivePage := tabMain;

              List := TStringList.Create;
              try
                List := GetCCSubscribedList;
                {for i := 0 to pred(List.Count) do
                  begin
                    with lvUsers.Items.Add do
                      begin
                        Info := TCCSubscriberInfo(List.Objects[i]);
                        Data := Info;
                        if Info <> nil
                          then
                            begin
                              Caption := Info.fName;
                              SubItems.Add(IntToStr(Info.fSubID)); // SubsID
                              SubItems.Add(DateToStr(Info.fChrgDate));
                              SubItems.Add(Info.fcc_disp);
                              SubItems.Add(Info.fcc_exp);
                              SubItems.Add(Info.fStr_Addr);
                              SubItems.Add(Info.fZip);
                              SubItems.Add(Info.fLastResp);
                              ImageIndex := Info.fAcStatus;
                            end
                          else
                            begin
                              Caption := List[i];
                              ImageIndex := 1;
                            end;
                      end;
                  end;}
              finally
                List.Free;
              end;
            finally
              Cursor := crDefault;
              lvUsers.Cursor := crDefault;
            end;
            btnCollect.Enabled := lvUsers.Items.Count > 0;
            rev := fChargeCount*StrToFloat(eAmount.Text);
            ShowMessage(Format('Total to collect: %d' + ^M^J + 'Revenue: %f', [fChargeCount, rev]));
            btnRefresh.Caption := 'Refresh';
          end
        else
          begin
            fRefreshing := false;
            btnRefresh.Caption := 'Refresh';
          end;
    end;

  function TMainForm.GetProxy(var Proxy : OleVariant) : boolean;
    var
      DSCnnt  : IRDOConnectionInit;
      DSProxy : OleVariant;
      sId     : integer;
    begin
      Proxy  := Unassigned;
      DSCnnt := TWinSockRDOConnection.Create('Main');
      DSCnnt.Server := eServer.Text;
      DSCnnt.Port := StrToInt(ePort.Text);
      if DSCnnt.Connect(20*1000)
        then
          begin
            DSProxy := TRDOObjectProxy.Create as IDispatch;
            DSProxy.SetConnection(DSCnnt);
            DSProxy.TimeOut := 20*1000;
            if DSProxy.BindTo('DirectoryServer')
              then
                begin
                  sId := DSProxy.RDOOpenSession;
                  if (sId <> 0) and DSProxy.BindTo(sId)
                    then
                      begin
                        result := true;
                        Proxy  := DSProxy;
                      end
                    else result := false;
                end
              else result := false;
          end
        else result := false;
    end;

  function TMainForm.GetCCSubscribedList : TStringList;
    var
      Proxy   : OleVariant;
      alias   : string;
      key     : string;
      i       : integer;
      Info    : TCCSubscriberInfo;
      billPth : string;
      today   : TDateTime;
      custUsr : string;
      subType : string;
    begin
      billPth := eBillInfo.Text;
      if eChargeDate.Text = ''
        then today := Now
        else today := StrToDate(eChargeDate.Text);
      if GetProxy(Proxy)
        then
          begin
            if mCustUsers.Lines.Count > 0
              then custUsr := mCustUsers.Lines.Text
              else custUsr := '';
            if (custUsr <> '') or Proxy.RDOSetCurrentKey(eSearchPath.Text)
              then
                begin
                  result := TStringList.Create;
                  if custUsr <> ''
                    then result.Text := custUsr
                    else result.Text := Proxy.RDOGetKeyNames;
                  pbCCRefresh.Max := result.Count;
                  i := 0;
                  while (i < result.Count) and fRefreshing do
                    begin
                      pbCCRefresh.Position := i;
                      Application.ProcessMessages;
                      alias := result[i];
                      key   := GetUserPath(alias);
                      if Proxy.RDOSetCurrentKey(key)
                        then
                          begin
                            Info  := TCCSubscriberInfo.Create;
                            Info.fAcStatus := Proxy.RDOReadInteger('accountstatus');
                            Info.fName     := alias;
                            Info.fTrialExp := Proxy.RDOReadDate('trialexpires');
                            Info.fChrgDate := Proxy.RDOReadDate('chargedate');
                            if Info.fChrgDate < Info.fTrialExp
                              then Info.fChrgDate := Info.fTrialExp;
                            // change to billing info folder
                            key := key + billPth;
                            if (Info.fChrgDate <= today) and (Info.fChrgDate >= Info.fTrialExp) and Proxy.RDOSetCurrentKey(key)
                              then
                                try
                                  subType := Proxy.RDOReadString('type');
                                  if lowerCase(subType) = 'cc'
                                    then
                                      begin
                                        Info.fSubID := Proxy.RDOReadInteger('subscriber_id');
                                        Info.fcc_crypt := Proxy.RDOReadString('cc_no_crypt');
                                        Info.fcc_disp := Proxy.RDOReadString('cc_no_disp');
                                        Info.fcc_exp := Proxy.RDOReadString('cc_exp');
                                        Info.fStr_Addr := Proxy.RDOReadString('street_addr');
                                        Info.fZip := Proxy.RDOReadString('zip');
                                        Info.fEmail := Proxy.RDOReadString('email');
                                        Info.fLastResp := Proxy.RDOReadString('response_msg');
                                        result.Objects[i] := Info;
                                        RenderCCItem(Info);
                                      end
                                    else Info.Free;
                                except
                                  Log('Error reading info: ' + Info.fName);
                                end
                              else Info.Free;
                          end;
                      inc(i);
                    end;
                  pbCCRefresh.Position := pbCCRefresh.Max;
                  // Eliminate
                  for i := pred(Result.Count) downto 0 do
                    if result.Objects[i] = nil
                      then result.Delete(i);
                  Proxy := Unassigned;
                end
              else result := nil;
          end
        else result := nil;
    end;

  procedure TMainForm.btnCollectClick(Sender: TObject);
    var
      Proxy     : OleVariant;
      i         : integer;
      Info      : TCCSubscriberInfo;
      inv_no    : string;
      pnref     : string;
      resp_code : string;
      resp_msg  : string;
      sum       : currency;
    begin
      if ePassword.Text <> ''
        then
          begin
            sum := 0;
            btnCollect.Enabled := false;
            btnRefresh.Enabled := false;
            Cursor := crHourGlass;
            lvUsers.Cursor := crHourGlass;
            try
              fAmount := StrToCurr(eAmount.Text);
              if GetProxy(Proxy)
                then
                  begin
                    pbCCRefresh.Max := lvUsers.Items.Count;
                    for i := 0 to pred(lvUsers.Items.Count) do
                      begin
                        pbCCRefresh.Position := i;
                        Info := TCCSubscriberInfo(lvUsers.Items[i].Data);
                        if (Info <> nil) and (Info.fAcStatus = 0)
                          then
                            begin
                              Application.ProcessMessages;
                              if ChargePlayer(Info, fAmount, inv_no, pnref, resp_code, resp_msg)
                                then
                                  begin
                                    case SucceedCharge(resp_code) of
                                      CHRG_OK :
                                        begin
                                          if Proxy.RDONotifyCharge(IntToStr(Info.fSubID), pnref, resp_code, resp_msg) <> DIR_NOERROR
                                            then Log('Notify Charge Error: ' + Info.fName);
                                          sum := sum + fAmount;
                                        end;
                                      CHRG_USER_FAIL :
                                        begin
                                          if Proxy.RDONotifyCharge(IntToStr(Info.fSubID), pnref, resp_code, resp_msg) <> DIR_NOERROR
                                            then Log('Notify Failure Error: ' + Info.fName)
                                            else Log('Notify Failure OK: ' + Info.fName);
                                          NotifyCCUser(Info, resp_code, resp_msg);
                                        end;
                                      CHRG_SYS_ERROR :
                                        Log('Investigate Invoice: ' + inv_no);
                                    end;
                                  end
                                else Log('Investigate Invoice: ' + inv_no);
                              if lvUsers.Items[i].SubItems.Count >= 6
                                then lvUsers.Items[i].SubItems[6] := resp_msg;
                            end;
                      end;
                  end;
            finally
              Cursor := crDefault;
              lvUsers.Cursor := crDefault;
              btnRefresh.Enabled := true;
              eTotalCol.Text := '$' + CurrToStr(sum);
            end;
          end
        else ShowMessage('Cannot collect without the password, go "Settings" and enter the password');
    end;

  function TMainForm.GetTransfSubscribedList : TStringList;
    var
      Proxy   : OleVariant;
      alias   : string;
      key     : string;
      i       : integer;
      Info    : TTransfSubscriberInfo;
      billPth : string;
      custUsr : string;
      subType : string;
    begin
      billPth := eBillInfo.Text;
      if GetProxy(Proxy)
        then
          begin
            if mCustUsers.Lines.Count > 0
              then custUsr := mCustUsers.Lines.Text
              else custUsr := '';
            if (custUsr <> '') or Proxy.RDOSetCurrentKey(eSearchPath.Text)
              then
                begin
                  result := TStringList.Create;
                  if custUsr <> ''
                    then result.Text := custUsr
                    else result.Text := Proxy.RDOGetKeyNames;
                  for i := 0 to pred(result.Count) do
                    begin
                      Application.ProcessMessages;
                      alias := result[i];
                      key   := GetUserPath(alias);
                      if Proxy.RDOSetCurrentKey(key)
                        then
                          begin
                            Info  := TTransfSubscriberInfo.Create;
                            Info.fAcStatus := Proxy.RDOReadInteger('accountstatus');
                            Info.fName     := alias;
                            Info.fTrialExp := Proxy.RDOReadDate('trialexpires');
                            Info.fChrgDate := Proxy.RDOReadDate('chargedate');
                            if Info.fChrgDate < Info.fTrialExp
                              then Info.fChrgDate := Info.fTrialExp;
                            // change to billing info folder
                            key := key + billPth;
                            if Proxy.RDOSetCurrentKey(key)
                              then
                                try
                                  subType := Proxy.RDOReadString('type');
                                  if lowerCase(subType) <> 'cc'
                                    then
                                      begin
                                        Info.fSubID := Proxy.RDOReadInteger('subscriber_id');
                                        Info.fInfo := Proxy.RDOReadString('info');
                                        Info.fType := Proxy.RDOReadString('type');
                                        Info.fPeriod := Proxy.RDOReadInteger('period');
                                        Info.fVerified := Proxy.RDOReadString('verified');
                                        result.Objects[i] := Info;
                                      end
                                    else Info.Free;
                                except
                                  Log('Error reading info: ' + Info.fName);
                                end
                              else Info.Free;
                          end;
                    end;
                  // Eliminate
                  for i := pred(Result.Count) downto 0 do
                    if result.Objects[i] = nil
                      then result.Delete(i);
                  Proxy := Unassigned;
                end
              else result := nil;
          end
        else result := nil;
    end;

  procedure TMainForm.Log(text : string);
    begin
      reLog.Lines.Add(text);
    end;

  procedure TMainForm.btnClearClick(Sender: TObject);
    begin
      mCustUsers.Clear;
    end;

  procedure TMainForm.Clear1Click(Sender: TObject);
    begin
      reLog.Lines.Clear;
    end;

  procedure TMainForm.Savetofile1Click(Sender: TObject);
    begin
      if LogSaveDialog.Execute
        then
          begin
            reLog.PlainText := true;
            reLog.Lines.SaveToFile(LogSaveDialog.FileName);
          end;
    end;

  procedure TMainForm.SelectExceptions1Click(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(lvUsers.Items.Count) do
        if lvUsers.Items[i].ImageIndex <> 0
          then lvUsers.Items[i].Selected := true;
    end;

  procedure TMainForm.UnsubscribeSelected1Click(Sender: TObject);
    var
      Proxy : OleVariant;
      Info  : TCCSubscriberInfo;
      i     : integer;
      res   : variant;
    begin
      if GetProxy(Proxy)
        then
          begin
            for i := 0 to pred(lvUsers.Items.Count) do
              if lvUsers.Items[i].Selected
                then
                  begin
                    Info := TCCSubscriberInfo(lvUsers.Items[i].Data);
                    if Info <> nil
                      then
                        begin
                          res := Proxy.RDOUnsubscribe(Info.fName, IntToStr(Info.fSubID));
                          Log('Unsubscribed: ' + Info.fName + ' result: ' + IntToStr(res));
                        end
                      else
                        if lvUsers.Items[i].Caption <> ''
                          then
                            begin
                              res := Proxy.RDODeleteFullPathNode('root/paying/' + lvUsers.Items[i].Caption);
                              Log('Unsubscribed: ' + lvUsers.Items[i].Caption + ' result: ' + IntToStr(res));
                            end;
                  end;
          end
    end;

  procedure TMainForm.btnRefreshTransfersClick(Sender: TObject);
    var
      List : TStringList;
      i    : integer;
      Info : TTransfSubscriberInfo;
    begin
      Cursor := crHourGlass;
      lvTransfSubs.Cursor := crHourGlass;
      try
        lvTransfSubs.Items.BeginUpdate;
        try
          lvTransfSubs.Items.Clear;
        finally
          lvTransfSubs.Items.EndUpdate;
        end;

        pctrlMain.ActivePage := tabTransfers;
        List := GetTransfSubscribedList;
        try
          for i := 0 to pred(List.Count) do
            begin
              with lvTransfSubs.Items.Add do
                begin
                  Info := TTransfSubscriberInfo(List.Objects[i]);
                  Data := Info;
                  if Info <> nil
                    then
                      begin
                        Caption := Info.fName;
                        SubItems.Add(IntToStr(Info.fSubID)); // SubsID
                        SubItems.Add(DateToStr(Info.fChrgDate));
                        SubItems.Add(Info.fType);
                        SubItems.Add(Info.fInfo);
                        SubItems.Add(IntToStr(Info.fPeriod));
                        SubItems.Add(Info.fVerified);
                        ImageIndex := Info.fAcStatus;
                      end
                    else
                      begin
                        Caption := List[i];
                        ImageIndex := 1;
                      end;
                end;
            end;
        finally
          List.Free;
        end;
      finally
        Cursor := crDefault;
        lvTransfSubs.Cursor := crDefault;
      end;
      btnCollect.Enabled := lvTransfSubs.Items.Count > 0;
    end;

  procedure TMainForm.FormCreate(Sender: TObject);
    begin
      fKeys := TStringList.Create;
      LoadKeys;
    end;

  procedure TMainForm.LoadKeys;
    var
      path   : string;
      Stream : TStream;
      i      : integer;
      cnt    : integer;
      id     : integer;
      len    : integer;
      cphKey : string;
      plnKey : string;
      KeyEnt : TKeyEntry;
    begin
      path := ExtractFilePath(ParamStr(0)) + 'gwkeys.dat';
      if FileExists(path)
        then
          begin
            Stream := TFileStream.Create(path, fmOpenRead);
            try
              Stream.Read(cnt, sizeof(cnt));
              for i := 0 to pred(cnt) do
                begin
                  Stream.Read(id, sizeof(id));
                  Stream.Read(len, sizeof(len));
                  SetLength(cphkey, len);
                  if len > 0
                    then Stream.Read(cphkey[1], len)
                    else cphkey := '';
                  try
                    plnKey := DecypherKey(cphKey);
                    KeyEnt := TKeyEntry.Create(id, cphKey, plnKey);
                    fKeys.AddObject(IntToStr(id), KeyEnt);
                    //Log('Loaded: ' + plnKey);
                  except
                    Log('Error: Decyphering key id: ' + IntToStr(id));
                  end;
                end;
            finally
              Stream.Free;
            end;
          end;
    end;

  procedure TMainForm.SaveKeys;
    var
      path   : string;
      Stream : TStream;
      i, cnt : integer;
      len    : integer;
      KeyEnt : TKeyEntry;
    begin
      path := ExtractFilePath(ParamStr(0)) + 'gwkeys.dat';
      Stream := TFileStream.Create(path, fmCreate);
      try
        cnt := fKeys.Count;
        Stream.Write(cnt, sizeof(cnt));
        for i := 0 to pred(fKeys.Count) do
          begin
            KeyEnt := TKeyEntry(fKeys.Objects[i]);
            if KeyEnt <> nil
              then
                begin
                  Stream.Write(KeyEnt.fId, sizeof(KeyEnt.fId));
                  len := length(KeyEnt.fCypherKey);
                  Stream.Write(len, sizeof(len));
                  if len > 0
                    then Stream.Write(KeyEnt.fCypherKey[1], len);
                end;
          end;
      finally
        Stream.Free;
      end
    end;

  procedure TMainForm.UpdateKeys;
    var
      Proxy : OleVariant;
      cKeys : integer;
      i     : integer;
      key   : string;
    begin
      if GetProxy(Proxy)
        then
          begin
            cKeys := Proxy.RDOLastKey;
            if fKeys.Count <> cKeys
              then
                begin
                  fKeys.Clear;
                  for i := 0 to pred(cKeys) do
                    begin
                      key := Proxy.RDORetrieveKey(i);
                      Log('Retrieved: ' + AddKey(i, key));
                    end;
                end;
          end;
    end;

  function TMainForm.AddKey(id : integer; cphKey : string) : string;
    var
      KeyEnt   : TKeyEntry;
      plnKey   : string;
    begin
      if cphKey <> ''
        then
          try
            plnKey := DecypherKey(cphKey);
            KeyEnt := TKeyEntry.Create(id, cphKey, plnKey);
            fKeys.AddObject(IntToStr(id), KeyEnt);
            result := plnKey;
          except
            result := '';
            Log('Error: Decyphering key id: ' + IntToStr(id));
          end
        else
          begin
            result := '';
            Log('Error: Unknown key requested id: ' + IntToStr(id));
          end;
    end;

  function TMainForm.GetKey(id : integer) : string;
    var
      idx      : integer;
      Proxy    : OleVariant;
    begin
      idx := fKeys.IndexOf(IntToStr(id));
      if idx <> -1
        then result := TKeyEntry(fKeys.Objects[idx]).fPlainText
        else
          begin
            if Getproxy(Proxy)
              then
                try
                  result := AddKey(id, Proxy.RDORetrieveKey(id));
                except
                  result := '';
                  Log('Error: Retrieving key id: ' + IntToStr(id));
                end
              else
                begin
                  Log('Error: Unknown key requested id: ' + IntToStr(id));
                  result := '';
                end;
          end;
    end;

  function TMainForm.DecypherKey(key : widestring) : string;
    var
      CM      : OleVariant;
      cfStore : OleVariant;
      pvkCerts : OleVariant;
      pvkCert : OleVariant;
      Ctx     : OleVariant;
      Msg     : OleVariant;
      pvkPath : string;
      ctxName : widestring;
      pswd    : widestring;
      certfId : widestring;
    begin
      try
        pvkPath := ExtractFilePath(ParamStr(0)) + eCertPath.Text;
        CM := CreateOleObject('Persits.CryptoManager');
        CM.OpenContextEx('Microsoft Enhanced Cryptographic Provider v1.0', '', true);
        pswd := ePassword.Text;
        cfStore := CM.OpenStoreFromPFX(pvkPath, pswd);
        pvkCerts := cfStore.Certificates;
        certfId  := eCertificade.Text;
        pvkCert  := pvkCerts.Item(certfId);
        Ctx := pvkCert.PrivateKeyContext;
        Msg := Ctx.CreateMessage(true);
        ctxName := Ctx.ContainerName;
        result := Msg.DecryptText(key, ctxName);
      except
        result := '';
      end;
    end;

  function TMainForm.SucceedCharge(resp_code : string) : integer;
    var
      resp_val : integer;
    begin
      resp_val := StrToInt(resp_code);
      if resp_val = 0
        then result := CHRG_OK
        else
          if resp_val > 0
            then result := CHRG_USER_FAIL
            else result := CHRG_SYS_ERROR;
    end;

  procedure TMainForm.NotifyCCUser(Info : TCCSubscriberInfo; resp_code, resp_msg : string);
    var
      Msg  : OleVariant;
      Body : TStringList;
    begin
      if (Info <> nil) and (Info.fEmail <> '')
        then
          try
            Msg := CreateOleObject('Persits.MailSender');
            Msg.Host := eMailerHost.Text;
            Msg.From := eMailer.Text;
            Msg.FromName := eMailerName.Text;
            Msg.Subject := 'Star Peace Subscription';

            Body := TStringList.Create;
            try
              Body.LoadFromFile(ExtractFilePath(ParamStr(0)) + eMessageFile.Text);
              Msg.Body := Format(Body.Text, [Info.fName, resp_msg]);
              Msg.AddAddress(Info.fEmail, Info.fName);
              Msg.Send;
            finally
              Body.Free;
            end;
          except
            Log(Format('Error mailing response %s to [%s] %s', [resp_msg, Info.fName, Info.fEmail]));
          end
        else
          if Info <> nil
            then Log(Format('No email to notify %s', [Info.fName]));
    end;

  procedure TMainForm.btnReloadKeysClick(Sender: TObject);
    begin
      UpdateKeys;
    end;

  procedure TMainForm.FormDestroy(Sender: TObject);
    begin
      SaveKeys;
    end;

  procedure TMainForm.eAccStatusClick(Sender: TObject);
    var
      Proxy : OleVariant;
      i     : integer;
      key   : string;
      Info  : TCCSubscriberInfo;
      res   : boolean;
    begin
      try
        fAmount := StrToCurr(eAmount.Text);
        if GetProxy(Proxy)
          then
            begin
              pbCCRefresh.Max := lvUsers.Items.Count;
              for i := 0 to pred(lvUsers.Items.Count) do
                begin
                  pbCCRefresh.Position := i;
                  Info := TCCSubscriberInfo(lvUsers.Items[i].Data);
                  if (Info <> nil) and (Info.fAcStatus = 1)// and (LowerCase(Info.fLastResp) <> 'subscribed')
                    then
                      begin
                        key := 'root/paying/' + Info.fName;
                        res := Proxy.RDODeleteFullPathNode(key);
                      end;
                end;
            end;
      except
        ShowMessage('Unknown Error Fixing AccountStatus.');
      end;
    end;

  procedure TMainForm.btnFindCCClick(Sender: TObject);
    var
      CC    : string;
      Proxy : OleVariant;
      List  : TStringList;
      i     : integer;
      key   : string;
      crypt : string;
    begin
      CC := InputBox('Enter Number', 'Card:', '');
      if (CC <> '') and GetProxy(Proxy) and Proxy.RDOSetCurrentKey(eSearchPath.Text)
        then
          begin
            List := TStringList.Create;
            List.Text := Proxy.RDOGetKeyNames;
            for i := 0 to pred(List.Count) do
              begin
                key := GetUserPath(List[i]);
                if Proxy.RDOSetCurrentKey(key + '/subscription')
                  then
                    begin
                      crypt := Proxy.RDOReadString('cc_no_crypt');
                      if Decrypt(crypt) = CC
                        then Log('Found: ' + List[i]);
                      Application.ProcessMessages;
                    end;
              end;
          end;
    end;

end.
