unit ChatListHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Protocol, VisualControls, TiledPanel,
  FramedButton, GradientBox, VoyagerServerInterfaces, VoyagerInterfaces,
  ChatHandler, InternationalizerComponent, Privacy, ServerCnxEvents,
  ImgList, EnhanceComponents;

type
  PIconInfo = ^TIconInfo;
  TIconInfo =
    record
      Mask: word;
      Icon: word;
      CancelIcon: array[0..5] of word;
      StrFormat : string;
    end;

  PNobility = ^TNobility;
  TNobility =
    record
      Nobility : word;
      Icon  : array[0..2] of integer;
    end;

  TIcomChat =
    record
      ic_Write      : integer;
      ic_Writing    : integer;
      ic_Void       : integer;
      ic_IgnoreUser : integer;
      ic_AFK        : integer;
    end;

  TChatListHandlerView =
    class(TVisualControl)
        Panel2: TPanel;
        ImageList: TImageList;
        Timer: TTimer;
        Panel3: TPanel;
        GradientBox1: TGradientBox;
        GradientBox2: TGradientBox;
        Label1: TLabel;
        IgnoreUser: TFramedButton;
        ChatOverMap: TFramedButton;
        FollowUser: TFramedButton;
        Image1: TImage;
        VoiceChat: TFramedButton;
        InternationalizerComponent1: TInternationalizerComponent;
        LogBtn: TFramedButton;
        UserList: TMerchiseListBox;
        procedure TimerTimer(Sender: TObject);
        procedure FollowUserClick(Sender: TObject);
        procedure Image1Click(Sender: TObject);
        procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure VoiceChatClick(Sender: TObject);
        procedure IgnoreUserClick(Sender: TObject);
        procedure LogBtnClick(Sender: TObject);
        function  UserListImagenCache(const sender: TObject; const index: Integer): TGraphic;
        procedure UserListClick(Sender: TObject);
      public
        procedure AddUser(const UserName : string );
        procedure DelUser( UserName : string );
        function  FindIndex(const UserName: string): integer;
        procedure ChatMsg( Msg : TChatMsgInfo );
        procedure UserHasSpoken(const UserName : string );
        procedure MsgCompostionChanged( UserName : string; State : TMsgCompositionState );
        procedure LoadIconInfo(const list: TStringlist);
        function  DecodeCodeMSGChat(const MSGChat: string): string;
      private
        fClientView       : IClientView;
        fMasterURLHandler : IMasterURLHandler;
        fPrivacyHandler   : IPrivacyHandler;
        fChatLog          : TStringList;
        fIconList         : TStringList;
        fIconInfo         : array of TIconInfo;
        fNobilityList     : array of TNobility;
        fIcomChat         : TIcomChat;
      public
        destructor Destroy; override;
      public
        property ClientView       : IClientView       write fClientView;
        property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
        property PrivacyHandler   : IPrivacyHandler   write fPrivacyHandler;
        property ChatLog          : TStringList       read  fChatLog;
        property IconList         : TStringList       read  fIconList write fIconList;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;

implementation

  uses
    ChatListHandler, VoiceHandler, ChatLogWin, CommCtrl, CoolSB, mr_StrUtils;

  const
    CImageBack = 3;
  {$R *.DFM}

  function  TChatListHandlerView.DecodeCodeMSGChat(const MSGChat: string): string;
    var
      i : integer;
      j : cardinal;
      k : integer;
      count : integer;
      user : string;
      UserFormat: string;
      Nobility : word;
      Modif    : word;
      b : boolean;
      Image : string;
      UserName : string;
      p, q: pchar;
      afk : boolean;
    begin
      result := MSGChat;
      p := pchar(MSGChat);
      while (p[0]<>#0) and ((p[0]='<') or (p[0]=' ')) do
        if p[0]='<'
          then
            begin
              while (p[0]<>'>') and (p[0]<>#0) do
                inc(p);
              if p[0]='>'
                then inc(p);
            end
          else inc(p);
      if p[0]<>#0
        then
          begin
            q := p;
            b := false;
            while (p[0]<>':') and (p[0]<>'<') and (p[0]<>#0) do
              begin
                b:= b or (p[0]='/');
                inc(p);
              end;
            if b
              then
                begin
                  UserName := copy(MSGChat, q - pchar(MSGChat)+1, p-q);
                  if ParseChatUser(UserName, user, j, afk)
                    then
                      begin
                        if (j>0) and (system.length(fIconInfo)>0)
                          then
                            begin
                              BreakAccDesc(j, Nobility, Modif);
                              i := 0;

                              count := system.length(fIconInfo);
                              while (i<count) and (Modif<>0) do
                                begin
                                  with fIconInfo[i] do
                                    if (Modif and Mask)>0
                                      then
                                        begin
                                          k := 0;
                                          Modif := Modif and not Mask;
                                          b := false;
                                          while (k<system.length(CancelIcon)) and (CancelIcon[k]<>0) and not b do
                                            begin
                                              b := (CancelIcon[k] and Modif) >0;
                                              inc(k);
                                            end;
                                          if not b
                                            then
                                              begin
                                                Image := Image+ '<i '+fIconList[Icon]+'>';
                                                UserFormat := StrFormat;
                                              end;

                                        end;
                                  inc(i);
                                end;
                              if (system.length(fNobilityList)>0)
                                then
                                  begin
                                    count := system.length(fNobilityList)-1;
                                    while (count>=0) and (Nobility<fNobilityList[count].Nobility) do
                                      dec(count);
                                    if count>=0
                                      then Image := Image+'<i '+ fIconList[fNobilityList[count].Icon[0]]+'>';
                                  end;
                            end;
                      end;
                      i := q - pchar(MSGChat)+1;
                      delete(result, i, p-q);
                      insert(Image+User, Result, i);
                      if (UserFormat<>'')
                        then
                          begin
                            i := system.pos(':', Result);
                            if (i>0)
                              then
                                begin
                                  inc(i);
                                  if (result[i]='<') or (result[i+1]='<')
                                    then
                                      begin
                                        repeat
                                          inc(i);
                                        until (result[i]='>') or (result[i]=#13);
                                        inc(i);
                                      end;
                                  insert(UserFormat, Result, i);
                                end;
                          end;
                    end;
          end;
    end;

  procedure TChatListHandlerView.AddUser(const UserName : string );
    var
      i : integer;
      j : Cardinal;
      k : integer;
      count : integer;
      user : string;
      Nobility : word;
      Modif    : word;
      b : boolean;
      Nobi : PNobility;
      afk : boolean;
    begin
      if ParseChatUser(UserName, user, j, afk)
        then
          begin
            with UserList.SyntaxWriter do
              begin
                UserList.SyntaxWriter.Clear;
                if (j>0) and (system.length(fIconInfo)>0)
                  then
                    begin
                      BreakAccDesc(j, Nobility, Modif);
                      Nobi := nil;

                      if (system.length(fNobilityList)>0)
                        then
                          begin
                            count := system.length(fNobilityList)-1;
                            while (count>=0) and (Nobility<fNobilityList[count].Nobility) do
                              dec(count);
                            if count>=0
                              then Nobi := @fNobilityList[count];
                          end;
                      if (Nobi<>nil) and (Nobi.Icon[1]<>-1)
                        then
                          begin
                            Image := Nobi.Icon[1];
                            Return;
                          end;
                      if (fPrivacyHandler <> nil) and fPrivacyHandler.UserIsIgnored(user)
                        then Image := fIcomChat.ic_IgnoreUser
                        else Image := fIcomChat.ic_Void;
                      i := 0;
                      count := system.length(fIconInfo);
                      while (i<count) and (Modif<>0) do
                        begin
                          with fIconInfo[i] do
                            if (Modif and Mask)>0
                              then
                                begin
                                  k := 0;
                                  Modif := Modif and not Mask;
                                  b := false;
                                  while (k<system.length(CancelIcon)) and (CancelIcon[k]<>0) and not b do
                                    begin
                                      b := (CancelIcon[k] and Modif) >0;
                                      inc(k);
                                    end;
                                  if not b
                                    then Image := Icon;
                                end;
                          inc(i);
                        end;
                      if (Nobi<>nil) and (Nobi.Icon[0]<>-1)
                        then Image := Nobi.Icon[0];
                      add(user);
                      if (Nobi<>nil) and (Nobi.Icon[2]<>-1)
                        then
                          begin
                            Return;
                            Image := Nobi.Icon[2];
                          end;
                    end
                  else
                    begin
                      if (fPrivacyHandler <> nil) and fPrivacyHandler.UserIsIgnored(user)
                        then Image := fIcomChat.ic_IgnoreUser
                        else Image := fIcomChat.ic_Void;
                      add(user);
                    end;
              end;
            UserList.AddSyntaxWriterSorted;
            if afk
              then MsgCompostionChanged(user, mstAFK);
          end;
    end;

  procedure TChatListHandlerView.DelUser( UserName : string );
    var
      i : integer;
    begin
      i := FindIndex(UserName);
      if i>=0
        then UserList.Items.Delete(i);
    end;

  procedure TChatListHandlerView.ChatMsg( Msg : TChatMsgInfo );
    var
      s : string;
      i : integer;
    begin
      if Parent <> nil
        then
          begin
            UserHasSpoken( Msg.From );
            LogBtn.Enabled := true;
          end;
      if fChatLog = nil
        then fChatLog := TStringList.Create;

      with Msg do
        begin
          i := system.pos('/', From);
          if i>0
            then s := copy(From, 1, i-1)
            else s:= From;
          s := s + ': ' + Msg;
        end;
      fChatLog.Add(s);
      if ChatLogWindow.Visible
        then
          with ChatLogWindow.Memo.Lines do
            begin
              BeginUpdate;
              Add(s);
              EndUpdate;
            end;
    end;

  procedure TChatListHandlerView.UserHasSpoken(const UserName : string );
    var
      i : integer;
      j : cardinal;
      user : string;
      afk : boolean;
    begin
      if ParseChatUser(UserName, user, j, afk)
        then
          begin
            i := FindIndex(user);
            if (i>=0) and not fPrivacyHandler.UserIsIgnored(UserName)
              then
                with UserList.SyntaxWriter, UserList do
                  begin
                    text := Items[i];
                    if IsReturn(2)
                      then j := 3
                      else j := 1;
                    CharForImage(j, fIcomChat.ic_Write);
                    Items[i] := text;
                end;
            if afk
              then MsgCompostionChanged(user, mstAFK);
          end;
    end;

  procedure TChatListHandlerView.MsgCompostionChanged( UserName : string; State : TMsgCompositionState );
    var
      i : integer;
      j : integer;
    begin
      i := FindIndex(UserName);
      if (i>=0) and not fPrivacyHandler.UserIsIgnored(UserName)
        then
          with UserList.SyntaxWriter, UserList do
            begin
              text := Items[i];
              if IsReturn(2)
                then j := 3
                else j := 1;
              if State=mstIdle
                then
                  begin
                    CharForImage(j, fIcomChat.ic_Void);
                    Items[i] := text;
                  end
                else
                  if State=mstComposing
                    then
                      begin
                        CharForImage(j, fIcomChat.ic_Writing);
                        Items[i] := text;
                      end
                    else
                      if State=mstAFK
                        then
                          begin
                            CharForImage(j, fIcomChat.ic_AFK);
                            Items[i] := text;
                          end;
          end;
    end;

  procedure TChatListHandlerView.TimerTimer(Sender: TObject);
    var
      i : integer;
      j : integer;
      k : integer;
    begin
      try
        with UserList do
          for i := 0 to pred(Items.Count) do
            with SelectSyntax(i) do
              begin
                if IsReturn(2)
                  then k := 3
                  else k := 1;
                j := ImagenIn[k];
                if j=fIcomChat.ic_Write
                  then
                    begin
                      CharForImage(k, fIcomChat.ic_Void);
                      Items[i] := text;
                    end;
              end;
      except
      end;
    end;

  procedure TChatListHandlerView.FollowUserClick(Sender: TObject);
    var
      ErrorCode : TErrorCode;
    begin
      with UserList do
        if ItemIndex>=0
          then
            begin
              with UserList.SelectSyntax(ItemIndex) do
                fClientView.Chase(RawText,  ErrorCode);
            end;
    end;

  procedure TChatListHandlerView.Image1Click(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=' + tidHandlerName_ChatList + '&frame_Close=yes' );
    end;

  procedure TChatListHandlerView.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      Color := clGreen;
    end;

  procedure TChatListHandlerView.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMasterURLHandler.HandleEvent( evnEndOfVoiceRecording, self );
      Color := clBlack;
    end;

  procedure TChatListHandlerView.VoiceChatClick(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=VoiceHandler&frame_Class=VoiceHandler&frame_Align=bottom&frame_Height=30&frame_Visibility=switch' );
    end;

  procedure TChatListHandlerView.IgnoreUserClick(Sender: TObject);
    var
      s : string;
      i : integer;
      j : integer;
    begin
      i := UserList.ItemIndex;
      if i>=0
        then
          with UserList.SelectSyntax(i) do
            begin
              s := RawText;
              if IsReturn(2)
                then j := 3
                else j := 1;
              if fPrivacyHandler.UserIsIgnored(s)
                then
                  begin
                    fPrivacyHandler.ClearIgnoredUser(s);
                    CharForImage(j, fIcomChat.ic_Void);
                    IgnoreUser.Selected := false;
                  end
                else
                  begin
                    fPrivacyHandler.IgnoreUser(s);
                    CharForImage(j, fIcomChat.ic_IgnoreUser);
                    IgnoreUser.Selected := true;
                  end;
              UserList.Items[i] := Text;
            end;
    end;

procedure TChatListHandlerView.LogBtnClick(Sender: TObject);
  begin
    if fChatLog <> nil
      then
        begin
          ChatLogWindow.Memo.Lines.Assign( fChatLog );
          ChatLogWindow.Show;
        end;
  end;

procedure TChatListHandlerView.SetParent(which : TWinControl);
  begin
    inherited;
    if (which<>nil)
      then
        begin
          UserList.DoubleBuffered := true;
          if InitSkinImage
            then
              begin
                InitializeCoolSB(UserList.Handle);
                if hThemeLib <> 0
                  then
                    SetWindowTheme(UserList.Handle, ' ', ' ');
              end;
        end;
    if Timer<>nil
      then Timer.Enabled := which<>nil;
  end;

destructor TChatListHandlerView.Destroy;
  begin
    fChatLog.Free;
    inherited;
  end;

function TChatListHandlerView.FindIndex(const UserName: string): integer;
  var
    i : integer;
  begin
    with UserList do
      begin
        i := Items.Count;
        result := -1;
        while (i>0) do
          begin
            dec(i);
            with UserList.SelectSyntax(i) do
              begin
                if comparetext(RawText, UserName)=0
                  then
                    begin
                      result := i;
                      i := -1;
                    end;
                end;
          end;
      end;
  end;

procedure TChatListHandlerView.LoadIconInfo(const list: TStringlist);
  var
    i : integer;
    p : integer;
    s : string;
    t : string;
    Pos : integer;
    CountMask : integer;
    CountNobility : integer;
  function DecodeLine(const s: string; const Icons: PIconInfo): boolean;
    var
      j : integer;
      i : integer;
      x : integer;
    begin
      try
        // mask
        with Icons^ do
          begin
            Mask := strtoint(t);
            // Icon
            t := GetNextWord(s, Pos, [' ', ',', #9]);
            j := fIconList.IndexOf(t);
            if j>=0
              then
                begin
                  Icon := j;
                  i := 0;
                  repeat
                    t := GetNextWord(s, Pos, [' ', ',', #9]);
                    if t<>''
                      then
                        begin
                          if (t[1]='{')
                            then
                              begin
                                if (system.pos('}', s)>0)
                                  then
                                    begin
                                      x := system.pos('{', s)+1;
                                      StrFormat := copy(s, x, system.pos('}', s)-x);
                                    end;
                                t := '';
                              end
                            else
                              begin
                                try
                                  j := strtoint(t);
                                except
                                  j := -1;
                                end;
                                if j>0
                                  then
                                    begin
                                      CancelIcon[i] := j;
                                      inc(i);
                                    end;
                              end;
                        end;
                  until (t='') or (i>sizeof(CancelIcon));
                  result := true;
                end
              else result := false;
            end;
      except
        result := false;
      end;
    end;

  function DecodeNobility(const s: string; const Nobi: PNobility): boolean;
    var
      j : integer;
      i : integer;
    begin
      try
        // mask
        with Nobi^ do
          begin
            Nobility := strtoint(t);
            fillchar(Icon, sizeof(icon), 255);
            i := 0;
            repeat
              t := GetNextWord(s, Pos, [' ', ',', #9]);
              if t<>''
                then
                  begin
                    j := fIconList.IndexOf(t);
                    if j>=0
                      then
                        begin
                          Icon[i] := j;
                          result := true;
                        end;
                    inc(i);
                  end;
            until (i>2) or (t='');
          end;
      except
        result := false;
      end;
    end;

  begin
    if list.count>0
      then
        begin
          CountMask     := 0;
          CountNobility := 0;
          p := 1;
          for i:=0 to pred(list.count) do
            begin
              s := list[i];
              Pos := 1;
              t := GetNextWord(s, Pos, [' ', ',', #9]);
              if CompareText('[Mask]', t)=0
                then
                  begin
                    p := 0;
                    if length(fIconInfo)<=0
                      then setlength(fIconInfo, list.count-i);
                  end
                else
                  if CompareText('[Nobility]', t)=0
                    then
                      begin
                        p := 1;
                        if length(fNobilityList)<=0
                          then setlength(fNobilityList, list.count-i);
                      end
                    else
                      begin
                        if (t<>'') and (t[1]<>'[') and (t[1]<>'''') and (t[1]<>'/')
                          then
                            case p of
                              0:
                                begin
                                  if DecodeLine(s, @fIconInfo[CountMask])
                                    then inc(CountMask);
                                end;
                              1:
                                begin
                                  if DecodeNobility(s, @fNobilityList[CountNobility])
                                    then inc(CountNobility);
                                end;
                            end;
                      end;
            end;
          setlength(fIconInfo, CountMask);
          setlength(fNobilityList, CountNobility);
          with fIcomChat do
            begin
              ic_Write     := fIconList.IndexOf('write');
              ic_Writing   := fIconList.IndexOf('writing');
              ic_Void      := fIconList.IndexOf('void');
              ic_IgnoreUser := fIconList.IndexOf('IgnoreUser');
              ic_AFK    := fIconList.IndexOf('AFK');
            end;
        end;
  end;

function TChatListHandlerView.UserListImagenCache(const sender: TObject; const index: Integer): TGraphic;
  begin
    try
      result := fIconList.objects[index] as TGraphic;
    except
      result := nil;
    end;
  end;

procedure TChatListHandlerView.UserListClick(Sender: TObject);
  var
    enable : boolean;
    s : string;
    j : integer;
  begin
    enable  := (UserList.ItemIndex>=0);
    if enable
      then
        begin
          with UserList.SelectSyntax(UserList.ItemIndex) do
            begin
              s := RawText;
              enable := CompareText(s, fClientView.getUserName)<>0;
              FollowUser.Enabled := enable;
              IgnoreUser.Enabled := enable;
              if s<>''
                then
                  begin
                    if IsReturn(2)
                      then j := 3
                      else j := 1;

                    if fPrivacyHandler.UserIsIgnored(s)
                      then
                        begin
                          if ImagenIn[j]<>fIcomChat.ic_IgnoreUser
                            then
                              begin
                                CharForImage(j, fIcomChat.ic_IgnoreUser);
                                UserList.Items[UserList.ItemIndex] := Text;
                              end;
                          IgnoreUser.Selected := true;
                        end
                      else
                        begin
                          if (ImagenIn[j]<>fIcomChat.ic_Void) and (ImagenIn[j]<>fIcomChat.ic_AFK)
                            then
                              begin
                                CharForImage(j, fIcomChat.ic_Void);
                                UserList.Items[UserList.ItemIndex] := Text;
                              end;
                           IgnoreUser.Selected := false;
                        end
                  end
                else IgnoreUser.Selected := false;
            end;
        end;
  end;

end.


