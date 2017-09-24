unit ChatListHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Protocol, VisualControls, TiledPanel,
  FramedButton, GradientBox, VoyagerServerInterfaces, VoyagerInterfaces,
  ChatHandler, InternationalizerComponent;

type
  TChatListHandlerView = class(TVisualControl)
    Panel2: TPanel;
    UserList: TListView;
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
    procedure TimerTimer(Sender: TObject);
    procedure FollowUserClick(Sender: TObject);
    procedure UserListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure Image1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VoiceChatClick(Sender: TObject);
    procedure IgnoreUserClick(Sender: TObject);
  public
    procedure AddUser( UserName : string );
    procedure DelUser( UserName : string );
    procedure UserHasSpoken( UserName : string );
    procedure MsgCompostionChanged( UserName : string; State : TMsgCompositionState );
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fPrivacyHandler   : IPrivacyHandler;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property PrivacyHandler   : IPrivacyHandler   write fPrivacyHandler;
  end;

implementation

  uses
    ChatListHandler, VoiceHandler;

  {$R *.DFM}

  procedure TChatListHandlerView.AddUser( UserName : string );
    begin
      with UserList.Items.Add do
        begin
          Caption    := UserName;
          StateIndex := -1;
        end;
    end;

  procedure TChatListHandlerView.DelUser( UserName : string );
    var
      Item : TListItem;
    begin
      Item := UserList.FindCaption( 0, UserName, true, true, false );
      if Item <> nil
        then Item.Delete;
    end;

  procedure TChatListHandlerView.UserHasSpoken( UserName : string );
    var
      Item : TListItem;
    begin
      Item := UserList.FindCaption( 0, UserName, true, true, false );
      if Item <> nil
        then Item.StateIndex := 0;
    end;

  procedure TChatListHandlerView.MsgCompostionChanged( UserName : string; State : TMsgCompositionState );
    var
      Item : TListItem;
    begin
      Item := UserList.FindCaption( 0, UserName, true, true, false );
      if Item <> nil
        then
          case State of
            mstIdle :
              Item.StateIndex := -1;
            mstComposing :
              Item.StateIndex := 1;
          end;
    end;

  procedure TChatListHandlerView.TimerTimer(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(UserList.Items.Count) do
        if UserList.Items[i].StateIndex = 0
          then UserList.Items[i].StateIndex := -1;
    end;

  procedure TChatListHandlerView.FollowUserClick(Sender: TObject);
    var
      ErrorCode : TErrorCode;
    begin
      if UserList.Selected <> nil
        then fClientView.Chase( UserList.Selected.Caption, ErrorCode );
    end;

  procedure TChatListHandlerView.UserListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    var
      enable : boolean;
    begin
      enable := (UserList.Selected <> nil) and (UserList.Selected.Caption <> fClientView.getUserName);
      IgnoreUser.Enabled := enable;
      FollowUser.Enabled := enable;
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
    begin
      if UserList.Selected <> nil
        then fPrivacyHandler.IgnoreUser( UserList.Selected.Caption );
    end;

end.


