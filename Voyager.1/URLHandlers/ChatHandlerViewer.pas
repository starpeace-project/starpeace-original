unit ChatHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, VisualControls, Protocol, TiledPanel, GradientBox,
  VoyagerServerInterfaces, ComCtrls, FramedButton, MarqueeCtrl,
  InternationalizerComponent;

type
  TOnMessageComposed           = procedure( Msg : string ) of object;
  TOnMessageCompositionChanged = procedure( State : TMsgCompositionState ) of object;
  TOnCreateChannel             = procedure( Name, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer ) of object;
  TOnJoinChannel               = procedure( Name, Password : string ) of object;

type
  TChatHandlerView = class(TVisualControl)
    Panel2: TPanel;
    Timer: TTimer;
    Splitter1: TSplitter;
    Panel4: TPanel;
    ChatText: TMemo;
    Panel1: TPanel;
    TextInput: TMemo;
    GradientBox1: TGradientBox;
    GradientBox2: TGradientBox;
    Splitter2: TSplitter;
    Channels: TPanel;
    ChannelList: TListView;
    ChannelImages: TImageList;
    ChannelHeader: TPanel;
    Image1: TImage;
    ChannelName: TPanel;
    NoChannels: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    ChannelsButtons: TPanel;
    CreateChannel: TFramedButton;
    JoinChannel: TFramedButton;
    LeaveChannel: TFramedButton;
    Marquee: TMarquee;
    MarqueeTimer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure OnKeyUp( Sender: TObject; var Key: Word; Shift : TShiftState );
    procedure TimerTimer(Sender: TObject);
    procedure TextInputChange(Sender: TObject);
    procedure CreateChannelClick(Sender: TObject);
    procedure LeaveChannelClick(Sender: TObject);
    procedure ChannelListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure JoinChannelClick(Sender: TObject);
    procedure MarqueeTimerTimer(Sender: TObject);
    procedure ChannelListExit(Sender: TObject);
  public
    procedure DisplayMsg( From, Msg : string );
    procedure Scroll( dy : integer );
    procedure AddChannel( Name, Password : string );
    procedure DelChannel( Name : string );
    procedure SetChannel( Name : string );
  private
    fOnMessageComposed           : TOnMessageComposed;
    fOnMessageCompositionChanged : TOnMessageCompositionChanged;
    fOnCreateChannel             : TOnCreateChannel;
    fOnJoinChannel               : TOnJoinChannel;
    fCompositionState            : TMsgCompositionState;
    fClientView                  : IClientView;
  public
    property OnMessageComposed           : TOnMessageComposed           write fOnMessageComposed;
    property OnMessageCompositionChanged : TOnMessageCompositionChanged write fOnMessageCompositionChanged;
    property OnCreateChannel             : TOnCreateChannel             write fOnCreateChannel;
    property OnJoinChannel               : TOnJoinChannel               write fOnJoinChannel;
    property ClientView                  : IClientView                  write fClientView;
  private
    procedure threadedGetChannelInfo( const parms : array of const );
    procedure syncGetChannelInfo( const parms : array of const );
  end;

implementation

  uses
    NewChannelForm, PasswordVerifyForm, Threads, Literals;

  {$R *.DFM}

  procedure TChatHandlerView.DisplayMsg( From, Msg : string );
    var
      i : integer;
    begin
      if ChatText <> nil
        then
          begin
            if ChatText.Lines.Count > 1000
              then
                begin
                  LockWindowUpdate( Handle );
                  ChatText.Lines.BeginUpdate;
                  for i := 1 to 100 do
                    ChatText.Lines.Delete( 0 );
                  ChatText.Lines.EndUpdate;
                  LockWindowUpdate( 0 );
                end;
            ChatText.Lines.Add( From + ': ' + Msg );
          end;
    end;

  procedure TChatHandlerView.Scroll( dy : integer );
    begin
    end;

  procedure TChatHandlerView.AddChannel( Name, Password : string );
    begin
      if ChannelList <> nil
        then
          begin
            with ChannelList.Items.Add do
              begin
                Caption    := Name;
                Data       := NewStr( Password );
                if Password = ''
                  then ImageIndex := 0
                  else ImageIndex := 1;
              end;
            NoChannels.Visible  := false;
            ChannelList.Visible := true;
            if ChannelList.Selected = nil
              then Marquee.Caption := GetFormattedLiteral('Literal204', [uppercase(Name)]);
          end;
    end;

  procedure TChatHandlerView.DelChannel( Name : string );
    var
      i : integer;
    begin
      for i := pred(ChannelList.Items.Count) downto 0 do
        if ChannelList.Items[i].Caption = Name
          then ChannelList.Items.Delete( i );
      if ChannelList.Items.Count = 0
        then
          begin
            NoChannels.Visible  := true;
            ChannelList.Visible := false;
          end;
      if ChannelList.Selected = nil
        then Marquee.Caption := GetFormattedLiteral('Literal206', [uppercase(Name)]);
    end;

  procedure TChatHandlerView.SetChannel( Name : string );
    begin
      ChannelHeader.Visible := Name <> '';
      LeaveChannel.Enabled  := Name <> '';
      ChannelName.Caption   := Name;
      JoinChannel.Enabled   := false;
      if Name <> ''
        then Marquee.Caption := GetFormattedLiteral('Literal208', [uppercase(Name)]) + '.'
        else Marquee.Caption := GetLiteral('Literal209')
    end;

  procedure TChatHandlerView.threadedGetChannelInfo( const parms : array of const );
    var
      ChannelName : string absolute parms[0].vPChar;
      Info        : string;
      ErrorCode   : TErrorCode;
    begin
      if ChannelName <> ''
        then Info := fClientView.GetChannelInfo( ChannelName, ErrorCode )
        else Info := GetFormattedLiteral('Literal210', [fClientView.ContextText]);
      if (ErrorCode = NOERROR) or (ChannelName = '')
        then Join( syncGetChannelInfo, [Info] );
    end;

  procedure TChatHandlerView.syncGetChannelInfo( const parms : array of const );
    var
      Info : string absolute parms[0].vPChar;
    begin
      Marquee.Caption := uppercase(Info);
    end;

  procedure TChatHandlerView.OnKeyUp( Sender: TObject; var Key: Word; Shift : TShiftState );
    var
      text : string;
      p    : integer;
    begin
      if Key = vk_Return
        then
          begin
            text := TextInput.Text;
            p := pos( #$D#$A, text );
            if p > 0
              then delete( text, p, 2 );
            if assigned(fOnMessageComposed)
              then fOnMessageComposed( text );
            TextInput.Clear;
            fCompositionState := mstIdle;
            Timer.Interval    := 0;
          end;
    end;

  procedure TChatHandlerView.TimerTimer(Sender: TObject);
    begin
      if assigned(fOnMessageCompositionChanged)
         then fOnMessageCompositionChanged( mstIdle );
      fCompositionState := mstIdle;
      Timer.Interval    := 0;
    end;

  procedure TChatHandlerView.TextInputChange(Sender: TObject);
    begin
      if fCompositionState = mstIdle
        then
          if assigned(fOnMessageCompositionChanged)
            then fOnMessageCompositionChanged( mstComposing );
      fCompositionState := mstComposing;
      Timer.Interval    := 0;
      Timer.Interval    := CompositionTimeOut;
    end;

  procedure TChatHandlerView.CreateChannelClick(Sender: TObject);
    begin
      NewChannelFrm.Tabs.CurrentTab := 0;
      if (NewChannelFrm.ShowModal = mrOk) and assigned(fOnCreateChannel)
        then fOnCreateChannel( NewChannelFrm.ChannelName.Text, NewChannelFrm.Password.Text, '', '', 100 );
    end;

  procedure TChatHandlerView.LeaveChannelClick(Sender: TObject);
    begin
      if assigned(fOnJoinChannel)
        then fOnJoinChannel( '', '' );
    end;

  procedure TChatHandlerView.ChannelListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      if Change = ctState
        then
          begin
            if ChannelList.Selected <> nil
              then Fork( threadedGetChannelInfo, priNormal, [ChannelList.Selected.Caption] )
              else Fork( threadedGetChannelInfo, priNormal, [''] );
          end;
      JoinChannel.Enabled := (ChannelList.Selected <> nil) and (ChannelName.Caption <> ChannelList.Selected.Caption)
    end;

  procedure TChatHandlerView.JoinChannelClick(Sender: TObject);
    var
      Password : string;
    begin
      if (ChannelList.Selected <> nil) and (ChannelName.Caption <> ChannelList.Selected.Caption) and assigned(fOnJoinChannel)
        then
          begin
            Password := PString(ChannelList.Selected.Data)^;
            PasswordVerifyFrm.CheckPassword := Password;
            if (Password = '') or (PasswordVerifyFrm.ShowModal = mrOk) and (uppercase(PasswordVerifyFrm.Password.Text) = uppercase(Password))
              then fOnJoinChannel( ChannelList.Selected.Caption, PString(ChannelList.Selected.Data)^ );
          end;
    end;

  procedure TChatHandlerView.MarqueeTimerTimer(Sender: TObject);
    begin
      Marquee.Tick;
    end;

  procedure TChatHandlerView.ChannelListExit(Sender: TObject);
    begin
      JoinChannel.Enabled := false;
    end;

end.

                                                                     
