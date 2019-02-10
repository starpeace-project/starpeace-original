unit GMChatViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, UnitComp_TALogger,
  StdCtrls, ComCtrls, ExtCtrls, FramedButton, VoyagerInterfaces, VoyagerServerInterfaces,
  VisualControls, GMList, InternationalizerComponent;

type
  TOnMessageComposed = procedure( Msg : string ) of object;
  TOnCallGM          = procedure of object;
  TOnDisconnectGM    = procedure of object;

type
  TGMChatView =
    class(TVisualControl)
        Panel1: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        ChatLine: TMemo;
        btnConnect: TFramedButton;
        Panel2: TPanel;
        ChatText: TRichEdit;
        btGMOptions: TFramedButton;
        btClose: TFramedButton;
        Label8: TLabel;
        lbGMName: TLabel;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure btnConnectClick(Sender: TObject);
        procedure ChatLineKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btCloseClick(Sender: TObject);
        procedure btGMOptionsClick(Sender: TObject);
      public
        fClientView        : IClientView;
        fMasterURLHandler  : IMasterURLHandler;
        fOnMessageComposed : TOnMessageComposed;
        fOnCallGM          : TOnCallGM;
        fOnDisconnectGM    : TOnDisconnectGM;
        fCharCount         : integer;
        fGameMasterList    : TGameMasterList;
      public
        property ClientView : IClientView write fClientView;
        property MasterURLHandler : IMasterURLHandler   write fMasterURLHandler;
        property OnMessageComposed : TOnMessageComposed write fOnMessageComposed;
        property OnCallGM          : TOnCallGM          write fOnCallGM;
        property OnDisconnectGM    : TOnDisconnectGM    write fOnDisconnectGM;
      protected
        procedure Loaded; override;
      public
        procedure AddChatString( Header, Body : string; HeaderColor : TColor );
        procedure AddGMToOptions( GMName : string );
        function  GetGMOptionString : string;
      protected
        procedure SetParent(which : TWinControl);  override;
  end;

var
  GMChatView: TGMChatView;

implementation

{$R *.DFM}

  uses
    ChatUtils, GMChatHandler, ConnectOptionsFrm, Config, CoolSB;

  procedure TGMChatView.Loaded;
    var
      ConfigHolder : IConfigHolder;
    begin
      inherited;
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      fGameMasterList := TGameMasterList.Create;
      fGameMasterList.LoadFromString( ConfigHolder.ReadString( false, fClientView.getUserName, 'GMList', '' ));
    end;

  procedure TGMChatView.AddChatString( Header, Body : string; HeaderColor : TColor );
    begin
      ChatUtils.AddChatString( ChatText, Header + ': ', Body, HeaderColor, fCharCount );
    end;

  procedure TGMChatView.AddGMToOptions( GMName : string );
    var
      ConfigHolder : IConfigHolder;
      useless      : integer;
      str          : string;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      fGameMasterList.AddGameMaster( GMName, GMCO_NORMALPRIORITY, useless );
      fGameMasterList.SaveToString( str );
      ConfigHolder.WriteString( false, fClientView.getUserName, 'GMList', str );
    end;

  function TGMChatView.GetGMOptionString : string;
    begin
      fGameMasterList.SaveToString( result );
    end;
              
  procedure TGMChatView.btnConnectClick(Sender: TObject);
    begin
      if assigned(fOnCallGM)
        then fOnCallGM;                         
    end;

  procedure TGMChatView.ChatLineKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    begin
      if (Key = VK_Return) and assigned(fOnMessageComposed)
        then
          begin
            fOnMessageComposed( ChatLine.Text );
            Key := 0;
            ChatLine.Text := '';
          end;
    end;

  procedure TGMChatView.btCloseClick(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_id=GMChat&frame_Close=yes' );
    end;

  procedure TGMChatView.btGMOptionsClick(Sender: TObject);
    var
      ConfigHolder  : IConfigHolder;
      ConnOptions   : TGMConnOptions;
      str           : string;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );

      ConnOptions := TGMConnOptions.Create( nil );
      ConnOptions.GameMasters := fGameMasterList;
      ConnOptions.ShowModal;
      ConnOptions.Free;
      fGameMasterList.SaveToString( str );
      ConfigHolder.WriteString( false, fClientView.getUserName, 'GMList', str );
    end;

  procedure TGMChatView.SetParent(which: TWinControl);
    begin
      inherited;
      if InitSkinImage and (which<>nil)
        then
          begin
            InitializeCoolSB(ChatText.Handle);
            if hThemeLib <> 0
              then
                SetWindowTheme(ChatText.Handle, ' ', ' ');
          end;
    end;

end.
