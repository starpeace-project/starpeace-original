unit UniversalMapViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, PDTabControl, GradientBox, BlockTicker, ComCtrls,
  FramedButton, VisualControls,
  RDOInterfaces, WinSockRDOConnection, RDOServer, RDOObjectProxy,
  VoyagerInterfaces, VoyagerServerInterfaces, InternationalizerComponent;

type
  TUniversalMapView = class(TVisualControl)
    GradientBox1: TGradientBox;
    Tabs: TPDTabControl;
    lbName: TLabel;
    Panel1: TPanel;
    Label11: TLabel;
    WorldList: TListView;
    Panel2: TPanel;
    Ticker: TBlockTicker;
    PlanetImages: TImageList;
    Panel3: TPanel;
    Jump: TFramedButton;
    Refresh: TFramedButton;
    Label9: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RightLine: TPanel;
    TickerTimer: TTimer;
    UnvMapIcon: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure JumpClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure WorldListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TickerTimerTimer(Sender: TObject);
    procedure HideClick(Sender: TObject);
    procedure UnvMapIconClick(Sender: TObject);
    procedure TabsTabChange(Sender: TObject);
  private
    fCache            : string;
    fMasterURLHandler : IMasterURLHandler;
    fDSAddr           : string;
    fDSPort           : integer;
    fWorlds           : TStringList;
    fUserName         : string;
    fPassword         : string;
    fLogon            : boolean;
  public
    property Cache            : string            write fCache;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property DSAddr           : string            write fDSAddr;
    property DSPort           : integer           write fDSPort;
    property UserName         : string            write fUsername;
    property Password         : string            write fPassword;
    property Logon            : boolean           write fLogon;
  public
    procedure Start;
  private
    procedure threadedGetWorlds( const parms : array of const );
    procedure syncGetWorlds( const parms : array of const );
  private
    fLastTab : integer;
  end;

var
  UniversalMapView: TUniversalMapView;

implementation

  {$R *.DFM}

  uses
    Threads, Config, ServerCnxEvents, Literals;

  const
    tidWorldProp_Count      = 'Count';
    tidWorldProp_Key        = 'Key';
    tidWorldProp_Population = 'General/Population';
    tidWorldProp_Investors  = 'General/Investors';
    tidWorldProp_Online     = 'General/Online';
    tidWorldProp_Date       = 'General/Date';
    tidWorldProp_ISAddr     = 'Interface/IP';
    tidWorldProp_ISPort     = 'Interface/Port';
    tidWorldProp_URL        = 'Interface/URL';
    tidWorldProp_Running    = 'Interface/Running';

  const
    AreaIds : array[0..2] of string = ('America', 'Europe', 'Asia'); // >> Generalize this later!!!
    //AreaIds : array[0..2] of string = ('Literal401', 'Literal402', 'Literal403'); // >> Generalize this later!!!

  procedure TUniversalMapView.Start;
    var
      ConfigHolder : IConfigHolder;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      DSAddr := ConfigHolder.ReadString( true, '', 'DSAddr', 'dir.starpeace.net' );
      DSPort := ConfigHolder.ReadInteger( true, '', 'DSPort', 1111 );
      WorldList.Items.Clear;
      WorldList.ViewStyle := vsList;
      with WorldList.Items.Add do
        begin
          Caption := GetLiteral('Literal404');
          ImageIndex := -1;
        end;
      Fork( threadedGetWorlds, priNormal, [AreaIds[Tabs.CurrentTab]] );
    end;

  procedure TUniversalMapView.threadedGetWorlds( const parms : array of const );
    var
      Area      : string absolute parms[0].vPChar;
      WorldList : TStringList;
      props     : TStringList;
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      key       : string;                                        
    begin
      try
        WorldList    := TStringList.Create;
        try
          WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
          DSCnx        := WSDSCnx;
          DSCnx.Server := fDSAddr;
          DSCnx.Port   := fDSPort;
          DSProxy      := TRDOObjectProxy.Create as IDispatch;
          if DSCnx.Connect( 20000 )
            then
              begin
                DSProxy.SetConnection( DSCnx );
                DSProxy.BindTo( 'DirectoryServer' );
                DSProxy.TimeOut := 20000;
                session         := DSProxy.RDOOpenSession;
                if session <> 0
                  then
                    begin
                      try
                        DSProxy.BindTo( session );
                        //DSProxy.RDOCurrentKey
                        key := 'Root/Areas/' + Area + '/Worlds';
                        DSProxy.WaitForAnswer := true;
                        props := TStringList.Create;
                        try
                          props.Add( tidWorldProp_Population );
                          props.Add( tidWorldProp_Investors );
                          props.Add( tidWorldProp_Online );
                          props.Add( tidWorldProp_Date );
                          props.Add( tidWorldProp_ISAddr );
                          props.Add( tidWorldProp_ISPort );
                          props.Add( tidWorldProp_URL );
                          props.Add( tidWorldProp_Running );
                          WorldList.Text := DSProxy.RDOQueryKey( key, props.text );   
                        finally
                          props.Free;
                        end;
                        //WorldList.Text := DSProxy.RDOGetKeyNames;
                      finally
                        DSProxy.RDOEndSession;
                      end;
                    end;
              end;
        finally
          WorldList.Values['Area'] := Area;
          Join( syncGetWorlds, [WorldList] );
        end;
      except
      end;
    end;

  procedure TUniversalMapView.syncGetWorlds( const parms : array of const );
    var
      Worlds : TStringList absolute parms[0].vPointer;
      count  : integer;
      i      : integer;
    begin
      try
        try
          count := StrToInt(Worlds.Values[tidWorldProp_Count]);
        except
          count := 0;
        end;
        WorldList.Items.Clear;
        if Worlds.Count > 0
          then
            begin
              WorldList.ViewStyle := vsIcon;
              for i := 0 to pred(count) do
                with WorldList.Items.Add do
                  begin
                    Caption := Worlds.Values[tidWorldProp_Key + IntToStr(i)];
                    SubItems.Add( Worlds.Values[tidWorldProp_Population + IntToStr(i)] );
                    SubItems.Add( Worlds.Values[tidWorldProp_Investors + IntToStr(i)] );
                    SubItems.Add( Worlds.Values[tidWorldProp_Online + IntToStr(i)] );
                    SubItems.Add( Worlds.Values[tidWorldProp_Date + IntToStr(i)] );
                    SubItems.Add( IntToStr(100 + random(100)) );
                    if Worlds.Values[tidWorldProp_Running + IntToStr(i)] = 'true'
                      then
                        begin                                                      
                          ImageIndex := i mod (PlanetImages.Count - 1);
                          Data := pointer(true);
                        end
                      else                
                        begin
                          ImageIndex := PlanetImages.Count - 1;
                          Caption := Caption + #13#10 + GetLiteral('Literal405');
                          Data := pointer(false);
                        end;
                  end;
            end
          else
            begin
              WorldList.ViewStyle := vsList;
              with WorldList.Items.Add do
                begin
                  Caption := GetLiteral('Literal406');
                  ImageIndex := -1;
                end;
            end;
      finally
        fWorlds.Free;
        fWorlds := Worlds;
      end;
    end;

  procedure TUniversalMapView.JumpClick(Sender: TObject);
    var
      URL        : string;
      ClientView : IClientView;
      dummy      : integer;
    begin
      if WorldList.Selected <> nil
        then
          begin
            if fUserName = ''
              then
                begin
                  ClientView := nil;
                  fMasterURLHandler.HandleEvent( evnAnswerClientView, ClientView );
                  if ClientView <> nil
                    then
                      begin
                        //fUserName := ClientView.getUserName;
                        fUserName := ClientView.getUserMasterName;
                        fPassword := ClientView.getUserPassword;
                      end;
                end;
            URL :=
              '?' +
              'frame_Action=Logon&' +
              'frame_Id=CnxHandler&' +
              'frame_Class=CnxHandler&' +
              'frame_NoBorder=yes&' +
              'frame_NoScrollBar=yes&' +
              'frame_Align=client&' +
              'Threaded=yes&' +
              {'frame_Target=Main&' +}
              'UserName=' + fUserName + '&' +
              'Password=' + fPassword + '&' +
              'WorldName=' + WorldList.Selected.Caption + '&' +
              'ISAddr=' + fWorlds.Values[tidWorldProp_ISAddr + IntToStr(WorldList.Selected.Index)] + '&' +
              'ISPort=' + fWorlds.Values[tidWorldProp_ISPort + IntToStr(WorldList.Selected.Index)] + '&' +
              'DSAddr=' + fDSAddr + '&' +
              'DSPort=' + IntToStr(fDSPort) + '&' +
              'Area=' + fWorlds.Values['Area'] + '&' +
              'ResultURL=' + fWorlds.Values[tidWorldProp_URL + IntToStr(WorldList.Selected.Index)] + 'Visual/Voyager/NewLogon/logonComplete.asp';
            try
              fMasterURLHandler.HandleEvent( evnLogonStarted, dummy );
            except
              fMasterURLHandler.HandleEvent( evnLogonStarted, dummy );
            end;
            fMasterURLHandler.HandleURL( URL );
            Jump.Enabled := false;
          end;
    end;

  procedure TUniversalMapView.RefreshClick(Sender: TObject);
    begin
      Start;
    end;

  procedure TUniversalMapView.WorldListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      Jump.Enabled := (WorldList.Selected <> nil);// and boolean(WorldList.Selected.Data);
      if WorldList.Selected <> nil
        then
          begin
            Ticker.Caption := GetFormattedLiteral('Literal407',
                                                   [
                                                     WorldList.Selected.Caption,
                                                     fWorlds.Values[tidWorldProp_Population + IntToStr(WorldList.Selected.Index)],
                                                     fWorlds.Values[tidWorldProp_Investors + IntToStr(WorldList.Selected.Index)],
                                                     fWorlds.Values[tidWorldProp_Date + IntToStr(WorldList.Selected.Index)],
                                                     fWorlds.Values[tidWorldProp_Online + IntToStr(WorldList.Selected.Index)]
                                                   ]
                                                 );
          end
        else Ticker.Caption := GetLiteral('Literal411')
    end;

  procedure TUniversalMapView.TickerTimerTimer(Sender: TObject);
    begin
      Ticker.Tick;
    end;

  procedure TUniversalMapView.HideClick(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Close=yes' );
    end;

  procedure TUniversalMapView.UnvMapIconClick(Sender: TObject);
    begin
      if not fLogon
        then fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Close=yes' );
    end;

  procedure TUniversalMapView.TabsTabChange(Sender: TObject);
    begin
      if Tabs.CurrentTab <> fLastTab
        then
          begin
            fLastTab := Tabs.CurrentTab;
            WorldList.Items.Clear;
            WorldList.ViewStyle := vsList;
            with WorldList.Items.Add do
              begin
                Caption := GetLiteral('Literal412');
                ImageIndex := -1;
              end;
            Fork( threadedGetWorlds, priNormal, [AreaIds[Tabs.CurrentTab]] );
          end;
    end;




end.


