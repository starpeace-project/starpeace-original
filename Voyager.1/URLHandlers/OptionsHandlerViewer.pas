unit OptionsHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GradientBox, VisualControls, PercentEdit,
  FramedButton, VoyagerInterfaces, VoyagerServerInterfaces, FingerTabs,
  ColoredGauge, ComCtrls, JukeBox, Synchro, ChatHandler, checklst,
  InternationalizerComponent, Config;

type
  TOptionsHandlerView = class(TVisualControl)
    GradientBox1: TGradientBox;
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Timer: TTimer;
    Tabs: TFingerTabs;
    Panel: TPanel;
    Notebook: TNotebook;
    UseTransparency: TCheckBox;
    AnimateBuildings: TCheckBox;
    ShowCars: TCheckBox;
    Label2: TLabel;
    SoundFXVolume: TPercentEdit;
    Label4: TLabel;
    MusicVol: TPercentEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbByteSent: TLabel;
    lbByteRecv: TLabel;
    lbTotalSent: TLabel;
    Label11: TLabel;
    lbSpeed: TLabel;
    Label12: TLabel;
    lbTotalRecv: TLabel;
    Label14: TLabel;
    lbRate: TLabel;
    Label16: TLabel;
    lbLatency: TLabel;
    AutomaticLogon: TCheckBox;
    GradientBox2: TGradientBox;
    Label3: TLabel;
    btnAddFile: TFramedButton;
    btnAddDir: TFramedButton;
    btnDelFile: TFramedButton;
    btnDownload: TFramedButton;
    Panel1: TPanel;
    OverallProg: TColorGauge;
    Prog: TColorGauge;
    Playlist: TListView;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    Label5: TLabel;
    btRemoveFromIgnore: TFramedButton;
    IgnoreList: TListBox;
    Label6: TLabel;
    edChannelName: TEdit;
    Label7: TLabel;
    Label13: TLabel;
    edPassword: TEdit;
    FramedButton1: TFramedButton;
    ShowPlanes: TCheckBox;
    FacIdsListBox: TCheckListBox;
    Label15: TLabel;
    TransparentOverlays: TCheckBox;
    HideAll: TFramedButton;
    ShowAll: TFramedButton;
    EnableSounds: TCheckBox;
    InternationalizerComponent1: TInternationalizerComponent;
    Label17: TLabel;
    btnSetLang: TFramedButton;
    cbLangList: TComboBox;
    procedure OnCheckBoxClick(Sender: TObject);
    procedure UpdateLogonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure MusicVolChange(Sender: TObject);                                                   
    procedure TabsOnFingerChange(Sender: TObject);
    procedure PlaylistDblClick(Sender: TObject);
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddDirClick(Sender: TObject);
    procedure btnDelFileClick(Sender: TObject);
    procedure PlaylistKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnDownloadClick(Sender: TObject);
    procedure IgnoreListClick(Sender: TObject);
    procedure btRemoveFromIgnoreClick(Sender: TObject);
    procedure FramedButton1Click(Sender: TObject);
    procedure HideAllClick(Sender: TObject);
    procedure ShowAllClick(Sender: TObject);
    procedure FacIdsListBoxClickCheck(Sender: TObject);
    procedure SoundFXVolumeMoveBar(Sender: TObject);
    procedure btnSetLangClick(Sender: TObject);
    procedure cbLangListChange(Sender: TObject);
  private
    fCounting         : boolean;
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fPlayListInt      : IPlayList;
    fPrivacyHandler   : IPrivacyHandler;
    fByteSent         : integer;
    fByteRecv         : integer;
    fInitialTotal     : integer;
    fSecondCount      : integer;
    fLastTime         : TDateTime;
    fLatFreqDiv       : integer;
    fLatency          : integer;
    fSyncTask         : TThreadedTask;
    fLangList         : TStringList;
    fConfigHolder     : IConfigHolder;
  public
    procedure Loaded; override;
  private
    procedure SetMasterURLHandler( aMasterURLHandler : IMasterURLHandler );
  public
    property ClientView       : IClientView       read fClientView write fClientView;
    property MasterURLHandler : IMasterURLHandler write SetMasterURLHandler;
    property PlayListInt      : IPlayList         write fPlayListInt;
    property PrivacyHandler   : IPrivacyHandler   write fPrivacyHandler;
  public
    procedure StartCounting;
    procedure threadedRequestLatency(const parms : array of const);
    procedure threadedRenderLatency(const parms : array of const);
    procedure SyncNotify( SyncTask : TSyncTask; EventId  : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
  end;

var
  OptionsHandlerView: TOptionsHandlerView;

implementation

  {$R *.DFM}

  uses
    Events, SocketComp, Threads, OptionsHandler, FileCtrl, Literals, ClientMLS;

  const
    cValidFacIds = 58;

  type
    TFacIdData =
      record
        id   : integer;
        name : string;
      end;

  const
    cFacIdData : array [0..pred(cValidFacIds)] of TFacIdData =
      (
        (id : 10; name : 'Literal257'),
        (id : 11; name : 'Literal258'),
        (id : 12; name : 'Literal259'),
        (id : 13; name : 'Literal260'),
        (id : 14; name : 'Literal261'),
        (id : 20; name : 'Literal262'),
        (id : 21; name : 'Literal263'),
        (id : 22; name : 'Literal264'),
        (id : 25; name : 'Literal265'),
        (id : 26; name : 'Literal266'),
        (id : 27; name : 'Literal267'),
        (id : 30; name : 'Literal268'),
        (id : 40; name : 'Literal269'),
        (id : 42; name : 'Literal270'),
        (id : 44; name : 'Literal271'),
        (id : 46; name : 'Literal272'),
        (id : 48; name : 'Literal273'),
        (id : 50; name : 'Literal274'),
        (id : 52; name : 'Literal275'),
        (id : 54; name : 'Literal276'),
        (id : 56; name : 'Literal277'),
        (id : 58; name : 'Literal278'),
        (id : 60; name : 'Literal279'),
        (id : 62; name : 'Literal280'),
        (id : 64; name : 'Literal281'),
        (id : 66; name : 'Literal282'),
        (id : 68; name : 'Literal283'),
        (id : 69; name : 'Literal284'),
        (id : 200; name : 'Literal285'),
        (id : 201; name : 'Literal286'),
        (id : 202; name : 'Literal287'),
        (id : 70; name : 'Literal288'),
        (id : 71; name : 'Literal289'),
        (id : 72; name : 'Literal290'),
        (id : 74; name : 'Literal291'),
        (id : 75; name : 'Literal292'),
        (id : 76; name : 'Literal293'),
        (id : 77; name : 'Literal294'),
        (id : 78; name : 'Literal295'),
        (id : 79; name : 'Literal296'),
        (id : 90; name : 'Literal297'),
        (id : 91; name : 'Literal298'),
        (id : 80; name : 'Literal299'),
        (id : 100; name : 'Literal300'),
        (id : 101; name : 'Literal301'),
        (id : 102; name : 'Literal302'),
        (id : 103; name : 'Literal303'),
        (id : 104; name : 'Literal304'),
        (id : 105; name : 'Literal305'),
        (id : 110; name : 'Literal306'),
        (id : 111; name : 'Literal307'),
        (id : 112; name : 'Literal308'),
        (id : 120; name : 'Literal309'),
        (id : 121; name : 'Literal310'),
        (id : 122; name : 'Literal311'),
        (id : 123; name : 'Literal312'),
        (id : 124; name : 'Literal313'),
        (id : 140; name : 'Literal314')
      );

  procedure TOptionsHandlerView.OnCheckBoxClick(Sender: TObject);
    var
      enabled : boolean;
    begin
      //
      enabled := TCheckBox(Sender).Checked;
      case TControl(Sender).Tag of
        0 :
          begin
            fMasterURLHandler.HandleEvent(evnGlassBuildings, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'UseTransparency', enabled);
          end;
        1 :
          begin
            fMasterURLHandler.HandleEvent(evnAnimateBuildings, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'AnimateBuildings', enabled);
          end;
        2 :
          begin
            fMasterURLHandler.HandleEvent(evnShowCars, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'ShowCars', enabled);
          end;
        3 :
          begin
            fMasterURLHandler.HandleEvent(evnShowPlanes, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'ShowPlanes', enabled);
          end;
        4 :
          begin
            fMasterURLHandler.HandleEvent(evnTranspOverlays, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'TransparentOverlays', enabled);
          end;
        5 :
          begin
            fMasterURLHandler.HandleEvent(evnSoundsEnabled, enabled);
            fConfigHolder.WriteBoolean(false, fClientView.getUserName, 'EnableSounds', enabled);
          end;
        6 :
          fClientView.SetAutologon( AutomaticLogon.Checked );
      end;
    end;

  procedure TOptionsHandlerView.UpdateLogonClick(Sender: TObject);
    begin
      if AutomaticLogon.Checked
        then fClientView.SetAutologon( true );
    end;

  function GetSecCount(date : TDateTime) : integer;
    var
      hh, mm, ss, sss : word;
    begin
      DecodeTime(date, hh, mm, ss, sss);
      result := mm*60 + ss;
    end;

  function GetMSecCount(date : TDateTime) : integer;
    var
      hh, mm, ss, sss : word;
    begin
      DecodeTime(date, hh, mm, ss, sss);
      result := mm*60*1000 + ss*1000 + sss;
    end;

  procedure TOptionsHandlerView.TimerTimer(Sender: TObject);
    var
      tm     : TDateTime;
      total  : integer;
      deltat : integer;
      rate   : integer;
    begin
      if fCounting
        then
          begin
            lbByteSent.Caption := IntToStr(SocketComp.ByteSent - fByteSent);
            lbByteRecv.Caption := IntToStr(SocketComp.ByteRecv - fByteRecv);
            rate               := SocketComp.ByteSent + SocketComp.ByteRecv - fByteSent - fByteRecv;
            total              := SocketComp.ByteSent + SocketComp.ByteRecv - fInitialTotal;
            //lbTotal.Caption    := IntToStr(total);
            fByteRecv          := SocketComp.ByteRecv;
            fByteSent          := SocketComp.ByteSent;
            tm                 := Now;
            deltat             := GetSecCount(tm - fLastTime);
            fSecondCount       := fSecondCount + deltat;
            fLastTime          := tm;
            if deltat > 0
              then lbRate.Caption := GetFormattedLiteral('Literal315', [round(rate/deltat)])
              else lbRate.Caption := GetLiteral('Literal316');
            if fSecondCount > 0
              then lbSpeed.Caption := GetFormattedLiteral('Literal317', [round(total/fSecondCount)])
              else lbSpeed.Caption := GetLiteral('Literal318');
            lbTotalSent.Caption := IntToStr(SocketComp.ByteSent);
            lbTotalRecv.Caption := IntToStr(SocketComp.ByteRecv);
            lbLatency.Caption   := IntToStr(fLatency);
          end
        else fCounting := true;
      if (fLatFreqDiv = 0) or (fLatFreqDiv > 10)
        then
          begin
            fLatFreqDiv := 1;
            Threads.Fork(threadedRequestLatency, priNormal, [0]);
          end
        else inc(fLatFreqDiv);
    end;

  procedure TOptionsHandlerView.StartCounting;
    begin
      fInitialTotal := SocketComp.ByteSent + SocketComp.ByteRecv;
      fLastTime     := Now;
      fSecondCount  := 0;
    end;

  procedure TOptionsHandlerView.threadedRequestLatency(const parms : array of const);
    var
      res  : integer;
      date : TDateTime;
    begin
      date := Now;
      ClientView.Echo(integer(0));
      date := Now - date;
      res  := GetMSecCount(date);
      Threads.Join(threadedRenderLatency, [res]);
    end;

  procedure TOptionsHandlerView.threadedRenderLatency(const parms : array of const);
    begin
      fLatency := parms[0].vInteger;
    end;

  procedure TOptionsHandlerView.SyncNotify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    begin
      case EventId of
        syncEvnFileDone :
          fPlayListInt.AddFile( TaskDesc );
        syncEvnDone :
          begin
            OverallProg.Position := 0;
            Prog.Position        := 0;
            // btnDownload.Text := 'Download more music';
            btnDownload.Enabled := true;
            btnDownload.Tag := 0;
          end;
        else
          begin
            OverallProg.Position := OverallProgress;
            Prog.Position        := Progress;
          end;
      end;
      Cancel := false;
    end;

  procedure TOptionsHandlerView.Image1Click(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=' + tidHandlerName_Options + '&frame_Close=yes' );
    end;

  procedure TOptionsHandlerView.MusicVolChange(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=SetVolume&MediaId=MainSoundTrack&Volume=' + IntToStr(MusicVol.Value) );
    end;

  procedure TOptionsHandlerView.TabsOnFingerChange(Sender: TObject);
    begin
      Notebook.PageIndex := Tabs.CurrentFinger;
    end;

  procedure TOptionsHandlerView.PlaylistDblClick(Sender: TObject);
    begin
      if Playlist.Selected <> nil
        then fPlayListInt.JumpToFile( Playlist.Selected.Index );
    end;

  procedure TOptionsHandlerView.btnAddFileClick(Sender: TObject);
    begin
      if OpenDialog.Execute
        then fPlayListInt.AddFile( OpenDialog.filename );
    end;

  procedure TOptionsHandlerView.btnAddDirClick(Sender: TObject);
    var
      path : string;
    begin
      if SelectDirectory( path, [], 0 )
        then fPlayListInt.AddDir( path );
    end;

  procedure TOptionsHandlerView.btnDelFileClick(Sender: TObject);
    var
      i : integer;
    begin
      for i := pred(Playlist.Items.Count) downto 0 do
        if Playlist.Items[i].Selected
          then fPlayListInt.DelFile( i );
    end;

  procedure TOptionsHandlerView.PlaylistKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
    begin
      if Key = VK_Delete
        then btnDelFileClick( self );
    end;

  procedure TOptionsHandlerView.btnDownloadClick(Sender: TObject);
    var
      fCachePath : string;
    begin
      if btnDownload.Tag = 0
        then
          begin
            fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, fCachePath );
            fSyncTask := AsyncSynchronize( fClientView.getWorldURL + 'client/cache/sound/extra/', fCachePath + 'sound\extra\', SyncNotify, 1 );
            //btnDownload.Text := 'Stop';
            btnDownload.Enabled := false;
            btnDownload.Tag := 1;
          end
        else
          begin
            fSyncTask.Free;
            //btnDownload.Text := 'Download more music';
            btnDownload.Enabled := true;
            btnDownload.Tag := 0;
          end;
    end;

  procedure TOptionsHandlerView.IgnoreListClick(Sender: TObject);
    begin
      btRemoveFromIgnore.Enabled := IgnoreList.ItemIndex >= 0;
    end;

  procedure TOptionsHandlerView.btRemoveFromIgnoreClick(Sender: TObject);
    begin
      if IgnoreList.ItemIndex >= 0
        then fPrivacyHandler.ClearIgnoredUser( IgnoreList.Items[IgnoreList.ItemIndex] );
    end;

  procedure TOptionsHandlerView.FramedButton1Click(Sender: TObject);
    begin
      fPrivacyHandler.SetDefaultChannelData( edChannelName.Text, edPassword.Text );
    end;

  procedure TOptionsHandlerView.ShowAllClick(Sender: TObject);
    var
      dummy : integer;
      i     : integer;
    begin
      for i := 0 to pred(cValidFacIds) do
        FacIdsListBox.Checked[i] := true;
      fMasterURLHandler.HandleEvent(evnShowAllFacilities, dummy);
    end;

  procedure TOptionsHandlerView.HideAllClick(Sender: TObject);
    var
      dummy : integer;
      i     : integer;
    begin
      for i := 0 to pred(cValidFacIds) do
        FacIdsListBox.Checked[i] := false;
      fMasterURLHandler.HandleEvent(evnHideAllFacilities, dummy);
    end;

  procedure TOptionsHandlerView.FacIdsListBoxClickCheck(Sender: TObject);
    var
      hideshowdata : THideShowFacilityData;
    begin
      hideshowdata.show := FacIdsListBox.Checked[FacIdsListBox.ItemIndex];
      hideshowdata.facid := cFacIdData[FacIdsListBox.ItemIndex].id;
      fMasterURLHandler.HandleEvent(evnHideShowFacility, hideshowdata);
    end;

  procedure TOptionsHandlerView.Loaded;
    var
      i, selected  : integer;

    procedure InitOptionControls;
      var
        volume : single;
      begin
        EnableSounds.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'EnableSounds', true);
        UseTransparency.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'UseTransparency', true);
        AnimateBuildings.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'AnimateBuildings', true);
        ShowCars.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'ShowCars', true);
        ShowPlanes.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'ShowPlanes', true);
        TransparentOverlays.Checked := fConfigHolder.ReadBoolean(false, fClientView.getUserName, 'TransparentOverlays', true);
        volume := StrToFloat(fConfigHolder.ReadString(false, fClientView.getUserName, 'SoundFxVolume', '1'));
        SoundFXVolume.Value := round(SoundFXVolume.MidValue*volume);
        fMasterURLHandler.HandleEvent(evnSetSoundVolume, volume);
      end;

    begin
      inherited;
      selected := 0;
      for i := 0 to pred(cValidFacIds) do
        begin
          FacIdsListBox.Items.Add(GetLiteral(cFacIdData[i].name));
          FacIdsListBox.Checked[i] := true;
        end;
      InitOptionControls;
      for i := 0 to pred(fLangList.count) do
        begin
          cbLangList.Items.Add( fLangList.Names[i] );
          if fLangList.Values[fLangList.Names[i]] = ActiveLanguage
            then selected := i;
        end;
      cbLangList.ItemIndex := selected;
    end;

  procedure TOptionsHandlerView.SetMasterURLHandler( aMasterURLHandler : IMasterURLHandler );
    var
      CachePath : string;
    begin
      fMasterURLHandler := aMasterURLHandler;
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, fConfigHolder );
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
      fLangList := TStringList.Create;
      try
        fLangList.LoadFromFile( CachePath + 'lang.dat' );
      except
      end;
    end;

  procedure TOptionsHandlerView.SoundFXVolumeMoveBar(Sender: TObject);
    var
      volume : single;
    begin
      volume := SoundFXVolume.Value/SoundFXVolume.MidValue;
      fMasterURLHandler.HandleEvent(evnSetSoundVolume, volume);
      fConfigHolder.WriteString(false, fClientView.getUserName, 'SoundFxVolume', FloatToStr(volume));
    end;

  procedure TOptionsHandlerView.btnSetLangClick(Sender: TObject);
    var
      langid : string;
    begin
      langid := fLangList.Values[cbLangList.Text];
      fClientView.SetLanguage( langid );
      ClientMLS.SetLanguage( langid );
    end;

  procedure TOptionsHandlerView.cbLangListChange(Sender: TObject);
    begin
      btnSetLang.Enabled := cbLangList.Text <> '';
    end;

end.

