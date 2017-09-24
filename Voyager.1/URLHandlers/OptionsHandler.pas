unit OptionsHandler;

interface

  uses
    Classes, VoyagerInterfaces, VoyagerServerInterfaces, Controls, OptionsHandlerViewer,
    Protocol, JukeBox, ChatHandler;

  type
    TMetaOptionsHandler =
      class( TInterfacedObject, IMetaURLHandler )
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TOptionsHandler =
      class( TInterfacedObject, IURLHandler, IPlayList, IPrivacyHandler )
        private
          constructor Create;
          destructor  Destroy; override;
        private
          fControl          : TOptionsHandlerView;
          fClientView       : IClientView;
          fMasterURLHandler : IMasterURLHandler;
          fCachePath        : string;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        // IPlayList
        private
          function  SelectedFile : string;
          procedure SelectNextFile;
          procedure Reset;
          procedure JumpToFile( index    : integer );
          procedure AddFile   ( filename : string );
          procedure AddDir    ( path     : string );
          procedure DelFile   ( index    : integer );
        private
          fPlayList     : TStringList;
          fSelectedFile : integer;
        private
          function CollectMediaFiles( path, pattern : string ) : TStringList;
        // IPrivacyHandler
        private
          procedure IgnoreUser( username : string );
          procedure ClearIgnoredUser( username : string );
          function  UserIsIgnored( username : string ) : boolean;
          procedure GetDefaultChannelData( out name, password : string );
          procedure SetDefaultChannelData( name, password : string );
        private
          fIgnoreList  : TStringList;
          fDefChannel  : string;
          fDefPassword : string;
          fLoaded      : boolean;
        private
          procedure LoadPrivData;
          procedure StorePrivData;
      end;

  const
    tidHandlerName_Options  = 'OptionsView';
    tidFileName_PlayList    = 'playlist.dat';
    tidFileName_IgnoreList  = 'ignore.dat';
    tidFileName_PrivInfo    = 'privacy.dat';
    tidFolderName_UserData  = 'userdata\';

implementation

  uses
    ServerCnxHandler, VoyagerUIEvents, Events, ServerCnxEvents, Config, SysUtils, FileCtrl;

  function TrimFileName( filename : string ) : string;
    var
      p : integer;
    begin
      result := ExtractFileName( filename );
      p := pos( '.', result );
      if p > 0
        then result := copy( result, 1, p - 1 );
    end;


  // TMetaOptionsHandler

  function TMetaOptionsHandler.getName : string;
    begin
      result := tidHandlerName_Options;
    end;

  function TMetaOptionsHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopEnabledWhenCached];
    end;

  function TMetaOptionsHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaOptionsHandler.Instantiate : IURLHandler;
    begin
      result := TOptionsHandler.Create;
    end;


  // TOptionsHandler

  constructor TOptionsHandler.Create;
    begin
      inherited Create;
      fControl := TOptionsHandlerView.Create( nil );
    end;

  destructor TOptionsHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  function TOptionsHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TOptionsHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;

    procedure InitPlayList( playlistfile : string );
      begin
        if FileExists( playlistfile )
          then
            begin
              fPlayList := TStringList.Create;
              fPlayList.LoadFromFile( playlistfile );
            end
          else
            begin
              ForceDirectories( ExtractFilePath( playlistfile ));
              fPlayList := CollectMediaFiles( fCachePath + '\Sound\', 'inmap*.mp3' );
            end;
      end;

    var
      PlayList       : IPlayList absolute info;
      PrivacyHandler : IPrivacyHandler absolute info;
      ConfigHolder   : IConfigHolder;
      i              : integer;
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          begin
            fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
            fControl.AutomaticLogon.Checked := ConfigHolder.ReadBoolean( false, '', 'AutoLogon', false );
            fControl.Tabs.CurrentFinger := 3;
            fControl.Tabs.CurrentFinger := 0;
            fControl.edChannelName.Text := fDefChannel;
            fControl.edPassword.Text    := fDefPassword;
            fControl.IgnoreList.Items.Assign( fIgnoreList );
            fControl.Playlist.Items.BeginUpdate;
            try
              fControl.Playlist.Items.Clear;
              for i := 0 to pred(fPlayList.count) do
                with fControl.Playlist.Items.Add do
                  begin
                    Caption := TrimFileName(fPlayList[i]);
                    if i = fSelectedFile
                      then StateIndex := 0
                      else StateIndex := -1;
                  end;
            finally
              fControl.Playlist.Items.EndUpdate;
            end;
            if fPlayList.Count > 0
              then fControl.Playlist.Items[fSelectedFile].MakeVisible( false );
          end;
        evnHandlerUnexposed :
          begin
          end;
        evnGetPlaylist :
          begin
            PlayList := self;
            if fPlayList = nil
              then InitPlayList( fCachePath + tidFolderName_UserData + fClientView.getUserName + '\' + tidFileName_PlayList );
          end;
        evnAnswerPrivacyHandler :
          begin
            PrivacyHandler := self;
            if not fLoaded
              then
                begin
                  LoadPrivData;
                  fLoaded := true;
                end;
          end;
        else
          result := evnNotHandled;
      end;
    end;

  function TOptionsHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TOptionsHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      fMasterURLHandler := URLHandler;
      fControl.ClientView       := fClientView;
      fControl.MasterURLHandler := fMasterURLHandler;
      fControl.PlayListInt      := self;
      fControl.PrivacyHandler   := self;
      fControl.StartCounting;
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, fCachePath );
    end;

  function TOptionsHandler.SelectedFile : string;
    begin
      if (fPlayList.Count > 0) and (fSelectedFile >= 0)
        then result := fPlayList[fSelectedFile]
        else result := '';
    end;

  procedure TOptionsHandler.SelectNextFile;
    begin
      if (fControl.Playlist <> nil) and (fSelectedFile >= 0)
        then fControl.Playlist.Items[fSelectedFile].StateIndex := -1;
      if fSelectedFile < pred(fPlayList.count)
        then inc(fSelectedFile)
        else fSelectedFile := 0;
      if (fControl.Playlist <> nil) and (fSelectedFile >= 0)
        then fControl.Playlist.Items[fSelectedFile].StateIndex := 0;
    end;

  procedure TOptionsHandler.Reset;
    begin
      if fControl.Playlist <> nil
        then fControl.Playlist.Items[fSelectedFile].StateIndex := -1;
      fSelectedFile := 0;
      if fControl.Playlist <> nil
        then fControl.Playlist.Items[fSelectedFile].StateIndex := 0;
    end;

  procedure TOptionsHandler.JumpToFile( index : integer );
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=JukeBox&frame_Class=JukeBox&frame_Action=Stop' );
      if (fControl.Playlist <> nil) and (fSelectedFile >= 0)
         then fControl.Playlist.Items[fSelectedFile].StateIndex := -1;
      fSelectedFile := index;
      if (fControl.Playlist <> nil) and (fSelectedFile >= 0)
         then fControl.Playlist.Items[fSelectedFile].StateIndex := 0;
      fMasterURLHandler.HandleURL( '?frame_Id=JukeBox&frame_Class=JukeBox&frame_Action=Play' );
    end;

  procedure TOptionsHandler.AddFile( filename : string );
    var
      folder : string;
    begin
      if fPlayList.IndexOf( filename ) = -1
        then
          begin
            fPlayList.Add( filename );
            if fControl.Playlist <> nil
              then
                with fControl.Playlist.Items.Add do
                  begin
                    Caption := TrimFileName(filename);
                    MakeVisible( false );
                  end;
            folder := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\';
            ForceDirectories( folder );
            fPlayList.SaveToFile( folder + tidFileName_PlayList );
          end;
    end;

  procedure TOptionsHandler.AddDir( path : string );
    var
      files  : TStringList;
      i      : integer;
      folder : string;
    begin
      files := CollectMediaFiles( path, '*.mp3' );
      try
        for i := 0 to pred(files.Count) do
          if fPlayList.IndexOf( files[i] ) = -1
            then
              begin
                fPlayList.Add( files[i] );
                if fControl.Playlist <> nil
                  then
                    with fControl.Playlist.Items.Add do
                      begin
                        Caption := TrimFileName(files[i]);
                        MakeVisible( false );
                      end;
              end;
      finally
        files.Free;
      end;
      folder := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\';
      ForceDirectories( folder );
      fPlayList.SaveToFile( folder + tidFileName_PlayList );
    end;

  procedure TOptionsHandler.DelFile( index : integer );
    var
      folder : string;
    begin
      if index = fSelectedFile
        then Reset;
      fPlayList.Delete( index );
      if fControl.Playlist <> nil
        then fControl.Playlist.Items.Delete( index );
      folder := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\';
      ForceDirectories( folder );
      fPlayList.SaveToFile( folder + tidFileName_PlayList );
    end;

  function TOptionsHandler.CollectMediaFiles( path, pattern : string ) : TStringList;
    var
      CachePath : string;
      SearchRec : TSearchRec;
      found     : boolean;
    begin
      if path[length(path)] <> '\'
        then path := path + '\';
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
      result := TStringList.Create;
      if CachePath <> ''
        then
          begin
            found := FindFirst( path + pattern, faArchive, SearchRec ) = 0;
            try
              while found do
                begin
                  result.Add( path + SearchRec.Name );
                  found := FindNext( SearchRec ) = 0;
                end;
            finally
              FindClose( SearchRec );
            end;
          end;
    end;

  procedure TOptionsHandler.IgnoreUser( username : string );
    begin
      if not UserIsIgnored( username )
        then
          begin
            fIgnoreList.Add( username );
            fControl.IgnoreList.Items.Assign( fIgnoreList );
            StorePrivData;
          end;
    end;

  procedure TOptionsHandler.ClearIgnoredUser( username : string );
    var
      idx : integer;
    begin
      try
        idx := fIgnoreList.IndexOf( username );
        fIgnoreList.Delete( idx );
        fControl.IgnoreList.Items.Assign( fIgnoreList );
        StorePrivData;
      except
      end;
    end;

  function TOptionsHandler.UserIsIgnored( username : string ) : boolean;
    begin
      result := fIgnoreList.IndexOf( username ) <> -1
    end;

  procedure TOptionsHandler.GetDefaultChannelData( out name, password : string );
    begin
      name := fDefChannel;
      password := fDefPassword;
    end;

  procedure TOptionsHandler.SetDefaultChannelData( name, password : string );
    begin
      fDefChannel  := name;
      fDefPassword := password;
      StorePrivData;
    end;
    
  procedure TOptionsHandler.LoadPrivData;
    var
      filename : string;
      TmpInfo  : TStringList;
    begin
      fIgnoreList := TStringList.Create;
      try
        filename := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\' + tidFileName_IgnoreList;
        if FileExists( filename )
          then fIgnoreList.LoadFromFile( filename );
        filename := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\' + tidFileName_PrivInfo;
        if FileExists( filename )
          then
            begin
              TmpInfo := TStringList.Create;
              try
                TmpInfo.LoadFromFile( filename );
                fDefChannel  := TmpInfo.Values['DefChannel'];
                fDefPassword := TmpInfo.Values['DefPassword'];
              finally
                TmpInfo.Free;
              end;
            end;
      except
        fIgnoreList.Clear;
      end;
    end;

  procedure TOptionsHandler.StorePrivData;
    var
      folder  : string;
      TmpInfo : TStringList;
    begin
      try
        folder := fCachePath + tidFolderName_UserData + fClientView.getUserName + '\';
        ForceDirectories( folder );
        fIgnoreList.SaveToFile( folder + tidFileName_IgnoreList );
        TmpInfo := TStringList.Create;
        try
          TmpInfo.Values['DefChannel'] := fDefChannel;
          TmpInfo.Values['DefPassword'] := fDefPassword;
          TmpInfo.SaveToFile( folder + tidFileName_PrivInfo );
        finally
          TmpInfo.Free;
        end;
      except
      end;
    end;


end.


