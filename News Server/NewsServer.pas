unit NewsServer;

interface

  uses
    Classes, Collection, News, RDOServer, RDOInterfaces, SyncObjs, Languages;

  type
    {$M+}
    TNewsServer =
      class
        public
          constructor Create( Port : integer; aPath : string; aLog : ILog );
          destructor  Destroy; override;
        private
          fPath        : string;
          fNewsCenters : TLockableCollection;
          fDAConn      : IRDOConnectionsServer;
          fDAServ      : TRDOServer;
          fNewsLock    : TCriticalSection;
          fLog         : ILog;
        public
          property NewsCenters : TLockableCollection read fNewsCenters;
        published
          procedure RDOCreateNewsCenter  ( World : widestring; DAAddr : widestring; DAPort : integer; LangId : widestring );
          procedure RDOCreateNewspaper   ( World, Name, Style, Town : widestring; aDAAddr : widestring; aDAPort : integer; LangId : widestring );
          procedure RDOGenerateNewspapers( World : widestring; Date : TDateTime; aDAAddr : widestring; aDAPort : integer; LangId : widestring );
        private
          function GetNewsCenter( World : string; Language : TLanguageId ) : TNewsCenter;
        private
          fAllowGenerate : boolean;
        public
          property AllowGenerate : boolean read fAllowGenerate write fAllowGenerate;
      end;
    {$M-}

  const
    tidRDOHook_NewsServer = 'NewsServer';

implementation

  uses
    SysUtils, IniFiles, FileCtrl, WinSockRDOConnectionsServer, NewsRegistry,
    Windows, Registry;

  constructor TNewsServer.Create( Port : integer; aPath : string; aLog : ILog );

    procedure RegisterNewsServer;
      var
        Reg : TRegistry;
      begin
        try
          Reg := TRegistry.Create;
          try
            Reg.RootKey := HKEY_LOCAL_MACHINE;
            if Reg.OpenKey( tidRegKey_News, true )
              then Reg.WriteString( 'Path', fPath )
          finally
            Reg.Free;
          end;
        except
        end
      end;

    begin
      inherited Create;
      fLog         := aLog;
      fPath        := aPath;
      fNewsCenters := TLockableCollection.Create( 0, rkBelonguer );
      fNewsLock    := TCriticalSection.Create;
      fDAConn      := TWinSockRDOConnectionsServer.Create( Port );
      fDAServ      := TRDOServer.Create( fDAConn as IRDOServerConnection, 7, nil );
      fDAServ.RegisterObject( tidRDOHook_NewsServer, integer(self) );
      fDAConn.StartListening;
      fDAServ.SetCriticalSection( fNewsLock );
      RegisterNewsServer;
      fLog.LogThis( 'News server started.' );
      fAllowGenerate := true; 
    end;

  destructor TNewsServer.Destroy;
    begin
      fDAServ.Free;
      fNewsCenters.Free;
      fNewsLock.Free;
      inherited;
    end;

  procedure TNewsServer.RDOCreateNewsCenter( World : widestring; DAAddr : widestring; DAPort : integer; LangId : widestring );
    var
      NewsCenter : TNewsCenter;
    begin
      try
        fLog.LogThis( 'Creating news center for ' + World + '.' );
        NewsCenter := GetNewsCenter( World, LangId );
        if NewsCenter = nil
          then
            begin
              NewsCenter := TNewsCenter.Create( World, fPath, fLog, DAAddr, DAPort, LangId );
              fNewsCenters.Insert( NewsCenter );
            end;
      except
        on E : Exception do
          fLog.LogThis( 'ERROR: ' + E.Message );
      end
    end;                                                                     
    
  procedure TNewsServer.RDOCreateNewspaper( World, Name, Style, Town : widestring; aDAAddr : widestring; aDAPort : integer; LangId : widestring );
    var
      NewsCenter : TNewsCenter;
      Newspaper  : TNewspaper;
      //Properties : TStringList;
      //path       : string;
    begin
      fLog.LogThis( 'Registering newspaper: ' + Name + ', world: ' + World + ', town: ' + Town + ', style : ' + Style );
      try
        fNewsLock.Enter;
        try
          NewsCenter := GetNewsCenter( World, LangId );
          if (Newscenter = nil) or (Newscenter <> nil) and (NewsCenter.GetNewspaper( Name ) = nil)
            then
              begin
                if NewsCenter = nil
                  then
                    begin
                      NewsCenter := TNewsCenter.Create( World, fPath, fLog, aDAAddr, aDAPort, LangId );
                      fNewsCenters.Insert( NewsCenter );
                    end;
                Newspaper := TNewspaper.Create( Name, Style, Town, NewsCenter );
                NewsCenter.Newspapers.Insert( Newspaper );
                {path := fPath + tidPath_Newspaper + World + '\' + Name;
                ForceDirectories( path );
                Properties := TStringList.Create;
                try
                  Properties.Values['Name']      := Name;
                  Properties.Values['Style']     := Style;
                  Properties.Values['Town']      := Town;
                  Properties.Values['LastIssue'] := '0';
                  Properties.SaveToFile( path + '\newspaper.ini' );
                finally
                  Properties.Free;
                end;}
              end;
        finally
          fNewsLock.Leave;
        end;
      except
        on E : Exception do
          fLog.LogThis( 'ERROR: ' + E.Message );
      end;
    end;
                                                                    
  procedure TNewsServer.RDOGenerateNewspapers( World : widestring; Date : TDateTime; aDAAddr : widestring; aDAPort : integer; LangId : widestring );
    var
      NewsCenter : TNewsCenter;
    begin
      if fAllowGenerate
        then
          try
            NewsCenter := GetNewsCenter( World, LangId );
            if NewsCenter = nil
              then
                begin
                  NewsCenter := TNewsCenter.Create( World, fPath, fLog, aDAAddr, aDAPort, LangId );
                  fNewsCenters.Insert( NewsCenter );
                end;
            NewsCenter.RenderNewspapers( Date );
          except
            on E : Exception do
              fLog.LogThis( 'ERROR: ' + E.Message );
          end;
    end;

  function TNewsServer.GetNewsCenter( World : string; Language : TLanguageId ) : TNewsCenter;
    var
      i : integer;
    begin
      fNewsLock.Enter;
      try
        i := 0;
        while (i < fNewsCenters.Count) and (TNewsCenter(fNewsCenters[i]).Name <> World) and (TNewsCenter(fNewsCenters[i]).LangId <> Language) do
          inc( i );
        if i < fNewsCenters.Count
          then result := TNewsCenter(fNewsCenters[i])
          else result := nil;
      finally
        fNewsLock.Leave;
      end;
    end;


end.
