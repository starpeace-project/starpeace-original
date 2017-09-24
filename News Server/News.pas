unit News;

interface

  uses
    Classes, Collection, SysUtils, Persistent, BackupInterfaces, SyncObjs, Languages;

  type
    TSourceHTML = string;
    THTML       = string;

  const
    HistoryMax = 10;
    IssueMax   = MaxInt;

  type
    TPoliticalOrientation = (polUltraRight, polRight, polNeutral, polLeft, polUltraLeft);
    TProbFunc             = function( date : integer ) : single;

  type
    TStoryReqs =
      record
        Length      : integer;
        Orientation : TPoliticalOrientation;
      end;

  type
    ILog =
      interface
        procedure LogThis( str : string );
      end;

  function StoryReqs( aLength : integer; aOrientation : TPoliticalOrientation ) : TStoryReqs;

  type
    TStyle        = class;
    TLayout       = class;
    TFrame        = class;
    TMetaReporter = class;
    TReporter     = class;
    TSymbol       = class;
    TStory        = class;
    TNewspaper    = class;
    TNewsCenter   = class;

    CMetaReporter = class of TMetaReporter;
    
    TNamedItem =
      class( TPersistent )
        public
          constructor Create( aName : string );
        private
          fName : string;
        public
          property Name : string read fName;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TStyle =
      class( TNamedItem )
        public
          function RenderCSSLink    : THTML;
          function RenderHeaderLink : THTML;
          function RenderFooterLink : THTML;
      end;

    TLayout =
      class( TNamedItem )
        public
          destructor Destroy; override;
        private
          fFrames   : TSortedCollection;
          fFrameSec : TCollection;
          fBody     : THTML;
        public
          property Frames   : TSortedCollection read fFrames;
          property FrameSec : TCollection       read fFrameSec;
          property Body     : string            read fBody;
        public
          procedure LoadFromTemplate( filename : TSourceHTML );
          function  Render( Stories : TStringList ) : THTML;
        private
          function CompareFrames( Frame1, Frame2 : TObject ) : integer;
      end;

    TFrame =
      class( TNamedItem )
        private
          fLayout     : TLayout;
          fOffset     : integer;
          fImportance : integer;
          fLength     : integer;
        public
          property Layout     : TLayout read fLayout;
          property Offset     : integer read fOffset;
          property Importance : integer read fImportance;
          property Length     : integer read fLength;
      end;

    TMetaReporter =
      class( TNamedItem )
        public
          constructor Create( aName, filename : string; aLog : ILog ); virtual;
          destructor  Destroy; override;
        private
          fStories  : TCollection;
          fProbFunc : TProbFunc;
          fLog      : ILog;
        public
          property Stories  : TCollection read fStories  write fStories;
          property ProbFunc : TProbFunc   read fProbFunc write fProbFunc;
        protected
          property Log : ILog read fLog;
        public
          function RenderStory( StoryReqs : TStoryReqs; Newspaper : TNewspaper; Reporter : TReporter ) : THTML; virtual;
        public
          function StoryStrength( aNewspaper : TNewspaper ) : integer; virtual;
          function SolveSymbol( Symbol, DefVal : string; aNewspaper : TNewspaper ) : THTML; virtual;
        private
          procedure StoryFound( path : string; SearchRec : TSearchRec );
      end;

    TSymbol =
      class( TNamedItem )
        private
          fStory  : TStory;
          fOffset : integer;
          fDefVal : string;
        public
          property Story  : TStory  read fStory;
          property Offset : integer read fOffset;
          property DefVal : string  read fDefVal;
      end;

    TStory =
      class( TNamedItem )
        public
          constructor Load( aMetaReporter : TMetaReporter; filename : string );
          destructor  Destroy; override;
        private
          fMetaReporter : TMetaReporter;
          fReqs         : TStoryReqs;
          fBody         : TSourceHTML;
          fSymbols      : TCollection;
        public
          property Reqs    : TStoryReqs  read fReqs;
          property Body    : TSourceHTML read fBody;
          property Symbols : TCollection read fSymbols;
      end;

    TReporter =
      class( TPersistent )
        public
          constructor Create( aMetaReporter : TMetaReporter; aNewspaper : TNewspaper );
          destructor  Destroy; override;
        private
          fMetaReporter : TMetaReporter;
          fNewspaper    : TNewspaper;
          fLastStory    : integer;
          fStrength     : integer;
          fLapse        : integer;
        public
          property MetaReporter : TMetaReporter read fMetaReporter;
          property Newspaper    : TNewspaper    read fNewspaper;
          property LastStory    : integer       read fLastStory;
          property Strength     : integer       read fStrength;
          property Lapse        : integer       read fLapse;
        public
          function  Render( StoryReqs : TStoryReqs ) : THTML;
          procedure Update; virtual;
      end;

    TNewspaper =
      class( TNamedItem )
        public
          constructor Create( aName, aStyle, aTown : string; aNewsCenter : TNewsCenter );
          destructor  Destroy; override;
        private
          fNewsCenter  : TNewsCenter;
          fStyle       : TStyle;
          fTown        : string;
          fLastIssue   : integer;
          fReporters   : TSortedCollection;
          fIssue       : integer;
          fLastDate    : TDateTime;
          fTownProxy   : olevariant;
          fSpecialSymb : TStringList;
        public
          property NewsCenter : TNewsCenter       read fNewsCenter;
          property Town       : string            read fTown;
          property Style      : TStyle            read fStyle;
          property LastIssue  : integer           read fLastIssue write fLastIssue;
          property Reporters  : TSortedCollection read fReporters;
        public
          function Render( Date : TDateTime ) : THTML;
          function SolveSymbol( Symbol, DefVal : string ) : THTML; virtual;
        private
          function CompareReporters( Rep1, Rep2 : TObject ) : integer;
        public
          procedure ManageSite;
      end;

    TNewsCenter =
      class( TNamedItem )
        public
          constructor Create( aName, aNewsPath : string; aLog : ILog; aDAAddr : string; aDAPort : integer; aLangId : TLanguageId );
          destructor  Destroy; override;
        private
          fNewsPath      : string;
          fMetaReporters : TCollection;
          fStyles        : TCollection;
          fLayouts       : TCollection;
          fNewspapers    : TCollection;
          fLog           : ILog;
          fLock          : TCriticalSection;
          fDAAddr        : string;
          fDAPort        : integer;
          fLangId        : TLanguageId;
        public
          property NewsPath      : string      read fNewsPath;
          property MetaReporters : TCollection read fMetaReporters;
          property Styles        : TCollection read fStyles;
          property Layouts       : TCollection read fLayouts;
          property Newspapers    : TCollection read fNewspapers;
          property DAAddr        : string      read fDAAddr;
          property DAPort        : integer     read fDAPort;
          property LangId        : TLanguageId read fLangId;
        protected
          property Log : ILog read fLog;
        public
          procedure RenderNewspapers( Date : TDateTime );
          procedure ReloadReporters;
          function  GetStyle    ( Name : string ) : TStyle;
          function  GetNewspaper( Name : string ) : TNewspaper;
        private
          procedure ReporterFound ( path : string; SearchRec : TSearchRec );
          procedure LayoutFound   ( path : string; SearchRec : TSearchRec );
          procedure StyleFound    ( path : string; SearchRec : TSearchRec );
          procedure NewspaperFound( path : string; SearchRec : TSearchRec );
      end;

  const
    tidSymbol_MaleName   = 'MALENAME';
    tidSymbol_FemaleName = 'FEMALENAME';
    tidSymbol_LastName   = 'LASTNAME';
    tidSymbol_CityName   = 'CITYNAME';

  const
    tidPath_Reporters = 'Reporters\';
    tidPath_Layouts   = 'Layouts\';
    tidPath_Newspaper = 'Newspapers\';
    tidPath_Boards    = 'Boards\';     
    tidPath_Styles    = 'Styles\';

  const
    tidStyle_Standard = 'Standard';

  const
    tidHTML_NewsMarkOpen  = '<+';
    tidHTML_NewsMarkClose = '+>';

  procedure InitNews;
  procedure DoneNews;

  procedure RegisterMetaReporterType( Id : string; MetaReporterType : CMetaReporter );
  function  GetMetaReporterType( Id : string ) : CMetaReporter;

implementation

  uses
    ActiveX, StrUtils, Windows, ShellAPI, CompStringsParser, Protocol, IniFiles,
    FileCtrl, ComObj;

  type
    TFolderIterator = procedure( path : string; SearchRec : TSearchRec ) of object;

  procedure FolderIterate( path, mask : string; Attr : integer; FolderIterator : TFolderIterator );
    var
      SearchRec : TSearchRec;
      found     : integer;
    begin
      if mask = ''
        then mask := '*.*';
      found := SysUtils.FindFirst( path + mask, Attr, SearchRec );
      try
        while found = 0 do
          begin
            FolderIterator( path, SearchRec );
            found := FindNext( SearchRec );
          end;
      finally
        SysUtils.FindClose( SearchRec );
      end;
    end;

  function StoryReqs( aLength : integer; aOrientation : TPoliticalOrientation ) : TStoryReqs;
    begin
      result.Length      := aLength;
      result.Orientation := aOrientation;
    end;

  function GetQuotedStr( var str : string; var at : integer; OpenQuote, CloseQuote : string; RemoveQuote : boolean ) : string;
    var
      p1, p2, p3 : integer;
    begin
      p1 := pos( OpenQuote, str, at );
      if p1 > 0
        then
          begin
            p3 := p1;
            inc( p1, length(OpenQuote) );
            p2 := pos( CloseQuote, str, p1 );
            if p2 > 0
              then
                begin
                  result := copy( str, p1, p2 - p1 );
                  inc( p2, length(CloseQuote) );
                  if RemoveQuote
                    then
                      begin
                        delete( str, p3, p2 - p3 );
                        at := p3;
                      end
                    else at := p2;
                end
              else result := '';
          end
        else result := '';
    end; 


  // TNamedItem

  constructor TNamedItem.Create( aName : string );
    begin
      inherited Create;
      fName := aName;
    end;

  procedure TNamedItem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fName := Reader.ReadString( 'Name', '' );
    end;

  procedure TNamedItem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'Name', fName );
    end;

    
  // TStyle

  function TStyle.RenderCSSLink : THTML;
    begin
      result :=
        '<link rel="STYLESHEET" href="../../../../styles/default.css" type="text/css">' + LineBreak +
        '<link rel="STYLESHEET" href="../../../../styles/' + Name + '/' + Name + '.css" type="text/css">';
    end;

  function TStyle.RenderHeaderLink : THTML;
    begin
      result := '<!--#include file="..\..\..\..\styles\' + Name + '\' + Name + '.header" -->';
    end;

  function TStyle.RenderFooterLink : THTML;
    begin
      result := '<!--#include file="..\..\..\..\styles\' + Name + '\' + Name + '.footer" -->';
    end;


  // TLayout

  destructor TLayout.Destroy;
    begin
      fFrames.Free;
      fFrameSec.Free;
      inherited;
    end;

  procedure TLayout.LoadFromTemplate( filename : TSourceHTML );

    procedure InitFrames( buffer : string );
      var
        p1, p2    : integer;
        framedef  : string;
        framename : string;
        frameimp  : string;
        framesize : string;
        Frame     : TFrame;
      begin
        fFrames   := TSortedCollection.Create( 0, rkBelonguer, CompareFrames );
        fFrameSec := TCollection.Create( 0, rkUse );
        p1 := 1;
        repeat
          framedef := KillSpaces( GetQuotedStr( buffer, p1, tidHTML_NewsMarkOpen, tidHTML_NewsMarkClose, true ));
          if framedef <> ''
            then
              begin
                p2 := 1;
                framename := GetNextString( framedef, p2, [','] );
                frameimp  := GetNextString( framedef, p2, [','] );
                framesize := GetNextString( framedef, p2, [','] );
                if (framename <> '') and (frameimp <> '') and (framesize <> '')
                  then
                    begin
                      Frame := TFrame.Create( framename );
                      Frame.fImportance := StrToInt( frameimp );
                      Frame.fLength     := StrToInt( framesize );
                      Frame.fOffset     := p1;
                      fFrames.Insert( Frame );
                      fFrameSec.Insert( Frame );
                    end;
              end;
        until framedef = '';
        fBody := buffer;
      end;

    var
      Stream : TFileStream;
      buffer : string;
    begin
      Stream := TFileStream.Create( filename, fmOpenRead );
      try
        setlength( buffer, Stream.Size );
        Stream.ReadBuffer( buffer[1], Stream.Size );
        InitFrames( buffer );
      finally
        Stream.Free;
      end;
    end;

  function TLayout.Render( Stories : TStringList ) : THTML;
    var
      delta  : integer;
      i, idx : integer;
    begin
      if Stories.Count = Frames.Count
        then
          begin
            result := Body;
            delta  := 0;
            for i := 0 to pred(FrameSec.Count) do
              with TFrame(FrameSec[i]) do
                begin
                  idx := Frames.IndexOf( FrameSec[i] );
                  insert( Stories[idx], result, Offset + delta );
                  inc( delta, system.length(Stories[idx]) );
                end
          end
        else result := '';
    end;

  function TLayout.CompareFrames( Frame1, Frame2 : TObject ) : integer;
    begin
      if TFrame(Frame1).Importance > TFrame(Frame2).Importance
        then result := 1
        else
          if TFrame(Frame1).Importance < TFrame(Frame2).Importance
            then result := -1
            else result := 0;
    end;


  // Prob functions

  function RampProb( x : integer ) : single;
    begin
      if x < HistoryMax
        then result := (HistoryMax - x)/HistoryMax
        else result := 0;
    end;

    
  // TMetaReporter

  constructor TMetaReporter.Create( aName, filename : string; aLog : ILog );

    procedure LoadStories( filename : string );
      begin
        FolderIterate( filename, '*.story', faArchive, StoryFound );
      end;

    begin
      inherited Create( aName );
      fStories  := TCollection.Create( 0, rkBelonguer );
      fProbFunc := RampProb;
      fLog      := aLog;
      LoadStories( filename );
    end;

  destructor TMetaReporter.Destroy;
    begin
      fStories.Free;
      inherited;
    end;

  function TMetaReporter.RenderStory( StoryReqs : TStoryReqs; Newspaper : TNewspaper; Reporter : TReporter ) : THTML;

    function EvaluateStory( Story : TStory; Newspaper : TNewspaper ) : THTML;
      var
        delta : integer;
        i     : integer;
        value : string;
      begin
        delta  := 0;
        result := Story.Body;
        for i := 0 to pred(Story.Symbols.Count) do
          with TSymbol(Story.Symbols[i]) do
            begin
              value := SolveSymbol( Name, DefVal, Newspaper );
              insert( value, result, Offset + delta );
              inc( delta, length(value) );
            end;
      end;

    {
    var
      i            : integer;
      min, mindist : integer;
      Story        : TStory;
    }
    begin
      if Stories.Count > 0
        then result := EvaluateStory( TStory(Stories[random(Stories.Count)]), Newspaper )
        else result := '';
      {
      mindist := high(mindist);
      min     := -1;
      for i := 0 to pred(Stories.Count) do
        with TStory(Stories[i]) do
          if abs(StoryReqs.Length - Reqs.Length) < mindist
            then
              begin
                min     := i;
                mindist := abs(StoryReqs.Length - Reqs.Length);
              end;
      if min <> NoIndex
        then
          begin
            Story := TStory(Stories[min]);
            result := EvaluateStory( Story, Newspaper );
          end
        else result := '';
        }
    end;

  function TMetaReporter.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      result := 1 + random(20);
    end;

  function TMetaReporter.SolveSymbol( Symbol, DefVal : string; aNewspaper : TNewspaper ) : THTML;
    begin
      result := aNewspaper.SolveSymbol( Symbol, DefVal );
    end;

  procedure TMetaReporter.StoryFound( path : string; SearchRec : TSearchRec );
    var
      Story : TStory;
    begin
      Story := TStory.Load( self, path + SearchRec.Name );
      fStories.Insert( Story );
      Log.LogThis( '    Story ' + SearchRec.Name + ' was succefully parsed.' );
    end;


  // TStory

  constructor TStory.Load( aMetaReporter : TMetaReporter; filename : string );

    procedure InitStory( var buffer : string );
      var
        p        : integer;
        storydef : string;
      begin
        p := 1;
        storydef := GetQuotedStr( buffer, p, tidHTML_NewsMarkOpen, tidHTML_NewsMarkClose, true );
        if storydef <> ''
          then
            begin
              p := 1;
              fName := GetNextString( storydef, p, [','] );
              fReqs.Orientation := TPoliticalOrientation(StrToInt(KillSpaces(GetNextString( storydef, p, [','] ))));
            end;
      end;

    procedure InitSymbols( var buffer : string );
      var
        p1, p2       : integer;
        symboldef    : string;
        symbolname   : string;
        symboldefval : string;
        Symbol       : TSymbol;
      begin
        fSymbols := TCollection.Create( 0, rkBelonguer );
        p1 := 1;
        repeat
          symboldef := KillSpaces( GetQuotedStr( buffer, p1, tidHTML_NewsMarkOpen, tidHTML_NewsMarkClose, true ));
          if symboldef <> ''
            then
              begin
                p2 := 1;
                symbolname     := uppercase(GetNextString( symboldef, p2, [','] ));
                symboldefval   := GetNextString( symboldef, p2, [','] );
                Symbol         := TSymbol.Create( symbolname );
                Symbol.fStory  := self;
                Symbol.fOffset := p1;
                Symbol.fDefVal := symboldefval;
                fSymbols.Insert( Symbol );
              end;
        until symboldef = '';
      end;

    var
      Stream : TFileStream;
      buffer : string;
    begin
      inherited Create( ExtractFileName( filename ));
      fMetaReporter := aMetaReporter;
      Stream := TFileStream.Create( filename, fmOpenRead );
      try
        setlength( buffer, Stream.Size );
        Stream.ReadBuffer( buffer[1], Stream.Size );
        InitStory( buffer );
        InitSymbols( buffer );
        fBody := buffer;
      finally
        Stream.Free;
      end;
    end;

  destructor TStory.Destroy;
    begin
      fSymbols.Free;
      inherited;
    end;


  // TReporter

  constructor TReporter.Create( aMetaReporter : TMetaReporter; aNewspaper : TNewspaper );
    begin
      inherited Create;
      fMetaReporter := aMetaReporter;
      fNewspaper    := aNewspaper;
      fLastStory    := random(HistoryMax);
    end;

  destructor TReporter.Destroy;
    begin
      inherited;
    end;

  function TReporter.Render( StoryReqs : TStoryReqs ) : THTML;
    begin
      result := MetaReporter.RenderStory( StoryReqs, Newspaper, self );
    end;

  procedure TReporter.Update;
    begin
      fStrength := round(MetaReporter.StoryStrength( fNewspaper )*MetaReporter.ProbFunc( fLastStory ));
    end;


  // TNewspaper

  constructor TNewspaper.Create( aName, aStyle, aTown : string; aNewsCenter : TNewsCenter );
    var
      i        : integer;
      Reporter : TReporter;
    begin
      inherited Create( aName );
      fTown        := aTown;
      fNewsCenter  := aNewsCenter;
      fReporters   := TSortedCollection.Create( 0, rkBelonguer, CompareReporters );
      fStyle       := fNewsCenter.GetStyle( aStyle );
      fSpecialSymb := TStringList.Create;
      if fStyle = nil
        then fStyle := fNewsCenter.GetStyle( tidStyle_Standard );
      for i := 0 to pred(fNewsCenter.MetaReporters.Count) do
        begin
          Reporter := TReporter.Create( TMetaReporter(fNewsCenter.MetaReporters[i]), self );
          fReporters.Insert( Reporter );
        end;
    end;

  destructor TNewspaper.Destroy;
    begin
      fSpecialSymb.Free;
      fReporters.Free;
      inherited;
    end;

  function TNewspaper.Render( Date : TDateTime ) : THTML;

    procedure InitSpecialSymbols;
      var
        count : integer;
        i     : integer;
        name  : string;
      begin
        try
          fSpecialSymb.Clear;
          count := StrToInt( fTownProxy.Properties( 'srvCount' ) );
          for i := 0 to pred(count) do
            begin
              name := 'Town.Services.' + KillSpaces(fTownProxy.Properties( 'svrName' + IntToStr(i) )) + '.';
              fSpecialSymb.Add( uppercase(name + 'MarketPrice=') + fTownProxy.Properties( 'svrMarketPrice' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Demand=') + fTownProxy.Properties( 'svrDemand' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Offer=') + fTownProxy.Properties( 'svrOffer' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Capacity=') + fTownProxy.Properties( 'svrCapacity' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Ratio=') + fTownProxy.Properties( 'svrRatio' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Quality=') + fTownProxy.Properties( 'svrQuality' + IntToStr(i) ));
              fSpecialSymb.Add( uppercase(name + 'Price=') + fTownProxy.Properties( 'svrPrice' + IntToStr(i) ));
            end;
        except
          NewsCenter.Log.LogThis( '    ERROR: Cannot init special symbols.' );
        end;
      end;

    const
      LB = LineBreak;
      TB = TabSpace;
    var
      Layout   : TLayout;
      body     : THTML;
      i, idx   : integer;
      Stories  : TStringList;
      story    : THTML;
      Reqs     : TStoryReqs;
      Reporter : TReporter;
    begin
      try
        OleInitialize( nil );
        try
          fTownProxy := CreateOleObject( 'CacheManager.CachedObject' );
          fTownProxy.Recache := false;
          if (Town = '') or not VarIsEmpty(fTownProxy) and fTownProxy.SetWorld( NewsCenter.Name ) and fTownProxy.SetPath( 'Towns\' + Town + '.five\' )
            then
              begin
                NewsCenter.Log.LogThis( '    Connection to cache sucessfully established.' );
                if Town <> ''
                  then InitSpecialSymbols;
                // Generate header
                fLastDate := Date;
                result :=
                  '<% NewspaperName = "' + Name + ' "%>' + LB +
                  '<% NewspaperDate = #' + DateToStr(Date) + '# %>' + LB +
                  '<% NewspaperTown = "' + Town + '" %>' + LB +
                  '<html>' + LB +
                  TB + '<head>' + LB +
                  TB + TB + fStyle.RenderCSSLink + LB +
                  TB + '</head>' + LB +
                  TB + '<body>' + LB +
                  TB + TB + fStyle.RenderHeaderLink + LB;

                // Generate body
                Layout := TLayout(NewsCenter.Layouts[random(NewsCenter.Layouts.Count)]);
                for i := 0 to pred(Reporters.Count) do
                  with TReporter(Reporters[i]) do
                    begin
                      fLapse := 0;
                      Update;
                    end;
                fReporters.Sort( CompareReporters );
                Stories := TStringList.Create;
                Reqs.Orientation := polNeutral;
                idx := -1;
                NewsCenter.Log.LogThis( '    Selected reporters:' );
                for i := 0 to pred(Layout.Frames.Count) do
                  with TFrame(Layout.Frames[i]) do
                    begin
                      Reqs.Length := Length;
                      repeat
                        if idx < pred(Reporters.Count)
                          then inc( idx )
                          else idx := 0;
                      until TReporter(Reporters[idx]).Strength <> 0;
                      Reporter := TReporter(Reporters[idx]);
                      story    := '<% LayoutImp = "' + Name + '" %>' + LB;
                      story    := story + Reporter.Render( Reqs );
                      Reporter.fLastStory := 0;
                      inc( Reporter.fLapse );
                      NewsCenter.Log.LogThis( '      ' + IntToStr(i) + '. ' + Reporter.MetaReporter.Name );
                      Stories.Add( story );
                    end;
                body := Layout.Render( Stories );
                Stories.Free;
                result := result + body + LB;

                // Close document
                result :=
                  result + LB +
                  TB + TB + fStyle.RenderFooterLink + LB +
                  TB + '</body>' + LB +
                  '</html>';

                inc( fIssue );
                for i := Layout.Frames.Count to pred(Reporters.Count) do
                  inc( TReporter(Reporters[i]).fLastStory );
              end
            else result := '';
          fTownProxy := null;
        finally
          OleUninitialize;
        end;
      except
        result := '';
      end;
    end;

  var
    MaleNames   : TStringList = nil;
    FemaleNames : TStringList = nil;
    LastNames   : TStringList = nil;

  function TNewspaper.SolveSymbol( Symbol, DefVal : string ) : THTML;

    function GetTownSymbol( Symbol, DefVal : string ) : THTML;
      var
        p    : integer;
        prop : string;
      begin
        result := fSpecialSymb.Values[Symbol];
        if result = ''
          then
            try
              p      := system.pos( '.', Symbol );
              prop   := system.copy( Symbol, p + 1, length(Symbol) - p );
              result := fTownProxy.Properties( prop );
            except
              result := '';
            end;
      end;

    begin
      Symbol := uppercase(Symbol);
      if Symbol = tidSymbol_MaleName
        then result := MaleNames[random(MaleNames.Count)]
        else
          if Symbol = tidSymbol_FemaleName
            then result := FemaleNames[random(FemaleNames.Count)]
            else
              if Symbol = tidSymbol_LastName
                then result := LastNames[random(LastNames.Count)]
                else
                  if Symbol = tidSymbol_CityName
                    then result := Town
                    else
                      if (system.pos( 'TOWN.', Symbol ) <> 0) and (Town <> '')
                        then result := GetTownSymbol( Symbol, DefVal )
                        else result := DefVal;
    end;

  function TNewspaper.CompareReporters( Rep1, Rep2 : TObject ) : integer;
    begin
      if TReporter(Rep1).Strength < TReporter(Rep2).Strength
        then result := 1
        else
          if TReporter(Rep1).Strength > TReporter(Rep2).Strength
            then result := -1
            else result := 0;
    end;

  procedure TNewspaper.ManageSite;
    const
      MaxIssues = 12;

    function RemoveFullPath(const Path : string) : boolean;
      var
        FileOp : TSHFileOpStruct;
        tmp    : array[0..MAX_PATH] of char;
      begin
        fillchar(tmp, sizeof(tmp), 0);
        strpcopy(tmp, Path);
        // If Path is a folder the last '\' must be removed.
        if Path[length(Path)] = '\'
          then tmp[length(Path)-1] := #0;
        with FileOp do
          begin
            wFunc  := FO_DELETE;
            Wnd    := 0;
            pFrom  := tmp;
            pTo    := nil;
            fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
            hNameMappings := nil;
          end;
        result := SHFileOperation( FileOp ) = 0;
      end;

    var
      path      : string;
      Issues    : TStringList;
      SearchRec : TSearchRec;
      found     : integer;
    begin
      path := fNewsCenter.NewsPath + tidPath_Newspaper + fNewsCenter.Name + '\' + Name + '\';
      Issues := TStringList.Create;
      Issues.Sorted := true;
      found := SysUtils.FindFirst( path + '*.*', faDirectory, SearchRec );
      try
        while found = 0 do
          begin
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
              then Issues.Add( SearchRec.Name );
            found := FindNext( SearchRec );
          end;
      finally
        SysUtils.FindClose( SearchRec );
      end;
      while Issues.Count > MaxIssues do
        begin
          RemoveFullPath( path + Issues[Issues.Count - 1] );
          Issues.Delete( Issues.Count - 1 );
        end;
    end;


  // TNewsCenter

  constructor TNewsCenter.Create( aName, aNewsPath : string; aLog : ILog; aDAAddr : string; aDAPort : integer; aLangId : TLanguageId );
    begin
      inherited Create( aName ); 
      fNewsPath      := Format( aNewsPath, [aLangId] );     
      fMetaReporters := TCollection.Create( 0, rkBelonguer );
      fStyles        := TCollection.Create( 0, rkBelonguer );
      fLayouts       := TCollection.Create( 0, rkBelonguer );
      fNewspapers    := TCollection.Create( 0, rkBelonguer );
      fLangId        := aLangId;
      fLog           := aLog;
      fLock          := TCriticalSection.Create;
      FolderIterate( fNewsPath + tidPath_Reporters, '*.*',      faDirectory, ReporterFound );
      FolderIterate( fNewsPath + tidPath_Layouts,   '*.layout', faArchive,   LayoutFound );
      FolderIterate( fNewsPath + tidPath_Styles,    '*.*',      faDirectory, StyleFound );
      FolderIterate( fNewsPath + tidPath_Newspaper + aName, '\*.*', faDirectory, NewspaperFound );
      fDAAddr := aDAAddr;
      fDAPort := aDAPort;
    end;

  destructor TNewsCenter.Destroy;
    begin
      fLock.Free;
      fMetaReporters.Free;
      fStyles.Free;
      fLayouts.Free;
      fNewspapers.Free;
      inherited;
    end;

  procedure TNewsCenter.RenderNewspapers( Date : TDateTime );

    function TranslateIssueId( Issue : integer ) : string;
      begin
        result := IntToStr( IssueMax - Issue );
        while length( result ) < 12 do
          result := '0' + result;
      end;

    function TranslateDateId( Date : TDateTime ) : string;
      var
        i : integer;
      begin
        result := DateToStr(Date);
        for i := 1 to length(result) do
          if result[i] = '/'
            then result[i] := '-';
      end;

    procedure CreateNewspaperFile( Newspaper : TNewspaper );
      var
        body       : THTML;
        path       : string;
        S          : TStream;
        Properties : TStringList;
      begin
        Log.LogThis( '' );
        Log.LogThis( 'Rendering newspaper: ' + Newspaper.Name + ' (' + DateToStr(Date) + ')' );
        body := Newspaper.Render( Date );
        if body <> ''
          then
            begin
              path := fNewsPath + tidPath_Newspaper + fName + '\' + Newspaper.Name + '\' + TranslateIssueId( Newspaper.LastIssue ) + '@' + TranslateDateId( Date );
              ForceDirectories( path );
              S := TFileStream.Create( path + '\home.asp', fmCreate );
              try
                S.WriteBuffer( body[1], length(body) );
                Log.LogThis( 'Newspaper file saved' );
              finally
                S.Free;
              end;
              try
                Newspaper.ManageSite;
                Log.LogThis( 'Newspaper site optimized.' );
              except
                Log.LogThis( 'Error optimizing newspaper site.' );
              end;
              Newspaper.LastIssue := Newspaper.LastIssue + 1;
              path := fNewsPath + tidPath_Newspaper + fName + '\' + Newspaper.Name;
              Properties := TStringList.Create;
              try
                // Properties.LoadFromFile( path + '\newspaper.ini' );
                Properties.Values['Name']      := Newspaper.Name;
                Properties.Values['Style']     := Newspaper.Style.Name;
                Properties.Values['Town']      := Newspaper.Town;
                Properties.Values['LastIssue'] := IntToStr(Newspaper.LastIssue + 1);
                Properties.SaveToFile( path + '\newspaper.ini' );
              finally
                Properties.Free;
              end;
            end;
      end;

    var
      i : integer;
    begin
      try
        fLock.Enter;
        try
          for i := 0 to pred(fNewspapers.Count) do
            CreateNewspaperFile( TNewspaper(fNewspapers[i]) )
        finally
          fLock.Leave;
        end;
      except
        on E : Exception do
          Log.LogThis( 'ERROR: ' + E.Message );
      end;
    end;

  procedure TNewsCenter.ReloadReporters;
    begin
      try
        fLock.Enter;
        try
          Log.LogThis( '' );
          Log.LogThis( 'Reloading reporter info...' );
          fMetaReporters.DeleteAll;
          FolderIterate( fNewsPath + tidPath_Reporters, '*.*', faDirectory, ReporterFound );
          Log.LogThis( 'Reporters loaded.' );
        finally
          fLock.Leave;
        end;
      except
        on E : Exception do
          Log.LogThis( 'ERROR: ' + E.Message );
      end;
    end;

  function TNewsCenter.GetStyle( Name : string ) : TStyle;
    var
      i : integer;
    begin
      i := 0;
      name := uppercase(name);
      while (i < Styles.Count) and (uppercase(TStyle(Styles[i]).Name) <> name) do
        inc( i );
      if i < Styles.Count
        then result := TStyle(Styles[i])
        else result := nil;
    end;

  function TNewsCenter.GetNewspaper( Name : string ) : TNewspaper;
    var
      i : integer;
    begin
      i := 0;
      name := uppercase(name);
      while (i < Newspapers.Count) and (uppercase(TNewspaper(Newspapers[i]).Name) <> name) do
        inc( i );
      if i < Newspapers.Count
        then result := TNewspaper(Newspapers[i])
        else result := nil;
    end;
    
  procedure TNewsCenter.ReporterFound( path : string; SearchRec : TSearchRec );
    var
      MetaRepTypeId    : string;
      MetaReporterType : CMetaReporter;
      MetaReporter     : TMetaReporter;
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        then
          begin
            MetaRepTypeId := system.copy( SearchRec.Name, 1, system.pos( '.', SearchRec.Name ) - 1 );
            if MetaRepTypeId = ''
              then MetaRepTypeId := SearchRec.Name;
            MetaReporterType := GetMetaReporterType( MetaRepTypeId );
            if MetaReporterType <> nil
              then
                begin
                  Log.LogThis( 'Reporter agent ' + SearchRec.Name + ' found.' );
                  MetaReporter := MetaReporterType.Create( SearchRec.Name, path + SearchRec.Name + '\', fLog );
                  fMetaReporters.Insert( MetaReporter );
                end;
          end;
    end;

  procedure TNewsCenter.LayoutFound( path : string; SearchRec : TSearchRec );
    var
      Layout : TLayout;
    begin
      Layout := TLayout.Create( SearchRec.Name );
      Layout.LoadFromTemplate( path + SearchRec.Name );
      fLayouts.Insert( Layout );
      Log.LogThis( 'Layout ' + SearchRec.Name + ' found.' );
    end;

  procedure TNewsCenter.StyleFound( path : string; SearchRec : TSearchRec );
    var
      Style : TStyle;
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        then
          begin
            Style := TStyle.Create( SearchRec.Name );
            fStyles.Insert( Style );
            Log.LogThis( 'Style ' + SearchRec.Name + ' found.' );
          end;
    end;

  procedure TNewsCenter.NewspaperFound( path : string; SearchRec : TSearchRec );
    var
      IniFile   : TStringList;
      Name      : string;
      Style     : string;
      Town      : string;
      Newspaper : TNewspaper;
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        then
          begin
            IniFile := TStringList.Create;
            try
              IniFile.LoadFromFile( path + '\' + SearchRec.Name + '\newspaper.ini' );
              Name      := IniFile.Values['Name'];
              Style     := IniFile.Values['Style'];
              Town      := IniFile.Values['Town'];
              Newspaper := TNewspaper.Create( Name, Style, Town, self );
              Newspaper.LastIssue := StrToInt(IniFile.Values['LastIssue']);
              fNewspapers.Insert( Newspaper );
              Log.LogThis( 'Newspaper ' + Name + ' found.' );
            finally
              IniFile.Free;
            end;
          end;
    end;
    

  // RegisterMetaReporterType

  var
    MetaReporterTypes : TStringList = nil;

  procedure RegisterMetaReporterType( Id : string; MetaReporterType : CMetaReporter );
    begin
      if MetaReporterTypes = nil
        then MetaReporterTypes := TStringList.Create;
      MetaReporterTypes.AddObject( Id, TObject(MetaReporterType) );
    end;

  function GetMetaReporterType( Id : string ) : CMetaReporter;
    var
      idx : integer;
    begin
      idx := MetaReporterTypes.IndexOf( Id );
      if idx <> NoIndex
        then result := CMetaReporter(MetaReporterTypes.Objects[Idx])
        else result := nil;
    end;

  procedure InitNews;
    begin
      MaleNames   := TStringList.Create;
      with MaleNames do
        begin
          Add( 'Frederick' );
          Add( 'Gustav' );
          Add( 'Jaroslav' );
          Add( 'Rudolf' );
          Add( 'John' );
          Add( 'Elias' );
          Add( 'Henry' );
          Add( 'Gabriel' );
          Add( 'Ethan' );
          Add( 'Silvester' );
          Add( 'Arnold' );
          Add( 'Allan' );
          Add( 'Allen' );
          Add( 'Michael' );
          Add( 'George' );
          Add( 'Jonas' );
          Add( 'Jason' );
          Add( 'William' );
          Add( 'Nathaniel' );
          Add( 'Cristopher' );
        end;
      FemaleNames := TStringList.Create;
      with FemaleNames do
        begin
          Add( 'Eva' );
          Add( 'Ada' );
          Add( 'Augusta' );
          Add( 'Marion' );
          Add( 'Julia' );
          Add( 'Juliette' );
          Add( 'Amy' );
          Add( 'Marylin' );
          Add( 'Gloria' );
          Add( 'Anne' );
          Add( 'Sylvia' );
          Add( 'Mary' );
          Add( 'Glen' );
          Add( 'Mia' );
          Add( 'Tanya' );
          Add( 'Alice' );
        end;
      LastNames := TStringList.Create;
      with LastNames do
        begin
          Add( 'MacKenna' );
          Add( 'Rodriguez' );
          Add( 'Kingston' );
          Add( 'Gates' );
          Add( 'Zamora' );
          Add( 'Thurman' );
          Add( 'Swanson' );
          Add( 'Lionese' );
          Add( 'Lovecraft' );
          Add( 'Poe' );
          Add( 'Tartoleti' );
          Add( 'Knor' );
          Add( 'Beltrand' );
          Add( 'Mirage' );
          Add( 'Totum-te' );
          Add( 'Mantegna' );
        end;
    end;

  procedure DoneNews;
    begin
      MaleNames.Free;
      FemaleNames.Free;
      LastNames.Free;
      MetaReporterTypes.Free;
    end;

end.





