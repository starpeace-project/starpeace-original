unit StdReporters;

interface

  uses
    News;

  type
    TCrimeHi =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TCrimeLo =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    THealthHi =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    THealthLo =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TSchoolHi =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TSchoolLo =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TOverempHiClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TOverempMidClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TOverempLoClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TUnempHiClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TUnempMidClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TUnempLoClass =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
      end;

    TBoardMessage =
      class( TMetaReporter )
        protected
          function StoryStrength( aNewspaper : TNewspaper ) : integer; override;
        public
          function RenderStory( StoryReqs : TStoryReqs; Newspaper : TNewspaper; Reporter : TReporter ) : THTML; override;
      end;

  procedure RegisterReporters;

implementation

  uses
    SysUtils, ComObj, Protocol;

  function TCrimeHi.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covPolice', '0', aNewspaper )) < 80
        then result := 100 - StrToInt(SolveSymbol( 'Town.covPolice', '0', aNewspaper ))
        else result := 0;
    end;

  function TCrimeLo.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covPolice', '0', aNewspaper )) > 95
        then result := 10
        else result := 0;
    end;

  function THealthHi.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covHealth', '0', aNewspaper )) > 95
        then result := 10
        else result := 0;
    end;

  function THealthLo.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covHealth', '100', aNewspaper )) < 80
        then result := 100 - StrToInt(SolveSymbol( 'Town.covHealth', '100', aNewspaper ))
        else result := 0;
    end;

  function TSchoolHi.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covSchool', '0', aNewspaper )) > 95
        then result := 10
        else result := 0;
    end;

  function TSchoolLo.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      if StrToInt(SolveSymbol( 'Town.covSchool', '100', aNewspaper )) < 80
        then result := 100 - StrToInt(SolveSymbol( 'Town.covSchool', '100', aNewspaper ))
        else result := 0;
    end;

  function TOverempHiClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      WorkDemand : integer;
      Population : integer;
    begin
      WorkDemand := StrToInt(SolveSymbol( 'Town.hiWorkDemand', '0', aNewspaper ));
      Population := StrToInt(SolveSymbol( 'Town.hiPopulation', '0', aNewspaper ));
      if (Population > 0) and (WorkDemand/Population > 0.05)
        then result := round(1000*WorkDemand/Population)
        else result := 0;
    end;

  function TOverempMidClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      WorkDemand : integer;
      Population : integer;
    begin
      WorkDemand := StrToInt(SolveSymbol( 'Town.midWorkDemand', '0', aNewspaper ));
      Population := StrToInt(SolveSymbol( 'Town.midPopulation', '0', aNewspaper ));
      if (Population > 0) and (WorkDemand/Population > 0.05)
        then result := round(1000*WorkDemand/Population)
        else result := 0;
    end;

  function TOverempLoClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      WorkDemand : integer;
      Population : integer;
    begin
      WorkDemand := StrToInt(SolveSymbol( 'Town.loWorkDemand', '0', aNewspaper ));
      Population := StrToInt(SolveSymbol( 'Town.loPopulation', '0', aNewspaper ));
      if (Population > 0) and (WorkDemand/Population > 0.05)
        then result := round(1000*WorkDemand/Population)
        else result := 0;
    end;

  function TUnempHiClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      Unemployment : integer;
      Population   : integer;
    begin
      Unemployment := StrToInt(SolveSymbol( 'Town.hiUnemployment', '0', aNewspaper ));
      Population   := StrToInt(SolveSymbol( 'Town.hiPopulation', '0', aNewspaper ));
      if (Population > 150) and (Unemployment > 5)
        then result := 10*Unemployment
        else result := 0;
    end;

  function TUnempMidClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      Unemployment : integer;
      Population   : integer;
    begin
      Unemployment := StrToInt(SolveSymbol( 'Town.midUnemployment', '0', aNewspaper ));
      Population   := StrToInt(SolveSymbol( 'Town.midPopulation', '0', aNewspaper ));
      if (Population > 150) and (Unemployment > 5)
        then result := 10*Unemployment
        else result := 0;
    end;

  function TUnempLoClass.StoryStrength( aNewspaper : TNewspaper ) : integer;
    var
      Unemployment : integer;
      Population   : integer;
    begin
      Unemployment := StrToInt(SolveSymbol( 'Town.loUnemployment', '0', aNewspaper ));
      Population   := StrToInt(SolveSymbol( 'Town.loPopulation', '0', aNewspaper ));
      if (Population > 150) and (Unemployment > 5)
        then result := 10*Unemployment
        else result := 0;
    end;

  function TBoardMessage.StoryStrength( aNewspaper : TNewspaper ) : integer;
    begin
      result := 50 + random(90);
    end;

  function TBoardMessage.RenderStory( StoryReqs : TStoryReqs; Newspaper : TNewspaper; Reporter : TReporter ) : THTML;

    function GetSample( source : string; out trucated : boolean ) : string;
      const
        MaxLength = 300;
      var
        i      : integer;
        finish : boolean;
      begin
        result := '';
        finish := false;
        i      := 1;
        while (i <= length(source)) and (length(result) < MaxLength + 10) and not finish do
          begin
            result := result + source[i];
            case result[length(result)] of
              '.', ' ', '?', '!', ',', ';' :
                finish := length(result) > MaxLength - 10;
            end;
            inc( i );
          end;
        trucated := i < length(source);
      end;

    var
      NewsObj  : olevariant;
      rootpath : string;
      path     : string;
      trunc    : boolean;
    begin
      NewsObj := CreateOleObject( 'NewsBoard.NewsObject' );
      rootpath := Newspaper.NewsCenter.NewsPath + tidPath_Boards + Newspaper.NewsCenter.Name + '\' + Newspaper.Name;
      NewsObj.SetRootPath( rootpath );
      NewsObj.SetIndexSize( 10 );
      path := NewsObj.GlobalMsgPath( Reporter.Lapse );
      if (path <> '') and (NewsObj.Open( path ) = 0)
        then
          begin
            result :=
              '<h<%= LayoutImp %>>' + LineBreak +
              NewsObj.Subject  + LineBreak +
              '</h<%= LayoutImp %>>' + LineBreak +
              '<div class=author>' + LineBreak +
              '    by ' + NewsObj.Author + LineBreak +
              '</div>' + LineBreak + LineBreak +
              GetSample( NewsObj.BodyHTML, trunc );
            result :=
              result + LineBreak +
              '<a href="../../../../boardreader.asp?root=' + rootpath + '&path=' + path + '&tycoon=<%= Request("Tycoon") %>&PaperName=' + Newspaper.Name + '&WorldName=' + Newspaper.NewsCenter.Name + '&DAAddr=' + Newspaper.NewsCenter.DAAddr + '&DAPort=' + IntToStr(Newspaper.NewsCenter.DAPort) + '&TownName=' + Newspaper.Town + '">';
            if trunc
              then result := result + '[>>]'
              else result := result + '[>>]';
            result := result + '</a>';
          end
        else result := ''
    end;


  // RegisterReporters

  procedure RegisterReporters;
    begin
      RegisterMetaReporterType( 'Misc',            TMetaReporter );
      RegisterMetaReporterType( 'CrimeLo',         TCrimeLo );
      RegisterMetaReporterType( 'CrimeHi',         TCrimeHi );
      RegisterMetaReporterType( 'HealthLo',        THealthLo );
      RegisterMetaReporterType( 'HealthHi',        THealthHi );
      RegisterMetaReporterType( 'SchoolLo',        TSchoolLo );
      RegisterMetaReporterType( 'SchoolHi',        TSchoolHi );
      RegisterMetaReporterType( 'OverempHiclass',  TOverempHiclass );
      RegisterMetaReporterType( 'OverempMidclass', TOverempMidclass );
      RegisterMetaReporterType( 'OverempLoclass',  TOverempLoclass );
      RegisterMetaReporterType( 'UnempHiclass',    TUnempHiclass );
      RegisterMetaReporterType( 'UnempMidclass',   TUnempMidclass );
      RegisterMetaReporterType( 'UnempLoclass',    TUnempLoclass );
      RegisterMetaReporterType( 'BoardMessage',    TBoardMessage );
    end;

end.



