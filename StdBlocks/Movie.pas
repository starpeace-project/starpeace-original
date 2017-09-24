unit Movie;

interface

  uses
    Kernel, Surfaces, WorkCenterBlock, StdFluids, ServiceBlock, MediaGates,
    Protocol;

  const
    tidService_Movie = 'Movie';

  const
    MovieAveragePrice = 5;

  //const
    //MoviePotencialClients : array[TPeopleKind] of integer = (90, 150, 250);

  type
    TMetaMovieBlock =
      class(TMetaServiceBlock)
        public
          constructor Create(anId         : string;
                             aCapacities  : array of TFluidValue;
                             aCustomerMax : TFluidValue;
                             aPricePerc   : TPercent;
                             EvlBuyProb   : array of TBuyProbability;
                             aMaxAd       : TFluidValue;
                             aBlockClass  : CBlock);
      end;

    TMovieBlock =
      class(TServiceBlock)
        protected
          function  Evaluate : TEvaluationResult; override;
        public
          procedure AutoConnect(loaded : boolean); override;
          function  GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
          procedure CopySettingsFrom(Block : TBlock; Options : integer); override;
          procedure EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer); override;
        protected
          fFilms      : TInputData;
          fFilmsInput : TMediaInput;
      end;

  procedure RegisterBackup;


implementation

  uses
    ClassStorage, PyramidalModifier, Classes, BackupInterfaces,
    Population, StdAccounts, MathUtils, MetaInstances, SimHints, CloneOptions;

  // TMetaMovieBlock

  constructor TMetaMovieBlock.Create(anId         : string;
                                     aCapacities  : array of TFluidValue;
                                     aCustomerMax : TFluidValue;
                                     aPricePerc   : TPercent;
                                     EvlBuyProb   : array of TBuyProbability;
                                     aMaxAd       : TFluidValue;
                                     aBlockClass  : CBlock);
    var
      Sample : TMovieBlock;
      Films  : TFluidValue;
    begin
      inherited Create(anId,
        aCapacities,
        accIdx_Movie_Supplies,
        accIdx_Movie_Salaries,
        accIdx_Movie_Sales,
        aMaxAd,
        aBlockClass);
      Sample := nil;
      Films  := 1;
      // Inputs
      MetaInputs.Insert(
        TMetaInput.Create(
          tidGate_Films,
          inputZero,
          InputData(Films, 100),
          inputZero,
          qIlimited,
          TMediaInput, //TPullInput,
          TMetaFluid(TheClassStorage.ClassById[tidClassFamily_Fluids, tidFluid_Films]),
          5,
          mglBasic,
          [mgoptCacheable, mgoptEditable],
          sizeof(Sample.fFilms),
          Sample.Offset(Sample.fFilms)));

      // Service: Movie
      with TMetaServiceEvaluator.Create(
        TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, tidService_Movie]),
        'Movie',
        aPricePerc,
        aCustomerMax,
        100,
        EvlBuyProb) do
        begin
          RegisterInput(
            TMetaServiceEvaluatorInput.Create(
              InputByName[tidGate_Films],
              1,
              50));
          Register(self);
        end;
    end;


  // TMovieBlock

  function TMovieBlock.Evaluate : TEvaluationResult;
    var
      upgrlv : byte;
    begin
      result := inherited Evaluate;
      upgrlv := max(1, UpgradeLevel);
      if fFilmsInput.ActualMaxFluid.Q > 0
        then fFilmsInput.ActualMaxFluid.Q := upgrlv*fFilmsInput.MetaInput.MaxFluid.Q;
      fFilmsInput.LastExp := min(CustomerCount, round(upgrlv*TMetaMovieBlock(MetaBlock).Services[0].MaxServices));
    end;

  procedure TMovieBlock.AutoConnect(loaded : boolean);
    begin
      inherited;
      fFilmsInput := TMediaInput(InputsByName[tidGate_Films]);
    end;

  function TMovieBlock.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      titles : string;
    begin
      result := inherited GetStatusText(kind, ToTycoon);
      case kind of
        sttSecondary :
          if not Facility.CriticalTrouble // >> MLS2
            then
              begin
                titles := fFilmsInput.Titles;
                if titles <> ''
                  then result := result + '  ' + mtidNowPlaying.Values[ToTycoon.Language] + ' ' + titles;
              end;
      end;
    end;

  procedure TMovieBlock.CopySettingsFrom(Block : TBlock; Options : integer);
    begin
      inherited;
      if ObjectIs('TMovieBlock', Block) and (Options and cloneOption_Suppliers <> 0)
        then fFilmsInput.SortMode := TMovieBlock(Block).fFilmsInput.SortMode;
    end;

  procedure TMovieBlock.EndOfPeriod(PeriodType : TPeriodType; PeriodCount : integer);
    begin
      inherited;
      case PeriodType of
        perDay :
          if not fFilmsInput.Sorted
            then fFilmsInput.SortConnections;
      end;
    end;

  // Register backup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TMovieBlock);
    end;


end.

