unit PolluterWorkCenter;

interface

  uses
    Kernel, WorkCenterBlock, Surfaces, OutputEvaluators, BackupInterfaces,
    EvaluatedBlock;

  const
    modPollutionStrengh = 10;

  type
    TMetaPolluterWorkCenterBlock =
      class(TMetaEvaluatedBlock)
      end;

    TPolluterWorkCenterBlock =
      class(TEvaluatedBlock)
        public
          destructor Destroy; override;
        public
          procedure AutoConnect( loaded : boolean ); override;
        protected
          function Evaluate : TEvaluationResult; override;
        private
          fPollutionModifier : TSurfaceModifier;
          fPollution         : single;
        private
          function  GetPolutionLevel : TSurfaceValue;
          procedure SetPolutionLevel( aPollutionLevel : TSurfaceValue );
        public
          property PolutionLevel : TSurfaceValue read GetPolutionLevel write SetPolutionLevel;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        protected
          procedure Deleted; override;
      end;

  const
    MaxPollution     = 900;
    PollutionPerHour = 0.1;

implementation

  uses
    PyramidalModifier, Classes, MathUtils;

  // TPolluterWorkCenterBlock

  destructor TPolluterWorkCenterBlock.Destroy;
    begin
      fPollutionModifier.Delete;
      inherited;
    end;

  procedure TPolluterWorkCenterBlock.AutoConnect( loaded : boolean );
    begin
      inherited;
      fPollutionModifier :=
        TPyramidalModifier.Create(
          tidEnvironment_Pollution,
          Point(xOrigin, yOrigin),
          0,
          modPollutionStrengh);
    end;
                                                 
  function TPolluterWorkCenterBlock.Evaluate : TEvaluationResult;
    var
      OpRatio : byte;
    begin
      result := inherited Evaluate;
      OpRatio := EvaluatorPool.OperationRatio;
      if OpRatio > 0
        then fPollution := realmin( MaxPollution, fPollution + PollutionPerHour*(OpRatio/100)*dt )
        else fPollution := realmax( 0, fPollution - (1/100)*dt );
      PolutionLevel := fPollution;
    end;

  function TPolluterWorkCenterBlock.GetPolutionLevel : TSurfaceValue;
    begin
      result := fPollutionModifier.Value;
    end;

  procedure TPolluterWorkCenterBlock.SetPolutionLevel( aPollutionLevel : TSurfaceValue );
    begin
      fPollutionModifier.Value := aPollutionLevel;
    end;

  procedure TPolluterWorkCenterBlock.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fPollution := realmax( 0, Reader.ReadSingle( 'Pollution', 0 ));
    end;

  procedure TPolluterWorkCenterBlock.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle( 'Pollution', fPollution );
    end;

  procedure TPolluterWorkCenterBlock.Deleted;
    begin
      fPollutionModifier.Delete;
      fPollutionModifier := nil;
      inherited;
    end;

end.



