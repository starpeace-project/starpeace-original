unit Portal;

interface

  uses
    Protocol, Kernel, Environmental;

  type
    TPortal =
      class( TEnvironmentalBlock )
        published
          function GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string; override;
        public
          function Evaluate : TEvaluationResult; override;
        protected
          function GetVisualClassId  : TVisualClassId; override;
        private
          fLastPop : TFluidValue;
          fPopFlow : TFluidValue;
      end;

  procedure RegisterBackup;

implementation

  uses
    World, Population, BackupInterfaces;

  // TPortal

  function TPortal.GetStatusText( kind : TStatusKind; ToTycoon : TTycoon ) : string;
    var
      TH : TTownHall;
    begin
      if kind = sttSecondary
        then
          begin
            TH := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
            result := TH.GetStatusText( kind, ToTycoon );
          end
        else result := inherited GetStatusText( kind, ToTycoon );
    end;

  function TPortal.Evaluate : TEvaluationResult;
    var
      TH : TTownHall;
      //CurrPop : TFluidValue;
    begin
      result := inherited Evaluate;
      TH := TTownHall(TInhabitedTown(Facility.Town).TownHall.CurrBlock);
      fPopFlow := TH.TotalPopulation - fLastPop;
      fLastPop := TH.TotalPopulation;
    end;

  function TPortal.GetVisualClassId : TVisualClassId;
    begin
      if fPopFlow > 0
        then result := 0
        else result := 1;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TPortal );
    end;


end.
