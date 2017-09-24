unit TrainsSystem;

interface

  uses
    Kernel, World, Circuits, Train, ServerTrain, Collection, BackupInterfaces, ActorPool, ExtCtrls;

  type
    {$M+}
    TTrainsSystem =
      class( TWorldExtension )
        public
          constructor Create( aWorld : TWorld );
          destructor  Destroy; override;
        private
          fWorld      : TWorld;
          fCircuitMap : TCircuitMap;
          fCompanies  : TLockableCollection;
          fActorPool  : TServerActorPool;
          fTimer      : TTimer;
        public
          function  GetId : string; override;
          procedure EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer ); override;
          procedure Loaded( World : TWorld ); override;
        published
          function RDOTrainCreate            ( CompanyId, EngineType, x, y : integer )        : olevariant;
          function RDOTrainDelete            ( CompanyId, TrainId : integer )                 : olevariant;
          function RDOTrainAddCar            ( CompanyId, TrainId, CarType, index : integer ) : olevariant;
          function RDOTrainDelCar            ( CompanyId, TrainId, index : integer )          : olevariant;
          function RDOTrainSwapCars          ( CompanyId, TrainId, index1, index2 : integer ) : olevariant;
          function RDOTrainStop              ( CompanyId, TrainId : integer )                 : olevariant;
          function RDOTrainResume            ( CompanyId, TrainId : integer )                 : olevariant;
          function RDOTrainGoTo              ( CompanyId, TrainId, x, y : integer )           : olevariant;
          function RDOTrainAddSchedulePoint  ( CompanyId, TrainId, x, y, index : integer )    : olevariant;
          function RDOTrainDelSchedulePoint  ( CompanyId, TrainId, index : integer )          : olevariant;
          function RDOTrainSwapSchedulePoints( CompanyId, TrainId, index1, index2 : integer ) : olevariant;
          function RDOTrainStatusText        ( CompanyId, TrainId, kind : integer )           : olevariant;
          function RDOTrainCount             ( CompanyId : integer )                          : olevariant;
          function RDOTrainId                ( CompanyId : integer; index : integer )         : olevariant;
          function RDOTrainCarCount          ( CompanyId, TrainId : integer )                 : olevariant;
          function RDOTrainCarId             ( CompanyId, TrainId, index : integer )          : olevariant;
          function RDOTrainCarStatusText     ( CompanyId, TrainId, index : integer )          : olevariant;
        protected
          procedure CompanyCreated ( Company  : TCompany );  override;
          procedure TycoonCreated  ( Tycoon   : TTycoon );   override;
          procedure FacilityCreated( Facility : TFacility ); override;
          procedure CompanyDeleted ( Company  : TCompany );  override;
          procedure TycoonDeleted  ( Tycoon   : TTycoon );   override;
          procedure FacilityDeleted( Facility : TFacility ); override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
        private
          procedure InitTimer;
          procedure OnTick( Sender : TObject );
      end;
    {$M-}

  procedure RegisterBackup;

implementation

  uses
    LogFile, ClassStorage, SysUtils, Protocol, MetaInstances, Automaton, ActorTypes, StateEngine;


  // TTrainsSystem

  constructor TTrainsSystem.Create( aWorld : TWorld );
    begin
      inherited Create;
      fWorld      := aWorld;
      fCompanies  := TLockableCollection.Create( 0, rkUse );
      fCircuitMap := TCircuitMap.Create( cirRailRoads, TRailroadNode, TRailroadSegment, TRailroadCircuit );
      fCircuitMap.OnRenderExtraSegInfo := fWorld.OnRenderExtraSegInfo;
      fCircuitMap.OnAuthorizeBreak     := fWorld.OnAuthorizeBreak;
      fWorld.Circuits.Insert( fCircuitMap );
      fActorPool := TServerActorPool.Create( poolIdTrains, nil );
      fWorld.RegisterActorPool( fActorPool );
      InitTimer
    end;

  destructor TTrainsSystem.Destroy;
    begin
      fCompanies.Free;
      fActorPool.Free;
      inherited;
    end;

  function TTrainsSystem.GetId : string;
    begin
      result := tidRDOHook_Trains;
    end;

  procedure TTrainsSystem.EndOfPeriod( PeriodType : TPeriodType; PeriodCount : integer );
    begin
    end;

  procedure TTrainsSystem.Loaded( World : TWorld );

    procedure InsertTrainAutomatons;
      var
        i, j, k : integer;
        MetaAut : TMetaServerAutomaton;
        Aut     : TServerAutomaton;
      begin
        for i := 0 to pred(fCompanies.Count) do
          with TRailRoadCompany(fCompanies[i]) do
            for j := 0 to pred(Trains.Count) do
              with TTrain(Trains[j]) do
                for k := 0 to pred(Cars.Count) do
                  with TServerCar(Cars[k]) do
                    try
                      MetaAut := TMetaServerAutomaton(TheClassStorage.ClassById[IntToStr(fActorPool.Id), IntToStr(MetaCar.Id)]);
                      Aut     := MetaAut.Instantiate( integer(Cars[k]), self );
                      Aut.Automatable := TServerCar(Cars[k]);
                      Aut.Automatable.SetAutomationEngine( Aut );
                      Aut.Automatable.SetActor( Aut );
                      IServerActor(Aut).getStatePool.StateIdTransition( nil, carstRunning, 1, OwnerFrecuency, self, lbDisappear );
                      fActorPool.AddActor( Aut );
                    except
                    end;
      end;

    begin
      inherited;
      fActorPool := TServerActorPool.Create( poolIdTrains, nil );
      fWorld.RegisterActorPool( fActorPool );
      if fCircuitMap = nil
        then
          begin
            fCircuitMap := TCircuitMap.Create( cirRailRoads, TRailroadNode, TRailroadSegment, TRailroadCircuit );
            fCircuitMap.OnRenderExtraSegInfo := fWorld.OnRenderExtraSegInfo;
            fCircuitMap.OnAuthorizeBreak     := fWorld.OnAuthorizeBreak;
            fWorld.Circuits.Insert( fCircuitMap );
          end;
      fCircuitMap.NodeClass    := TRailroadNode;
      fCircuitMap.SegmentClass := TRailroadSegment;
      fCircuitMap.CircuitClass := TRailroadCircuit;
      fCircuitMap.OnRenderExtraSegInfo := fWorld.OnRenderExtraSegInfo;
      fCircuitMap.OnAuthorizeBreak     := fWorld.OnAuthorizeBreak;
      InsertTrainAutomatons;
      InitTimer;
    end;

  function TTrainsSystem.RDOTrainCreate( CompanyId, EngineType, x, y : integer ) : olevariant;
    var
      Train    : TTrain;
      Company  : TRailRoadCompany;
      Segments : TCollection;
      i        : integer;
      rating   : integer;
      best     : integer;
      iBest    : integer;
      Owner    : TTycoon;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain.Create( Company );
                Segments := TCollection.Create( 0, rkUse );
                fCircuitMap.FindSegsInArea( x, y - RailroadTolerance, x + 1, y + RailroadTolerance, Segments );
                fCircuitMap.FindSegsInArea( y - RailroadTolerance, y, x + RailroadTolerance, y + 1, Segments );
                best  := 0;
                iBest := 0;
                for i := 0 to pred(Segments.Count) do
                  with TRailRoadSegment(Segments[i]) do
                    begin
                      Owner := fWorld.TycoonById[OwnerId];
                      rating :=
                        integer(Owner.MasterRole = Company.Owner.MasterRole)*integer(ReservedBy = nil)*integer(Cars.Count = 0)*Length -
                        integer(IsHorizontal)*abs(NodeA.y - y) - integer(not IsHorizontal)*abs(NodeA.x - x);
                      if rating > best
                        then
                          begin
                            best  := rating;
                            iBest := i;
                          end;                    
                    end;
                if best > 0
                  then
                    begin
                      Train.StartSeg := TRailroadSegment(Segments[iBest]);
                      if Train.StartSeg.IsHorizontal
                        then
                          begin
                            Train.StartX := x;
                            Train.StartY := Train.StartSeg.NodeA.y;
                          end
                        else
                          begin
                            Train.StartX := Train.StartSeg.NodeA.x;
                            Train.StartY := y;
                          end;
                      if RDOTrainAddCar( CompanyId, integer(Train), EngineType, 0 ) = NOERROR
                        then
                          begin
                            Company.Trains.Insert( Train );
                            Company.LastTrainId := Company.LastTrainId + 1;
                            Train.Name := Company.Name + ' ' + IntToStr(Company.LastTrainId);
                            result := NOERROR;
                          end
                        else
                          begin
                            Train.Free;
                            result := ERROR_InvalidParameter;
                          end
                    end
                  else result := ERROR_InsuficientSpace
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainDelete( CompanyId, TrainId : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
      i       : integer;
      Car     : TServerCar;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      Company.Trains.Delete( Train );
                      for i := 0 to pred(Train.Cars.Count) do
                        begin
                          Car := TServerCar(Train.Cars[i]);
                          fActorPool.DelActor( Car.Actor );
                        end;
                      Train.Free;
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainAddCar( CompanyId, TrainId, CarType, index : integer ) : olevariant;

    function FindNextSeg( var x, y : integer ) : TRailroadSegment;
      begin
        result := nil;
      end;

    function PositionNewCar( NewCar : TServerCar ) : boolean;
      var
        Tail : TServerCar;
        x    : integer;
        y    : integer;
        seg  : TRailroadSegment;
      begin
        if NewCar.Train.Cars.Count > 0
          then
            begin
              Tail := TServerCar(NewCar.Train.Cars[pred(NewCar.Train.Cars.Count)]);
              x := trunc(Tail.x);
              y := trunc(Tail.y);
              if Tail.CurrSeg.IsHorizontal
                then
                  if Tail.CurrSeg.NodeB.x > Tail.x
                    then
                      begin
                        inc( x );
                        seg := Tail.CurrSeg;
                      end
                    else seg := FindNextSeg( x, y )
                else
                  if Tail.CurrSeg.NodeB.y > y
                    then
                      begin
                        inc( y );
                        seg := Tail.CurrSeg;
                      end
                    else seg := FindNextSeg( x, y );
              if seg <> nil
                then
                  begin
                    NewCar.x := x;
                    NewCar.y := y;
                    NewCar.CurrSeg := seg;
                    NewCar.CurrSeg.Cars.Insert( NewCar );
                    result := true;
                  end
                else result := false;
            end
          else
            begin
              NewCar.x := NewCar.Train.StartX;
              NewCar.y := NewCar.Train.StartY;
              NewCar.CurrSeg := NewCar.Train.StartSeg;
              NewCar.CurrSeg.Cars.Insert( NewCar );
              result := true;
            end;
      end;

    var
      Company : TRailRoadCompany;
      Train   : TTrain;
      MetaCar : TMetaCar;
      MetaAut : TMetaServerAutomaton;
      NewCar  : TServerCar;
      Aut     : TServerAutomaton;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      MetaCar := TMetaCar(TheClassStorage.ClassById[tidClassFamily_TrainCars, IntToStr(CarType)]);
                      MetaAut := TMetaServerAutomaton(TheClassStorage.ClassById[IntToStr(fActorPool.Id), IntToStr(CarType)]);
                      if (MetaCar <> nil) and (MetaAut <> nil)
                        then
                          begin
                            NewCar       := TServerCar(MetaCar.Instantiate);
                            NewCar.Train := Train;
                            if PositionNewCar( NewCar )
                              then
                                begin
                                  Train.Cars.AtInsert( index, NewCar );
                                  index := Train.Cars.IndexOf( NewCar );
                                  if index > 0
                                    then
                                      begin
                                        NewCar.PrevCar := TServerCar(Train.Cars[index - 1]);
                                        TServerCar(Train.Cars[index - 1]).NextCar := NewCar;
                                      end;
                                  if index < pred(Train.Cars.Count)
                                    then
                                      begin
                                        NewCar.NextCar := TServerCar(Train.Cars[index + 1]);
                                        TServerCar(Train.Cars[index + 1]).PrevCar := NewCar;
                                      end;
                                  Aut := MetaAut.Instantiate( integer(NewCar), self );
                                  Aut.Automatable := NewCar;
                                  Aut.Automatable.SetAutomationEngine( Aut );
                                  Aut.Automatable.SetActor( Aut );
                                  IServerActor(Aut).getStatePool.StateIdTransition( nil, carstRunning, 1, OwnerFrecuency, self, lbDisappear );
                                  //AddInitialState( carstRunning, cplNormal, self );

                                  fActorPool.AddActor( Aut );
                                  result := NOERROR;
                                end
                              else
                                begin
                                  NewCar.Free;
                                  result := ERROR_InsuficientSpace;
                                end;
                          end
                        else result := ERROR_InvalidParameter
                    end
                  else result := ERROR_InvalidCompanyId;
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainDelCar( CompanyId, TrainId, index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
      Car     : TServerCar;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      Car := TServerCar(Train.Cars[index]);
                      fActorPool.DelActor( Car.Actor );
                      Train.Cars.Delete( Car );
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainSwapCars( CompanyId, TrainId, index1, index2 : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainStop( CompanyId, TrainId : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainResume( CompanyId, TrainId : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainGoTo( CompanyId, TrainId, x, y : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      Train.AltX := x;
                      Train.AltY := y;
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainAddSchedulePoint( CompanyId, TrainId, x, y, index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
      Point   : TRoutePoint;
      Fac     : TFacility;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if true //Train.Company = Company
                  then
                    begin
                      Fac := fWorld.FacilityAt( x, y );
                      if (Fac <> nil) // and (Fac.Company.Owner.MasterRole = Company.Owner.MasterRole)
                        then
                          begin
                            Point := TRoutePoint.Create;
                            Point.Station := Fac.CurrBlock;
                            Train.Route.AtInsert( index, Point );
                            result := NOERROR;
                          end
                        else result := ERROR_InvalidCompanyId
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainDelSchedulePoint( CompanyId, TrainId, index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      Train.Route.AtDelete( index );
                      if Train.NextPoint >= Train.Route.Count
                        then Train.NextPoint := Train.NextPoint - 1;
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainSwapSchedulePoints( CompanyId, TrainId, index1, index2 : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainStatusText( CompanyId, TrainId, kind : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then result := 'Heading to Malangalatuerca.'
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainCount( CompanyId : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then result := Company.Trains.Count
            else result := -1
        finally
          Unlock;
        end;
      except
        result := -1;
      end;
    end;

  function TTrainsSystem.RDOTrainId( CompanyId : integer; index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then result := integer(Company.Trains[index])
            else result := -1
        finally
          Unlock;
        end;
      except
        result := -1;
      end;
    end;

  function TTrainsSystem.RDOTrainCarCount( CompanyId, TrainId : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainCarId( CompanyId, TrainId, index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  function TTrainsSystem.RDOTrainCarStatusText( CompanyId, TrainId, index : integer ) : olevariant;
    var
      Company : TRailRoadCompany;
      Train   : TTrain;
    begin
      try
        Lock;
        try
          Company := TRailRoadCompany(fWorld.GetCompany( CompanyId ));
          if (Company <> nil) and ObjectIs( TRailRoadCompany.ClassName, Company )
            then
              begin
                Train := TTrain(TrainId);
                if Train.Company = Company
                  then
                    begin
                      result := NOERROR;
                    end
                  else result := ERROR_InvalidCompanyId
              end
            else result := ERROR_InvalidCompanyId
        finally
          Unlock;
        end;
      except
        result := ERROR_Unknown;
      end;
    end;

  procedure TTrainsSystem.CompanyCreated( Company : TCompany );
    begin
      if ObjectIs( TRailRoadCompany.ClassName, Company )
        then fCompanies.Insert( Company );
    end;

  procedure TTrainsSystem.TycoonCreated( Tycoon   : TTycoon );
    begin
    end;

  procedure TTrainsSystem.FacilityCreated( Facility : TFacility );
    begin
    end;

  procedure TTrainsSystem.CompanyDeleted( Company  : TCompany );
    begin
      if ObjectIs( TRailRoadCompany.ClassName, Company )
        then fCompanies.Delete( Company );
    end;

  procedure TTrainsSystem.TycoonDeleted( Tycoon   : TTycoon );
    begin
    end;

  procedure TTrainsSystem.FacilityDeleted( Facility : TFacility );
    begin
    end;

  procedure TTrainsSystem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'World', fWorld, nil );
      Reader.ReadObject( 'CircuitMap', fCircuitMap, nil );
      Reader.ReadObject( 'Companies', fCompanies, nil );
      Reader.ReadObject( 'ActorPool', fActorPool, nil );
    end;

  procedure TTrainsSystem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteObjectRef( 'World', fWorld );
      Writer.WriteObjectRef( 'CircuitMap', fCircuitMap );
      Writer.WriteObject( 'Companies', fCompanies );
      Writer.WriteObjectRef( 'ActorPool', fActorPool );
    end;

  procedure TTrainsSystem.InitTimer;
    begin
      fTimer := TTimer.Create( nil );
      fTimer.OnTimer  := OnTick;
      fTimer.Interval := poolTrainsInterval;
    end;

  procedure TTrainsSystem.OnTick( Sender : TObject );
    var
      i, j : integer;
    begin
      Lock;
      try
        try
          LogThis( 'Server TickCount: ' + IntToStr(fActorPool.TickCount) );
          for i := 0 to pred(fCompanies.Count) do
            with TRailRoadCompany(fCompanies[i]) do
              for j := 0 to pred(Trains.Count) do
                TTrain(Trains[j]).Act;
          fActorPool.Act;
        except
        end;
      finally
        Unlock;
      end;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TTrainsSystem );
    end;


end.

