unit KernelCache;

interface

  uses
    ModelServerCache, CacheAgent, Kernel, Population;

  const
    tidCachePath_Clusters       = 'Clusters\';
    tidCachePath_MetaFluids     = 'Fluids\';
    tidCachePath_MetaFacilities = 'Facilities\';
    tidCachePath_MetaBlocks     = 'Blocks\';
    tidCachePath_EvlStages      = 'EvlStages\';
    tidCachePath_Companies      = 'Companies\';
    tidCachePath_Tycoons        = 'Tycoons\';
    tidCachePath_Towns          = 'Towns\';
    tidCachePath_Inputs         = 'Inputs\';
    tidCachePath_Outputs        = 'Outputs\';

  type
    TMetaInstanceCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TInstanceCacheAgent =
      class( TCacheAgent )
      end;

    TMetaFluidCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TFacilityKindCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TInventionCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TMetaFacilityCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TEvlStageCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TMetaBlockCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TFacilityCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TGateCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function  GetCache( Obj : TObject ) : TObjectCache;    override;
          class function  UpdateCache( Obj : TObject ) : TObjectCache; override;
          class procedure CacheExtraInfo( Name : string; ExtraConnectionInfo : PExtraConnectionInfo; MetaFluid : TMetaFluid; Cache : TObjectCache );
      end;

    TInputCacheAgent =
      class( TGateCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;          override;
          class function GetCache( Obj : TObject ) : TObjectCache;    override;
          class function UpdateCache( Obj : TObject ) : TObjectCache; override;
      end;

    TOutputCacheAgent =
      class( TGateCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;          override;
          class function GetCache( Obj : TObject ) : TObjectCache;    override;
          class function UpdateCache( Obj : TObject ) : TObjectCache; override;
      end;

    TMoneyDealerCacheAgent =
      class( TCacheAgent )
        public
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TCompanyCacheAgent =
      class( TMoneyDealerCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TTycoonCacheAgent =
      class( TMoneyDealerCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TTownCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject ) : string;       override;
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

    TInhabitedTownCacheAgent =
      class( TTownCacheAgent )
        public
          class function GetCache( Obj : TObject ) : TObjectCache; override;
      end;

  procedure RegisterCachers;


implementation

  uses
    SysUtils, SpecialChars, CacheCommon, MathUtils, CacheNameUtils, Plotter,
    ClassStorage, Trade, Collection, Protocol;

    
  // TMetaInstance

  class function TMetaInstanceCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TMetaInstance(Obj) do
        result := Family + '\' + Id + '.five';
    end;

  class function TMetaInstanceCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TMetaInstance(Obj) do
        begin
          result.WriteString( 'Id', Id );
          result.WriteString( 'Family', Family );
        end;
    end;


  // TMetaFluidCacheAgent

  class function TMetaFluidCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TMetaFluid(Obj) do
        begin
          result.WriteString( 'Id', Id );
          result.WriteString( 'Name', Name );
          result.WriteString( 'UnitName', UnitName );
          result.WriteString( 'FluidName', FluidName );
          result.WriteString( 'Description', Description );
          result.WriteFloat( 'TransCost', TransCost );
          result.WriteInteger( 'MarketPrice', round(MarketPrice) );
          result.WriteBoolean( 'Tradeable', mfTradeable in Options );
        end;
    end;


  // TFacilityKindCacheAgent

  class function TFacilityKindCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TFacilityKind(Obj) do
        result := tidCachePath_Clusters + ClusterName + '\' + tidCachePath_MetaFacilities + Format('%.8x', [Index]) + '.' + Id + '.five\';
    end;

  class function TFacilityKindCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TFacilityKind(Obj) do
        begin
          result.WriteString( 'Name', Name );
          result.WriteString( 'ClusterName', ClusterName );
          result.WriteString( 'TechnologyName', Technology );
          result.WriteString( 'SuperType', SuperType );
        end;
    end;


  // TInventionCacheAgent

  class function TInventionCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TInvention(Obj) do
        begin
          result.WriteInteger( 'NumId', NumId );
          result.WriteString( 'Name', Name );
          result.WriteString( 'Kind', Kind );
          result.WriteString( 'Desc', Desc );
          result.WriteString( 'Price', FormatMoney( Price ) );
          result.WriteCurrency( 'nfPrice', Price );
          result.WriteInteger( 'Time', Time );
        end;
    end;


  // TMetaFacilityCacheAgent

  class function TMetaFacilityCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TMetaFacility(Obj) do
        if Kind <> nil
          then result := TFacilityKindCacheAgent.GetPath( Kind ) + Format('%.8x', [Index]) + '.' + Id + '.five\'
          else result := tidCachePath_Clusters + ClusterName + '\' + Family + '\' + FacilityKind + '.five\' + Format('%.8x', [Index]) + '.' + Id + '.five\';
    end;

  class function TMetaFacilityCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    var
      i : integer;
    begin
      result := inherited GetCache( Obj );
      with TMetaFacility(Obj) do
        begin
          result.WriteString( 'Name', Name );
          result.WriteString( 'Desc', Desc );
          result.WriteInteger( 'Level', Level );
          result.WriteInteger( 'XSize', XSize );
          result.WriteInteger( 'YSize', YSize );
          result.WriteString( 'Size', IntToStr(YSize*YSize*400) + ' m²' );
          result.WriteBoolean( 'InTown', mfcInTown in Options );
          result.WriteString( 'Cluster', ClusterName );
          result.WriteString( 'Kind', FacilityKind );
          result.WriteString( 'TechnologyName', TechnologyName );
          result.WriteString( 'Price', FormatMoney( Price/1000 ) + 'K' );
          result.WriteCurrency( 'nfPrice', Price );
          result.WriteString( 'ImportPrice', FormatMoney( TradeCenterPrice*Price/100000 ) + 'K' );
          result.WriteCurrency( 'nfImportPrice', TradeCenterPrice*Price/100 );
          result.WriteString( 'TypicalStageBlock', TypicalStage.MetaBlock.Id );
          result.WriteString( 'TypicalStage', TypicalStage.Id );
          result.WriteInteger( 'TypicalVisualClass', VisualClass + TypicalStage.Index );
          for i := 0 to pred(EvlStages.Count) do
            CacheMetaObject( EvlStages[i] )
        end;
    end;


  // TEvlStageCacheAgent

  class function TEvlStageCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TEvlStage(Obj) do
        result := TMetaFacilityCacheAgent.GetPath( MetaFacility ) + tidCachePath_EvlStages + Id + '.five'
    end;

  class function TEvlStageCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TEvlStage(Obj) do
        begin
          result.WriteString( 'Id', Id );
          result.WriteString( 'Name', Name );
          result.WriteString( 'Description', Description );
          result.WriteString( 'MetaBlock', MetaBlock.Id );
        end;
    end;


  // TMetaBlockCacheAgent

  class function TMetaBlockCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TMetaBlock(Obj) do
        begin
          {
          result.WriteInteger( 'XSize', XSize );
          result.WriteInteger( 'YSize', YSize );
          }
        end;
    end;


  // TFacilityCacheAgent

  class function TFacilityCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TFacility(Obj) do
        if (Company <> nil) and (Company.Name <> '')
         then result := GetObjectPath( Company ) + GetCacheName + '.five\'
         else result := tidCachePath_Companies + 'General\' + GetCacheName + '.five\';
    end;

  class function TFacilityCacheAgent.GetCache( Obj : TObject ) : TObjectCache;

    function RenderCircuitStr( Circuits : TCollection ) : string;
      var
        i : integer;
      begin
        result := '';
        if Circuits <> nil
          then
            for i := 0 to pred(Circuits.Count) do
              result := result + IntToStr(integer(Circuits[i])) + ',';
      end;

    var
      i : integer;
    begin
      result := inherited GetCache( Obj );
      with TFacility(Obj) do
        begin
          Lock;
          try
            result.WriteString( ppTTL, CreateTTL( 0, 0, 2, 0 ));
            result.WriteInteger( 'VisualId', VisualClass );
            if (Company <> nil) and (Company.Owner <> nil)
              then result.WriteInteger( 'SecurityId', integer(Company.Owner) );
            result.WriteString( 'Name', Name );
            result.WriteString( 'Path', TFacilityCacheAgent.GetPath( Obj ));
            if Company <> nil
              then
                begin
                  result.WriteString( 'Company', Company.Name );
                  result.WriteString( 'Cluster', Company.Cluster.Id );
                  if Company.Owner <> nil
                    then result.WriteString( 'Creator', Company.Owner.Name );
                end;
            result.WriteInteger( 'Trouble', Trouble );
            result.WriteString( 'MetaFacility', MetaFacility.Id );
            result.WriteString( 'MetaFacilityName', MetaFacility.Name );
            if Town <> nil
              then result.WriteString ( 'Town', Town.Name );
            result.WriteInteger( 'xPos', xPos );
            result.WriteInteger( 'yPos', yPos );
            result.WriteInteger( 'CurrBlock', integer(CurrBlock) );
            try
              CurrBlock.StoreToCache( result );
            except
              on E : Exception do
                raise Exception.Create( 'Cannot cache facility: ' + Name + ' (' + E.Message + ')' );
            end;
            if MoneyGraph <> nil
              then
                begin
                  result.WriteBoolean( 'MoneyGraph', true );
                  for i := 0 to pred(PlotterPointCount) do
                    result.WriteInteger( 'MoneyGraphValue' + IntToStr(i), MoneyGraph.Points[i] );
                  result.WriteInteger( 'MoneyGraphMin', MoneyGraph.MinVal );
                  result.WriteInteger( 'MoneyGraphMax', MoneyGraph.MaxVal );
                  result.WriteInteger( 'MoneyGraphZero', MoneyGraph.Zero );
                end
              else result.WriteBoolean( 'MoneyGraph', false );
            result.WriteString( 'NearCircuits', RenderCircuitStr( CurrBlock.Circuits[cirRoads] ) );
            {
            try
              Kind := TFacilityKind(TheClassStorage.ClassById[tidClassFamily_FacilityKinds, MetaFacility.FacilityKind]);
              result.AddLink( TCompanyCacheAgent.GetPath(Company) + Kind.Name + '\' + Name );
            except
            end;
            }
            result.AddLink(CreateMapLink(xPos, yPos, result.Path));
            if MetaFacility.Kind <> nil
              then
                begin
                  //result.AddLink(GetGlobalPath(tidCachePath_MetaFacilities + MetaFacility.Kind.SuperType + '\'));
                  if (Town <> nil) and (Company <> nil) and (Company.Owner <> nil)
                    then
                      begin
                        result.AddLink(GetGlobalPath(tidCachePath_Towns + Town.Name + '.five\Facilities\' + MetaFacility.Kind.SuperType + '\'));
                        result.AddLink(GetGlobalPath(tidCachePath_Towns + Town.Name + '.five\Companies\' + Company.GetCacheName + '\' + MetaFacility.Kind.SuperType + '\'));
                        result.AddLink(GetGlobalPath(tidCachePath_Tycoons + Company.Owner.GetCacheName + '.five\Companies\' + Company.GetCacheName + '\Facilities\' + MetaFacility.Kind.SuperType + '\'));
                      end;
                end;
          finally
            Unlock;
          end;
        end;
    end;


  // TGateCacheAgent

  class function TGateCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TGate(Obj) do
        begin
          Block.Facility.Lock;
          try
            result.WriteString( ppTTL, NULLTTL);
          finally
            Block.Facility.Unlock;
          end;
        end;
    end;

  class function TGateCacheAgent.UpdateCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited UpdateCache( Obj );
      with TGate(Obj) do
        begin
          Block.Facility.Lock;
          try
            result.WriteString( ppTTL, CreateTTL( 0, 0, 2, 0 ));
            result.WriteInteger( 'cnxCount', ConnectionCount );
            result.WriteCurrency( 'LastCost', LastCost );
          finally
            Block.Facility.Unlock;
          end;
        end;
    end;

  class procedure TGateCacheAgent.CacheExtraInfo( Name : string; ExtraConnectionInfo : PExtraConnectionInfo; MetaFluid : TMetaFluid; Cache : TObjectCache );
    begin
      if ExtraConnectionInfo <> nil
        then
          begin
            Cache.WriteString( 'LastValue' + Name, MetaFluid.FormatValue( ExtraConnectionInfo.LastFluid ));
            Cache.WriteFloat( 'nfLastValue' + Name, MetaFluid.ConvertToUnits( ExtraConnectionInfo.LastFluid ));
            Cache.WriteString( 'YearValue' + Name, MetaFluid.FormatValueAbs( ExtraConnectionInfo.YearValue ));
            Cache.WriteFloat( 'nfYearValue' + Name, MetaFluid.ConvertToUnits( ExtraConnectionInfo.YearValue ));
            Cache.WriteString( 'YearCost' + Name, FormatMoney( ExtraConnectionInfo.YearCost ));
            Cache.WriteBoolean( 'Connected' + Name, ExtraConnectionInfo.Connected );
            Cache.WriteInteger( 'OverPrice' + Name, ExtraConnectionInfo.OverPrice );
            // Cache.WriteInteger( 'Priority' + Name, ExtraConnectionInfo.Priority );
          end;
    end;


  // TInputCacheAgent

  class function TInputCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TInput(Obj) do
        result := GetObjectPath( Block.Facility ) + tidCachePath_Inputs + Format('%.8x', [MetaInput.Index]) + '.' + MetaInput.Name + '.five';
    end;

  class function TInputCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    var
      CompanyName : string;
    begin
      result := inherited GetCache( Obj );
      with TInput(Obj) do
        begin
          Block.Facility.Lock;
          try
            if (MetaInput.MetaFluid <> nil) and (Block.Facility.Town <> nil)
              then
                begin
                  if Block.Facility.Company <> nil
                    then CompanyName := Block.Facility.Company.GetCacheName
                    else CompanyName := 'General';
                  result.AddLink(
                    CreateInputLink(
                      Block.Facility.xPos,
                      Block.Facility.yPos,
                      0,
                      ConnectionCount,
                      MetaInput.Metafluid.Id,
                      Block.Facility.Town.Name,
                      CompanyName,
                      Block.Facility.GetCacheName,
                      '',
                      Block.Facility.MetaFacility.Kind.Role)); // >>
                end
              //else raise Exception.Create('Invalid Town or fluid in "' + Block.Facility.GetCacheName + '"');
          finally
            Block.Facility.Unlock;
          end;
        end;
    end;

  class function TInputCacheAgent.UpdateCache( Obj : TObject ) : TObjectCache;

    function GetFluidType( MetaFluid : TMetaFluid ) : string;
      begin
        if mfAdvertisement in MetaFluid.Options
          then result := 'ADVERTISEMENT'
          else
            if mfService in MetaFluid.Options
              then result := 'SERVICE'
              else result := 'NORMAL';
      end;

    var
      i    : integer;
      Name : string;
    begin
      result := inherited UpdateCache( Obj );
      try
        with TInput(Obj) do
          begin
            Block.Facility.Lock;
            try
              if MetaInput.MetaFluid <> nil
                then
                  begin
                    result.WriteString( 'Name', MetaInput.MetaFluid.Name );
                    result.WriteString( 'MetaFluid', MetaInput.MetaFluid.Id );
                    result.WriteString( 'FluidType', GetFluidType( MetaInput.MetaFluid ));
                  end
                else
                  begin
                    result.WriteString( 'Name', 'Unknown' );
                    result.WriteString( 'MetaFluid', 'Unknown' );
                  end;
              result.WriteInteger( 'GateIndex', MetaInput.Index );
              result.WriteString( 'FluidValue', MetaInput.MetaFluid.FormatValue( FluidData.Q ) );
              result.WriteFloat( 'nfFluidValue', MetaInput.MetaFluid.ConvertToUnits( FluidData.Q ));
              result.WriteInteger( 'FluidQuality', LastValue.K );
              result.WriteString( 'MetaInput', MetaInput.Name );
              result.WriteString( 'ActualMaxFluidValue', MetaInput.MetaFluid.FormatValue( ActualMaxFluid.Q ) );
              result.WriteFloat( 'nfActualMaxFluidValue', MetaInput.MetaFluid.ConvertToUnits( ActualMaxFluid.Q ));
              result.WriteString( 'Capacity', MetaInput.MetaFluid.FormatValue( MetaInput.MaxFluid.Q ));
              result.WriteFloat( 'nfCapacity', MetaInput.MetaFluid.ConvertToUnits( MetaInput.MaxFluid.Q ));
              for i := 0 to pred(ConnectionCount) do
                begin
                  Connections[i].Block.Facility.Lock;
                  try
                    result.WriteString( 'cnxFacilityName' + IntToStr(i), Connections[i].Block.Facility.Name );
                    if Connections[i].Block.Facility.Company <> nil
                      then Name := Connections[i].Block.Facility.Company.Name
                      else Name := 'Unknown';
                    result.WriteString( 'cnxCompanyName' + IntToStr(i), Name );
                    result.WriteString( 'cnxPrice' + IntToStr(i), FormatMoney(Connections[i].Price) );
                    result.WriteCurrency( 'cnxNfPrice', Connections[i].Price );
                    result.WriteFloat( 'cnxNfPrice' + IntToStr(i), Connections[i].Price );
                    result.WriteString( 'cnxQuality' + IntToStr(i), IntToStr(Connections[i].FluidData.K) + '%' );
                    result.WriteString( 'cnxFacilityPath' + IntToStr(i), GetObjectPath( Connections[i].Block.Facility ));
                    result.WriteString( 'cnxFacilityId' + IntToStr(i), Connections[i].Block.Facility.GetCacheName + NameSeparator + IntToStr(Block.Facility.xPos) + NameSeparator + IntToStr(Block.Facility.xPos));
                    if (Connections[i].Block.Facility.Company <> nil) and (Connections[i].Block.Facility.Company.Owner <> nil)
                      then result.WriteString( 'cnxCreatedBy' + IntToStr(i), Connections[i].Block.Facility.Company.Owner.Name );
                    result.WriteInteger( 'cnxXPos' + IntToStr(i), Connections[i].Block.Facility.xPos );
                    result.WriteInteger( 'cnxYPos' + IntToStr(i), Connections[i].Block.Facility.yPos );
                    result.WriteInteger( 'cnxTrouble' + IntToStr(i), Connections[i].Block.Facility.Trouble );
                    result.WriteInteger( 'cnxVisualId' + IntToStr(i), Connections[i].Block.Facility.VisualClass );
                    result.WriteInteger( 'cnxConnectionCount' + IntToStr(i), Connections[i].ConnectionCount );
                    result.WriteString( 'cnxLastYear' + IntToStr(i), FormatMoney( Connections[i].Block.Facility.PeriodMoney ));
                    result.WriteCurrency( 'cnxNfLastYear', Connections[i].Block.Facility.PeriodMoney );
                    CacheExtraInfo( 'CnxInfo' + IntToStr(i), ExtraConnectionInfo[i], MetaInput.MetaFluid, result );
                  finally
                    Connections[i].Block.Facility.Unlock;
                  end;
                end;
            finally
              Block.Facility.Unlock;
            end;
          end;
      except
      end;
    end;


  // TOutputCacheAgent

  class function TOutputCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TOutput(Obj) do
        result := GetObjectPath( Block.Facility ) + tidCachePath_Outputs + MetaOutput.Name + '.five';
    end;

  class function TOutputCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    var
      CompanyName : string;
    begin
      result := inherited GetCache( Obj );
      with TOutput(Obj) do
        begin
          Block.Facility.Lock;
          try
            if (MetaOutput.Metafluid <> nil) and (Block.Facility.Town <> nil) // >> This is to avoid a particular case
              then
                begin
                  if Block.Facility.Company <> nil
                    then CompanyName := Block.Facility.Company.GetCacheName
                    else CompanyName := 'General';
                  result.AddLink(
                    CreateOutputLink(
                      Block.Facility.xPos,
                      Block.Facility.yPos,
                      FluidData.K,
                      round(SmallFloatShift*Price),
                      round(SmallFloatShift*MetaOutput.Metafluid.TransCost),
                      MetaOutput.Metafluid.Id,
                      Block.Facility.Town.Name,
                      CompanyName,
                      Block.Facility.GetCacheName,
                      '',
                      Block.Facility.MetaFacility.Kind.Role)); // >>
                end
              // else raise Exception.Create('Invalid Town or fluid in "' + Block.Facility.GetCacheName + '"');
          finally
            Block.Facility.Unlock;
          end;
        end;
    end;

  class function TOutputCacheAgent.UpdateCache( Obj : TObject ) : TObjectCache;
    var
      i           : integer;
      CompanyName : string;
    begin
      result := inherited UpdateCache( Obj );
      try
        with TOutput(Obj) do
          begin
            Block.Facility.Lock;
            try
              if MetaOutput.MetaFluid <> nil
                then
                  begin
                    result.WriteString( 'Name', MetaOutput.MetaFluid.Name );
                    result.WriteString( 'MetaFluid', MetaOutput.MetaFluid.Id );
                  end
                else
                  begin
                    result.WriteString( 'Name', 'Unknown' );
                    result.WriteString( 'MetaFluid', 'Unknown' );
                  end;
              result.WriteInteger( 'GateIndex', MetaOutput.Index );
              result.WriteString( 'FluidValue', MetaOutput.MetaFluid.FormatValue( FluidData.Q ) );
              result.WriteFloat( 'nfFluidValue', MetaOutput.MetaFluid.ConvertToUnits( FluidData.Q  ));
              result.WriteInteger( 'FluidQuality', FluidData.K );
              result.WriteString( 'MetaOutput', MetaOutput.Name );
              result.WriteString( 'Price', FormatMoney( Price ));
              result.WriteCurrency( 'Price', Price );
              result.WriteString( 'Capacity', MetaOutput.MetaFluid.FormatValue( MetaOutput.MaxFluid.Q ) );
              result.WriteFloat( 'nfCapacity', MetaOutput.MetaFluid.ConvertToUnits( MetaOutput.MaxFluid.Q ));
              result.WriteCurrency( 'MarketPrice', MetaOutput.MetaFluid.MarketPrice );
              result.WriteInteger( 'PricePc', PricePerc );
              for i := 0 to pred(ConnectionCount) do
                begin
                  Connections[i].Block.Facility.Lock;
                  try
                    if Connections[i].MetaInput.MetaFluid <> nil
                      then result.WriteString( 'cnxName' + IntToStr(i), Connections[i].MetaInput.MetaFluid.Name )
                      else result.WriteString( 'cnxName' + IntToStr(i), 'Unknown' );
                    result.WriteString( 'cnxId' + IntToStr(i), Connections[i].MetaInput.Name );
                    result.WriteString( 'cnxFacilityName' + IntToStr(i), Connections[i].Block.Facility.Name );
                    result.WriteString( 'cnxFacilityPath' + IntToStr(i), GetObjectPath( Connections[i].Block.Facility ));
                    result.WriteString( 'cnxFacilityId' + IntToStr(i), Connections[i].Block.Facility.GetCacheName + NameSeparator + IntToStr(Block.Facility.xPos) + NameSeparator + IntToStr(Block.Facility.xPos));
                    if Connections[i].Block.Facility.Company <> nil
                      then
                        begin
                          CompanyName := Connections[i].Block.Facility.Company.Name;
                          if Connections[i].Block.Facility.Company.Owner <> nil
                            then result.WriteString( 'cnxCreatedBy' + IntToStr(i), Connections[i].Block.Facility.Company.Owner.Name );
                        end
                      else CompanyName := 'Unknown';
                    result.WriteString( 'cnxCompanyName' + IntToStr(i), CompanyName );
                    result.WriteInteger( 'cnxXPos' + IntToStr(i), Connections[i].Block.Facility.xPos );
                    result.WriteInteger( 'cnxYPos' + IntToStr(i), Connections[i].Block.Facility.yPos );
                    result.WriteInteger( 'cnxTrouble' + IntToStr(i), Connections[i].Block.Facility.Trouble );
                    result.WriteInteger( 'cnxVisualId' + IntToStr(i), Connections[i].Block.Facility.VisualClass );
                    result.WriteInteger( 'cnxConnectionCount' + IntToStr(i), Connections[i].ConnectionCount );
                    result.WriteString( 'cnxLastYear' + IntToStr(i), FormatMoney( Connections[i].Block.Facility.PeriodMoney ));
                    result.WriteCurrency( 'cnxNfLastYear' + IntToStr(i), Connections[i].Block.Facility.PeriodMoney );
                    CacheExtraInfo( 'CnxInfo' + IntToStr(i), ExtraConnectionInfo[i], MetaOutput.MetaFluid, result );
                  finally
                    Connections[i].Block.Facility.Unlock;
                  end;
                end;
            finally
              Block.Facility.Unlock;
            end;
          end;
      except
      end;
    end;


  // TMoneyDealerCacheAgent

  class function TMoneyDealerCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    var
      MoneyReason : TMoneyReason;
    begin
      result := inherited GetCache( Obj );
      with TMoneyDealer(Obj) do
        begin
          Lock;
          try
            for MoneyReason := low(MoneyReason) to high(MoneyReason) do
              begin
                result.WriteString( MoneyReasonId[MoneyReason], FormatMoney( PeriodMoney[MoneyReason] ));
                result.WriteCurrency( 'nf' + MoneyReasonId[MoneyReason], PeriodMoney[MoneyReason] );
              end;
          finally
            Unlock;
          end;
        end;
    end;


  // TCompanyCacheAgent

  class function TCompanyCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TCompany(Obj) do
        result := tidCachePath_Companies + GetCacheName + '.five\'
    end;

  class function TCompanyCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache( Obj );
      with TCompany(Obj) do
        begin
          Lock;
          try
            result.WriteString( 'Name', Name );
            result.WriteInteger( 'CompanyId', Id );
            result.WriteString( ppTTL, CreateTTL( 0, 0, 10, 0 )); // >> Is this necesary?
            if Owner <> nil
              then result.WriteString( 'Tycoon', Owner.Name );
            if Cluster <> nil
              then result.WriteString( 'Cluster', Cluster.Id );
            result.WriteString( 'Technology', TechDescriptor );
            result.AddLink(GetGlobalPath(tidCachePath_Clusters + Cluster.Id + '.five\Companies\'));
            if Owner <> nil
              then result.AddLink(GetGlobalPath(tidCachePath_Tycoons + Owner.GetCacheName + '.five\Companies\' + GetCacheName + '\'));
          finally
            Unlock;
          end;
        end;
    end;


  // TTycoonCacheAgent

  class function TTycoonCacheAgent.GetPath( Obj : TObject ) : string;
    begin
      with TTycoon(Obj) do
        result := tidCachePath_Tycoons + GetCacheName + '.five\'
    end;

  class function TTycoonCacheAgent.GetCache( Obj : TObject ) : TObjectCache;

    function GetFacilityId( Facility : TFacility ) : string;
      begin
        with Facility do
          result := IntToStr(xPos) + ',' + IntToStr(yPos) + ',';
      end;

    var
      i, j      : integer;
      MetaFluid : TMetaFluid;
    begin
      result := inherited GetCache( Obj );
      try
        with TTycoon(Obj) do
          begin
            Lock;
            try
              result.WriteString(ppTTL, CreateTTL( 0, 0, 2, 0 ));
              result.WriteString( 'Name', Name );
              result.WriteString( 'RealName', RealName );
              result.WriteInteger( 'SecurityId', integer(Obj) );
              result.WriteCurrency( 'Budget', Budget );
              result.WriteCurrency( 'Loan', Loan );
              result.WriteInteger( 'LoanInterest', LoanInterest );
              result.WriteInteger( 'AutoConnectionCount', AutoConnections.Count );
              result.WriteInteger( 'SuperRole', integer(SuperRole) );
              result.WriteCurrency( 'NetProfit', NetProfit );
              result.WriteInteger( 'Ranking', Ranking );
              AutoConnections.Lock;
              try
                for i := 0 to pred(AutoConnections.Count) do
                  with TAutoConnection(AutoConnections[i]) do
                    begin
                      MetaFluid := TMetaFluid(TheClassStorage.ClassById[ 'Fluids', MetaFluidId ]);
                      result.WriteString( 'AutoConnection' + IntToStr(i), MetaFluidId );
                      result.WriteString( 'AutoConnection' + MetaFluidId + 'Name', MetaFluid.Name );
                      result.WriteInteger( 'AutoConnection' + MetaFluidId + 'Count', Connections.Count );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'HireTradeCenter', HireTradeCenter );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'HireOnlyWarehouses', HireOnlyWarehouses );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'Storable', mfStorable in MetaFluid.Options );
                      Connections.Lock;
                      try
                        for j := 0 to pred(Connections.Count) do
                          begin
                            result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityName', TFacility(Connections[j]).Name );
                            result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityCompany', TFacility(Connections[j]).Company.Name );
                            result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityId', GetFacilityId( TFacility(Connections[j]) ));
                          end;
                      finally
                        Connections.Unlock;
                      end;
                    end;
              finally
                AutoConnections.Unlock;
              end;
              result.WriteInteger( 'RolesCount', Roles.Count );
              Roles.Lock;
              try
                for i := 0 to pred(Roles.Count) do
                  result.WriteString( 'Role' + IntToStr(i), TTycoon(Roles[i]).Name );
              finally
                Roles.Unlock;
              end;
            finally
              Unlock;
            end;
          end;
      except
      end;
    end;


  // TownCacheAgent

  class function TTownCacheAgent.GetPath ( Obj : TObject ) : string;
    begin
      with TTown(Obj) do
        result := tidCachePath_Towns + Name + '.five\'
    end;

  class function TTownCacheAgent.GetCache( Obj : TObject ) : TObjectCache;
    begin
      result := inherited GetCache(Obj);
      with TInhabitedTown(Obj) do
        begin
          Lock;
          try
            TownHall.CurrBlock.StoreToCache( result );
            result.WriteInteger( 'VisualId', TownHall.VisualClass );
            result.WriteInteger( 'xPos', TownHall.xPos );
            result.WriteInteger( 'yPos', TownHall.yPos );
            result.WriteString('Name', Name);
            result.WriteString(ppTTL, CreateTTL( 0, 0, 0, 30 ));
          finally
            Unlock;
          end;
        end;
    end;

  // TInhabitedTownCacheAgent

  class function TInhabitedTownCacheAgent.GetCache(Obj : TObject) : TObjectCache;
    begin
      result := inherited GetCache(Obj);
      with TInhabitedTown(Obj) do
        begin
          Lock;
          try
            TownHall.CurrBlock.StoreToCache( result );
          finally
            Unlock;
          end;
        end;
    end;


  // RegisterCachers

  procedure RegisterCachers;
    begin
      RegisterCacher( TMetaInstance.ClassName, TMetaInstanceCacheAgent );
      RegisterCacher( TMetaFluid.ClassName, TMetaFluidCacheAgent );
      RegisterCacher( TInvention.ClassName, TInventionCacheAgent );
      RegisterCacher( TMetaFacility.ClassName, TMetaFacilityCacheAgent );
      RegisterCacher( TFacilityKind.ClassName, TFacilityKindCacheAgent );
      RegisterCacher( TEvlStage.ClassName, TEvlStageCacheAgent );
      RegisterCacher( TMetaBlock.ClassName, TMetaBlockCacheAgent );
      RegisterCacher( TFacility.ClassName, TFacilityCacheAgent );
      RegisterCacher( TInput.ClassName, TInputCacheAgent );
      RegisterCacher( TOutput.ClassName, TOutputCacheAgent );
      RegisterCacher( TMoneyDealer.ClassName, TMoneyDealerCacheAgent );
      RegisterCacher( TCompany.ClassName, TCompanyCacheAgent );
      RegisterCacher( TTycoon.ClassName, TTycoonCacheAgent );
      RegisterCacher( TInhabitedTown.ClassName, TInhabitedTownCacheAgent );
    end;


end.


