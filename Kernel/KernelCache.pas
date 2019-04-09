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
    tidCachePath_Curriculum     = 'Curriculum\';
    tidCachePath_Towns          = 'Towns\';
    tidCachePath_Inputs         = 'Inputs\';
    tidCachePath_Outputs        = 'Outputs\';

  type
    TMetaInstanceCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TInstanceCacheAgent =
      class( TCacheAgent )
      end;

    TMetaFluidCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TFacilityKindCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TInventionCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TMetaFacilityCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TEvlStageCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TMetaBlockCacheAgent =
      class( TMetaInstanceCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TFacilityCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;        override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;  override;
          class function UpdateCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache; override;
      end;

    TGateCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function  GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;    override;
          class function  UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
          class procedure CacheExtraInfo(Name : string; ExtraConnectionInfo : PExtraConnectionInfo; MetaFluid : TMetaFluid; Cache : TObjectCache; dt : single);
      end;

    TInputCacheAgent =
      class( TGateCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;          override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;    override;
          class function UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TOutputCacheAgent =
      class( TGateCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;          override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;    override;
          class function UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TMoneyDealerCacheAgent =
      class( TCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TCompanyCacheAgent =
      class( TMoneyDealerCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TCurriculumCacheAgent =
      class( TCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TTycoonCacheAgent =
      class( TMoneyDealerCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
          class function UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TTownCacheAgent =
      class( TInstanceCacheAgent )
        public
          class function GetPath ( Obj : TObject; kind, info : integer ) : string;       override;
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

    TInhabitedTownCacheAgent =
      class( TTownCacheAgent )
        public
          class function GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache; override;
      end;

  procedure RegisterCachers;


implementation

  uses
    SysUtils, SpecialChars, CacheCommon, MathUtils, CacheNameUtils, Plotter,
    ClassStorage, Trade, Collection, Protocol, MetaInstances, Inventions,
    Languages, Logs, BasicAccounts, BasicTaxes, TycoonLevels, Variants;


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

  // TMetaInstance

  class function TMetaInstanceCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TMetaInstance(Obj) do
        result := Family + '\' + Id + '.five';
    end;

  class function TMetaInstanceCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TMetaInstance(Obj) do
        begin
          result.WriteString( 'Id', Id );
          result.WriteString( 'Family', Family );
        end;
    end;


  // TMetaFluidCacheAgent

  class function TMetaFluidCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TMetaFluid(Obj) do
        begin
          result.WriteString( 'Id', Id );
          //result.WriteString( 'Name', Name );
          StoreMultiStringToCache( 'Name', Name_MLS, result );
          //result.WriteString( 'UnitName', UnitName );
          StoreMultiStringToCache( 'UnitName', UnitName_MLS, result );
          //result.WriteString( 'FluidName', FluidName );
          StoreMultiStringToCache( 'FluidName', FluidName_MLS, result );
          //result.WriteString( 'Description', Description );
          StoreMultiStringToCache( 'Description', Description_MLS, result );
          result.WriteFloat( 'TransCost', TransCost );
          result.WriteInteger( 'MarketPrice', round(MarketPrice) );
          result.WriteBoolean( 'Tradeable', mfTradeable in Options );
        end;
    end;


  // TFacilityKindCacheAgent

  class function TFacilityKindCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TFacilityKind(Obj) do
        result := tidCachePath_Clusters + ClusterName + '\' + tidCachePath_MetaFacilities + Format('%.8x', [Index]) + '.' + Id + '.five\';
    end;

  class function TFacilityKindCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TFacilityKind(Obj) do
        begin
          result.WriteString( 'Name', Name );
          StoreMultiStringToCache( 'Name', Name_MLS, result );
          result.WriteString( 'ClusterName', ClusterName );
          result.WriteString( 'TechnologyName', Technology );
          result.WriteString( 'SuperType', SuperType );
        end;
    end;


  // TInventionCacheAgent

  class function TInventionCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
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

  class function TMetaFacilityCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    var
      MetaFac : TMetaFacility;
    begin
      MetaFac := TMetaFacility(Obj);
      if MetaFac.Kind <> nil
        then result := TFacilityKindCacheAgent.GetPath( MetaFac.Kind, kind, info ) + Format('%.8x', [MetaFac.Index]) + '.' + MetaFac.Id + '.five\'
        else result := tidCachePath_Clusters + MetaFac.ClusterName + '\' + MetaFac.Family + '\' + MetaFac.FacilityKind + '.five\' + Format('%.8x', [MetaFac.Index]) + '.' + MetaFac.Id + '.five\';
    end;

  class function TMetaFacilityCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      i : integer;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TMetaFacility(Obj) do
        begin
          //result.WriteString( 'Name', Name );
          //result.WriteString( 'Desc', Desc );
          //result.WriteString( 'Requires', Reqs );
          StoreMultiStringToCache( 'Name', Name_MLS, result );
          StoreMultiStringToCache( 'Desc', Desc_MLS, result );
          StoreMultiStringToCache( 'Requires', Requires_MLS, result );
          result.WriteInteger( 'Level', Level );
          result.WriteInteger( 'XSize', XSize );
          result.WriteInteger( 'YSize', YSize );
          result.WriteString( 'Size', IntToStr(YSize*YSize*400) + ' m²' );
          result.WriteBoolean( 'InTown', mfcInTown in Options );
          result.WriteString( 'Cluster', ClusterName );
          result.WriteString( 'Kind', FacilityKind );
          if (Technology <> nil) and not Technology.Obsolete
            then result.WriteString('TechnologyName', Technology.Id);
          result.WriteString( 'Price', FormatMoney( Price/1000 ) + 'K' );
          result.WriteCurrency( 'nfPrice', Price );
          result.WriteString( 'ImportPrice', FormatMoney( TradeCenterPrice*Price/100000 ) + 'K' );
          result.WriteCurrency( 'nfImportPrice', TradeCenterPrice*Price/100 );
          result.WriteInteger( 'ZoneType', ZoneType );
          result.WriteString( 'TypicalStageBlock', TypicalStage.MetaBlock.Id );
          result.WriteString( 'TypicalStage', TypicalStage.Id );
          TypicalStage.MetaBlock.StoreExtraInfoToCache( result );
          result.WriteInteger( 'TypicalVisualClass', VisualClass + TypicalStage.Index );
          result.WriteInteger( 'Uniqueness', UniquenessMask );
          for i := 0 to pred(EvlStages.Count) do
            CacheMetaObject( EvlStages[i], noKind, noInfo );
        end;
    end;


  // TEvlStageCacheAgent

  class function TEvlStageCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TEvlStage(Obj) do
        result := TMetaFacilityCacheAgent.GetPath( MetaFacility, kind, info ) + tidCachePath_EvlStages + Id + '.five'
    end;

  class function TEvlStageCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TEvlStage(Obj) do
        begin
          result.WriteString( 'Id', Id );
          result.WriteString( 'Name', Name );
          result.WriteString( 'Description', Description );
          result.WriteString( 'MetaBlock', MetaBlock.Id );
        end;
    end;


  // TMetaBlockCacheAgent

  class function TMetaBlockCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      {with TMetaBlock(Obj) do
        begin
          result.WriteInteger( 'XSize', XSize );
          result.WriteInteger( 'YSize', YSize );
        end;}
    end;


  // TFacilityCacheAgent

  class function TFacilityCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TFacility(Obj) do
        if (Company <> nil) and (Company.Name <> '')
         then result := GetObjectPath( Company, noKind, noInfo ) + Name + NameSeparator + IntToStr(xPos) + NameSeparator + IntToStr(yPos) + '.five\'
         else result := tidCachePath_Companies + 'General\' + Trim(Name) + NameSeparator + IntToStr(xPos) + NameSeparator + IntToStr(yPos) + '.five\';
    end;

  class function TFacilityCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TFacility(Obj) do
        begin
          Lock;
          try
            result.WriteString ( ppRnAgent, 'Facility' );
            result.WriteString ( ppObjLoc, IntToStr(xPos) + ',' + IntToStr(yPos) );
            result.WriteInteger( 'VisualId', VisualClass );
            result.WriteString ( 'Name', Name );
            result.WriteInteger( 'xPos', xPos );
            result.WriteInteger( 'yPos', yPos );
            if update
              then
                begin
                  try
                    CurrBlock.StoreLinksToCache( result );
                  except
                    on E : Exception do
                      raise Exception.Create( 'Cannot cache facility links: ' + Name + ' (' + E.Message + ')' );
                  end;
                  if MetaFacility.Kind <> nil
                    then
                      begin
                        if (Town <> nil) and (Company <> nil) and (Company.Owner <> nil)
                          then
                            begin
                              result.AddLink(GetGlobalPath(tidCachePath_Towns + Town.Name + '.five\Facilities\' + MetaFacility.Kind.SuperType + '\'));
                              result.AddLink(GetGlobalPath(tidCachePath_Towns + Town.Name + '.five\Companies\' + Company.Name + '\' + MetaFacility.Kind.SuperType + '\'));
                              //result.AddLink(GetGlobalPath(tidCachePath_Tycoons + Company.Owner.Name + '.five\Companies\' + Company.Name + '\Facilities\' + MetaFacility.Kind.SuperType + '\'));
                            end;
                      end;
                end;
            result.AddLink(CreateMapLink(xPos, yPos, result.Path));
            if Company <> nil
              then
                begin
                  result.WriteString( 'Company', Company.Name );
                  result.WriteString( 'Cluster', Company.Cluster.Id );
                  if Company.Owner <> nil
                    then
                      begin
                        result.WriteString( 'Creator', Company.Owner.Name );
                        if MetaFacility.Kind <> nil
                          then result.AddLink(GetGlobalPath(tidCachePath_Tycoons + Company.Owner.Name + '.five\Companies\' + Company.Name + '\Facilities\' + MetaFacility.Kind.SuperType + '\'));
                      end;
                end;
            result.WriteInteger('Years', Age div (365*24));
          finally
            Unlock;
          end;
        end;
    end;

  class function TFacilityCacheAgent.UpdateCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
    begin
      result := inherited UpdateCache( Obj, kind, info, update );
      with TFacility(Obj) do
        begin
          Lock;
          try
            result.WriteString(ppTTL, CreateTTL( 0, 0, 2, 0 )); // 2
            if (Company <> nil) and (Company.Owner <> nil)
              then result.WriteString( 'SecurityId', Company.Owner.SecurityId );
            result.WriteString( 'Path', TFacilityCacheAgent.GetPath( Obj, kind, info ));
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
            //result.WriteString( 'MetaFacilityName', MetaFacility.Name );
            StoreMultiStringToCache( 'MetaFacilityName', MetaFacility.Name_MLS, result );
            if Town <> nil
              then result.WriteString ( 'Town', Town.Name );
            result.WriteInteger( 'ROI', ROI );
            result.WriteCurrency( 'NetProfit', NetProfit );
            result.WriteCurrency( 'Cost', Cost );
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
                  result.WriteString( 'MoneyGraphInfo', MoneyGraph.Serialize );
                end
              else result.WriteBoolean( 'MoneyGraph', false );
            result.WriteString( 'NearCircuits', RenderCircuitStr( CurrBlock.Circuits[cirRoads] ) );
          finally
            Unlock;
          end;
        end;
    end;

  // TGateCacheAgent

  class function TGateCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      result.WriteString(ppRefLinks, 'yes');
      if kind = noKind
        then
          with TGate(Obj) do
            result.WriteString(ppTTL, NULLTTL);
    end;

  class function TGateCacheAgent.UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited UpdateCache( Obj, kind, info, update );
      if kind = noKind
        then
          with TGate(Obj) do
            begin
              result.WriteString(ppTTL, CreateTTL( 0, 0, 0, 30)); // was 5 minutes
              result.WriteInteger( 'cnxCount', ConnectionCount );
              result.WriteCurrency( 'LastCost', LastCost );
              StoreToCache( result );
            end;
    end;

  class procedure TGateCacheAgent.CacheExtraInfo(Name : string; ExtraConnectionInfo : PExtraConnectionInfo; MetaFluid : TMetaFluid; Cache : TObjectCache; dt : single);
    begin
      if ExtraConnectionInfo <> nil
        then
          begin
            Cache.WriteString( 'LastValue' + Name, MetaFluid.FormatValue(realmax(0, ExtraConnectionInfo.LastFluid), langDefault)); // >> MLS3
            Cache.WriteBoolean( 'Connected' + Name, ExtraConnectionInfo.Connected );
            Cache.WriteInteger( 'OverPrice' + Name, ExtraConnectionInfo.OverPrice );
            Cache.WriteString('tCost' + Name, FormatMoney(MetaFluid.ConvertToUnits(ExtraConnectionInfo.LastFluid)*ExtraConnectionInfo.Distance*MetaFluid.TransCost/2));
          end;
    end;


  // TInputCacheAgent

  class function TInputCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TInput(Obj) do
        result := GetObjectPath( Block.Facility, noKind, noInfo ) + tidCachePath_Inputs + Format('%.8x', [MetaInput.Index]) + '.' + MetaInput.Name + '.five\';
      if kind > noKind
        then result := result + IntToStr(info div SubObjForFile) + '.five';
    end;

  class function TInputCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      OwnerName : string;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      if kind = noKind
        then
          with TInput(Obj) do
            begin
              if (MetaInput.MetaFluid <> nil) and (Block.Facility.Town <> nil) and (Block.Facility.Company <> nil)
                then
                  begin
                    result.WriteString ( ppRnAgent, 'Input' );
                    result.WriteString ( ppObjLoc, IntToStr(Block.xPos) + ',' + IntToStr(Block.yPos) + ',' + MetaInput.Name); // >>
                    if (MetaInput.MetaFluid.MarketPrice > 0) and (LastValue.Q > 0)
                      then result.WriteInteger( 'LastCostPerc', round(100*LastCost/(LastValue.Q*MetaInput.MetaFluid.MarketPrice)) )
                      else result.WriteInteger( 'LastCostPerc', 0 );
                    if Block.Facility.Company.Owner <> nil
                      then OwnerName := Block.Facility.Company.Owner.Name
                      else OwnerName := Block.Facility.Company.Name;
                    if Block.Facility.MetaFacility.Kind <> nil
                      then
                        result.AddLink(
                          CreateInputLink(
                            Block.Facility.xPos,
                            Block.Facility.yPos,
                            0,
                            ConnectionCount,
                            MetaInput.Metafluid.Id,
                            Block.Facility.Town.Name,
                            OwnerName,
                            Block.Facility.Name,
                            RenderCircuitStr(Block.Circuits[cirRoads]),
                            Block.Role)); // >>
                  end
            end;
    end;

  class function TInputCacheAgent.UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;

    var
      dt : single;

    function GetFluidType( MetaFluid : TMetaFluid ) : string;
      begin
        if mfAdvertisement in MetaFluid.Options
          then result := 'ADVERTISEMENT'
          else
            if mfService in MetaFluid.Options
              then result := 'SERVICE'
              else result := 'NORMAL';
      end;

    procedure CacheConnection(idx : integer);
      var
        Name : string;
        fIdx : integer;
        tIdx : integer;
        i    : integer;
        iStr : string;
        Connection : TOutput;
      begin
        fIdx := SubObjForFile*(idx div SubObjForFile);
        tIdx := min(TInput(Obj).Connectioncount, fIdx + SubObjForFile);
        result.WriteString(ppTTL, CreateTTL( 0, 0, 0, 30)); // 5
        for i := fIdx to pred(tIdx) do
          with TInput(Obj) do
            begin
              iStr := IntToStr(i);
              Connection := Connections[i];
              result.WriteString( 'cnxFacilityName' + iStr, Connection.Block.Facility.Name );
              if Connection.Block.Facility.Company <> nil
                then Name := Connection.Block.Facility.Company.Name
                else Name := 'Unknown';
              result.WriteString( 'cnxCompanyName' + iStr, Name );
              result.WriteCurrency( 'cnxNfPrice' + iStr, Connection.Price );
              if (MetaInput.MetaFluid <> nil) and (MetaInput.MetaFluid.MarketPrice > 0)
                then result.WriteInteger( 'cnxPricePerc' + iStr, round(100*Connection.Price/MetaInput.MetaFluid.MarketPrice) )
                else result.WriteInteger( 'cnxPricePerc' + iStr, 0 );
              result.WriteString( 'cnxQuality' + iStr, IntToStr(Connection.FluidData.K) + '%' );
              if (Connection.Block.Facility.Company <> nil) and (Connection.Block.Facility.Company.Owner <> nil)
                then result.WriteString( 'cnxCreatedBy' + iStr, Connection.Block.Facility.Company.Owner.Name );
              result.WriteInteger( 'cnxXPos' + iStr, Connection.Block.Facility.xPos );
              result.WriteInteger( 'cnxYPos' + iStr, Connection.Block.Facility.yPos );
              CacheExtraInfo('CnxInfo' + iStr, ExtraConnectionInfo[i], MetaInput.MetaFluid, result, dt);
            end;
      end;

    begin
      result := inherited UpdateCache( Obj, kind, info, update );
      dt := TInput(Obj).Block.dt;
      try
        if kind = noKind
          then
            with TInput(Obj) do
              begin
                if MetaInput.MetaFluid <> nil
                  then
                    begin
                      //result.WriteString( 'Name', MetaInput.MetaFluid.Name );
                      StoreMultiStringToCache( 'Name', MetaInput.MetaFluid.Name_MLS, result );
                      //result.WriteString( 'UnitName', UnitName );
                      StoreMultiStringToCache( 'UnitName', MetaInput.MetaFluid.UnitName_MLS, result );
                      //result.WriteString( 'FluidName', FluidName );
                      StoreMultiStringToCache( 'FluidName', MetaInput.MetaFluid.FluidName_MLS, result );
                      //result.WriteString( 'Description', Description );
                      //>>StoreMultiStringToCache( 'Description', MetaInput.MetaFluid.Description_MLS, result );
                      result.WriteString( 'MetaFluid', MetaInput.MetaFluid.Id );
                      result.WriteString( 'FluidType', GetFluidType( MetaInput.MetaFluid ));
                    end
                  else
                    begin
                      result.WriteString( 'Name', 'Unknown' );
                      result.WriteString( 'MetaFluid', 'Unknown' );
                    end;
                result.WriteInteger( 'GateIndex', MetaInput.Index );
                result.WriteString('FluidValue', MetaInput.MetaFluid.FormatValue(LastValue.Q/dt, langDefault));
                result.WriteFloat('nfFluidValue', MetaInput.MetaFluid.ConvertToUnits(LastValue.Q/dt));
                result.WriteInteger( 'FluidQuality', LastValue.K );
                result.WriteString( 'MetaInput', MetaInput.Name );
                result.WriteString( 'ActualMaxFluidValue', MetaInput.MetaFluid.FormatValue( ActualMaxFluid.Q, langDefault ) );
                result.WriteFloat( 'nfActualMaxFluidValue', MetaInput.MetaFluid.ConvertToUnits( ActualMaxFluid.Q ));
                result.WriteString( 'Capacity', MetaInput.MetaFluid.FormatValue( MetaInput.MaxFluid.Q, langDefault ));
                result.WriteFloat( 'nfCapacity', MetaInput.MetaFluid.ConvertToUnits( MetaInput.MaxFluid.Q ));
              end
          else
            if info < TInput(Obj).ConnectionCount
              then CacheConnection(info);
      except
      end;
    end;


  // TOutputCacheAgent

  class function TOutputCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TOutput(Obj) do
        result := GetObjectPath( Block.Facility, noKind, noInfo ) + tidCachePath_Outputs + MetaOutput.Name + '.five\';
      if kind > noKind
        then result := result + IntToStr(info div SubObjForFile) + '.five';
    end;

  class function TOutputCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      OwnerName : string;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      if kind = noKind
        then
          with TOutput(Obj) do
            begin
              if (MetaOutput.Metafluid <> nil) and (Block.Facility.Town <> nil) and (Block.Facility.Company <> nil)// >> This is to avoid a particular case
                then
                  begin
                    result.WriteString ( ppRnAgent, 'Output' );
                    result.WriteString ( ppObjLoc, IntToStr(Block.xPos) + ',' + IntToStr(Block.yPos) + ',' + MetaOutput.Name); // >>
                    if Block.Facility.Company <> nil
                      then
                        begin
                          if Block.Facility.Company.Owner <> nil
                            then OwnerName := Block.Facility.Company.Owner.Name
                            else OwnerName := Block.Facility.Company.Name;
                          result.AddLink(
                            CreateOutputLink(
                              Block.Facility.xPos,
                              Block.Facility.yPos,
                              FluidData.K,
                              round(SmallFloatShift*Price),
                              round(SmallFloatShift*MetaOutput.Metafluid.TransCost),
                              MetaOutput.Metafluid.Id,
                              Block.Facility.Town.Name,
                              OwnerName,
                              Block.Facility.Name,
                              RenderCircuitStr(Block.Circuits[cirRoads]),
                              Block.Role)); // >>
                        end;
                  end;
            end;
    end;

  class function TOutputCacheAgent.UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;

    var
      dt : single;

    procedure CacheConnection(idx : integer);
      var
        Name : string;
        fIdx : integer;
        tIdx : integer;
        i    : integer;
        iStr : string;
        Connection : TInput;
      begin
        fIdx := SubObjForFile*(idx div SubObjForFile);
        tIdx := min(TOutput(Obj).ConnectionCount, fIdx + SubObjForFile);
        result.WriteString(ppTTL, CreateTTL(0, 0, 0, 30));
        for i := fIdx to pred(tIdx) do
          with TOutput(Obj) do
            begin
              Connection := Connections[i];
              iStr := IntToStr(i);
              {
              >> WEIRD!
              if Connection.MetaInput.MetaFluid <> nil
                then StoreMultiStringToCache( 'cnxName' + iStr + '.', Connection.MetaInput.MetaFluid.Name_MLS, result );
              }
              //Name := Connection.MetaInput.MetaFluid.Name_MLS.Values[langDefault] // >> MLS2
              //result.WriteString( 'cnxName' + iStr, Name );
              result.WriteString( 'cnxFacilityName' + iStr, Connection.Block.Facility.Name );
              if Connection.Block.Facility.Company <> nil
                then
                  begin
                    Name := Connection.Block.Facility.Company.Name;
                    if Connection.Block.Facility.Company.Owner <> nil
                      then result.WriteString( 'cnxCreatedBy' + iStr, Connection.Block.Facility.Company.Owner.Name );
                  end
                else Name := 'Unknown';
              result.WriteString( 'cnxCompanyName' + iStr, Name );
              result.WriteInteger( 'cnxXPos' + iStr, Connection.Block.Facility.xPos );
              result.WriteInteger( 'cnxYPos' + iStr, Connection.Block.Facility.yPos );
              CacheExtraInfo('CnxInfo' + iStr, ExtraConnectionInfo[i], MetaOutput.MetaFluid, result, dt);
            end;
      end;

    begin
      result := inherited UpdateCache( Obj, kind, info, update );
      try
        dt := TOutput(Obj).Block.dt;
        if kind = noKind
          then
            with TOutput(Obj) do
              begin
                if MetaOutput.MetaFluid <> nil
                  then
                    begin
                      //result.WriteString( 'Name', MetaOutput.MetaFluid.Name );
                      result.WriteString( 'MetaFluid', MetaOutput.MetaFluid.Id );
                      StoreMultiStringToCache( 'Name', MetaOutput.MetaFluid.Name_MLS, result );
                    end
                  else
                    begin
                      result.WriteString( 'Name', 'Unknown' );
                      result.WriteString( 'MetaFluid', 'Unknown' );
                    end;
                result.WriteInteger( 'GateIndex', MetaOutput.Index );
                result.WriteString('FluidValue', MetaOutput.MetaFluid.FormatValue(FluidData.Q/dt, langDefault));
                result.WriteFloat('nfFluidValue', MetaOutput.MetaFluid.ConvertToUnits(FluidData.Q/dt));
                result.WriteString('LastFluid', MetaOutput.MetaFluid.FormatValue(LastFluid/dt, langDefault));
                result.WriteInteger( 'FluidQuality', FluidData.K );
                result.WriteString( 'MetaOutput', MetaOutput.Name );
                result.WriteString( 'Price', FormatMoney( Price ));
                result.WriteCurrency( 'Price', Price );
                result.WriteString( 'Capacity', MetaOutput.MetaFluid.FormatValue( MetaOutput.MaxFluid.Q, langDefault ) );
                result.WriteFloat( 'nfCapacity', MetaOutput.MetaFluid.ConvertToUnits( MetaOutput.MaxFluid.Q ));
                result.WriteCurrency( 'MarketPrice', MetaOutput.MetaFluid.MarketPrice );
                result.WriteInteger( 'PricePc', PricePerc );
              end
          else
            if info < TOutput(Obj).ConnectionCount
              then CacheConnection(info);
      except
      end;
    end;


  // TMoneyDealerCacheAgent

  class function TMoneyDealerCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      i : integer;

    function VirtDateToDateTime(d : TVirtDateAbs) : TDateTime;
      var
        year0 : TDateTime;
        oneyr : TDateTime;
        rest  : TDateTime;
      begin
        year0  := EncodeDate(YearZero, 3, 9);
        oneyr  := 365;
        rest   := d/24;
        result := year0 + oneyr + rest; //EncodeDate(YearZero + 1, 3, 9) + d/24;
      end;

    begin
      result := inherited GetCache( Obj, kind, info, update );        
      with TMoneyDealer(Obj) do
        begin
          Lock;
          try
            Accounts.StoreToCache( result );
            result.WriteInteger( 'LoanCount', Loans.Count );
            for i := 0 to pred(Loans.Count) do
              with TLoan(Loans[i]) do
                begin
                  result.WriteString( 'LoanBankName' + IntToStr(i), Bank.Name );
                  result.WriteInteger( 'LoanInterest' + IntToStr(i), Interest );
                  result.WriteCurrency( 'LoanAmount' + IntToStr(i), Amount );
                  result.WriteCurrency( 'LoanSlice' + IntToStr(i), Slice );
                  result.WriteInteger( 'LoanTerm' + IntToStr(i), Term );
                  result.WriteInteger( 'LoanDate' + IntToStr(i), Date );
                  result.WriteString( 'PayDate' + IntToStr(i), DateToStr(VirtDateToDateTime(Date)));
                end;
          finally
            Unlock;
          end;
        end;
    end;


  // TCompanyCacheAgent

  class function TCompanyCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TCompany(Obj) do
        result := tidCachePath_Companies + Trim(Name) + '.five\'
    end;

  class function TCompanyCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      path : string;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TCompany(Obj) do
        begin
          Lock;
          try
            result.WriteString (ppRnAgent, 'Company' );
            result.WriteString (ppObjLoc, Name);
            result.WriteString( 'Name', Name );
            result.WriteInteger( 'CompanyId', Id );
            result.WriteString(ppTTL, CreateTTL(0, 0, 5, 0));
            if Owner <> nil
              then result.WriteString( 'Tycoon', Owner.Name );
            if Cluster <> nil
              then result.WriteString( 'Cluster', Cluster.Id );
            result.WriteString( 'Technology', TechDescriptor );
            result.WriteInteger( 'Uniqueness', UniquenessMask );
            //result.AddLink(GetGlobalPath(tidCachePath_Clusters + Cluster.Id + '.five\Companies\'));
            if Owner <> nil
              then
                begin
                  path := GetGlobalPath(tidCachePath_Tycoons + Owner.Name + '.five\Companies\' + Name + '\');
                  result.AddLink(path);
                  result.AddDelPath(path);
                end;
          finally
            Unlock;
          end;
        end;
    end;


  // TCurriculumCacheAgent

  class function TCurriculumCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TCurriculumItem(Obj) do
        result := TTycoonCacheAgent.GetPath( Tycoon, kind, info ) + tidCachePath_Curriculum + Format('%.3d', [Tycoon.Curriculum.IndexOf(Obj)]) + '.five';
    end;

  class function TCurriculumCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    var
      i : integer;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      with TCurriculumItem(Obj) do
        begin
          result.WriteString( ppTTL, TTLNever);
          result.WriteString( 'Id', Id );
          result.WriteInteger( 'Kind', Kind );
          for i := 0 to pred(LangList.Count) do
            result.WriteString( 'Desc' + IntToStr(i), Desc[LangList[i]] );
          result.WriteInteger( 'Prestige', round(Prestige) );
        end;
    end;

                                    
  // TTycoonCacheAgent

  class function TTycoonCacheAgent.GetPath( Obj : TObject; kind, info : integer ) : string;
    begin
      with TTycoon(Obj) do
        result := tidCachePath_Tycoons + Trim(Name) + '.five\'
    end;

  class function TTycoonCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache( Obj, kind, info, update );
      result.WriteString (ppRnAgent, 'Tycoon' );
      result.WriteString (ppObjLoc, TTycoon(Obj).Name);
      result.WriteString( 'Name', TTycoon(Obj).Name );
      result.WriteString( 'Password', TTycoon(Obj).Password );
      result.WriteBoolean( 'IsRole', TTycoon(Obj).IsRole );
      result.WriteBoolean('CanBuildAdvanced', TTycoon(Obj).CanBuildAdvanced);
      result.WriteInteger('Nobility', TTycoon(Obj).NobPoints);
    end;

  class function TTycoonCacheAgent.UpdateCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;

    function GetFacilityId( Facility : TFacility ) : string;
      begin
        with Facility do
          result := IntToStr(xPos) + ',' + IntToStr(yPos) + ',';
      end;

    procedure StoreRoleInfo( Tycoon : TTycoon; CacheObj : TObjectCache );
      var
        i : integer;
      begin
        Tycoon.StoreRoleInfoToCache( CacheObj );
        for i := 0 to pred(Tycoon.Roles.Count) do
          StoreRoleInfo( TTycoon(Tycoon.Roles[i]), CacheObj );
      end;

    procedure StoreRankings( Tycoon : TTycoon; CacheObj : TObjectCache );
      var
        i : integer;
      begin
        CacheObj.WriteInteger( 'RankCount', Tycoon.Rankings.Count );
        for i := 0 to pred(Tycoon.Rankings.Count) do
          with TRankingInfo(Tycoon.Rankings[i]) do
            begin
              CacheObj.WriteString( 'RankId' + IntToStr(i), Ranking.Id );
              //CacheObj.WriteString( 'RankName' + IntToStr(i), Ranking.Name );
              StoreMultiStringToCache( 'RankName' + IntToStr(i) + '.', Ranking.Name, CacheObj );
              CacheObj.WriteInteger( 'RankPos' + IntToStr(i), Position );
            end;
      end;

    procedure StoreLevel( cache : TObjectCache; Level : TTycoonLevel; Prefix : string );
      begin
        if Level <> nil
          then
            begin
              //cache.WriteString( Prefix + 'LevelName', Level.Name );
              //cache.WriteString( Prefix + 'LevelDesc', Level.Description );
              //cache.WriteString( Prefix + 'LevelCond', Level.Condition );
              StoreMultiStringToCache( Prefix + 'LevelName', Level.Name_MLS, cache );
              StoreMultiStringToCache( Prefix + 'LevelDesc', Level.Description_MLS, cache );
              StoreMultiStringToCache( Prefix + 'LevelCond', Level.Condition_MLS, cache );
            end;
      end;

    var
      i, j      : integer;
      MetaFluid : TMetaFluid;
      tot1 : currency;
      tot2 : currency;
      tot3 : currency;
      tot4 : currency;
      tot5 : currency;
      tot6 : currency;
      tot7 : currency;
    begin
      result := inherited UpdateCache( Obj, kind, info, update );
      try
        with TTycoon(Obj) do
          begin
            {$IFDEF USELogs}
            //Logs.Log('Cache', TimeToStr(Now) + ' CS.Enter');
            {$ENDIF}
            Lock;
            try
              result.WriteCurrency( 'IFELLoanEstimated', realmax( 0, WorldLocator.GetMainBank.EstimateLoan( TTycoon(Obj) )));
              result.WriteInteger( 'IFELoanTerm', WorldLocator.GetMainBank.Term );
              result.WriteInteger( 'IFELoanInterest', WorldLocator.GetMainBank.Interest );
              result.WriteString(ppTTL, CreateTTL(0, 0, 5, 0)); // 10
              result.WriteString( 'Name', Name );
              result.WriteString( 'RealName', RealName );
              result.WriteInteger( 'SecurityId', integer(Obj) );
              result.WriteCurrency( 'Budget', Budget );
              result.WriteInteger( 'AutoConnectionCount', AutoConnections.Count );
              result.WriteInteger( 'SuperRole', integer(SuperRole) );
              result.WriteCurrency( 'NetProfit', NetProfit );
              result.WriteInteger( 'Ranking', Ranking );
              result.WriteFloat( 'Prestige', Prestige );
              result.WriteString( 'LangId', Language );
              result.WriteFloat( 'LicLevel', LicenceLevel );
              result.WriteCurrency( 'LoanAmount', LoanAmount );
              result.WriteBoolean('Demo', IsDemo);
              if TournamentOn
                then
                  begin
                    result.WriteString('TournamentOn', '1');
                    result.WriteString('rkPts',  Cookie['rkPts']);
                    result.WriteString('lvPts',  Cookie['lvPts']);
                    result.WriteString('bnkPts', Cookie['bnkPts']);
                  end;
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' AC.Enter');
              {$ENDIF}
              AutoConnections.Lock;
              try
                for i := 0 to pred(AutoConnections.Count) do
                  with TAutoConnection(AutoConnections[i]) do
                    begin
                      MetaFluid := TMetaFluid(TheClassStorage.ClassById[ 'Fluids', MetaFluidId ]);
                      result.WriteString( 'AutoConnection' + IntToStr(i), MetaFluidId );
                      //result.WriteString( 'AutoConnection' + MetaFluidId + 'Name', MetaFluid.Name );
                      StoreMultiStringToCache( 'AutoConnection' + MetaFluidId + 'Name', MetaFluid.Name_MLS, result );
                      //result.WriteString( 'AutoConnection' + MetaFluidId + 'Name', MetaFluid.Name );
                      result.WriteInteger( 'AutoConnection' + MetaFluidId + 'Count', Connections.Count );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'HireTradeCenter', HireTradeCenter );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'HireOnlyWarehouses', HireOnlyWarehouses );
                      result.WriteBoolean( 'AutoConnection' + MetaFluidId + 'Storable', mfStorable in MetaFluid.Options );
                      Connections.Lock;
                      try
                        for j := 0 to pred(Connections.Count) do
                          try
                            result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityName', TFacility(Connections[j]).Name );
                            if TFacility(Connections[j]).Company <> nil
                              then result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityCompany', TFacility(Connections[j]).Company.Name );
                            result.WriteString( 'AutoConnection' + MetaFluidId + IntToStr(j) + 'FacilityId', GetFacilityId( TFacility(Connections[j]) ));
                          except
                          end;
                      finally
                        Connections.Unlock;
                      end;
                    end;
              finally
                AutoConnections.Unlock;
              end;
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Cur.Enter');
              {$ENDIF}
              Curriculum.Lock;
              try
                for i := 0 to pred(Curriculum.Count) do
                  CacheObject( Curriculum[i], noKind, noInfo ); // >> ???
              finally
                Curriculum.Unlock;
              end;
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Roles.Enter');
              {$ENDIF}
              result.WriteInteger( 'RolesCount', Roles.Count );
              Roles.Lock;
              try
                for i := 0 to pred(Roles.Count) do
                  result.WriteString( 'Role' + IntToStr(i), TTycoon(Roles[i]).Name );
              finally
                Roles.Unlock;
              end;
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Policies');
              {$ENDIF}
              StorePoliciesToCache( result );
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Level');
              {$ENDIF}
              if Level <> nil
                then
                  begin
                    result.WriteInteger( 'CurrLevel', Level.Tier );
                    StoreLevel( result, Level, '' );
                    if Level.Tier < TheGlobalConfigHandler.GetInteger( CFGID_LevelLimit )
                      then StoreLevel( result, Level.NextLevel, 'Next' );
                  end;
              result.WriteBoolean( 'AdvanceToNextLevel', AdvanceToNextLevel );
              if TheGlobalConfigHandler.GetConfigParm('LifeAfterLegend', '0') = '1'
              then
                begin
                  if (Accounts <> nil) and (Accounts.AccountArray[accIdx_ResearchCenter_Research] <> nil)
                  then
                    begin
                      tot1 := Accounts.MasterAccount.Value;
                      tot2 := Accounts.AccountArray[accIdx_Construction].Value;
                      tot3 := Accounts.AccountArray[accIdx_Taxes].Value;
                      tot4 := Accounts.AccountArray[accIdx_TransfersIn].Value;
                      tot5 := Accounts.AccountArray[accIdx_TransfersOut].Value;
                      tot6 := Accounts.AccountArray[accIdx_Bank].Value;
                      tot7 := Accounts.AccountArray[accIdx_ResearchCenter_Research].Value;
                      if (Copy(Level.Id, 0, length(tidTycoonLevel_OverLegend)) <> tidTycoonLevel_OverLegend)
                      then
                        result.WriteCurrency('ProfitAverage', (
                          (tot1
                           - tot2
                           - tot3
                           - tot4
                           - tot5
                           - tot6)/(WorldLocator.GetNumberOfDays*WorldLocator.GetHoursADay)))
                      else
                        result.WriteCurrency('ProfitAverage', (
                          (tot1
                           - tot2
                           - tot3
                           - tot4
                           - tot5
                           - tot6
                           - tot7)/(WorldLocator.GetNumberOfDays*WorldLocator.GetHoursADay)));
                      {then
                        result.WriteCurrency('ProfitAverage', (
                          (Accounts.MasterAccount.Value
                           - Accounts.AccountArray[accIdx_Construction].Value
                           - Accounts.AccountArray[accIdx_Taxes].Value
                           - Accounts.AccountArray[accIdx_TransfersIn].Value
                           - Accounts.AccountArray[accIdx_TransfersOut].Value
                           - Accounts.AccountArray[accIdx_Bank].Value)/(365*WorldLocator.GetHoursADay)))
                      else
                        result.WriteCurrency('ProfitAverage', (
                          (Accounts.MasterAccount.Value
                           - Accounts.AccountArray[accIdx_Construction].Value
                           - Accounts.AccountArray[accIdx_Taxes].Value
                           - Accounts.AccountArray[accIdx_TransfersIn].Value
                           - Accounts.AccountArray[accIdx_TransfersOut].Value
                           - Accounts.AccountArray[accIdx_Bank].Value
                           + Accounts.AccountArray[accIdx_ResearchCenter_Research].Value)/(365*WorldLocator.GetHoursADay)));}
                    end;
                end
              else
                result.WriteCurrency('ProfitAverage', (
                  (Accounts.MasterAccount.Value
                   - Accounts.AccountArray[accIdx_Construction].Value
                   - Accounts.AccountArray[accIdx_Taxes].Value
                   - Accounts.AccountArray[accIdx_TransfersIn].Value
                   - Accounts.AccountArray[accIdx_TransfersOut].Value
                   - Accounts.AccountArray[accIdx_Bank].Value)/(WorldLocator.GetNumberOfDays*WorldLocator.GetHoursADay)));
              //result.WriteString( 'LevelReqStatus', LevelReqStatus );
              if LevelReqStatus <> nil
                then StoreMultiStringToCache( 'LevelReqStatus', LevelReqStatus, result );
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' RoleInfo');
              {$ENDIF}
              StoreRoleInfo( TTycoon(Obj).MasterRole, result );
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' Rankings');
              {$ENDIF}
              StoreRankings( TTycoon(Obj), result );
              if Tutorial <> nil
                then
                  begin
                    result.WriteString('ActiveTutorial', 'YES');
                    Tutorial.StoreToCache('Tutorial', result );
                  end;
              {$IFDEF USELogs}
              //Logs.Log('Cache', TimeToStr(Now) + ' UC end');
              {$ENDIF}

            finally
              Unlock;
            end;
          end;
      except
      end;
    end;


  // TownCacheAgent

  class function TTownCacheAgent.GetPath ( Obj : TObject; kind, info : integer ) : string;
    begin
      with TTown(Obj) do
        result := tidCachePath_Towns + Trim(Name) + '.five\'
    end;

  class function TTownCacheAgent.GetCache( Obj : TObject; kind, info : integer; update : boolean ) : TObjectCache;
    begin
      result := inherited GetCache(Obj, kind, info, update);
      with TInhabitedTown(Obj) do
        begin
          Lock;
          try
            result.WriteString (ppRnAgent, 'Town' );
            result.WriteString (ppObjLoc, Name);
            TownHall.CurrBlock.StoreToCache( result );
            result.WriteInteger( 'VisualId', TownHall.VisualClass );
            result.WriteInteger( 'xPos', TownHall.xPos );
            result.WriteInteger( 'yPos', TownHall.yPos );
            result.WriteString('Name', Name);
            result.WriteString(ppTTL, CreateTTL( 0, 0, 5, 0 )); // was 30 sec
          finally
            Unlock;
          end;
        end;
    end;                                     

  // TInhabitedTownCacheAgent

  class function TInhabitedTownCacheAgent.GetCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
    begin
      result := inherited GetCache(Obj, kind, info, update);
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
      RegisterCacher( TCurriculumItem.ClassName, TCurriculumCacheAgent );
      RegisterCacher( TTycoon.ClassName, TTycoonCacheAgent );
      RegisterCacher( TInhabitedTown.ClassName, TInhabitedTownCacheAgent );
    end;


end.



