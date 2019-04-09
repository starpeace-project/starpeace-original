unit Trade;

interface

  uses
    Protocol, Kernel, Standards, CacheCommon, Variants;

  const
    TradeCenterPrice   = 200;
    TradeCenterQuality = 40;

  const
    tidFacilityKind_TradeCenter = 'TradeCenter';

  procedure RegisterTradeCenter( aClusterName, anId : string; aVisualClass : TVisualClassId; axSize, aySize : integer );
  procedure RegisterBackup;

implementation

  uses
    Collection, SysUtils, Logs, ClassStorage, BackupInterfaces, ConnectedBlock, BasicAccounts;


  type
    TUnlimitedPullOutput =
      class( TPullOutput )
        protected
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
        public
          procedure ValuePulled( Value : TFluidValue; Input : TInput; Idx, tick : integer );   override;
          procedure Slice      ( Value : TFluidValue );                                        override;
          function  GetSliceFor( Input : TInput; Idx : integer ) : TFluidValue;                override;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TMetaTradeCenter =
      class( TMetaBlock )
        public
          constructor Create( anId : string; aBlockClass : CBlock );
      end;


  // TUnlimitedPullOutput

  const
    pulloutputIlimited : TOutputData = (Q : qIlimited; K : TradeCenterQuality);

  constructor TUnlimitedPullOutput.Create( aMetaGate : TMetaGate; aBlock : TBlock );
    begin
      inherited;
      SetFluidData( @pulloutputIlimited );
      PricePerc := TradeCenterPrice;
    end;
                                             
  procedure TUnlimitedPullOutput.ValuePulled( Value : TFluidValue; Input : TInput; Idx, tick : integer );
    begin
    end;

  procedure TUnlimitedPullOutput.Slice( Value : TFluidValue );
    begin
    end;

  function TUnlimitedPullOutput.GetSliceFor( Input : TInput; Idx : integer ) : TFluidValue;
    begin
      if Idx = noIndex
        then
          if ConnectTo( Input ) = cnxValid
            then
              begin
                Logs.Log( 'Survival', 'Unknown Input found x:' + IntToStr(Input.Block.xPos) + ' y:' + IntToStr(Input.Block.yPos));
                Idx := vConnections.IndexOf( Input );
              end;
      if (Idx <> noIndex) and not Connections[Idx].Block.Facility.CriticalTrouble and TSlice(vSlices[Idx]).ExtraConnectionInfo.Connected and TPullInput(Input).TransferAllowed( self ) 
        then result := qIlimited
        else result := 0;
    end;

  procedure TUnlimitedPullOutput.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      SetFluidData( @pulloutputIlimited );
    end;

  procedure TUnlimitedPullOutput.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
    end;


  // TMetaTradeCenter

  constructor TMetaTradeCenter.Create( anId : string; aBlockClass : CBlock );
    var
      MetaFluid   : TMetaFluid;
      i           : integer;
      OutputClass : COutput;
    begin
      inherited Create( anId, accIdx_None, accIdx_None, aBlockClass );
      for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Fluids]) do
        begin
          MetaFluid := TMetaFluid(TheClassStorage.ClassByIdx[tidClassFamily_Fluids, i]);
          if (mfTradeable in MetaFluid.Options) and (mfImportable in MetaFluid.Options)
            then
              begin
                OutputClass := MetaFluid.OutputClass;
                if OutputClass = nil
                  then OutputClass := TUnlimitedPullOutput;
                MetaOutputs.Insert(
                  TMetaOutput.Create(
                    MetaFluid.Id,
                    fluidIlimited,
                    OutputClass,
                    MetaFluid,
                    1000,
                    [mgoptCacheable],
                    sizeof(TOutputData),
                    -1 ));
              end;
        end;
    end;


  // RegisterTradeCenter

  procedure RegisterTradeCenter( aClusterName, anId : string; aVisualClass : TVisualClassId; axSize, aySize : integer );
    var
      MetaBlock    : TMetaBlock;
      MetaFacility : TMetaFacility;
    begin
      with TFacilityKind.Create(aClusterName + tidFacilityKind_TradeCenter) do
        begin
          Name        := 'Trade Center';
          SuperType   := tidSuperFacKind_TradeCenter;
          ClusterName := aClusterName;
          Role        := rolImporter;
          Cacheable   := false;
          Register( tidClassFamily_FacilityKinds );
        end;
      MetaBlock := TMetaTradeCenter.Create( anId, TConnectedBlock );
      MetaFacility := TMetaFacility.Create( anId, 'Trade Center', aVisualClass, TFacility );
      with MetaFacility do
        begin
          xSize        := axSize;
          ySize        := aySize;
          Options      := [mfcShowCompanyInText, mfcStopDisabled, mfcIgnoreZoning, mfcForbiddenRename, mfcAceptsNoOwner];
          Cacheable    := false;
          ClusterName  := aClusterName;
          NeedsBudget  := false;
          FacilityKind := aClusterName + tidFacilityKind_TradeCenter;
        end;
      MetaFacility.EvlStages.Insert( TEvlStage.Create( '', '', '', MetaBlock ));
      MetaBlock.Register( tidClassFamily_Blocks );
      MetaFacility.Register( tidClassFamily_Facilities );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass( TUnlimitedPullOutput );
    end;

end.
