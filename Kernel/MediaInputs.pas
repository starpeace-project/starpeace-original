unit MediaInputs;

interface

  uses
    Kernel, Accounts, BackupInterfaces;

  const
    smPrice   = 0;
    smQuality = 1;

  type
    TMediaInput =
      class(TInput)
        private
          fSortMode : byte;
        protected
          procedure Collect; override;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        public
          function GetConnectionPrecedence(Connection : TGate) : integer; override;
      end;


implementation

  uses
    CacheCommon, BasicAccounts;

  // TMediaInput

  procedure TMediaInput.Collect;
    var
      UpLevel  : integer;
      CnxCount : integer;
      Output   : TOutput;
      QKsum    : TFluidValue;
      Qsum     : TFluidValue;
      AuxFluid : PFluidData;
      i, l     : integer;
      tcIdx    : integer;
      Cost     : TMoney;
      CostSum  : TMoney;
    begin
      inherited;
      UpLevel  := Block.UpgradeLevel;
      CnxCount := ConnectionCount;
      QKsum    := 0;
      Qsum     := 0;
      CostSum  := 0;
      FluidData.Q := 0;
      i := 0;
      l := 0;
      tcIdx := -1;
      while (l < UpLevel) and (i < CnxCount) do
        begin
          Output   := TOutput(Connections[i]);
          AuxFluid := Output.FluidData;
          if AuxFluid.Q > 0
            then
              begin
                if Output.Block.Role = rolImporter //and (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and (Block.Facility.Company.Owner.Level.Tier < 2)
                  then tcIdx := i;
                FluidData.Q := FluidData.Q + ActualMaxFluid.Q;
                // >> Pay the Output

                Cost := Output.PriceToDeliver(ActualMaxFluid.Q, self, nil, tnpNegativeShare);
                if Output.Block.MetaBlock.ProdAccount <> accIdx_None
                  then Output.Block.BlockGenMoney(Cost, Output.Block.MetaBlock.ProdAccount);

                CostSum := CostSum + Cost;

                QKsum := QKsum + AuxFluid.Q*AuxFluid.K;
                Qsum  := Qsum + AuxFluid.Q;
                inc(l);
              end;
          inc(i);
        end;
      if (l < UpLevel) and (tcIdx >= 0)
        then
          begin
            if (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and (Block.Facility.Company.Owner.Level.Tier < 2)
              then
                begin
                  Output := TOutput(Connections[tcIdx]);
                  AuxFluid := Output.FluidData;
                  FluidData.Q := FluidData.Q + (UpLevel - l)*ActualMaxFluid.Q;
                  QKsum := QKsum + (UpLevel - l)*AuxFluid.Q*AuxFluid.K;
                  Qsum  := Qsum + (UpLevel - l)*AuxFluid.Q;

                  Cost := Output.PriceToDeliver((UpLevel - l)*ActualMaxFluid.Q, self, nil, tnpNegativeShare);
                  if Output.Block.MetaBlock.ProdAccount <> accIdx_None
                    then Output.Block.BlockGenMoney(Cost, Output.Block.MetaBlock.ProdAccount);

                  CostSum := CostSum + Cost;
                end;
          end;
      if Block.Facility.Budget > 0
        then
          begin
            if Block.MetaBlock.SupplyAccount <> accIdx_None
              then Block.BlockGenMoney(-CostSum, Block.MetaBlock.SupplyAccount);
          end
        else Block.Facility.ReportTrouble(facNeedsBudget);
      if Qsum > 0
        then FluidData.K := round(QKsum/Qsum)
        else FluidData.K := 0;
    end;

  procedure TMediaInput.LoadFromBackup(Reader : IBackupReader);
    begin
      inherited;
      fSortMode := Reader.ReadByte('SortMode', smQuality);
    end;

  procedure TMediaInput.StoreToBackup(Writer : IBackupWriter);
    begin
      inherited;
      Writer.WriteByte('SortMode', fSortMode);
    end;

  function TMediaInput.GetConnectionPrecedence(Connection : TGate) : integer;
    begin
      result := inherited GetConnectionPrecedence(Connection);
      if (fSortMode <> smPrice) and (result > cprSameOwner)
        then result := result + 100 - Connection.FluidData.K;
    end;


end.
