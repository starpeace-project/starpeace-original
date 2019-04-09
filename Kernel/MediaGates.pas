unit MediaGates;

interface

  uses
    Classes, SysUtils, Kernel, Accounts, BackupInterfaces, CacheAgent, Variants;

  const
    smPrice   = 0;
    smQuality = 1;

  const
    ID_MediaBlock : TGUID = '{41A21981-C257-11d5-9926-004854664ED7}';

  type
    IMediaBlock =
      interface['{41A21981-C257-11d5-9926-004854664ED7}']
        function  getCurrentTitle : string;
        procedure reportSales(value : TFluidValue);
      end;

  type
    PTitleArray = ^TTitleArray;
    TTitleArray = array[0..99] of IMediaBlock;

  type
    TMediaInput =
      class(TPullInput)
        public
          constructor Create( aMetaGate : TMetaGate; aBlock : TBlock ); override;
          destructor  Destroy; override;
        private
          fSortMode : byte;
          fCovRooms : byte;
          fSorted   : boolean;
          fLastExp  : integer;
          fTitles   : PTitleArray;
        protected
          procedure Collect; override;
          procedure CollectExtra; override;
        protected
          procedure LoadFromBackup(Reader : IBackupReader); override;
          procedure StoreToBackup (Writer : IBackupWriter); override;
        public
          function  GetConnectionPrecedence(Connection : TGate) : integer; override;
          procedure SortConnections; override;
        private
          procedure AllocTitles;
          procedure DeallocTitles;
          procedure AddTitle(theBlock : TBlock; index : integer);
          function  GetTitles : string;
          procedure ReportSales(theBlock : TBlock; value : TFluidValue);
        public
          property Titles   : string  read GetTitles;
          property LastExp  : integer read fLastExp  write fLastExp;
        protected
          procedure SetSortMode(mode : byte); override;
        published
          property SortMode : byte    read fSortMode write SetSortMode;
          property Sorted   : boolean read fSorted;
          property CovRooms : byte    read fCovRooms;
        public
          procedure StoreToCache( Cache : TObjectCache ); override;
      end;

    TMediaOutput =
      class(TPullOutput)
        protected
          procedure Spread; override;
        public
          procedure ValuePulled(Value : TFluidValue; Input : TInput; Idx, tick : integer); override;
          procedure Slice(Value : TFluidValue); override;
          function  GetSliceFor(Input : TInput; Idx : integer) : TFluidValue; override;
        private
          fCurFluid : TFluidValue;
        public
          procedure UpdateFluid; override;
      end;

  // RegisterBackup

  procedure RegisterBackup;


implementation

  uses
    Collection, CacheCommon, BasicAccounts;

  // TMediaInput

  constructor TMediaInput.Create(aMetaGate : TMetaGate; aBlock : TBlock);
    begin
      inherited;
    end;

  destructor TMediaInput.Destroy;
    begin
      DeallocTitles;
      inherited;
    end;

  procedure TMediaInput.Collect;
    var
      UpLevel  : integer;
      CnxCount : integer;
      Output   : TPullOutput;
      QKsum    : TFluidValue;
      Qsum     : TFluidValue;
      Idx      : integer;
      ExInf    : PExtraConnectionInfo;
      dQ       : TFluidValue;
      AuxFluid : PFluidData;
      i, l     : integer;
      tcIdx    : integer;
      Cost     : TMoney;
      CostSum  : TMoney;
      budget   : TMoney;
      theDt    : single;
      aux      : TFluidValue;
      focused  : boolean;
      tick     : integer;
      lastQlt  : TPercent;
    begin
      //do not call inherited;

      // Check the inv index
      CheckIndex;

      theDt   := Block.dt;
      budget  := Block.Facility.Budget;
      UpLevel := Block.UpgradeLevel;
      lastQlt := high(TPercent);
      fSorted := true;

      focused := Block.Facility.Focused;
      if focused
        then AllocTitles
        else DeallocTitles;

      // get tick
      if Block.Facility.Town <> nil
        then tick := Block.Facility.Town.Timer.getTickId
        else tick := 0;

      CnxCount := ConnectionCount;

      // Clear Last Fluids
      i := 0;
      while i < CnxCount do
        begin
          Output := TPullOutput(Connections[i]);
          Idx := IndexAtOutput(Output, i);
          if Idx <> noIndex
            then
              begin
                ExInf := Output.ExtraConnectionInfo[Idx];
                if ExInf <> nil
                  then ExInf.LastFluid := 0;
                inc(i);
              end
            else
              begin
                fConnections.Extract(Output); // >> This fixes the symetry problem.
                dec(CnxCount);
              end;
        end;

      // Collect now
      QKsum    := 0;
      Qsum     := 0;
      CostSum  := 0;
      FluidData.Q := 0;
      i := 0;
      l := 0;
      tcIdx := -1;
      while (budget > 0) and (l < UpLevel) and (i < CnxCount) do
        begin
          Output := TPullOutput(Connections[i]);
          // Get the index of the input in the output
          Idx := IndexAtOutput(Output, i);

          if Idx <> noIndex
            then
              begin
                dQ := Output.GetSliceFor( self, Idx );
                ExInf := Output.ExtraConnectionInfo[Idx];
              end
            else
              begin
                dQ := 0;
                ExInf := nil;
              end;

          if dQ > 0
            then
              begin
                if Output.Block.Role = rolImporter //and (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and (Block.Facility.Company.Owner.Level.Tier < 2)
                  then tcIdx := i;

                if focused
                  then AddTitle(Output.Block, l);

                ReportSales(Output.Block, fLastExp/UpLevel);

                aux         := theDt*ActualMaxFluid.Q/UpLevel;
                FluidData.Q := FluidData.Q + aux;

                if lastQlt >= FluidData.K
                  then lastQlt := FluidData.K
                  else fSorted := fSortMode = smPrice;

                if ExInf <> nil
                  then ExInf.LastFluid := MetaInput.MaxFluid.Q;

                Cost := Output.PriceToDeliver(aux, self, ExInf, tnpPositiveShare);
                if Output.Block.MetaBlock.ProdAccount <> accIdx_None
                  then Output.Block.BlockGenMoney(Cost, Output.Block.MetaBlock.ProdAccount);

                Output.ValuePulled(aux, self, Idx, tick);

                CostSum := CostSum + Cost;

                QKsum := QKsum + aux*Output.FluidData.K;
                Qsum  := Qsum  + aux;
                inc(l);
              end
            else
              if ExInf <> nil
                then ExInf.LastFluid := 0;

          // Fix inconsistences
          if Idx = noIndex
            then
              begin
                fConnections.Extract(Output);
                dec(CnxCount);
              end
            else inc(i);
        end;
      if (budget > 0) and (l < UpLevel) and (tcIdx >= 0)
        then
          begin
            if (Block.Facility.Company <> nil) and (Block.Facility.Company.Owner <> nil) and (Block.Facility.Company.Owner.Level.Tier < 2)
              then
                begin
                  Output := TPullOutput(Connections[tcIdx]);
                  AuxFluid := Output.FluidData;
                  aux := (UpLevel - l)*theDt*ActualMaxFluid.Q/UpLevel;
                  FluidData.Q := FluidData.Q + aux;
                  QKsum := QKsum + aux*AuxFluid.K;
                  Qsum  := Qsum + aux;

                  Cost := Output.PriceToDeliver(aux, self, nil, tnpPositiveShare);
                  if Output.Block.MetaBlock.ProdAccount <> accIdx_None
                    then Output.Block.BlockGenMoney(Cost, Output.Block.MetaBlock.ProdAccount);

                  CostSum := CostSum + Cost;
                  l := UpLevel;
                end;
          end;
      vLastCost := CostSum;
      if budget > 0
        then
          begin
            if Block.MetaBlock.SupplyAccount <> accIdx_None
              then Block.BlockGenMoney(-CostSum, Block.MetaBlock.SupplyAccount);
          end
        else Block.Facility.ReportTrouble(facNeedsBudget);
      if Qsum > 0
        then FluidData.K := round(QKsum/Qsum)
        else FluidData.K := 0;
      fCovRooms := l;
    end;

  procedure TMediaInput.CollectExtra;
    begin
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
      if fSortMode = smPrice
        then result := inherited GetConnectionPrecedence(Connection)
        else result := 100*GetUniversalPrecedence(Connection) - Connection.FluidData.K;
    end;

  procedure TMediaInput.SortConnections;
    begin
      inherited;
      fSorted := true;
    end;

  procedure TMediaInput.AllocTitles;
    var
      lvls : integer;
    begin
      lvls := Block.MetaBlock.MaxUpgrade;
      if fTitles = nil
        then GetMem(fTitles, lvls*sizeof(fTitles[0]));
      FillChar(fTitles^, lvls*sizeof(fTitles[0]), 0);
    end;

  procedure TMediaInput.DeallocTitles;
    var
      i    : integer;
      lvls : integer;
    begin
      lvls := Block.MetaBlock.MaxUpgrade;
      if fTitles <> nil
        then
          begin
            for i := 0 to pred(lvls) do
              fTitles[i] := nil;
            FreeMem(fTitles);
            fTitles := nil;
          end;
    end;

  procedure TMediaInput.AddTitle(theBlock : TBlock; index : integer);
    var
      MediaBlock : IMediaBlock;
    begin
      if theBlock.GetInterface(ID_MediaBlock, MediaBlock)
        then fTitles[index] := MediaBlock
        else fTitles[index] := nil;
    end;

  function TMediaInput.GetTitles : string;
    var
      i : integer;
      Town : TTown;
    begin
      Block.Facility.Lock;
      try
        Town := Block.Facility.Town;
        result := '';
        if fTitles <> nil
          then
            for i := pred(fCovRooms) downto 0 do
              if fTitles[i] <> nil
                then
                  if result <> ''
                    then result := '"' + fTitles[i].getCurrentTitle + '", ' + result
                    else result := '"' + fTitles[i].getCurrentTitle + '"'
                else
                  if result <> ''
                    then result := '"' + Town.GetRndName(i) + '", ' + result
                    else result := '"' + Town.GetRndName(i) + '"';
      finally
        Block.Facility.Unlock;
      end;
    end;

  procedure TMediaInput.ReportSales(theBlock : TBlock; value : TFluidValue);
    var
      MediaBlock : IMediaBlock;
    begin
      if theBlock.GetInterface(ID_MediaBlock, MediaBlock) and (MediaBlock <> nil)
        then MediaBlock.ReportSales(value);
    end;

  procedure TMediaInput.SetSortMode(mode : byte);
    begin
      mode := mode and $01;
      if fSortMode <> mode
        then
          begin
            fSortMode := mode;
            SortConnections;
          end;
    end;

  procedure TMediaInput.StoreToCache(Cache : TObjectCache);
    begin
      inherited StoreToCache(Cache);
      Cache.WriteString('QPSorted', '1');
      Cache.WriteInteger('SortMode', fSortMode);
    end;

  // TMediaOutput

  procedure TMediaOutput.Spread;
    begin
      inherited;
    end;

  procedure TMediaOutput.ValuePulled(Value : TFluidValue; Input : TInput; Idx, tick : integer);
    begin
      fCurFluid := fCurFluid + Value;
    end;

  procedure TMediaOutput.Slice(Value : TFluidValue);
    begin
    end;

  function TMediaOutput.GetSliceFor(Input : TInput; Idx : integer) : TFluidValue;
    begin
      if Idx = noIndex
        then
          if ConnectTo(Input) = cnxValid
            then
              begin
                //Logs.Log( 'Survival', 'Unknown Input found x:' + IntToStr(Input.Block.xPos) + ' y:' + IntToStr(Input.Block.yPos));
                Idx := vConnections.IndexOf( Input );
              end;
      if (Idx <> noIndex) and not Connections[Idx].Block.Facility.CriticalTrouble and TSlice(vSlices[Idx]).ExtraConnectionInfo.Connected and TPullInput(Input).TransferAllowed( self )
        then result := FluidData.Q
        else result := 0;
    end;

  procedure TMediaOutput.UpdateFluid;
    begin
      fLastQ := round(fCurFluid);
      fCurFluid := 0;
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      BackupInterfaces.RegisterClass(TMediaInput);
      BackupInterfaces.RegisterClass(TMediaOutput);
    end;

end.
