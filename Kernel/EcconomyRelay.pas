unit EcconomyRelay;

interface

  uses
    Persistent, Collection, Kernel, BackupInterfaces, Accounts;

  const
    MinRecessionFact = 0.25;
    MaxStimulusFact  = 1.25;
    RecessionFactDec = 0.25/(24*365);
    RecessionFactInc = 0.25/(24*365);

  type
    TEcconomyRelay =
      class(TPersistent)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAreas : TCollection;
        public
          procedure Update;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

implementation

  uses
    ServiceBlock, ClassStorage, MathUtils;

  // TEcconomyRelay

  constructor TEcconomyRelay.Create;
    begin
      inherited;
      fAreas := TCollection.Create(0, rkUse);
    end;

  destructor TEcconomyRelay.Destroy;
    begin
      fAreas.Free;
      inherited;
    end;

  procedure TEcconomyRelay.Update;
    var
      count   : integer;
      i       : integer;
      Service : TMetaService;
      sum     : TMoney;
      avg     : TMoney;
    begin
      count := fAreas.Count;
      if count = 0
        then
          begin
            count := TheClassStorage.ClassCount[tidClassFamily_Services];
            for i := 0 to pred(count) do
              begin
                Service := TMetaService(TheClassStorage.ClassByIdx[tidClassFamily_Services, i]);
                fAreas.Insert(Service);
              end;
          end;
      if count > 0
        then
          begin
            sum := 0;
            for i := 0 to pred(count) do
              begin
                Service := TMetaService(fAreas[i]);
                sum := sum + Service.SaleProfit;
              end;
            avg := sum/count;
            for i := 0 to pred(count) do
              begin
                Service := TMetaService(fAreas[i]);
                if Service.SaleProfit > avg
                  then Service.RecFact := realmax(MinRecessionFact, Service.RecFact - RecessionFactDec)
                  else
                    if Service.SaleProfit < avg
                      then Service.RecFact := realmin(MaxStimulusFact, Service.RecFact + RecessionFactInc);
              end;
          end;
    end;

  procedure TEcconomyRelay.LoadFromBackup(Reader : IBackupReader);
    begin
      // >>
    end;

  procedure TEcconomyRelay.StoreToBackup(Writer : IBackupWriter);
    begin
      // >>
    end;

end.
