unit EconomyRelay;

interface

  uses
    Persistent, Collection, Kernel, BackupInterfaces, Accounts, Variants;

  const
    MinRecessionFact = 0.1/(2*24*365);
    MaxStimulusFact  = 3;
    RecessionFactDec = 0.1/(24*365);
    RecessionFactInc = 0.2/(24*365);

  type
    TEconomyRelay =
      class(TPersistent)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fAreas : TCollection;
        public
          procedure Update( const Context );                                               
          function  RenderText : string;
          procedure Reset;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, ServiceBlock, ClassStorage, MathUtils, World;

  // TEconomyRelay

  constructor TEconomyRelay.Create;
    begin
      inherited;
      fAreas := TCollection.Create(0, rkUse);
    end;

  destructor TEconomyRelay.Destroy;
    begin
      fAreas.Free;
      inherited;
    end;

  procedure TEconomyRelay.Update( const Context );
    const
      MarkPop  : extended = 1000*1000/1000000;
      MarkTime : extended = 100*365*24/1000000;
    var
      World   : TWorld absolute Context;
      count   : integer;
      i       : integer;
      Service : TMetaService;
      sum     : TMoney;
      avg     : TMoney;
      max     : TMoney;
      devIdx  : extended;
      Profit  : TMoney;
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
            max := 0;
            devIdx := (World.TotalPop/1000000)*(World.VirtualTimeAbs/1000000);
            if devIdx > 0
              then devIdx := realmin( 1, (MarkPop*MarkTime)/devIdx )
              else devIdx := 1;
            for i := 0 to pred(count) do
              begin
                Service := TMetaService(fAreas[i]);
                Service.Depression := 0.75 + 0.25*sin( (World.VirtualTimeAbs + Service.Phase)*2*pi/Service.Freq );
                Service.Depression := realmax( devIdx, Service.Depression );
                Profit := (2 - Service.Depression)*Service.SaleProfit;
                sum := sum + Profit;
                if max < Profit
                  then max := Profit;
              end;
            avg := 0.2*max + 0.8*(sum/count); // weighted average between the max and the average of profit
            for i := 0 to pred(count) do
              begin
                Service := TMetaService(fAreas[i]);
                if Service.SaleProfit > Service.Depression*avg
                  then Service.RecFact := realmax(MinRecessionFact, Service.RecFact - RecessionFactDec)
                  else
                    if Service.SaleProfit < Service.Depression*avg
                      then Service.RecFact := realmin(MaxStimulusFact, Service.RecFact + RecessionFactInc);
              end;
          end;
    end;

  function TEconomyRelay.RenderText : string;
    const
      LB = ^M^J;
    var
      i : integer;
      Service : TMetaService;
    begin
      result := DateTimeToStr(Now) + LB + '{' + LB;
      for i := 0 to pred(fAreas.Count) do
        begin
          Service := TMetaService(fAreas[i]);
          result :=
            result +
            'Name: ' + Service.Id + LB +
            'Value: ' + FloatToStr(Service.SaleProfit) + LB +
            'Depression: ' + FloatToStr(Service.Depression) + LB +
            'Rec:' + FloatToStr(Service.RecFact) + LB;
        end;
      result := result + '}' + LB;
    end;

  procedure TEconomyRelay.Reset;
    var
      i : integer;
      Service : TMetaService;
    begin
      for i := 0 to pred(fAreas.Count) do
        begin
          Service := TMetaService(fAreas[i]);
          Service.SaleProfit := 0;
        end;
    end;

  procedure TEconomyRelay.LoadFromBackup(Reader : IBackupReader);
    var
      i, cnt  : integer;
      id      : string;
      val     : single;
      Service : TMetaService;
    begin
      fAreas := TCollection.Create(0, rkUse);
      cnt := Reader.ReadInteger('count', 0);
      for i := 0 to pred(cnt) do
        begin
          id  := Reader.ReadString('', '');
          val := Reader.ReadSingle('', 0);
          Service := TMetaService(TheClassStorage.ClassById[tidClassFamily_Services, id]);
          if Service <> nil
            then
              begin
                Service.RecFact := val;
                fAreas.Insert(Service);
              end;
        end;
    end;

  procedure TEconomyRelay.StoreToBackup(Writer : IBackupWriter);
    var
      i       : integer;
      Service : TMetaService;
    begin
      Writer.WriteInteger('count', fAreas.Count);
      for i := 0 to pred(fAreas.Count) do
        begin
          Service := TMetaService(fAreas[i]);
          Writer.WriteString('srv' + IntToStr(i), Service.Id);
          Writer.WriteSingle('val' + IntToStr(i), Service.RecFact);
        end;
    end;

  procedure RegisterBackup;
    begin
      RegisterClass(TEconomyRelay);
    end;


end.
