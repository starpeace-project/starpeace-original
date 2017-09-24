unit Ministers;

interface

  uses
    Kernel, WorldPolitics, Classes;

  type
    TEconomyMinister = class;
    TPublicMinister  = class;
    CEconomyMinister = class of TEconomyMinister;
    CPublicMinister  = class of TPublicMinister;

    TEconomyMinistry =
      class( TMinistry )
        public
          constructor Create( aName : string; aMinId : TMinistryId; aMinisterClass : CEconomyMinister );
          destructor  Destroy; override;
        private
          fServices : TStringList;
        private
          function GetServiceId    ( index : integer ) : string;
          function GetServiceWeight( index : integer ) : integer;
          function GetServiceCount : integer;
        public
          property ServiceId    [index : integer] : string  read GetServiceId;
          property ServiceWeight[index : integer] : integer read GetServiceWeight;
          property ServiceCount : integer read GetServiceCount;
        public
          procedure AddService( Id : string; Weight : integer );
      end;

    TEconomyMinister =
      class( TMinister )
        protected
          function ComputeRating : TPercent; override;
      end;

    TPublicMinistry =
      class( TMinistry )
        public
          constructor Create( aName : string; aMinId : TMinistryId; aPubFacId : string; aMinisterClass : CPublicMinister );
        private
          fPubFacId : string;
        public
          property PubFacId : string read fPubFacId;
      end;

    TPublicMinister =
      class( TMinister )
        protected
          function ComputeRating : TPercent; override;
      end;

    THousingMinister =
      class( TMinister )
        protected
          function ComputeRating : TPercent; override;
      end;

    TComerceMinister =
      class( TEconomyMinister )
        protected
          function ComputeRating : TPercent; override;
      end;


    // RegisterBackup

    procedure RegisterBackup;

implementation

  uses
    World, MathUtils, BackupInterfaces;
    

  // TEconomyMinistry

  constructor TEconomyMinistry.Create( aName : string; aMinId : TMinistryId; aMinisterClass : CEconomyMinister );
    begin
      inherited Create( aName, aMinId, aMinisterClass );
      fServices := TStringList.Create;
    end;

  destructor TEconomyMinistry.Destroy;
    begin
      fServices.Free;
      inherited;
    end;

  function TEconomyMinistry.GetServiceId( index : integer ) : string;
    begin
      if index < fServices.Count
        then result := fServices[index]
        else result := ''; 
    end;

  function TEconomyMinistry.GetServiceWeight( index : integer ) : integer;
    begin
      if index < fServices.Count
        then result := integer(fServices.Objects[index])
        else result := 0; 
    end;

  function TEconomyMinistry.GetServiceCount : integer;
    begin
      result := fServices.Count;
    end;

  procedure TEconomyMinistry.AddService( Id : string; Weight : integer );
    begin
      fServices.AddObject( Id, TObject(Weight) );
    end;


  // TEconmyMinister

  function TEconomyMinister.ComputeRating : TPercent;
    var
      i     : integer;
      sum   : single;
      count : single;
      SCov  : TServiceCoverage;
    begin
      sum   := 0;
      count := 0;
      with TEconomyMinistry(Ministry) do
        for i := 0 to pred(ServiceCount) do
          begin
            SCov  := World.ServiceInfo[ServiceId[i]];
            sum   := sum + ServiceWeight[i]*SCov.Ratio;
            count := count + ServiceWeight[i];
          end;
      if count > 0
        then result := min(100, round(sum/count))
        else result := 0;
    end;


  // TPublicMinistry

  constructor TPublicMinistry.Create( aName : string; aMinId : TMinistryId; aPubFacId : string; aMinisterClass : CPublicMinister );
    begin
      inherited Create( aName, aMinId, aMinisterClass );
      fPubFacId := aPubFacId;
    end;


  // TPublicMinister

  function TPublicMinister.ComputeRating : TPercent;
    begin
      result := World.PublicFacInfo[TPublicMinistry(Ministry).PubFacId].Ratio;
    end;


  // THousingMinister

  function THousingMinister.ComputeRating : TPercent;
    begin
      result := 100;
    end;


  // TComerceMinister

  function TComerceMinister.ComputeRating : TPercent;
    begin
      result := inherited ComputeRating;
    end;
    


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TEconomyMinister );
      RegisterClass( TPublicMinister );
      RegisterClass( THousingMinister );
      RegisterClass( TComerceMinister );
    end;


end.


