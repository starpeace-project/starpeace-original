unit FluidLinks;

interface

  uses
    CacheCommon, Collection, {AutoLog,} SysUtils;

  const
    smDist    = 0;
    smPrice   = 1;
    smQuality = 2;

  type
    TFluidLink         = class; // A link to an output.
    TFluidSearchResult = class; // A sorted list with the resulting links.

    TFluidLink =
      class
        public
          constructor Create(const Name : string);
        protected
          fRole     : TFacilityRole;
          fX        : word;
          fY        : word;
          fTown     : string;
          fCompany  : string;
          fFacility : string;
          fCircuits : string;
        private
          function HasCircuit(const Circuit : string) : boolean;
        public
          function Distance(aX, aY : word) : integer;
          function Intercept(const aCircuits : string) : boolean;
        public
          property X        : word    read fX;
          property Y        : word    read fY;
          property Town     : string  read fTown;
          property Company  : string  read fCompany;
          property Facility : string  read fFacility;
          property Circuits : string  read fCircuits;
      end;

    TFluidSearchResult =
      class
        public
          constructor Create(aCount : integer; aX, aY, aMode : word; aRole : TFacilityRoleSet);
          destructor  Destroy; override;
        protected
          fCount : integer;
          fList  : TSortedCollection;
          fX     : word;
          fY     : word;
          fMode  : word;
          fRoles : TFacilityRoleSet;
        public
          procedure Add(Name : string);
        private
          function  GetCount : integer;
        public
          property  Role  : TFacilityRoleSet read fRoles write fRoles;
          property  Count : integer          read GetCount;
        protected
          function  CompareFluidLinks(Item1, Item2 : TObject) : integer; virtual; abstract;
          function  CreateFluidLink(Name : string) : TFluidLink;         virtual; abstract; 
      end;


implementation

  uses
    CacheObjects, MathUtils, CompStringsParser;

  // TFluidLink

  constructor TFluidLink.Create(const Name : string);
    begin
      inherited Create;
      case Name[1] of
        'P', 'p':
          fRole := rolProducer;
        'D', 'd':
          fRole := rolDistributer;
        'B', 'b':
          fRole := rolBuyer;
        'I', 'i':
          fRole := rolImporter;
        'X', 'x' :
          fRole := rolCompExport;
        'N', 'n' :
          fRole := rolCompInport;
        else
          fRole := rolNeutral;
      end;
    end;

  function TFluidLink.Distance(aX, aY : word) : integer;
    begin
      result := MathUtils.Dist(fX, fY, aX, aY);
    end;

  function TFluidLink.HasCircuit(const Circuit : string) : boolean;
    var
      token : string;
      p     : integer;
    begin
      p := 1;
      token := CompStringsParser.GetNextStringUpTo(fCircuits, p, ',');
      while (token <> '') and (token <> Circuit) do
        begin
          inc(p);
          token := CompStringsParser.GetNextStringUpTo(fCircuits, p, ',');
        end;
      result := token <> '';
    end;

  function TFluidLink.Intercept(const aCircuits : string) : boolean;
    var
      aux : string;
      p   : integer;
    begin
      if (fCircuits <> '') and (aCircuits <> '')
        then
          begin
            p   := 1;
            aux := CompStringsParser.GetNextStringUpTo(aCircuits, p, ',');
            while (aux <> '') and not HasCircuit(aux) do
              begin
                inc(p);
                aux := CompStringsParser.GetNextStringUpTo(aCircuits, p, ',');
              end;
            result := aux <> '';
          end
        else result := false;
    end;

  // TFluidSearchResult

  constructor TFluidSearchResult.Create(aCount : integer; aX, aY, aMode : word; aRole : TFacilityRoleSet);
    begin
      inherited Create;
      fList  := TSortedCollection.Create(aCount, rkBelonguer, CompareFluidLinks);
      fCount := aCount;
      fX     := aX;
      fY     := aY;
      fMode  := aMode;
      fRoles := aRole;
    end;

  destructor TFluidSearchResult.Destroy;
    begin
      fList.Free;
      inherited;
    end;

  procedure TFluidSearchResult.Add(Name : string);
    var
      Fluid : TFluidLink;
      lcnt  : integer;
    begin
      Fluid := CreateFluidLink(Name);
      if ((Fluid.fX <> fX) or (Fluid.fY <> fY)) and (Fluid.fRole in fRoles)
        then
          begin
            lcnt := fList.Count;
            //AutoLog.Log('output', DateTimeToStr(Now) + ': adding ' + Name);
            if lcnt < fCount
              then
                fList.Insert(Fluid)
              else
                if CompareFluidLinks(fList[pred(lcnt)], Fluid) > 0
                  then
                    begin
                      fList.AtDelete(pred(lcnt));
                      fList.Insert(Fluid);
                    end
                  else Fluid.Free;
            //AutoLog.Log('output', DateTimeToStr(Now) + ': end ' + Name);
          end
        else Fluid.Free;
    end;

  function TFluidSearchResult.GetCount : integer;
    begin
      result := fList.Count;
    end;

end.

