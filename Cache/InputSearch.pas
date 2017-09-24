unit InputSearch;

interface

  uses
    Classes, SysUtils, Collection, FluidLinks, CacheCommon;

  // InputLink = X×Y×M×Town×Company×FacilityName

  type
    TInputLink   = class; // Temporal object to store an Input
    TInputSearch = class; // Performs a query to find the best group of Inputs

    TInputLink =
      class(TFluidLink)
        public
          constructor Create(const Name : string);
        protected
          fCapacity : integer;
          fSupLevel : integer;
        public
          property Capacity : integer read fCapacity;
          property SupLevel : integer read fSupLevel;
      end;

    TInputSearchResult =
      class(TFluidSearchResult)
        protected
          function CreateFluidLink(Name : string) : TFluidLink;         override;
          function CompareFluidLinks(Item1, Item2 : TObject) : integer; override;
        private
          function GetFluidLink(index : integer) : TInputLink;
        public
          property Links[index : integer] : TInputLink read GetFluidLink; default;
      end;

    TInputSearch =
      class
        public
          constructor Create(const aPath : string; Town, Company : string; aCount : integer; aX, aY, aSortMode : word; aRole : TFacilityRoleSet);
          destructor  Destroy; override;
        private
          fResult : TInputSearchResult;
        public
          property Result : TInputSearchResult read fResult;
      end;

implementation

  uses
    CacheObjects, CompStringsParser, SpecialChars;

  // TInputLink

  constructor TInputLink.Create(const Name : string);
    var
      p : integer;
    begin
      inherited Create(Name);
      try
        if fRole = rolNeutral
          then p := 1
          else p := 2;
        fX := StrToInt(GetNextStringUpTo(Name, p, BackslashChar));
        inc(p);
        fY := StrToInt(GetNextStringUpTo(Name, p, BackslashChar));
        inc(p);
        fCapacity := StrToInt(GetNextStringUpTo(Name, p, BackslashChar));
        inc(p);
        fSupLevel := StrToInt(GetNextStringUpTo(Name, p, BackslashChar));
        inc(p);
        fTown := GetNextStringUpTo(Name, p, BackslashChar);
        inc(p);
        fCompany := GetNextStringUpTo(Name, p, BackslashChar);
        inc(p);
        fFacility := GetNextStringUpTo(Name, p, BackslashChar);
        inc(p);
        fCircuits := GetNextStringUpTo(Name, p, #0);
      except
      end;
    end;

  // TInputSearchResult

  function TInputSearchResult.CreateFluidLink(Name : string) : TFluidLink;
    begin
      result := TInputLink.Create(Name);
    end;

  function TInputSearchResult.CompareFluidLinks(Item1, Item2 : TObject) : integer;
    var
      I1 : TInputLink absolute Item1;
      I2 : TInputLink absolute Item2;
    begin
      result := I1.Distance(fX, fY) - I2.Distance(fX, fY);
    end;

  function TInputSearchResult.GetFluidLink(index : integer) : TInputLink;
    begin
      result := TInputLink(fList[index]);
    end;

  // TInputSearch

  constructor TInputSearch.Create(const aPath : string; Town, Company : string; aCount : integer; aX, aY, aSortMode : word; aRole : TFacilityRoleSet);
    var
      FolderItr : TFolderIterator;
    begin
      inherited Create;
      fResult := TInputSearchResult.Create(aCount, aX, aY, aSortMode, aRole);
      if Town = ''
        then Town := '*';
      if Company = ''
        then Company := '*';                  //  X×Y×C×O×Town×Company×Facility
      FolderItr := TFolderIterator.Create(GetCacheRootPath + aPath, '*' + BackslashChar + '*' + BackslashChar + '*' + BackslashChar + '*' + BackslashChar + Town + BackslashChar + Company + BackslashChar + '*', onArchives);
      try
        if not FolderItr.Empty
          then
            repeat
              fResult.Add(FolderItr.Current);
            until not FolderItr.Next;
      finally
        FolderItr.Free;
      end;
    end;

  destructor TInputSearch.Destroy;
    begin
      fResult.Free;
      inherited;
    end;


end.
