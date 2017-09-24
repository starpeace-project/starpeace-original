unit OutputSearch;

interface

  uses
    Classes, SysUtils, FluidLinks, CacheCommon;

  type
    TOutputLink         = class; // Temporal object to store an output
    TOutputSearchResult = class; // List of the resulting best outputs
    TOuputSearch        = class; // Performs a query to find the best group of outputs

    TOutputLink =
      class(TFluidLink)
        public
          constructor Create(const Name : string);
        private
          fK : integer;
          fP : single;
          fT : single;
        public
          function Cost(aX, aY : word) : single;
        public
          property K : integer read fK;
          property P : single  read fP;
          property T : single  read fT;
      end;

    TOutputSearchResult =
      class(TFluidSearchResult)
        protected
          function  CreateFluidLink(Name : string) : TFluidLink;         override;
          function  CompareFluidLinks(Item1, Item2 : TObject) : integer; override;
        private
          function  GetFluidLink(index : integer) : TOutputLink;
          function  GetCost(index : integer) : single;
        public
          property  Links[index : integer] : TOutputLink read GetFluidLink; default;
          property  Costs[index : integer] : single      read GetCost;
      end;

    TOuputSearch =
      class
        public
          constructor Create(const aPath : string; Town, Company : string; aCount : integer; aX, aY, aSortMode : word; aRole : TFacilityRoleSet);
          destructor  Destroy; override;
        private
          fResult : TOutputSearchResult;
        public
          property Result : TOutputSearchResult read fResult;
      end;

implementation

  uses
    CacheObjects, CompStringsParser, SpecialChars, MathUtils;//, AutoLog;

  // TOutputLink

  constructor TOutputLink.Create(const Name : string);
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
        fK := StrToInt(GetNextStringUpTo(Name, p, BackslashChar));
        inc(p);
        fP := StrToInt(GetNextStringUpTo(Name, p, BackslashChar))/SmallFloatShift;
        inc(p);
        fT := StrToInt(GetNextStringUpTo(Name, p, BackslashChar))/SmallFloatShift;
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

  function TOutputLink.Cost(aX, aY : word) : single;
    begin
      result := fP + fT * Distance(aX, aY);
    end;

  // TOutputSearchResult

  function TOutputSearchResult.CreateFluidLink(Name : string) : TFluidLink;
    begin
      result := TOutputLink.Create(Name);
    end;

  function TOutputSearchResult.GetFluidLink(index : integer) : TOutputLink;
    begin
      result := TOutputLink(fList[index]);
    end;

  function TOutputSearchResult.GetCost(index : integer) : single;
    begin
      result := Links[index].Cost(fX, fY);
    end;

  function TOutputSearchResult.CompareFluidLinks(Item1, Item2 : TObject) : integer;
    var
      O1  : TOutputLink absolute Item1;
      O2  : TOutputLink absolute Item2;
      dif : single;
      d   : integer;
    begin
      d := MathUtils.Dist(O1.fX, O1.fY, O2.fX, O2.fY);
      if d <> 0
        then
          begin
            case fMode of
              smDist :
                result := O1.Distance(fX, fY) - O2.Distance(fX, fY);
              smPrice :
                begin
                  dif := O1.Cost(fX, fY) - O2.Cost(fX, fY);
                  if dif = 0
                   then
                     if O1.fK > O2.fK
                       then result := 1
                       else result := -1
                   else
                     if dif > 0
                       then result := 1
                       else result := -1;
                end;
              smQuality :
                begin
                  dif := O1.fK - O2.fK;
                  if dif = 0
                    then
                      if O1.Cost(fX, fY) > O2.Cost(fX, fY)
                        then result := 1
                        else result := -1
                    else
                      if dif > 0
                        then result := -1
                        else result := 1;
                end;
              else result := -1;
            end;
          end
        else result := 0;
    end;

  // TOuputSearch

  constructor TOuputSearch.Create(const aPath : string; Town, Company : string; aCount : integer; aX, aY, aSortMode : word; aRole : TFacilityRoleSet);
    var
      FolderItr : TFolderIterator;
    begin
      inherited Create;
      fResult := TOutputSearchResult.Create(aCount, aX, aY, aSortMode, aRole);
      if Town = ''
        then Town := '*';
      if Company = ''
        then Company := '*';
      //AutoLog.Log('output', DateTimeToStr(Now) + ': begin find cycle.. ');
      FolderItr := TFolderIterator.Create(GetCacheRootPath + aPath, '*' + BackslashChar + '*' + BackslashChar + '*' + BackslashChar + '*' + BackslashChar + Town + BackslashChar + Company + BackslashChar + '*', onArchives);
      try
        if not FolderItr.Empty
          then
            repeat
              fResult.Add(FolderItr.Current);
            until not FolderItr.Next;
      finally
        //AutoLog.Log('output', DateTimeToStr(Now) + ': end cycle.. ');
        FolderItr.Free;
      end;
    end;

  destructor TOuputSearch.Destroy;
    begin
      fResult.Free;
      inherited;
    end;


end.
