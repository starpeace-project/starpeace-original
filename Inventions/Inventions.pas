unit Inventions;

interface

  uses
    Collection, Classes, MetaInstances, Accounts, CacheAgent, Languages, Variants;

  // Inventions are developed by companies to improve production in general.
  // Each invention has an identifier (byte) that is unique within a
  // FacilityKind.

  const
    tidInvTag_Invention    = 'invention';
    tidInvTag_InventionSet = 'inventionset';
    tidInvTag_Class        = 'class';
    tidInvTag_Link         = 'link';

    tidInvElement_Props    = 'properties';
    tidInvAttr_Kind        = 'kind';
    tidInvAttr_Id          = 'id';
    tidInvAttr_Name        = 'name';
    tidInvAttr_Category    = 'category';
    tidInvAttr_Parent      = 'parent';
    tidInvAttr_Location    = 'location';
    tidInvAttr_Price       = 'price';
    tidInvAttr_Effic       = 'effic';
    tidInvAttr_Q           = 'Q';
    tidInvAttr_Blocks      = 'blocks';
    tidInvAttr_Host        = 'host';
    tidInvAttr_FacId       = 'facs';
    tidInvAttr_Prestige    = 'prestige';
    tidInvElement_Requires = 'requires';
    tidInvElement_Desc     = 'description';
    tidInvElement_Time     = 'time';
    tidInvElement_Cache    = 'cache';
    tidInvElement_Basic    = 'basic';
    tidInvElement_Applies  = 'applies';
    tidInvAttr_Class       = 'class';
    tidInvAttr_Enables     = 'enables';
    tidInvAttr_Technology  = 'tech';
    tidInvAttr_Value       = 'value';
    tidInvAttr_Tier        = 'tier';
    tidInvAttr_LicLevel    = 'lclevel';
    tidInvAttr_Volatile    = 'volatile';
    tidInvElement_Tiers    = 'tiers';
    tidInvAttr_Override    = 'override';
    tidInvAttr_Cluster     = 'cluster';
    tidInvAttr_Nobility    = 'nob';
    tidInvAttr_CatPos      = 'catpos';
    tidInvAttr_Obsolete    = 'obsolete';

  const
    BitCount = 8;

  const
    StdHourlyCost = 0.5; //%

  type
    TCollection = Collection.TCollection;

  type
    Bits              = 0..BitCount-1;
    TBitSet           = set of Bits;
    PInventionIdArray = ^TInventionIdArray;
    TInventionIdArray = array[0..$FFFF div BitCount] of TBitSet;

    TInventionNumId   = integer;

    TInventionSet =
      class
        public
          constructor Create(IniCount : integer);
          destructor  Destroy; override;
        private
          fCapacity : word;
          fSets     : PInventionIdArray;
        private
          function  CountToSets(count : integer) : integer;
          procedure Decompose(Elem : TInventionNumId; var Index : integer; var Value : Bits);
        public
          function  Include(Elem : TInventionNumId)  : boolean;
          function  Exclude(Elem : TInventionNumId)  : boolean;
          function  Included(Elem : TInventionNumId) : boolean;
          function  IsEmpty : boolean;
          procedure Clear;
      end;

    TInvention = class;
    CInvention = class of TInvention;

    TInvention =
      class( TMetaInstance )
        public
          constructor Create( anId, aName, aKind, aResp : string );
          constructor Load(xmlObj : OleVariant); virtual;
          destructor  Destroy; override;
        private
          fId       : string;
          fOldId    : integer;
          fNumId    : TInventionNumId;
          fName     : string;
          fKind     : string;
          fDesc     : string;
          fParent   : string;
          fName_MLS : TMultiString;
          fDesc_MLS : TMultiString;
          fResp_MLS : TMultiString;
          fParent_MLS : TMultiString;
          fPrice    : TMoney;
          fTime     : integer;
          fBasic    : boolean;
          fPrestige : integer;
          fResp     : string;
          fLevel    : integer;
          fTier     : integer;
          fTierInfo : TStringList;
          fReq      : TCollection;
          fReqIds   : TStringList;
          fLicLevel : single;
          fVolatile : boolean;
          fCache    : boolean;
          fEnablesTech : boolean;
          fNobility : integer;
          fCatPos   : integer;
          fObsolete : boolean;
        private
          procedure SetLicLevel(value : single);
        published
          property Id       : string          read fId;
          property OldId    : integer         read fOldId; // >> temp
          property NumId    : TInventionNumId read fNumId;
          property Name     : string          read fName;
          property Kind     : string          read fKind;
          property Desc     : string          read fDesc     write fDesc;
          property Name_MLS : TMultiString    read fName_MLS;
          property Desc_MLS : TMultiString    read fDesc_MLS;
          property Resp_MLS : TMultiString    read fResp_MLS;
          property Parent_MLS : TMultiString  read fParent_MLS;
          property Price    : TMoney          read fPrice    write fPrice;
          property Time     : integer         read fTime     write fTime;
          property Basic    : boolean         read fBasic    write fBasic;
          property Prestige : integer         read fPrestige write fPrestige;
          property Resp     : string          read fResp     write fResp;
          property Tier     : integer         read fTier;
          property Level    : integer         read fLevel    write fLevel;
          property LicLevel : single          read fLicLevel write SetLicLevel;
          property Volatile : boolean         read fVolatile write fVolatile;
          property EnablesTech : boolean      read fEnablesTech;
          property Nobility : integer         read fNobility;
          property CatPos   : integer         read fCatPos;
          property Obsolete : boolean         read fObsolete;
        public
          property Req : TCollection read fReq;
        private
          procedure SetRequired(xmlObj : OleVariant);
          procedure DoLinkInvention(cluster, blocks, host : string);
          procedure LinkInvention(Appls : OleVariant);
          procedure EnableFacilities(Obj : OleVariant);
          procedure ApplyToFacilities(facs, cluster : string);
        protected
          procedure FixDeps;
        public
          function  Enabled(Company : TObject) : boolean; virtual;
          function  GetFeePrice(Company : TObject) : TMoney; virtual;
          function  GetProperties(Company : TObject; LangId : TLanguageId) : string; virtual;
          procedure StoreToCache(Cache : TObjectCache; kind : string; index : integer); virtual;
          function  GetFullDesc( LangId : TLanguageId ) : string;
          function  GetTierOf( var Comp ) : integer;
        public
          procedure RetrieveTexts( Container : TDictionary ); override;
          procedure StoreTexts   ( Container : TDictionary ); override;
        public
          procedure StoreClientInfo( Dest : TStream; LangId : TLanguageId );
        public
          function GetClientProps(Company : TObject; LangId : TLanguageId ) : string; virtual;
        private
          fImplementable : boolean;
        public
          property Implementable : boolean read fImplementable write fImplementable;
      end;

    TInventionRecord  =
      class
        public
          constructor Create(aInvention : TInvention; aCost : TMoney);
        private
          fInvention : TInvention;
          fTotalCost : TMoney;
          fSubsidy   : TMoney;
          fUsage     : integer;
        public
          property Invention : TInvention read fInvention write fInvention;
          property TotalCost : TMoney     read fTotalCost write fTotalCost;
          property Subsidy   : TMoney     read fSubsidy   write fSubsidy;
          property Usage     : integer    read fUsage     write fUsage;
        public
          function HourlyCostPerFac(Perc : single) : TMoney;
          function HourlyCost(Perc : single) : TMoney;
          function YearCost(Perc : single) : TMoney;
      end;

    TInventionClass =
      class
        public
          constructor Create(aClass : CInvention);
        private
          fClass : CInvention;
      end;

  function  GetInventionById(id : string) : TInvention;
  procedure CreateInventions(path : string);
  procedure CreateInventionsFromFile(path : string);

  function FindInvention(id : string) : TInvention;
  function GetProperty(ppObj : OleVariant; name : string) : OleVariant;

  function FormatDelta( points : integer ) : string;


implementation

  uses
    SysUtils, ComObj, Kernel, Headquarters, ClassStorage, CompStringsParser, Protocol, SimHints,
    FacIds, Math, MathUtils, DelphiStreamUtils, TycoonLevels;

  const
    tidMSXML = 'msxml';

  const
    isDelta = 5;

  var
    LastInvention : integer     = 0;
    InventionList : TCollection = nil;

  function GetLevelOf( tier : integer; langId : TLanguageId ) : string;
    var
      Level : TTycoonLevel;
    begin
      Level := GetLevelOfTier( tier );
      if Level <> nil
        then result := Level.Name_MLS.Values[langId]
        else result := '';
      {
      case tier of
        0: result := 'Apprentice';
        1: result := 'Entrepreneur';
        2: result := 'Tycoon';
        3: result := 'Master';
        4: result := 'Paradigm';
        5: result := 'Legend';
        else result := '?';
      end;
      }
    end;

  function GetInventionById(id : string) : TInvention;
    begin
      result := TInvention(TheClassStorage.ClassById[tidClassFamily_Inventions, id]);
    end;

  type
    pinteger = ^integer;

  // TInventionSet

  constructor TInventionSet.Create(IniCount : integer);
    begin
      inherited Create;
      fCapacity := CountToSets(IniCount);
      ReallocMem(fSets, fCapacity*sizeof(fSets[0]));
      if fSets <> nil
        then FillChar(fSets[0], fCapacity*sizeof(fSets[0]), 0);
    end;

  destructor TInventionSet.Destroy;
    begin
      ReallocMem(fSets, 0);
      inherited;
    end;

  function TInventionSet.CountToSets(count : integer) : integer;
    begin
      result := succ(count div BitCount);
    end;

  procedure TInventionSet.Decompose(Elem : TInventionNumId; var Index : integer; var Value : Bits);
    begin
      Index := Elem div BitCount;
      Value := Elem mod BitCount;
    end;

  function TInventionSet.Include(Elem : TInventionNumId) : boolean;
    var
      idx    : integer;
      val    : Bits;
      OldCap : integer;
      NewCap : integer;
    begin
      Decompose(Elem, idx, val);
      if idx >= fCapacity
        then
          begin
            OldCap    := fCapacity;
            fCapacity := idx + 5;
            ReallocMem(fSets, fCapacity*sizeof(fSets[0]));
            NewCap := fCapacity;
            FillChar(fSets[OldCap], (NewCap - OldCap)*sizeof(fSets[0]), 0);
          end;
      if val in fSets[idx]
        then result := false
        else
          begin
            System.Include(fSets[idx], val);
            result := true;
          end;
    end;

  function TInventionSet.Exclude(Elem : TInventionNumId) : boolean;
    var
      idx : integer;
      val : Bits;
    begin
      Decompose(Elem, idx, val);
      if idx < fCapacity
        then
          begin
            result := val in fSets[idx];
            System.Exclude(fSets[idx], val);
          end
        else result := false;
    end;

  function TInventionSet.Included(Elem : TInventionNumId) : boolean;
    var
      idx : integer;
      val : Bits;
    begin
      Decompose(Elem, idx, val);
      if idx < fCapacity
        then result := val in fSets[idx]
        else result := false;
    end;

  function TInventionSet.IsEmpty : boolean;
    var
      cap : integer;
      i   : integer;
    begin
      cap := fCapacity;
      i   := 0;
      while (i < cap) and (fSets[i] = []) do
        inc(i);
      result := i = cap;
    end;

  procedure TInventionSet.Clear;
    begin
      fCapacity := 0;
      ReallocMem(fSets, 0);
    end;

  // TInvention

  constructor TInvention.Create( anId, aName, aKind, aResp : string );
    begin
      inherited Create( anId );
      fName := aName;
      fKind := aKind;
      fResp := aResp;
      fName_MLS := TMultiString.Create;
      fDesc_MLS := TMultiString.Create;
      fResp_MLS := TMultiString.Create;
      fParent_MLS := TMultiString.Create;
      fReq := TCollection.Create( 0, rkUse );
    end;

  constructor TInvention.Load(xmlObj : OleVariant);

    procedure OverrideTiers( Tiers : olevariant );
      var
        coll    : olevariant;
        item    : olevariant;
        i       : integer;
        cluster : string;
        value   : integer;
      begin
        coll := Tiers.Children;
        for i := 0 to pred(StrToInt(coll.Length)) do
          begin
            item := coll.item( i, Unassigned );
            if not VarIsEmpty(item)
              then
                begin
                  cluster := Item.getAttribute( tidInvAttr_Cluster );
                  value   := StrToInt(Item.getAttribute( tidInvAttr_Value ));
                  fTierInfo.AddObject( cluster, TObject(value) );
                end;
          end;
      end;

    var
      theId : string;
      Aux   : OleVariant;
    begin
      Aux   := xmlObj.children.item(tidInvElement_Props, Unassigned);
      theId := GetProperty(Aux, tidInvAttr_Id);
      inherited Create( theId );
      fId    := theId;
      // General stuff
      fOldId    := GetProperty(Aux, 'oldid'); // >> temp
      fName     := GetProperty(Aux, tidInvAttr_Name);
      fKind     := GetProperty(Aux, tidInvAttr_Kind);
      fPrice    := GetProperty(Aux, tidInvAttr_Price);
      fPrestige := GetProperty(Aux, tidInvAttr_Prestige);
      fTime     := GetProperty(Aux, tidInvElement_Time);
      fBasic    := lowercase(GetProperty(Aux, tidInvElement_Basic)) = 'true';
      fCache    := lowercase(GetProperty(Aux, tidInvElement_Cache)) <> 'no';
      fParent   := GetProperty(Aux, tidInvAttr_Parent);
      fResp     := GetProperty(Aux, tidInvAttr_Location);
      fTier     := GetProperty(Aux, tidInvAttr_Tier);
      fLicLevel := GetProperty(Aux, tidInvAttr_LicLevel);

      if StrToInt(TheGlobalConfigHandler.GetConfigParm('TornamentLength', '0')) = 0
        then fNobility := GetProperty(Aux, tidInvAttr_Nobility)
        else fNobility := 0;

      fCatPos   := GetProperty(Aux, tidInvAttr_CatPos);
      fObsolete := GetProperty(Aux, tidInvAttr_Obsolete);

      Aux       := GetProperty(Aux, tidInvAttr_Volatile);
      fVolatile := not VarIsEmpty(Aux) and (lowercase(Aux) = 'true') or (fLicLevel > 0);

      // Required to start
      fReq := TCollection.Create(0, rkUse);
      SetRequired(xmlObj);

      Aux := xmlObj.children.item(tidInvElement_Desc, Unassigned);
      if not VarIsEmpty(Aux)
        then fDesc := Aux.text;

      // Enable facilities
      try
        Aux := xmlObj.children.item(tidInvAttr_Enables, Unassigned);
      except
        Aux := Unassigned;
      end;
      if not VarIsEmpty(Aux)
        then
          begin
            EnableFacilities(Aux);
            fEnablesTech := true;
          end;

      // Tier overriding
      fTierInfo := TStringList.Create;
      try
        Aux := xmlObj.children.item(tidInvElement_Tiers, Unassigned);
      except
        Aux := Unassigned;
      end;
      if not VarIsEmpty(Aux)
        then OverrideTiers( Aux );
      fName_MLS := TMultiString.Create;
      fDesc_MLS := TMultiString.Create;
      fResp_MLS := TMultiString.Create;
      fParent_MLS := TMultiString.Create;
    end;

  destructor TInvention.Destroy;
    begin
      fName_MLS.Free;
      fDesc_MLS.Free;
      fResp_MLS.Free;
      fParent_MLS.Free;
      fReq.Free;
      fTierInfo.Free;
      inherited;
    end;

  procedure TInvention.SetLicLevel(value : single);
    begin
      fLicLevel := value;
      fVolatile := fVolatile or (value <> 0);
    end;

  procedure TInvention.SetRequired(xmlObj : OleVariant);
    var
      Req  : OleVariant;
      Item : OleVariant;
      i    : integer;
    begin
      if not VarIsEmpty(xmlObj.children)
        then
          begin
            try
              Req := xmlObj.children.item(tidInvElement_Requires, Unassigned);
            except
              Req := Unassigned;
            end;
            if not VarIsEmpty(Req)
              then
                begin
                  fReqIds := TStringList.Create;
                  Req     := Req.children;
                  i       := 0;
                  while i < Req.Length do
                    begin
                      Item := Req.item(i, Unassigned);
                      fReqIds.Add(Item.getAttribute(tidInvAttr_Id));
                      inc(i);
                    end;
                end;
          end;
    end;

  procedure TInvention.DoLinkInvention(cluster, blocks, host : string);
    var
      p       : integer;
      aux     : string;
      MetaBlk : TMetaBlock;
    begin
      p   := 1;
      aux := GetNextStringUpTo(blocks, p, ',');
      while aux <> '' do
        begin
          inc(p);
          MetaBlk := TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, cluster + aux]);
          if MetaBlk <> nil
            then MetaBlk.DeclareInvention(self);
            //else raise Exception.Create('Unknown block ' + aux + ' in cluster ' + cluster);
          aux := Trim(GetNextStringUpTo(blocks, p, ','));
        end;
      p := 1;
      aux := GetNextStringUpTo(host, p, ',');
      while aux <> '' do
        begin
          MetaBlk := TMetaBlock(TheClassStorage.ClassById[tidClassFamily_Blocks, cluster + aux{host}]);
          if MetaBlk <> nil
            then TMetaHeadquarterBlock(MetaBlk).RegisterInvention(self);
            //else raise Exception.Create('Unknown headquarter ' + aux{host} + ' in cluster ' + cluster);
          inc(p);  
          aux := GetNextStringUpTo(host, p, ',');
        end;
    end;

  procedure TInvention.LinkInvention(Appls : OleVariant);
    var
      Elems   : OleVariant;
      Cluster : OleVariant;
      i, len  : integer;
      clName  : string;
      blocks  : string;
      facs    : string;
      host    : string;
      clId    : string;
    begin
      if not VarIsEmpty(Appls)
        then
          try // <cluster name="Diss" blocks="Farm, FoodProc" host="IndHeadquarter"/>
            Elems := Appls.children;
            len   := Elems.Length;
            i     := 0;
            while i < len do
              begin
                Cluster := Elems.item(i, Unassigned);
                clName  := Cluster.getAttribute(tidInvAttr_Name);
                clId    := Cluster.getAttribute(tidInvAttr_Id);
                blocks  := Cluster.getAttribute(tidInvAttr_Blocks);
                host    := Cluster.getAttribute(tidInvAttr_Host);
                facs    := Cluster.getAttribute(tidInvAttr_FacId);
                DoLinkInvention(clName, blocks, host);
                if clId <> ''
                  then ApplyToFacilities(facs, clId)
                  else ApplyToFacilities(facs, clName);
                inc(i);
              end;
          except
          end;
    end;

  procedure TInvention.EnableFacilities(Obj : OleVariant);
    var
      tech : string;
      i    : integer;
      Fac  : TMetaFacility;
    begin
      tech := Obj.getAttribute(tidInvAttr_Technology);
      if tech <> ''
        then
          for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Facilities]) do
            begin
              Fac := TMetaFacility(TheClassStorage.ClassByIdx[tidClassFamily_Facilities, i]);
              if Fac.TechnologyKind = tech
                then Fac.RequiresInvention(self);
            end;
    end;

  procedure TInvention.ApplyToFacilities(facs, cluster : string);
    var
      ids : array[0..127] of TFacId;
      cnt : integer;
      p   : integer;
      i   : integer;
      aux : string;
      Fac : TMetaFacility;
    begin
      if facs <> ''
        then
          begin
            cnt := 0;
            p   := 1;
            CompStringsParser.SkipChars(facs, p, Spaces);
            aux := CompStringsParser.GetNextStringUpTo(facs, p, ',');
            while aux <> '' do
              begin
                ids[cnt] := StrToInt(aux);
                inc(cnt);
                inc(p);
                CompStringsParser.SkipChars(facs, p, Spaces);
                aux := CompStringsParser.GetNextStringUpTo(facs, p, ',');
              end;
            if cnt > 0
              then
                for i := 0 to pred(TheClassStorage.ClassCount[tidClassFamily_Facilities]) do
                  begin
                    Fac := TMetaFacility(TheClassStorage.ClassByIdx[tidClassFamily_Facilities, i]);
                    if FacIds.Contains(slice(ids, cnt), Fac.FacId) and (Fac.ClusterName = cluster)
                      then Fac.TypicalStage.MetaBlock.DeclareInvention(self);
                  end;
          end;
    end;

  procedure TInvention.FixDeps;
    var
      Inv : TInvention;
      i   : integer;
    begin
      if fReqIds <> nil
        then
          for i := 0 to pred(fReqIds.Count) do
            if fReqIds[i] <> ''
              then
                begin
                  Inv := GetInventionById(fReqIds[i]);
                  if Inv <> nil
                    then                                   
                      begin
                        fReq.Insert(Inv);
                        fTier := max(fTier, Inv.Tier);
                      end
                    else raise Exception.Create('Unknown invention ' + fReqIds[i]);
                end;
      fReqIds.Free;
      fReqIds := nil;
    end;

  function TInvention.Enabled(Company : TObject) : boolean;

    function TierAndNobilityMatches(Company : TCompany) : boolean;
      var
        Tycoon : TTycoon;
      begin
        if (Company <> nil) and (Company.Owner <> nil) and (Company.Owner.Level <> nil) and (Company.Owner.Level.Tier >= GetTierOf(Company))
          then
            begin
              if Nobility <= 0
                then result := true
                else
                  begin
                    Tycoon := Company.Owner;
                    if Tycoon.NobPoints < 0
                      then Tycoon.UpdateNobility;
                    result := Tycoon.NobPoints >= Nobility
                  end;
            end
          else result := false;
      end;

    var
      i : integer;
    begin
      if TierAndNobilityMatches( TCompany(Company) )
        then
          if Req.Count = 0
            then result := true
            else
              begin
                i := 0;
                while (i < Req.Count) and TCompany(Company).HasInvention[TInvention(Req[i]).NumId] do
                  inc(i);
                result := i = Req.Count;
              end
        else result := false;
    end;

  function TInvention.GetFeePrice(Company : TObject) : TMoney;
    begin
      {
      if (fLicLevel > 0) and (Company <> nil) and (TCompany(Company).Owner <> nil) and (TCompany(Company).Owner.LicenceLevel > 0)
        then result := 1000000*power(2, TCompany(Company).Owner.LicenceLevel)
        else result := 0;
      }
      if (fLicLevel > 0) and (Company <> nil) and (TCompany(Company).Owner <> nil) and (TCompany(Company).Owner.LicenceLevel > 0)
        then result := 1000000*power(2, TCompany(Company).Owner.LicenceLevel + fLicLevel - 1)
        else result := 0;
    end;

  function TInvention.GetProperties(Company : TObject; LangId : TLanguageId) : string;
    var
      lic : TMoney;
      Rec : TInventionRecord;
    begin
      // Price
      if fPrice > 0
        then result := SimHints.GetHintText(mtidInvPrice.Values[LangId], [FormatMoney(fPrice)]) + LineBreak //'Price: ' + FormatMoney(fPrice) + ^M^J
        else result := '';
      // Licence
      if Company <> nil
        then
          begin
            Rec := TCompany(Company).FindInventionRecord(self);
            if Rec <> nil //TCompany(Company).HasInvention[fNumId]
              then lic := Rec.TotalCost - fPrice
              else lic := GetFeePrice(Company);
            if lic > 0
              then result := result + SimHints.GetHintText(mtidInvLicense.Values[LangId], [FormatMoney(lic)]) + LineBreak;//'Licence: ' + FormatMoney(lic) + ^M^J;
            if fImplementable
              then
                if Rec <> nil
                  then
                    begin
                      result := result + SimHints.GetHintText(mtidInvImpCostHour.Values[LangId], [FormatMoney(Rec.HourlyCost(StdHourlyCost))]) + LineBreak;
                      result := result + SimHints.GetHintText(mtidInvUsage.Values[LangId], [Rec.Usage]) + LineBreak;
                    end
                  else result := result + SimHints.GetHintText(mtidInvImpCostYear.Values[LangId], [FormatMoney((StdHourlyCost/100)*fPrice)]) + LineBreak;
          end;
      // Prestige
      if Prestige <> 0
        then result := result + SimHints.GetHintText(mtidInvPrestige.Values[LangId], [FormatDelta(Prestige)]) + LineBreak;
      // Tier
      if (Company <> nil) and (TCompany(Company).Owner <> nil) and (TCompany(Company).Owner.Level <> nil) and (TCompany(Company).Owner.Level.Tier < fTier)
        then result := result + SimHints.GetHintText(mtidInvLevel.Values[LangId], [GetLevelOf(GetTierOf(Company), LangId)]) + LineBreak //'Level: ' + GetLevelOf(GetTierOf(Company), LangId) + ^M^J
        else result := result + SimHints.GetHintText(mtidInvLevel.Values[LangId], [GetLevelOf(fTier, LangId)]) + LineBreak;
      // Nobility
      if fNobility > 0
        then result := result + SimHints.GetHintText(mtidInvNobPoints.Values[LangId], [IntToStr(fNobility)]) + LineBreak
    end;

  procedure TInvention.StoreToCache(Cache : TObjectCache; kind : string; index : integer);
    var
      iStr : string;
    begin
      iStr := IntToStr(index);
      Cache.WriteString(kind + 'RsId'  + iStr, Id);
      if fVolatile
        then
          begin
            Cache.WriteString(kind + 'RsName' + iStr, Name);
            Cache.WriteString(kind + 'RsDyn'  + iStr, 'yes');
            if fParent <> ''
              then Cache.WriteString(kind + 'RsParent' + iStr, fParent)
          end;
    end;

  function TInvention.GetFullDesc( LangId : TLanguageId ) : string;
    var
      i   : integer;
      Inv : TInvention;
    begin
      result := Desc_MLS.Values[LangId];
      if fReq.Count > 0
        then result := result + ^M^J + mtidDescFactoryReq.Values[langId] + ': ';
      for i := 0 to pred(fReq.Count) do                 
        begin
          Inv := TInvention(fReq[i]);
          if i < pred(fReq.Count)
            then result := result + Inv.Name_MLS.Values[LangId] + ', '
            else result := result + Inv.Name_MLS.Values[LangId] + '.';
        end;
    end;

  function TInvention.GetTierOf( var Comp ) : integer;
    var
      Company : TCompany absolute Comp;
    begin
      if (Company <> nil) and (Company.Cluster <> nil) and (fTierInfo.IndexOf( Company.Cluster.Id ) <> NoIndex)
        then result := integer(fTierInfo.Objects[fTierInfo.IndexOf( Company.Cluster.Id )])
        else result := fTier
    end;

  procedure TInvention.RetrieveTexts( Container : TDictionary );
    var
      aux : string;
    begin
      inherited;
      // Assign multi strings
      fName_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Name'];
      fDesc_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Desc'];
      fResp_MLS.Values[Container.LangId] := Container.Values[Family + '.' + Id + '.' + 'Resp'];
      aux := Container.Values[Family + '.Category.' + fParent];
      if aux <> ''
        then fParent_MLS.Values[Container.LangId] := Container.Values[Family + '.Category.' + fParent]
        else fParent_MLS.Values[Container.LangId] := Container.Values[Family + '.Category.General'];
    end;

  procedure TInvention.StoreTexts( Container : TDictionary );
    begin
      inherited;
      Container.Values[Family + '.' + Id + '.' + 'Name']  := fName;
      Container.Values[Family + '.' + Id + '.' + 'Desc']  := fDesc;
      Container.Values[Family + '.' + Id + '.' + 'Resp']  := fResp;
      Container.Values[Family + '.Category.'   + fParent] := fParent;
    end;

  procedure TInvention.StoreClientInfo( Dest : TStream; LangId : TLanguageId );
    begin
      WriteString( Dest, fId );
      WriteString( Dest, Name_MLS.Values[LangId] + '|' + Resp_MLS.Values[LangId]);
      WriteString( Dest, GetFullDesc( LangId ) );
      if Parent_MLS.Values[LangId] <> ''
        then WriteString( Dest, Parent_MLS.Values[LangId] )
        else WriteString( Dest, fParent );
      Dest.WriteBuffer( fCache, sizeof(fCache) );
      WriteString(Dest, GetClientProps(nil, LangId));
    end;

  function TInvention.GetClientProps(Company : TObject; LangId : TLanguageId ) : string;
    begin
      result := GetProperties(Company, LangId);
    end;


  // TInventionRecord

  constructor TInventionRecord.Create(aInvention : TInvention; aCost : TMoney);
    begin
      inherited Create;
      fInvention := aInvention;
      fTotalCost := aCost;
    end;

  function TInventionRecord.HourlyCostPerFac(Perc : single) : TMoney;
    const
      HoursAYear = 365*24;
    begin
      result := (Perc/100)*fInvention.Price/HoursAYear;
    end;

  function TInventionRecord.HourlyCost(Perc : single) : TMoney;
    begin
      result := fUsage*HourlyCostPerFac(Perc);
    end;

  function TInventionRecord.YearCost(Perc : single) : TMoney;
    begin
      result := (Perc/100)*fInvention.Price;
    end;

  // TInventionClass

  constructor TInventionClass.Create(aClass : CInvention);
    begin
      inherited Create;
      fClass := aClass;
    end;

  function CreateInvention(InvObj, Appls : OleVariant) : TInvention;
    var
      ClassId  : string;
      InvClass : CInvention;
      Aux      : OleVariant;
    begin
      ClassId := InvObj.getAttribute(tidInvAttr_Class);
      if ClassId <> ''
        then InvClass := TInventionClass(TheClassStorage.ClassById[tidClassFamily_InvClasses, ClassId]).fClass
        else InvClass := TInvention;
      result := InvClass.Load(InvObj);
      result.fNumId := LastInvention;
      inc(LastInvention);
      try
        Aux := InvObj.children.item(tidInvElement_Applies, Unassigned);
        if VarIsEmpty(Aux)
          then Aux := Appls;
      except
        Aux := Appls;
      end;
      if not result.Obsolete
        then result.LinkInvention(Aux);
      result.Register(tidClassFamily_Inventions);
      InventionList.Insert(result);
    end;

  procedure ListInventionFileElements(Elems, Appls : OleVariant);
    var
      i, len : integer;
      Elem   : OleVariant;
      Aux    : OleVariant;
    begin
      i := 0;
      len := Elems.Length;
      while i < len do
        begin
          Elem := Elems.item(i, Unassigned);
          if lowercase(Elem.tagName) = tidInvTag_Invention
            then CreateInvention(Elem, Appls)
            else
              if lowercase(Elem.tagName) = tidInvTag_InventionSet
                then
                  begin
                    try
                      Aux := Elem.children.item(tidInvElement_Applies, Unassigned);
                    except
                      Aux := Unassigned;
                    end;
                    if VarIsEmpty(Aux)
                      then ListInventionFileElements(Elem.children, Appls)
                      else ListInventionFileElements(Elem.children, Aux);
                  end;
          inc(i);
        end;
    end;

  procedure CreateInventionsFromFile(path : string);
    var
      InvFile : OleVariant;
      Root    : OleVariant;
      Appls   : OleVariant;
    begin
      try
        InvFile := CreateOLEObject(tidMSXML);
        InvFile.url := path;
        Root := InvFile.root;
        try
          Appls := Root.children.item(tidInvElement_Applies, Unassigned);
        except
          Appls := Unassigned;
        end;
        ListInventionFileElements(Root.children, Appls);
      except
        raise Exception.Create('Error reading the invention file: ' + ExtractFileName(path));
      end;
    end;

  procedure CreateInventions(path : string);
    var
      Search : TSearchRec;
      found  : integer;
      i      : integer;
    begin
      InventionList := TCollection.Create(0, rkUse);
      try
        found := FindFirst(path + '*.xml', faArchive, Search);
        while found = 0 do
          begin
            CreateInventionsFromFile(path + Search.Name);
            found := FindNext(Search);
          end;
      finally
        FindClose(Search);
      end;
      for i := 0 to pred(InventionList.Count) do
        TInvention(InventionList[i]).FixDeps;
    end;

  function FindInvention(id : string) : TInvention;
    var
      i : integer;
    begin
      i := pred(InventionList.Count);
      while (i >= 0) and ((TInvention(InventionList[i]).kind + IntToStr(TInvention(InventionList[i]).fOldId) <> id)) do
        dec(i);
      if i < 0
        then result := nil
        else result := TInvention(InventionList[i]);
    end;

  function GetProperty(ppObj : OleVariant; name : string) : OleVariant;
    var                                                                          
      Aux : OleVariant;
    begin
      Aux := ppObj.children.item(UpperCase(name), Unassigned);
      if not VarIsEmpty(Aux)
        then result := Aux.getAttribute(tidInvAttr_Value)
        else result := ppObj.getAttribute(tidInvAttr_Value);
    end;

  function FormatDelta( points : integer ) : string;
    begin
      result := IntToStr( points );
      if points > 0
        then result := '+' + result;
    end;

initialization

  InventionList := nil;

finalization

  InventionList.Free;

end.
