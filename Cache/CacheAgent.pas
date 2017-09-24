unit CacheAgent;

interface

  uses
    CacheCommon, Classes;

  const
    noKind   = -1;
    noInfo   = -1;
    okSubObj = 0;
    recLinks = -2;

  type
    TObjectCache = class;
    TCacheAgent  = class;

    // TCacheAgent (CA) is a template for a class that knows how and where
    // to cache a particular class. CA should calculate the data that has been
    // request.

    CCacheAgent = class of TCacheAgent;
    TCacheAgent =
      class
        public
          class function GetPath    (Obj : TObject; kind, info : integer) : string;       virtual;
          class function GetCache   (Obj : TObject; kind, info : integer; update : boolean) : TObjectCache; virtual;
          class function UpdateCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache; virtual;
      end;

    // TObjectCache is the class of the objects that store temporarily the
    // cached data of an object, these data will be sent to the Web Server Cache.

    TObjectCache =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fProperties : TStringList;
          fAction     : TCacheAction;
          fRecLinks   : boolean;
          fUpdate     : boolean;
          fPath       : string;
          fLinks      : string;
          fDelPaths   : string;
        public
          procedure WriteString(const Name : string; const data : string);
          procedure WriteInteger(const Name : string; const data : integer);
          procedure WriteFloat(const Name : string; const data : double);
          procedure WriteCurrency(const Name : string; const data : double);
          procedure WriteBoolean(const Name : string; const data : boolean);
        private
          procedure SetProperty(const Name, Value : string);
          function  GetProperty(const Name : string) : string;
          function  GetPath : string;
          procedure SetPath(aPath : string);
        public
          property  PropertyList : TStringList read fProperties write fProperties;
          property  Properties[const name : string] : string read GetProperty write SetProperty; default;
          property  Path   : string       read GetPath write SetPath;
          property  Action : TCacheAction read fAction write fAction;
          property  RecLinks : boolean    read fRecLinks;
          property  Update : boolean      read fUpdate;
        public
          procedure Prepare;
          procedure AddLink(const aPath : string);
          procedure AddDelPath(const aPath : string);
      end;

implementation

  uses
    SysUtils, SpecialChars;


  // TCacheAgent

  class function TCacheAgent.GetPath(Obj : TObject; kind, info : integer) : string;
    begin
      result := '';
    end;

  class function TCacheAgent.GetCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
    begin
      result := TObjectCache.Create;
      result.fUpdate := update;
      result.Path := GetPath(Obj, kind, info);
      result.WriteInteger(ppObjId, integer(Obj));
      result.WriteString(ppTTL, NULLTTL);
      result.WriteInteger(ppKind, kind);
      result.fRecLinks := info = recLinks;
      if result.fRecLinks
        then result.WriteInteger(ppInfo, noInfo)
        else result.WriteInteger(ppInfo, info);
    end;

  class function TCacheAgent.UpdateCache(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
    begin
      result := GetCache(Obj, kind, info, update);
    end;


  // TObjectCache

  constructor TObjectCache.Create;
    begin
      inherited;
      fProperties := TStringList.Create;
    end;

  destructor TObjectCache.Destroy;
    begin
      fProperties.Free;
      inherited;
    end;

  procedure TObjectCache.WriteString(const Name : string; const data : string);
    begin
      //fProperties.Values[Name] := data;
      if data <> ''
        then fProperties.Insert(0, Name + '=' + data);
    end;

  procedure TObjectCache.WriteInteger(const Name : string; const data : integer);
    begin
      //fProperties.Values[Name] := IntToStr(data);
      fProperties.Insert(0, Name + '=' + IntToStr(data));
    end;

  procedure TObjectCache.WriteFloat(const Name : string; const data : double);
    begin
      //fProperties.Values[Name] := FloatToStr(data);
      fProperties.Insert(0, Name + '=' + FloatToStr(data));
    end;

  procedure TObjectCache.WriteCurrency(const Name : string; const data : double);
    begin
      //fProperties.Values[Name] := CurrToStr(data);
      fProperties.Insert(0, Name + '=' + CurrToStr(data));
    end;

  procedure TObjectCache.WriteBoolean(const Name : string; const data : boolean);
    begin
      {
      if data
        then fProperties.Values[Name] := '1'
        else fProperties.Values[Name] := '0';
      }
      if data
        then fProperties.Insert(0, Name + '=1')
        else fProperties.Insert(0, Name + '=0');
    end;

  procedure TObjectCache.SetProperty(const Name, Value : string);
    begin
      //fProperties.Values[Name] := Value;
      fProperties.Insert(0, Name + '=' + Value);
    end;

  function TObjectCache.GetProperty(const Name : string) : string;
    begin
      result := fProperties.Values[Name];
    end;

  function TObjectCache.GetPath : string;
    begin
      result := fPath;
    end;

  procedure TObjectCache.SetPath(aPath : string);
    begin
      fPath := aPath;
    end;

  procedure TObjectCache.Prepare;
    begin
      WriteString(PathName, fPath);
      WriteString(LinkName, fLinks);
      if fDelPaths = ''
        then WriteString(DelPName, LinkSep)
        else WriteString(DelPName, fDelPaths);
    end;

  procedure TObjectCache.AddLink(const aPath : string);
    begin
      if aPath <> ''
        then
          if fLinks <> ''
            then fLinks := fLinks + LinkSep + aPath
            else fLinks := aPath;
    end;

  procedure TObjectCache.AddDelPath(const aPath : string);
    begin
      if aPath <> ''
        then
          if fDelPaths <> ''
            then fDelPaths := fDelPaths + LinkSep + aPath
            else fDelPaths := aPath;
    end;

end.
