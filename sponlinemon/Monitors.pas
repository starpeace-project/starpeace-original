unit Monitors;

interface

  uses
    Classes, SysUtils;

  type
    PMonitorArray = ^TMonitorArray;
    TMonitorArray = array[0..1024] of integer;

  type
    TMonitorObject  = class;
    TServerMonitor = class;
    TAreaMonitor   = class;
    TWorldMonitor  = class;

    TMonitorObject =
      class
        public
          constructor Create(samplCount : integer);
          destructor  Destroy; override;
        private
          fSampleCount : integer;
          fSample      : PMonitorArray;
          fCursor      : integer;
        private
          procedure ShlSample;
        protected
          procedure SetCurrentValue(value : integer);
          function  GetCurrentValue : integer;
        public
          function  Update : integer; virtual; abstract;
          procedure Clean;  virtual;
          procedure SetSampleCount(count : integer); virtual;
          function  GetSampleCount : integer; virtual;
          function  GetSample(index : integer) : integer; virtual;
          function  GetMax : integer;
          function  GetMin : integer;
        public
          property SampleCount : integer read GetSampleCount write SetSampleCount;
          property Samples[index : integer] : integer read GetSample;
          property Cursor : integer read fCursor;
          property CurrentValue : integer read GetCurrentValue;
          property Max : integer read GetMax;
          property Min : integer read GetMin;
      end;

    TServerMonitor =
      class(TMonitorObject)
        public
          constructor Create(theProxy : OleVariant; samplCount : integer);
          //constructor Create(DSAddr : string; Port, samplCount : integer);
          destructor  Destroy; override;
        public
          function  Update : integer; override;
        private
          fProxy : OleVariant;
          fAreas : TStringList;
        public
          property Proxy : OleVariant  read fProxy;
          property Areas : TStringList read fAreas;
      end;

    TAreaMonitor =
      class(TMonitorObject)
        public
          constructor Create(Owner : TServerMonitor; aName : string);
          destructor  Destroy; override;
        private
          fName   : string;
          fOwner  : TServerMonitor;
          fWorlds : TStringList;
        public
          function  Update : integer; override;
        public
          property Name   : string read fName;
          property Owner  : TServerMonitor read fOwner;
          property Worlds : TStringList read fWorlds;
      end;

    TWorldMonitor =
      class(TMonitorObject)
        public
          constructor Create(Owner : TAreaMonitor; aName : string);
          destructor  Destroy; override;
        private
          fOwner  : TAreaMonitor;
          fName   : string;
        public
          function  Update : integer; override;
        public
          property Name  : string read fName;
      end;


implementation

  uses
    ComObj;

  // TMonitorObject

  constructor TMonitorObject.Create(samplCount : integer);
    begin
      inherited Create;
      fSampleCount := samplCount;
      SetSampleCount(samplCount);
    end;

  destructor TMonitorObject.Destroy;
    begin
      SetSampleCount(0);
      inherited;
    end;


  procedure TMonitorObject.Clean;
    begin
      fCursor := -1;
      if fSample <> nil
        then FillChar(fSample^, fSampleCount*sizeof(fSample[0]), 0);
    end;

  function TMonitorObject.GetSample(index : integer) : integer;
    begin
      result := fSample[index];
    end;

  function TMonitorObject.GetSampleCount : integer;
    begin
      result := fSampleCount;
    end;

  function TMonitorObject.GetMax : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to fCursor do
        if fSample[i] > result
          then result := fSample[i];
    end;

  function TMonitorObject.GetMin : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to fCursor do
        if fSample[i] < result
          then result := fSample[i];
    end;

  procedure TMonitorObject.SetCurrentValue(value : integer);
    begin
      if fSampleCount > 0
        then
          begin
            if fCursor = pred(fSampleCount)
              then ShlSample
              else inc(fCursor);
            fSample[fCursor] := value;
          end;
    end;

  function TMonitorObject.GetCurrentValue : integer;
    begin
      if (fCursor >= 0) and (fSampleCount > 0)
        then result := fSample[fCursor]
        else result := 0;
    end;

  procedure TMonitorObject.SetSampleCount(count: integer);
    var
      i : integer;
    begin
      ReallocMem(fSample, count*sizeof(fSample[0]));
      fCursor := pred(fSampleCount);
      fSampleCount := count;
      for i := 0 to pred(count) do
        fSample[i] := 0;
    end;

  procedure TMonitorObject.ShlSample;
    begin
      move(fSample[1], fSample[0], fCursor*sizeof(fSample[0]));
    end;

  // TServerMonitor

  constructor TServerMonitor.Create(theProxy : OleVariant; samplCount : integer);
    var
      key  : string;
      List : TStringList;
      i    : integer;
      Area : TAreaMonitor;
    begin
      inherited Create(samplCount);
      fProxy := theProxy;
      fAreas := TStringList.Create;
      key    := 'root/areas';
      if fProxy.RDOSetCurrentKey(key)
        then
          begin
            List := TStringList.Create;
            try
              List.Text := fProxy.RDOGetKeyNames;
              for i := 0 to pred(List.count) do
                begin
                  Area := TAreaMonitor.Create(Self, List[i]);
                  fAreas.AddObject(List[i], Area);
                end;
            finally
              List.Free;
            end;
          end;
    end;

  destructor TServerMonitor.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fAreas.Count) do
        TAreaMonitor(fAreas.Objects[i]).Free;
      inherited;
    end;

  function TServerMonitor.Update : integer;
    var
      i    : integer;
      Area : TAreaMonitor;
    begin
      result := 0;
      for i := 0 to pred(fAreas.Count) do
        begin
          Area := TAreaMonitor(fAreas.Objects[i]);
          result := result + Area.Update;
        end;
      SetCurrentValue(result);
    end;

  // TAreaMonitor

  constructor TAreaMonitor.Create(Owner : TServerMonitor; aName : string);
    var
      key   : string;
      List  : TStringList;
      i     : integer;
      World : TWorldMonitor;
    begin
      inherited Create(Owner.SampleCount);
      fName   := aName;
      fOwner  := Owner;
      fWorlds := TStringList.Create;
      key := Format('root/areas/%s/worlds', [fName]);
      if Owner.Proxy.RDOSetCurrentKey(key)
        then
          begin
            List := TStringList.Create;
            List.Text := Owner.Proxy.RDOGetKeyNames;
            for i := 0 to pred(List.Count) do
              begin
                World := TWorldMonitor.Create(Self, List[i]);
                fWorlds.AddObject(List[i], World);
              end;
          end;
    end;

  destructor TAreaMonitor.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fWorlds.Count) do
        TWorldMonitor(fWorlds.Objects[i]).Free;
      inherited;
    end;

  function TAreaMonitor.Update : integer;
    var
      i     : integer;
      World : TWorldMonitor;
    begin
      result := 0;
      for i := 0 to pred(fWorlds.Count) do
        begin
          World := TWorldMonitor(fWorlds.Objects[i]);
          result := result + World.Update;
        end;
      SetCurrentValue(result);
    end;


  // TWorldMonitor

  constructor TWorldMonitor.Create(Owner: TAreaMonitor; aName: string);
    begin
      inherited Create(Owner.SampleCount);
      fOwner := Owner;
      fName  := aName;
    end;

  destructor TWorldMonitor.Destroy;
    begin
      inherited;
    end;

  function TWorldMonitor.Update : integer;
    var
      Proxy : OleVariant;
      key   : string;
    begin
      try
        Proxy := fOwner.Owner.Proxy;
        key   := Format('root/areas/%s/worlds/%s/general', [fOwner.Name, fName]);
        if Proxy.RDOSetCurrentKey(key)
          then result := Proxy.RDOReadInteger('online')
          else result := GetCurrentValue;
      except
        result := GetCurrentValue;
      end;
      SetCurrentValue(result);
    end;

end.
