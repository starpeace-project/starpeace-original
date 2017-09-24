unit GateInfo;

interface

  uses
    Classes, SysUtils;

  type
    TGateInfo =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fLoaded    : boolean;
          fValues    : TStringList;
        public
          function  GetIntValue(ppName : string) : integer;
          procedure SetIntValue(ppName : string; value : integer);
          function  GetFloatValue(ppName : string) : double;
          procedure SetFloatValue(ppName : string; value : double);
          function  GetStrValue(ppName : string) : string;
          function  GetStrArray(ppName : string; index : integer) : string;
          procedure SetStrArray(ppName : string; index : integer; Value : string);
          function  GetMLSStrArray(ppName, langid : string; index : integer) : string;
          procedure SetMLSStrArray(ppName, langid : string; index : integer; value : string);
          procedure Load(source : string);
          procedure AddProp(ppName, ppValue : string);
        public
          property Loaded : boolean read fLoaded write fLoaded;
          property Values : TStringList read fValues;
          property IntValue[ppName : string] : integer read GetIntValue write SetIntValue;
          property FloatValue[ppName : string] : double read GetFloatValue write SetFloatValue;
          property StrValue[ppName : string] : string  read GetStrValue;
          property StrArray[ppName : string; index : integer] : string  read GetStrArray write SetStrArray;
          property MLSStrArray[ppName, langid : string; index : integer] : string  read GetMLSStrArray write SetMLSStrArray;
      end;


implementation

  // TGateInfo

  constructor TGateInfo.Create;
    begin
      inherited;
      fValues := TStringList.Create;
    end;

  destructor TGateInfo.Destroy;
    begin
      fValues.Free;
      inherited;
    end;

  function TGateInfo.GetIntValue(ppName : string) : integer;
    var
      aux : string;
    begin
      aux := fValues.Values[ppName];
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            result := 0;
          end
        else result := 0;
    end;

  function TGateInfo.GetFloatValue(ppName : string) : double;
    var
      aux : string;
    begin
      aux := fValues.Values[ppName];
      if aux <> ''
        then
          try
            result := StrToFloat(aux);
          except
            result := 0;
          end
        else result := 0;
    end;

  procedure TGateInfo.SetIntValue(ppName : string; value : integer);
    begin
      fValues.Values[ppName] := IntToStr(value);
    end;

  procedure TGateInfo.SetFloatValue(ppName : string; value : double);
    begin
      fValues.Values[ppName] := FloatToStr(value);
    end;

  function TGateInfo.GetStrValue(ppName : string) : string;
    begin
      result := fValues.Values[ppName];
    end;

  function TGateInfo.GetStrArray(ppName : string; index : integer) : string;
    begin
      result := fValues.Values[ppName + IntToStr(index)];
    end;

  procedure TGateInfo.SetStrArray(ppName : string; index : integer; Value : string);
    begin
      fValues.Values[ppName + IntToStr(index)] := Value;
    end;

  function TGateInfo.GetMLSStrArray(ppName, langid : string; index : integer) : string;
    begin
      result := fValues.Values[ppName + IntToStr(index) + '.' + langid];
    end;

  procedure TGateInfo.SetMLSStrArray(ppName, langid : string; index : integer; value : string);
    begin
      fValues.Values[ppName + IntToStr(index) + '.' + langid] := Value;
    end;

  procedure TGateInfo.Load(source : string);
    begin
      fValues.Text := source;
    end;

  procedure TGateInfo.AddProp(ppName, ppValue : string);
    begin
      fValues.Values[ppName] := ppValue;
    end;

end.
