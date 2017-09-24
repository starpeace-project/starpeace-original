unit tabNamesMLS;

interface

  function TranslateName(name : string) : string;

implementation

  uses
    Classes, SysUtils, Literals;

  var
    Tabs : TStringList = nil;

  function TranslateName(name : string) : string;
    var
      aux : string;
    begin
      aux := UpperCase(Tabs.Values[name]);
      if aux <> ''
        then result := GetLiteral(aux)
        else result := name;
    end;

  procedure InitTabNames;
    begin
      Tabs := TStringList.Create;
      // Do not translate none of this strings, they are already in pepe's table
      Tabs.Add('CLIENTS=Literal461');
      Tabs.Add('COMMERCE=Literal462');
      Tabs.Add('GENERAL=Literal463');
      Tabs.Add('HISTORY=Literal464');
      Tabs.Add('JOBS=Literal465');
      Tabs.Add('LOANS=Literal466');
      Tabs.Add('MANAGEMENT=Literal467');
      Tabs.Add('MINISTERIES=Literal468');
      Tabs.Add('PRODUCTS=Literal469');
      Tabs.Add('PUBLICITY=Literal470');
      Tabs.Add('RESEARCHES=Literal471');
      Tabs.Add('RESIDENTIALS=Literal472');
      Tabs.Add('SERVICES=Literal473');
      Tabs.Add('SUPPLIES=Literal474');
      Tabs.Add('TAXES=Literal475');
      Tabs.Add('MAUSOLEUM=Literal475');
      Tabs.Add('TOWNS=Literal478');
      Tabs.Add('FILMS=Literal479');
      Tabs.Add('ANTENNAS=Literal480');
      Tabs.Add('VOTES=Literal481');
      Tabs.Add('WARES=Literal492');
    end;

initialization

  InitTabNames;

finalization

  Tabs.Free;
  Tabs := nil;

end.
