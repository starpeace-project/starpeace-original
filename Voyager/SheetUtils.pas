unit SheetUtils;

interface

  uses
    Classes, comctrls;

  const
    NA = 'n/a';

  function GetProperties(Proxy : OleVariant; Names, Results : TStringList) : boolean;
  function GetPropertyArray(Proxy : OleVariant; Names : array of string; Results : TStringList) : boolean;

  procedure ClearListView(List : TListView);
  function  AddItem(List : TListView; Cols : array of string) : TListItem;

  function CreatePropNameList(Names : array of string) : string;
  function GetSubObjProperties(Proxy : OleVariant; subIdx : integer; Names : array of string; Results : TStringList) : boolean;

implementation

  uses
    SysUtils,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    CompStringsParser;

  procedure SetPropList(Names, Values : TStringList; resStr : string);
    var
      p     : integer;
      i     : integer;
      name  : string;
      value : string;
    begin
      if resStr <> ''
        then 
          begin
            p := 1;
            for i := 0 to pred(Names.Count) do
              begin
                if resStr[p] <> #9
                  then
                    begin
                      name  := Names[i];
                      value := GetNextStringUpTo(resStr, p, #9);
                      if name <> ''
                        then Values.Values[name] := value;
                    end;
                inc(p);
              end;
          end;
    end;

  function GetProperties(Proxy : OleVariant; Names, Results : TStringList) : boolean;
    var
      WStr   : WideString;
      index  : integer;
      resStr : string;
    begin
      if not VarIsEmpty(Proxy)
        then
          begin
            WStr := '';
            for index := 0 to pred(Names.Count) do
              WStr := WStr + Names[index] + #9;
            resStr := Proxy.GetPropertyList(WStr);
            SetPropList(Names, Results, resStr);
            result := true;
          end
        else result := false;
    end;

  function GetPropertyArray(Proxy : OleVariant; Names : array of string; Results : TStringList) : boolean;
    var
      WStr   : WideString;
      index  : integer;
      resStr : string;
      p      : integer;
    begin
      if not VarIsEmpty(Proxy)
        then
          begin
            WStr := '';
            for index := low(Names) to high(Names) do
              WStr := WStr + Names[index] + #9;
            resStr := Proxy.GetPropertyList(WStr);
            if resStr <> ''
              then
                begin
                  p := 1;
                  for index := low(Names) to high(Names) do
                    begin
                      if resStr[p] <> #9
                        then Results.Values[Names[index]] := GetNextStringUpTo(resStr, p, #9);
                      inc(p);
                    end;
                end;
            result := true;
          end
        else result := false;
    end;

  procedure ClearListView(List : TListView);
    begin
      if List <> nil
        then
          begin
            List.Items.BeginUpdate;
            try
              List.Items.Clear;
            finally
              List.Items.EndUpdate;
            end;
          end;
    end;

  function AddItem(List : TListView; Cols : array of string) : TListItem;
    var
      i : integer;
    begin
      try
        result := List.Items.Add;
        result.Caption := Cols[0];
        for i := succ(low(Cols)) to high(Cols) do
          result.SubItems.Add(Cols[i]);
      except
        result := nil;
      end;
    end;

  function CreatePropNameList(Names : array of string) : string;
    var
      i : integer;
    begin
      result := '';
      for i := low(Names) to high(Names) do
        result := result + Names[i] + #9;
    end;

  function GetSubObjProperties(Proxy : OleVariant; subIdx : integer; Names : array of string; Results : TStringList) : boolean;
    var
      WStr   : WideString;
      resStr : WideString;
      index  : integer;
      p      : integer;
      name   : string;
    begin
      if not VarIsEmpty(Proxy)
        then
          begin
            WStr := '';
            for index := low(Names) to high(Names) do
              WStr := WStr + Names[index] + #9;
            resStr := Proxy.GetSubObjectProps(subIdx, WStr);
            if resStr <> ''
              then
                begin
                  p := 1;
                  for index := low(Names) to high(Names) do
                    begin
                      name := Names[index];
                      try
                        if (resStr[p] <> #9) and (name <> '')
                          then Results.Values[name] := GetNextStringUpTo(resStr, p, #9);
                      finally
                        name := name + '.'; //>?>
                      end;
                      inc(p);
                    end;
                end;
            result := true;
          end
        else result := false;
    end;

end.
