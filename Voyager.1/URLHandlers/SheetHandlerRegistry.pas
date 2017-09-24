unit SheetHandlerRegistry;

interface

  uses
    ObjectInspectorInterfaces;

  procedure RegisterSheetHandler(aName : string; aFunct : TSheetHandlerCreatorFunction);
  function  GetSheetHandler(aName : string) : IPropertySheetHandler;

implementation

  uses
    SysUtils, Classes;

  var
    Handlers : TStringList = nil;

  type
    TRegHandler =
      class
        fFunct : TSheetHandlerCreatorFunction;
      end;

  procedure RegisterSheetHandler(aName : string; aFunct : TSheetHandlerCreatorFunction);
    var
      obj : TRegHandler;
    begin
      obj := TRegHandler.Create;
      obj.fFunct := aFunct;
      Handlers.AddObject(uppercase(aName), obj);
    end;

  function GetSheetHandler(aName : string) : IPropertySheetHandler;
    var
      index : integer;
      obj   : TRegHandler;
    begin
      index := Handlers.IndexOf(aName);
      if index <> -1
        then
          begin
            obj := TRegHandler(Handlers.Objects[index]);
            if Assigned(obj.fFunct)
              then result := obj.fFunct
              else result := nil;
          end
        else result := nil;
    end;

  procedure ReleaseHandlers;
    var
      i : integer;
    begin
      for i := 0 to pred(Handlers.Count) do
        Handlers.Objects[i].Free;
      Handlers.Free;
    end;

initialization

    Handlers := TStringList.Create;
    Handlers.Sorted := true;
    Handlers.Duplicates := dupIgnore;

finalization

    ReleaseHandlers;

end.
