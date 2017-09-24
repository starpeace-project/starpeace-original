unit Filters;

interface

  uses
    Classes, ColorTableMgr, GDI;

  type
    TPaletteFilter = (pfReddenPalette, pfTintPalette, pfColorToGray, pfRedFilter, pfGreenFilter, pfDarkenPalette);

  function FilterPalette(OrgPalette : TPaletteInfo; Filter : TPaletteFilter; Data : array of const) : TPaletteInfo;
  function CheckParams(Filter : TPaletteFilter; OldParams, NewParams : array of const) : boolean;

implementation

  uses
    Windows;

  type
    TFilterRoutine      = function (Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    TCheckParamsRoutine = function (OldParams, NewParams : array of const) : boolean;

  type
    TFilterInfo =
      record
        FilterRoutine      : TFilterRoutine;
        CheckParamsRoutine : TCheckParamsRoutine;
      end;

  function ReddenPalette(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      ReddedPalette : PRGBPalette;
      i             : integer;
    begin
      getmem(ReddedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(ReddedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          ReddedPalette[i].rgbRed := $FF;
          ReddedPalette[i].rgbGreen := Palette.RGBPalette[i].rgbGreen;
          ReddedPalette[i].rgbBlue := Palette.RGBPalette[i].rgbBlue;
        end;
      Result := ReddedPalette;
    end;

  function CheckReddenPaletteParams(OldParams, NewParams : array of const) : boolean;
    begin
      Result := true;
    end;

  function TintPalette(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      TintedPalette    : PRGBPalette;
      i                : integer;
      red, green, blue : byte;
      tintweight       : extended;
    begin
      red := Data[0].VInteger;
      green := Data[1].VInteger;
      blue := Data[2].VInteger;
      tintweight := Data[3].VExtended^;
      getmem(TintedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(TintedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          TintedPalette[i].rgbRed := round(Palette.RGBPalette[i].rgbRed*(1 - tintweight) + red*tintweight);
          TintedPalette[i].rgbGreen := round(Palette.RGBPalette[i].rgbGreen*(1 - tintweight) + green*tintweight);
          TintedPalette[i].rgbBlue := round(Palette.RGBPalette[i].rgbBlue*(1 - tintweight) + blue*tintweight);
        end;
      Result := TintedPalette;
    end;

  function CheckTintPaletteParams(OldParams, NewParams : array of const) : boolean;
    var
      oldred, oldgreen, oldblue : byte;
      oldtintweight             : extended;
      newred, newgreen, newblue : byte;
      newtintweight             : extended;
    begin
      if high(OldParams) = high(NewParams)
        then
          begin
            oldred := OldParams[0].VInteger;
            oldgreen := OldParams[1].VInteger;
            oldblue := OldParams[2].VInteger;
            oldtintweight := OldParams[3].VExtended^;
            newred := NewParams[0].VInteger;
            newgreen := NewParams[1].VInteger;
            newblue := NewParams[2].VInteger;
            newtintweight := NewParams[3].VExtended^;
            Result := (oldred = newred) and (oldgreen = newgreen) and (oldblue = newblue) and (oldtintweight = newtintweight);
          end
        else Result := false;
    end;

  function ColorToGray(Palette : TPaletteInfo; Data : array of const) : PRGBPalette;
    var
      GrayPalette : PRGBPalette;
      i           : integer;
      gray        : integer;
    begin
      getmem(GrayPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(GrayPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          gray := (30*Palette.RGBPalette[i].rgbRed + 59*Palette.RGBPalette[i].rgbGreen + 11*Palette.RGBPalette[i].rgbBlue) div 100;
          GrayPalette[i].rgbRed := gray;
          GrayPalette[i].rgbGreen := gray;
          GrayPalette[i].rgbBlue := gray;
        end;
      Result := GrayPalette;
    end;

  function CheckColorToGrayParams(OldParams, NewParams : array of const) : boolean;
    begin
      Result := true;
    end;

  function RedFilter(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      RedPalette : PRGBPalette;
      i          : integer;
    begin
      getmem(RedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(RedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          RedPalette[i].rgbRed := (30*Palette.RGBPalette[i].rgbRed + 59*Palette.RGBPalette[i].rgbGreen + 11*Palette.RGBPalette[i].rgbBlue) div 100;
          RedPalette[i].rgbGreen := 0;
          RedPalette[i].rgbBlue := 0;
        end;
      Result := RedPalette;
    end;

  function CheckRedFilterParams(OldParams, NewParams : array of const) : boolean;
    begin
      Result := true;
    end;

  function GreenFilter(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      GreenPalette : PRGBPalette;
      i            : integer;
    begin
      getmem(GreenPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(GreenPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          GreenPalette[i].rgbRed := 0;
          GreenPalette[i].rgbGreen := (30*Palette.RGBPalette[i].rgbRed + 59*Palette.RGBPalette[i].rgbGreen + 11*Palette.RGBPalette[i].rgbBlue) div 100;
          GreenPalette[i].rgbBlue := 0;
        end;
      Result := GreenPalette;
    end;

  function CheckGreenFilterParams(OldParams, NewParams : array of const) : boolean;
    begin
      Result := true;
    end;

  function DarkenPalette(Palette : TPaletteInfo; Data : array of const) : PRgbPalette;
    var
      DarkenedPalette : PRGBPalette;
      i               : integer;
    begin
      getmem(DarkenedPalette, Palette.Count*sizeof(TRGBQuad));
      fillchar(DarkenedPalette^, Palette.Count*sizeof(TRGBQuad), 0);
      for i := 0 to pred(Palette.Count) do
        begin
          DarkenedPalette[i].rgbRed := Palette.RGBPalette[i].rgbRed div 2;
          DarkenedPalette[i].rgbGreen := Palette.RGBPalette[i].rgbGreen div 2;
          DarkenedPalette[i].rgbBlue := Palette.RGBPalette[i].rgbBlue div 2;
        end;
      Result := DarkenedPalette;
    end;

  function CheckDarkenPaletteParams(OldParams, NewParams : array of const) : boolean;
    begin
      Result := true;
    end;

  const
    AvailableFilters : array[TPaletteFilter] of TFilterInfo =
      (
        (FilterRoutine : ReddenPalette; CheckParamsRoutine : CheckReddenPaletteParams),
        (FilterRoutine : TintPalette; CheckParamsRoutine : CheckTintPaletteParams),
        (FilterRoutine : ColorToGray; CheckParamsRoutine : CheckColorToGrayParams),
        (FilterRoutine : RedFilter; CheckParamsRoutine : CheckRedFilterParams),
        (FilterRoutine : GreenFilter; CheckParamsRoutine : CheckGreenFilterParams),
        (FilterRoutine : DarkenPalette; CheckParamsRoutine : CheckDarkenPaletteParams)
      );

  function FilterPalette(OrgPalette : TPaletteInfo; Filter : TPaletteFilter; Data : array of const) : TPaletteInfo;
    begin
      Result := OrgPalette.FilteredPalette(AvailableFilters[Filter].FilterRoutine, Data);
    end;

  function CheckParams(Filter : TPaletteFilter; OldParams, NewParams : array of const) : boolean;
    begin
      Result := AvailableFilters[Filter].CheckParamsRoutine(OldParams, NewParams);
    end;

end.
