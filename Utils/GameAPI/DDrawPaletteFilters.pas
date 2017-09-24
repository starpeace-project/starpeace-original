unit DDrawPaletteFilters;

interface

  uses
    Classes, DirectDraw;

  type
    TPaletteFilter = (pfReddenPalette, pfTintPalette, pfColorToGray, pfRedFilter, pfGreenFilter);

  function FilterPalette(const OrgPalette : IDirectDrawPalette; Filter : TPaletteFilter; Data : array of const) : IDirectDrawPalette;

implementation

  uses
    Windows;

  type
    TFilterRoutine = function (const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;

  type
    TFilterInfo =
      record
        FilterRoutine : TFilterRoutine;
      end;

  {

  function ReddenPalette(const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;
    var
      ReddedPalette : IDirectDrawPalette;
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

  function TintPalette(const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;
    var
      TintedPalette    : IDirectDrawPalette;
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

  function ColorToGray(const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;
    var
      GrayPalette : IDirectDrawPalette;
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

  function RedFilter(const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;
    var
      RedPalette : IDirectDrawPalette;
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

  function GreenFilter(const Palette : IDirectDrawPalette; Data : array of const) : IDirectDrawPalette;
    var
      GreenPalette : IDirectDrawPalette;
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

  const
    AvailableFilters : array[TPaletteFilter] of TFilterInfo =
      (
        (FilterRoutine : ReddenPalette),
        (FilterRoutine : TintPalette),
        (FilterRoutine : ColorToGray),
        (FilterRoutine : RedFilter),
        (FilterRoutine : GreenFilter)
      );
  }

  function FilterPalette(const OrgPalette : IDirectDrawPalette; Filter : TPaletteFilter; Data : array of const) : IDirectDrawPalette;
    begin
      //Result := OrgPalette.FilteredPalette(AvailableFilters[Filter].FilterRoutine, Data);
      Result := nil;
    end;

end.
