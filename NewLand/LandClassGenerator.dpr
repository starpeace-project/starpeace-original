{$APPTYPE CONSOLE }

program LandClassGenerator;

uses
  IniFiles,
  SysUtils,
  Graphics,
  Land in 'Land.pas';

type
  TRGB =
    record
      x, b, g, r : byte;
    end;

procedure GenerateClasses;

  function LandClassName( LandClassId : TLandVisualClassId ) : string;
    const
      LandClassNames :
        array[TLandClass] of string =
          ( 'Grass',
            'MidGrass',
            'DryGround',
            'Water' );
    begin
      result := LandClassNames[LandClassOf(LandClassId)];
    end;

  function LandColor( LandClassId : TLandVisualClassId ) : TColor;
    var
      CenterClass : TLandClass;
      BorderClass : TLandClass;
      NewRGB      : TRGB;
    const
      LandClassColors :
        array[TLandClass] of TColor =
          ( $0000AA00,
            $0044AA88,
            $00228888,
            $00AA7700 );
    begin
      CenterClass := LandClassOf( LandClassId );
      if LandTypeOf( LandClassId ) in [ldtCenter, ldtSpecial]
        then
          begin
            result := LandClassColors[CenterClass];
            TRGB(result).r := TRGB(result).r + 10*LandVarOf( LandClassId );
            TRGB(result).g := TRGB(result).g + 10*LandVarOf( LandClassId );
            TRGB(result).b := TRGB(result).b + 10*LandVarOf( LandClassId );
          end
        else
          begin
            BorderClass := LandBorders[CenterClass];
            NewRGB.r    := (TRGB(LandClassColors[CenterClass]).r + TRGB(LandClassColors[BorderClass]).r) div 2;
            NewRGB.g    := (TRGB(LandClassColors[CenterClass]).g + TRGB(LandClassColors[BorderClass]).g) div 2;
            NewRGB.b    := (TRGB(LandClassColors[CenterClass]).b + TRGB(LandClassColors[BorderClass]).b) div 2;
            result      := TColor(NewRGB);
          end;
    end;

  function LandTypeName( LandClassId : TLandVisualClassId ) : string;
    const
      LandTypeNames :
        array[TLandType] of string =
          ( 'Center',
            'N',
            'E',
            'S',
            'W',
            'NEo',
            'SEo',
            'SWo',
            'NWo',
            'NEi',
            'SEi',
            'SWi',
            'NWi',
            'Special' );
    begin
      result := LandTypeNames[LandTypeOf(LandClassId)];
    end;

  function LandVarIndex( LandClassId : TLandVisualClassId ) : string;
    begin
      result := IntToStr(LandVarOf(LandClassId) + 1);
    end;

  function Padd( numstr : string ) : string;
    begin
      result := numstr;
      while length(result) < 3 do
        result := '0' + result;
    end;

  var
    BasePath    : string;
    LandClassId : TLandVisualClassId;
    IniFile     : TIniFile;
    filename    : string;
    LandURL     : string;
  begin
    BasePath := ExtractFilePath( paramstr(0) ) + 'LandClasses\';
    for LandClassId := 0 to 255 do
      if (LandTypeOf(LandClassId) = ldtCenter) or
         (LandTypeOf(LandClassId) = ldtSpecial) or
         (LandVarOf(LandClassId) = 0)
        then
          begin
            if LandTypeOf(LandClassId) <> ldtSpecial
              then
                begin
                  filename :=
                    BasePath +
                    'land.' +
                    Padd(IntToStr( LandClassId )) + '.' +
                    LandClassName( LandClassId ) + '.' +
                    LandTypeName( LandClassId ) + '.' +
                    LandVarIndex( LandClassId ) + '.ini';
                  LandURL :=
                    LandClassName( LandClassId ) + '' +
                    LandTypeName( LandClassId ) + '' +
                    LandVarIndex( LandClassId ) + '.bmp';
                end
              else
                begin
                  filename :=
                    BasePath +
                    'special.' +
                    Padd(IntToStr( LandClassId )) + '.' +
                    LandClassName( LandClassId ) + '.ini';
                  LandURL :=
                    LandClassName( LandClassId ) + '' +
                    LandTypeName( LandClassId ) + '' +
                    LandVarIndex( LandClassId ) + '.bmp';
                end;
            writeln( 'Generating ', filename, '...' );
            IniFile := TIniFile.Create( filename );
            IniFile.WriteInteger( 'General', 'Id', LandClassId );
            IniFile.WriteInteger( 'General', 'MapColor', LandColor( LandClassId ));
            IniFile.WriteString( 'Images', '64x32', LandURL );
            IniFile.Free;
          end;
    writeln( 'Ok.' );
    write( 'Press ENTER to continue...' );
    readln;
  end;

begin
  GenerateClasses;
end.
