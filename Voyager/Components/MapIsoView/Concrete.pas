unit Concrete;

interface

  uses
    GameTypes, MapTypes;

  const
    cCenter = 0;
    cUp     = -1;
    cDown   = 1;
    cLeft   = -1;
    cRight  = 1;

  type
    ConcreteAround = (caUp, caDown, caLeft, caRight);

  type
    TConcreteConfig = array [cUp..cDown, cLeft..cRight] of boolean;
    TConcreteCfg    = array [0..7] of boolean;

  const
    cMaxConcreteConfigs = 13;

  const
    cFullConcrete    = 12;
    cSpecialConcrete = 15;
    cRoadConcrete    = $10;

  const
    cPlatformFlag = $80;
    cPlatformMask = $7F;

  const
    //cPlatformShift = 18;
    cPlatformShift = 12;

  const
    concreteNone = high(TConcrete);

  function GetConcreteId(const concreteconfig : TConcreteCfg) : TConcrete;
  function GetWaterConcreteId(const concreteconfig : TConcreteCfg): TConcrete;
  function PlaceSpecialConcrete(i, j : integer) : boolean;
  function RotateConcreteId(id : integer; rotation : TRotation) : integer;

implementation

  const
    caConcreteConfigs : array [0..pred(cMaxConcreteConfigs), 0..7] of boolean =
      ( //
        ( {}false, false, {}false,  true,  true,      true, true, true ), // 1
        ( false, true, true,    true,  true,      true, true, true), // 2
        ( false, false, false,  true,  false,     true, true, false ), // 3
        ( true, true, false,    true,  false,     false, false, false), // 4
        ( true, true, false,    true,  true,      true, true, true    ), // 5
        ( true, true, {}false,    true,  false,     true, true, {}false   ), // 6
        ( true, true, true,     true,  true,      false, false, false), //  7
        ( false, true, true,    false, true,      false, true, true    ), // 8
        ( true, true, true,    true,  true,       true, true, false  ), // 9
        ( false, true, true,   false, true,      false, false, false ), // 10
        ( false, false, false,    false, true,      false, true, true), // 11
        ( true, true, true,     true,  true,       false, true, true   ), // 12
        ( true, true, true,     true,  true,       true, true, true    )  // 13
      );                                                                  // 14 Not Fount

    cWaterConcreteConfigs : array [0..8, 0..3] of  boolean =
      (
       (true,   true,   true,   true),  // 0
       (false,   true,   false,  true), // 1
       (true,  true,   false,   false),  // 2
       (true,   true,   false,   true),  // 3
       (true,   true,   true,   false),  // 4
       (false,   false,   true,   true),  // 5
       (false,   true,   true,   true),  // 6
       (true,   false,   true,   true),  // 7
       (true,   false,   true,   false)  // 8
      );

  function GetWaterConcreteId(const concreteconfig : TConcreteCfg): TConcrete;
    var
      match : boolean;
    begin
      match := false;
      result := 0;
      while (not match) and (result<9) do
        begin
          match := (cWaterConcreteConfigs[result][0] = concreteconfig[1]) and
                   (cWaterConcreteConfigs[result][1] = concreteconfig[3]) and
                   (cWaterConcreteConfigs[result][2] = concreteconfig[4]) and
                   (cWaterConcreteConfigs[result][3] = concreteconfig[6]);
          inc(result);
        end;
      if match
        then
          begin
            dec(result);
            result := result or cPlatformFlag;
          end
        else result := concreteNone;
    end;

  function GetConcreteId(const concreteconfig : TConcreteCfg) : TConcrete;
 {   var
      match : boolean;
      i : integer;}
    begin
{      match  := false;
      result := 0;
      while (not match) and (result<cMaxConcreteConfigs) do
        begin
          match := ((caConcreteConfigs[result][0] = concreteconfig[0]) or (result=0) or (result=7) or (result=9) or (result=2)) and
                    (caConcreteConfigs[result][1] = concreteconfig[1]) and
                   ((caConcreteConfigs[result][2] = concreteconfig[2]) or (result=0) or (result=5) or (result=3) or (result=10)) and
                    (caConcreteConfigs[result][3] = concreteconfig[3]) and
                    (caConcreteConfigs[result][4] = concreteconfig[4]) and
                   ((caConcreteConfigs[result][5] = concreteconfig[5]) or (result=6) or (result=7) or (result=3) or (result=10)) and
                    (caConcreteConfigs[result][6] = concreteconfig[6]) and
                   ((caConcreteConfigs[result][7] = concreteconfig[7]) or (result=5) or (result=6) or (result=9) or (result=2));
          inc(result);
        end;
      if match
        then dec(Result)
        else result := 12;} // concreteNone;
      result := 12;
      if concreteconfig[1]
        then
          begin // 1, 3, 4, 5, 6, 7, 8, 9, 11, 12
            if concreteconfig[3]
              then
                begin  // 1, 3, 4, 5, 6, 8, 11, 12
                   if concreteconfig[4]
                     then
                       begin // 1, 4, 6, 8, 11, 12
                         if concreteconfig[6]
                           then
                             begin // 1, 4, 8, 12, 11
                               if concreteconfig[0]
                                 then
                                   begin // 4, 8, 12, 11
                                     if concreteconfig[2]
                                       then
                                         begin // 8, 11, 12
                                           if concreteconfig[7]
                                             then
                                               begin // 11, 2
                                                 if concreteconfig[5]
                                                   then result := 12
                                                   else result := 11;
                                               end
                                             else result := 8;
                                         end
                                       else result := 4;
                                   end
                                 else result := 1;
                             end
                           else
                             if concreteconfig[0]
                               then
                                 if concreteconfig[2]
                                   then result := 6
                                   else result := 3
                               else result := 9;
                       end
                     else
                       begin // 3, 5
                         if concreteconfig[5]
                           then
                             begin
                               if concreteconfig[0]
                                 then
                                   if concreteconfig[6]
                                     then result := 5
                                     else result := 3
                                 else result := 2;
                             end
                           else result := 3;
                       end;
                end
              else
                begin // 7, 9 10
                  if concreteconfig[7]
                    then
                      begin
                        if concreteconfig[2]
                          then
                            if concreteconfig[6]
                              then result := 7
                              else result := 9
                          else result := 10;
                      end
                    else result := 9;
                end;
          end
        else
          begin // 0, 2. 10
            if concreteconfig[3]
              then
                if concreteconfig[4]
                  then
                    begin
                      if concreteconfig[5]
                        then
                          if concreteconfig[7]
                            then result := 0
                            else result := 2
                          else result := 10;

                    end
                  else result := 2
              else result := 10;
          end;
    end;

  function PlaceSpecialConcrete(i, j : integer) : boolean;
    begin
      Result := ((i mod 2) = 0) and ((j mod 2) = 0);
    end;

  function RotateConcreteId(id : integer; rotation : TRotation) : integer;
    const
      cRotatedConcreteIds : array [TRotation, 0..pred(cMaxConcreteConfigs)] of TConcrete =
        (
          (0,   1,      2,      3,      4,      5,      6,      7,      8,      9,      10,     11,    12), // drNorth
          (5,   4,      3,      9,      8,      6,      7,      0,      11,     10,      2,      1,     12), // drEast
          (6,   8,      9,      10,      11,    7,      0,      5,      1,       2,       3,      4,     12), // drSouth
          (7,   11,     10,     2,      1,      0,      5,      6,      4,       3,       9,      8,     12)// drWest
        ); 
      cWaterRotatedConcreteIds : array [TRotation, 0..8] of TConcrete =
        (
          ($80, $81, $82,  $83, $84,  $85, $86,  $87, $88), // drNorth
          ($80, $82, $88,  $84, $87,  $81, $83,  $86, $85), // drEast
          ($80, $88, $85,  $87, $86,  $82, $84,  $83, $81), // drSouth
          ($80, $85, $81,  $86, $83,  $88, $87,  $84, $82)  // drWest
        );
    begin
      if (id and cPlatformFlag>0)
        then Result := cWaterRotatedConcreteIds[rotation, id and cPlatformMask]
        else
          begin
            if id<>cSpecialConcrete
              then
                begin
                  Result := cRotatedConcreteIds[rotation, id and not cRoadConcrete];
                  if (id and cRoadConcrete)<>0
                    then Result := Result or cRoadConcrete;
                end
              else result := cSpecialConcrete;
          end;
    end;

end.

