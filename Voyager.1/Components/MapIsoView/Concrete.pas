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
    TConcreteConfig = array [cUp..cDown, cLeft..cRight] of boolean;

  const
    cMaxConcreteConfigs = 16;

  const
    cFullConcrete    = 15;
    cSpecialConcrete = 16;

  const
    concreteNone = high(TConcrete);

  function GetConcreteId(const concreteconfig : TConcreteConfig) : TConcrete;
  function PlaceSpecialConcrete(i, j : integer) : boolean;
  function RotateConcreteId(id : integer; rotation : TRotation) : integer;

implementation

  const
    cConcreteConfigs : array [0..pred(cMaxConcreteConfigs)] of TConcreteConfig =
      (
        (
          ( false, false, false ), //0
          ( false, true,  false ),
          ( false, false, false )
        ),
        (
          ( false, false, false ), //1
          ( false, true,  false ),
          ( false, true,  false )
        ),
        (
          ( false, false, false ), //2
          ( false, true,  true  ),
          ( false, false, false )
        ),
        (
          ( false, false, false ), //3
          ( false, true,  true  ),
          ( false, true,  false )
        ),
        (
          ( false, false, false ), //4
          ( true,  true,  false ),
          ( false, false, false )
        ),
        (
          ( false, false, false ), //5
          ( true,  true,  false ),
          ( false, true,  false )
        ),
        (
          ( false, false, false ), //6
          ( true,  true,  true  ),
          ( false, false, false )
        ),
        (
          ( false, false, false ), //7
          ( true,  true,  true  ),
          ( false, true,  false )
        ),
        (
          ( false, true,  false ), //8
          ( false, true,  false ),
          ( false, false, false )
        ),
        (
          ( false, true,  false ), //9
          ( false, true,  false ),
          ( false, true,  false )
        ),
        (
          ( false, true,  false ), //10
          ( false, true,  true  ),
          ( false, false, false )
        ),
        (
          ( false, true,  false ), //11
          ( false, true,  true  ),
          ( false, true,  false )
        ),
        (
          ( false, true,  false ), //12
          ( true,  true,  false ),
          ( false, false, false )
        ),
        (
          ( false, true,  false ), //13
          ( true,  true,  false ),
          ( false, true,  false )
        ),
        (
          ( false, true,  false ), //14
          ( true,  true,  true  ),
          ( false, false, false )
        ),
        (
          ( false, true,  false ), //15
          ( true,  true,  true  ),
          ( false, true,  false )
        )
      );

  function GetConcreteId(const concreteconfig : TConcreteConfig) : TConcrete;
    var
      match : boolean;
      i     : TConcrete;
    begin
      match := false;
      i := low(cConcreteConfigs);
      while not match do
        begin
          match := (not (cConcreteConfigs[i][cUp, cCenter] xor concreteconfig[cUp, cCenter])) and
                   (not (cConcreteConfigs[i][cDown, cCenter] xor concreteconfig[cDown, cCenter])) and
                   (not (cConcreteConfigs[i][cCenter, cLeft] xor concreteconfig[cCenter, cLeft])) and
                   (not (cConcreteConfigs[i][cCenter, cRight] xor concreteconfig[cCenter, cRight]));
          if not match
            then inc(i);
        end;
      Result := i;
    end;

  function PlaceSpecialConcrete(i, j : integer) : boolean;
    begin
      Result := ((i mod 2) = 0) and ((j mod 2) = 0);
    end;

  function RotateConcreteId(id : integer; rotation : TRotation) : integer;
    const
      cRotatedConcreteIds : array [TRotation, 0..cMaxConcreteConfigs] of TConcrete =
       (
        (0, 1, 2,  3, 4,  5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
        (0, 2, 8, 10, 1,  3, 9, 7, 4, 6, 12, 11,  5, 13, 14, 15, 16),
        (0, 8, 4, 12, 2, 10, 6, 7, 1, 9,  5, 11,  3, 13, 14, 15, 16),
        (0, 4, 1,  5, 8, 12, 9, 7, 2, 6,  3, 11, 10, 13, 14, 15, 16)
       ); // The following ids are not rotated: 7, 11, 13, 14
    begin
      Result := cRotatedConcreteIds[rotation, id];
    end;

end.
