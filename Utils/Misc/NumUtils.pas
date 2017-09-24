unit NumUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    SysUtils;

  // --- MinMax ---

  function MinCardinal( i, j : cardinal ) : cardinal; // Returns the smallest
  function MaxCardinal( i, j : cardinal ) : cardinal; // Returns the largest

  function Min ( i, j : integer ) : integer;
  function Max ( i, j : integer ) : integer;
  function Min3( i, j, k : integer ) : integer;
  function Max3( i, j, k : integer ) : integer;

  function mcm( Num1, Num2 : integer ) : integer;
  function mcd( Num1, Num2 : integer ) : integer;

  // --- Integer sign ---

  function Sign( Num : integer ) : integer;

  function MulDiv( a, b, c : integer ) : integer;     // = (a * b) div c           (round down, signed)
  function MulDivRD( a, b, c : cardinal ) : cardinal; // = (a * b) div c           (round down, unsigned)
  function MulDivRN( a, b, c : cardinal ) : cardinal; // = (a * b + c div 2) div c (round nearest, unsigned)
  function MulDivRU( a, b, c : cardinal ) : cardinal; // = (a * b + c - 1) div c   (round up, unsigned)

  // Misc

  procedure Swap2( var Num1, Num2 );                                   // Exchanges 2-byte data
  procedure Swap4( var Num1, Num2 );                                   // Exchanges 4-byte data
  procedure Swap8( var Num1, Num2 );                                   // Exchanges 8-byte data
  procedure Swap10( var Num1, Num2 );                                  // Exchanges 10-byte data

  function NearestMult( Num : integer; Step : integer ) : integer;     // Nearest (to Num) multiple of Step

  function InvertInt( Num : integer ) : integer;                       // Converts $12345678 to $78563412

  // These functions are rather specific:

  function LeftShiftCount( Num : cardinal ) : cardinal;                // Find how many bits has Num set
                                                                       // LeftShiftCount( $f8 ) = 5, 11111000
  function RightShiftCount( Num : cardinal ) : cardinal;               // Find how many places was Num shifted left
                                                                       // RightShiftCount( $1000 ) = 12, 1 shl 12 = $1000
  // --- BCD ---

  function Bcd2Dec( Num : cardinal ) : cardinal;                       // BCD2Dec( $1234 ) = 1234

  const
    Square : array[-255..255] of word =
      (
        65025, 64516, 64009, 63504, 63001, 62500, 62001, 61504, 61009, 60516, 60025, 59536, 59049, 58564, 58081, 57600,
        57121, 56644, 56169, 55696, 55225, 54756, 54289, 53824, 53361, 52900, 52441, 51984, 51529, 51076, 50625, 50176,
        49729, 49284, 48841, 48400, 47961, 47524, 47089, 46656, 46225, 45796, 45369, 44944, 44521, 44100, 43681, 43264,
        42849, 42436, 42025, 41616, 41209, 40804, 40401, 40000, 39601, 39204, 38809, 38416, 38025, 37636, 37249, 36864,
        36481, 36100, 35721, 35344, 34969, 34596, 34225, 33856, 33489, 33124, 32761, 32400, 32041, 31684, 31329, 30976,
        30625, 30276, 29929, 29584, 29241, 28900, 28561, 28224, 27889, 27556, 27225, 26896, 26569, 26244, 25921, 25600,
        25281, 24964, 24649, 24336, 24025, 23716, 23409, 23104, 22801, 22500, 22201, 21904, 21609, 21316, 21025, 20736,
        20449, 20164, 19881, 19600, 19321, 19044, 18769, 18496, 18225, 17956, 17689, 17424, 17161, 16900, 16641, 16384,
        16129, 15876, 15625, 15376, 15129, 14884, 14641, 14400, 14161, 13924, 13689, 13456, 13225, 12996, 12769, 12544,
        12321, 12100, 11881, 11664, 11449, 11236, 11025, 10816, 10609, 10404, 10201, 10000,  9801,  9604,  9409,  9216,
         9025,  8836,  8649,  8464,  8281,  8100,  7921,  7744,  7569,  7396,  7225,  7056,  6889,  6724,  6561,  6400,
         6241,  6084,  5929,  5776,  5625,  5476,  5329,  5184,  5041,  4900,  4761,  4624,  4489,  4356,  4225,  4096,
         3969,  3844,  3721,  3600,  3481,  3364,  3249,  3136,  3025,  2916,  2809,  2704,  2601,  2500,  2401,  2304,
         2209,  2116,  2025,  1936,  1849,  1764,  1681,  1600,  1521,  1444,  1369,  1296,  1225,  1156,  1089,  1024,
          961,   900,   841,   784,   729,   676,   625,   576,   529,   484,   441,   400,   361,   324,   289,   256,
          225,   196,   169,   144,   121,   100,    81,    64,    49,    36,    25,    16,     9,     4,     1,    
            0,     1,     4,     9,    16,    25,    36,    49,    64,    81,   100,   121,   144,   169,   196,   225,
          256,   289,   324,   361,   400,   441,   484,   529,   576,   625,   676,   729,   784,   841,   900,   961, 
         1024,  1089,  1156,  1225,  1296,  1369,  1444,  1521,  1600,  1681,  1764,  1849,  1936,  2025,  2116,  2209, 
         2304,  2401,  2500,  2601,  2704,  2809,  2916,  3025,  3136,  3249,  3364,  3481,  3600,  3721,  3844,  3969, 
         4096,  4225,  4356,  4489,  4624,  4761,  4900,  5041,  5184,  5329,  5476,  5625,  5776,  5929,  6084,  6241, 
         6400,  6561,  6724,  6889,  7056,  7225,  7396,  7569,  7744,  7921,  8100,  8281,  8464,  8649,  8836,  9025, 
         9216,  9409,  9604,  9801, 10000, 10201, 10404, 10609, 10816, 11025, 11236, 11449, 11664, 11881, 12100, 12321, 
        12544, 12769, 12996, 13225, 13456, 13689, 13924, 14161, 14400, 14641, 14884, 15129, 15376, 15625, 15876, 16129, 
        16384, 16641, 16900, 17161, 17424, 17689, 17956, 18225, 18496, 18769, 19044, 19321, 19600, 19881, 20164, 20449, 
        20736, 21025, 21316, 21609, 21904, 22201, 22500, 22801, 23104, 23409, 23716, 24025, 24336, 24649, 24964, 25281, 
        25600, 25921, 26244, 26569, 26896, 27225, 27556, 27889, 28224, 28561, 28900, 29241, 29584, 29929, 30276, 30625, 
        30976, 31329, 31684, 32041, 32400, 32761, 33124, 33489, 33856, 34225, 34596, 34969, 35344, 35721, 36100, 36481, 
        36864, 37249, 37636, 38025, 38416, 38809, 39204, 39601, 40000, 40401, 40804, 41209, 41616, 42025, 42436, 42849, 
        43264, 43681, 44100, 44521, 44944, 45369, 45796, 46225, 46656, 47089, 47524, 47961, 48400, 48841, 49284, 49729, 
        50176, 50625, 51076, 51529, 51984, 52441, 52900, 53361, 53824, 54289, 54756, 55225, 55696, 56169, 56644, 57121, 
        57600, 58081, 58564, 59049, 59536, 60025, 60516, 61009, 61504, 62001, 62500, 63001, 63504, 64009, 64516, 65025
    );

implementation

  // Misc

  function MulDiv( a, b, c : integer ) : integer; // = (a * b) div c (round down, signed)
    // EAX = a
    // EDX = b
    // ECX = c
    asm
      imul   edx
      idiv   ecx
    end;

  function MulDivRD( a, b, c : cardinal ) : cardinal; // = (a * b) div c (round down, unsigned)
    // EAX = a
    // EDX = b
    // ECX = c
    asm
      mul    edx
      div    ecx
    end;

  function MulDivRN( a, b, c : cardinal ) : cardinal; // = (a * b + c div 2) div c (round nearest, unsigned)
    // EAX = a
    // EDX = b
    // ECX = c
    asm
      push ebx
      mul  edx
      mov  ebx, ecx
      shr  ebx, 1
      add  eax, ebx
      adc  edx, 0
      div  ecx
      pop  ebx
    end;

  function MulDivRU( a, b, c : cardinal ) : cardinal; // = (a * b + c - 1) div c (round up, unsigned)
    // EAX = a
    // EDX = b
    // ECX = c
    asm
      push ebx
      mul  edx
      mov  ebx, ecx
      dec  ebx
      add  eax, ebx
      adc  edx, 0
      div  ecx
      pop  ebx
    end;

  function RightShiftCount( Num : cardinal ) : cardinal;
    begin
      assert( Num <> 0, 'Invalid number in NumUtils.RightShiftCount' );
      Result := 0;
      while Num and 1 = 0 do
        begin
          inc( Result );
          Num := Num shr 1;
        end;
    end;

  function LeftShiftCount( Num : cardinal ) : cardinal;
    var
      i : integer;
    begin
      assert( Num <> 0, 'Invalid number in NumUtils.LeftShiftCount' );
      Result := 0;
      for i := 0 to 31 do
        begin
          if Num and 1 <> 0
            then inc( Result );
          Num := Num shr 1;
        end;
      Result := 8 - Result;
    end;

  function NearestMult( Num : integer; Step : integer ) : integer;
    begin
      Result := ( (Num + Step div 2) div Step ) * Step;
    end;

  procedure Swap2( var Num1, Num2 );
    var
      Tmp : word;
      a   : word absolute Num1;
      b   : word absolute Num2;
    begin
      Tmp := a;
      a   := b;
      b   := Tmp;
    end;

  procedure Swap4( var Num1, Num2 );
    var
      Tmp : longint;
      a   : longint absolute Num1;
      b   : longint absolute Num2;
    begin
      Tmp := a;
      a   := b;
      b   := Tmp;
    end;

  procedure Swap8( var Num1, Num2 );
    var
      Tmp : comp;
      a   : comp absolute Num1;
      b   : comp absolute Num2;
    begin
      Tmp := a;
      a   := b;
      b   := Tmp;
    end;

  procedure Swap10( var Num1, Num2 );
    var
      Tmp : extended;
      a   : extended absolute Num1;
      b   : extended absolute Num2;
    begin
      Tmp := a;
      a   := b;
      b   := Tmp;
    end;

  function mcm( Num1, Num2 : integer ) : integer;
    begin
      assert( (Num1 > 0 ) and (Num2 > 0 ), 'Bad parameter in NumUtils.mcm' );
      Result := Num1 * Num2 div mcd( Num1, Num2 ); //!!!! Segun Alcides
    end;

  function mcd( Num1, Num2 : integer ) : integer;
    begin
      assert( (Num1 > 0 ) and (Num2 > 0 ), 'Bad parameter in NumUtils.mcd' );
      while Num1 <> Num2 do
        begin
          if Num1 > Num2
            then Num1 := Num1 - Num2
            else Num2 := Num2 - Num1;
        end;
      Result := Num1;
    end;

  function InvertInt( Num : integer ) : integer;
    asm
      bswap     eax
    end;

  // --- MinMax ---

  function Min( i, j : integer ) : integer;
    asm
        cmp     eax, edx
        jle     @Exit
        mov     eax, edx
      @Exit:
    end;

  function Max( i, j : integer ) : integer;
    asm
        cmp     eax, edx
        jge     @Exit
        mov     eax, edx
      @Exit:
    end;

  function MinCardinal( i, j : cardinal ) : cardinal;
    asm
        cmp     eax, edx
        jbe     @Exit
        mov     eax, edx
      @Exit:
    end;

  function MaxCardinal( i, j : cardinal ) : cardinal;
    asm
        cmp     eax, edx
        jae     @Exit
        mov     eax, edx
      @Exit:
    end;

  function Min3( i, j, k : integer ) : integer;
    asm
        cmp     eax, edx
        jle     @1
        mov     eax, edx
      @1:
        cmp     eax, ecx
        jle     @2
        mov     eax, ecx
      @2:
    end;

  function Max3( i, j, k : integer ) : integer;
    asm
        cmp     eax, edx
        jge     @1
        mov     eax, edx
     @1:
        cmp     eax, ecx
        jge     @2
        mov     eax, ecx
     @2:
    end;

  // --- Integer sign ---

  function Sign( Num : integer ) : integer;
    asm
      rcl       eax, 1
      jc        @Negative

    @Positive:
      mov       eax, 1
      jmp       @Exit

    @Negative:
      mov       eax, -1

    @Exit:
    end;

  { --- BCD --- }

  function Bcd2Dec( Num : cardinal ) : cardinal;
    var
      dec : cardinal;
    begin
      Result := 0;
      dec    := 1;
      repeat
        inc( Result, (Num and 15) * dec );
        dec := dec * 10;
        Num := Num shr 4;
      until Num = 0;
    end;

end.

