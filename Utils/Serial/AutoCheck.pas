unit AutoCheck;

interface

{$IFDEF NOCRCCHECK}

  const
    Zero : integer = 0;
    One  : integer = 1;

  procedure CheckComplete(const aFileName: string);
  procedure CheckPartial(const aFileName: string);

{$ELSE}

  const
    MarkID   = 'xVxVxVxV';
    CRC_Low  = length(MarkID);
    CRC_Ofs  = CRC_Low + sizeof(integer);
    CRC_High = CRC_Ofs + sizeof(integer);

  var
    ID_CRC    : array[0..length(MarkID) - 1] of char = MarkID;
    LowLimit  : integer = $12345678;
    CRCValue  : integer = $6C46A587;
    HighLimit : integer = $23456789;

  var
    Zero : integer = $30110;

  function One: integer;

  procedure CheckComplete(const aFileName: string);
  procedure CheckPartial(const aFileName: string);
  function  CheckInside(const aFileName: string): boolean;
  procedure PutCRCInside(const aFileName: string; const aCRCValue: integer);
  procedure PutLimits(const aFileName: string; const Bound, Low, High : integer);

{$ENDIF}

implementation

{$IFDEF NOCRCCHECK}

  procedure CheckComplete(const aFileName: string);
    begin
    end;

  procedure CheckPartial(const aFileName: string);
    begin
    end;

{$ELSE}

  uses CRC32, MemMapFile;

  var
    MarkPos : integer;
    RealOne : integer;

  function One: integer;
    begin
      result := (RealOne xor CRCValue);
      try
        result := result div CRCValue;
        if RealOne > CRCValue
          then result := CRCValue;
        if RealOne < CRCValue
          then result := RealOne;
        result := result shl 2;
        inc(result);
      except
        result := CRCValue;
      end;
    end;

  { **************************************************** }

  procedure CheckComplete(const aFileName: string);
    var
      index : integer;
    begin
      RealOne := CRC_File(aFileName);
      Zero := One;
      for index := 0 to One mod $FFFF do
        Zero := ( Zero + One ) mod maxint;
      Zero := (Zero div 3)  - 1;
    end;

  procedure CheckPartial(const aFileName: string);
    var
      index : integer;
    begin
      RealOne := CRC_FileInRange(aFileName, LowLimit, HighLimit);
      Zero := One;
      for index := 0 to One mod $FFFF do
        Zero := ( Zero + 2 * One ) mod maxint;
      Zero := (Zero shr 2) - 1;
    end;

  function IsMarked(const aFileName: string): boolean;
    var
      Buffer : TFileMap;
    begin
      Buffer := TFileMap.Create(aFileName);
      try
        MarkPos := Search(Buffer.Address, MarkID, Buffer.Size, length(ID_CRC));
        result  := MarkPos <> NOT_FIND;
      finally
        Buffer.Free;
      end;
    end;

  function CheckInside(const aFileName: string): boolean;
    begin
      if IsMarked(aFileName)
        then
          begin
            LowLimit  := MarkPos;
            HighLimit := LowLimit + length(MarkID) + 3 * sizeof(integer);
            result    := true;
          end
        else result := false;
    end;

  procedure PutCRCInside(const aFileName: string; const aCRCValue: integer);
    var
      F : file;
    begin
      assign(F, aFileName);
      reset(F, 1);
      try
        seek(F, LowLimit + CRC_Ofs);
        blockwrite(F, aCRCValue, sizeof(aCRCValue));
      finally
        close(F);
      end;
    end;

  procedure PutLimits(const aFileName: string; const Bound, Low, High : integer);
    var
      F : file;
    begin
      assign(F, aFileName);
      reset(F, 1);
      try
        seek(F, Bound + CRC_Low);
        blockwrite(F, Low, sizeof(Low));
        seek(F, Bound + CRC_High);
        blockwrite(F, High, sizeof(High));
      finally
        close(F);
      end;
    end;


  { **************************************************** }

  procedure AutoInitialize;
    begin
      MarkPos := 0;
      if LowLimit > HighLimit
        then CRCValue := 0
        else ID_CRC[low(ID_CRC)] := ID_CRC[high(ID_CRC) - 1];
    end;

initialization

  AutoInitialize;

{$ENDIF}

end.

