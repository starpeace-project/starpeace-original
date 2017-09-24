unit rc4;

interface

  const
    DecChars : string = #1#2#3#4#5#6#7#8#9#10#11#12#13#15 + '¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ';

  type
    TRC4 =
      class
        public
          constructor Load(var cypherAry);
        private
          fBytCypherAry : array[0..255] of byte;
          fHasKey       : boolean;
        private
          //procedure InitializeCypher;
          procedure SetKey(pStrKey : string);
        public
          function Apply(pStrMessage : string) : string;
          function toHex(pStrMessage : string) : string;
          function toBin(pStrMessage : string) : string;
          function genKey(len : integer) : string;
        public
          property Key : string write SetKey;
      end;

implementation

  // TRC4

  constructor TRC4.Load(var cypherAry);
    begin
      inherited Create;
      move(cypherAry, fBytCypherAry[0], sizeof(fBytCypherAry[0]));
      fHasKey := true;
    end;

  procedure TRC4.SetKey(pStrKey : string);
    var
      lLngKeyLength : integer;
      lLngIndex     : integer;
      BytKeyAry     : array[0..255] of byte;
      lBytJump      : integer;
      lBytIndex     : integer;
      lBytTemp      : byte;
    begin
      // if the key is diff and not empty, change it
      if pStrKey <> ''
        then
          begin
            fHasKey := true;
            lLngKeyLength := Length(pStrKey);
            // spread the key all over the array
            for lLngIndex := 0 To 255 do
    	      BytKeyAry[lLngIndex] := byte(pStrKey[lLngIndex mod lLngKeyLength + 1]);

            // init the array
            for lBytIndex := 0 To 255 do
              fBytCypherAry[lBytIndex] := lBytIndex;

            // Switch values of Cypher arround based off of index and Key value
            lBytJump := 0;
            for lBytIndex := 0 to 255 do
              begin
                // Figure index To switch
                lBytJump := (lBytJump + fBytCypherAry[lBytIndex] + BytKeyAry[lBytIndex]) mod 256;
                // Do the switch
                lBytTemp := fBytCypherAry[lBytIndex];
                fBytCypherAry[lBytIndex] := fBytCypherAry[lBytJump];
                fBytCypherAry[lBytJump]  := lBytTemp;
              end;
          end;
    end;

  function TRC4.Apply(pStrMessage : string) : string;
    var
      lBytIndex : integer;
      lBytJump  : integer;
      lBytTemp  : byte;
      lBytY     : byte;
      lLngT     : integer;
      lLngX     : integer;
      len       : integer;
      TmpCypher : array[0..255] of byte;

    begin
      len := length(pStrMessage);
      if fHasKey and (len > 0)
        then
          begin
            SetLength(result, len);

            // save the cypherArray
            move(fBytCypherAry[0], TmpCypher[0], sizeof(fBytCypherAry));

            lBytIndex := 0;
            lBytJump  := 0;

            for lLngX := 1 To len do
              begin
                lBytIndex := (lBytIndex + 1) mod 256; // wrap index
                lBytJump  := (lBytJump + TmpCypher[lBytIndex]) mod 256; // ' wrap J+S()

                // Add/Wrap those two
                lLngT := (11 + TmpCypher[lBytIndex] + TmpCypher[lBytJump]) mod 256;

                // Switcheroo
                lBytTemp := TmpCypher[lBytIndex];
                TmpCypher[lBytIndex] := TmpCypher[lBytJump];
                TmpCypher[lBytJump]  := lBytTemp;
                lBytY := TmpCypher[lLngT];

                // Character Encryption ...
                result[lLngX] := char(byte(pStrMessage[lLngX]) xor lBytY);
              end;
          end
        else result := pStrMessage;
    end;

  function TRC4.toHex(pStrMessage : string) : string;
    const
      HEX_DIGITS : array[0..15] of char = '0123456789ABCDEF';
    var
      len : integer;
      i   : integer;
      tmp : byte;
    begin
      len := Length(pStrMessage);
      if pStrMessage <> ''
        then
          begin
            SetLength(result, 2*len);
            for i := 1 to len do
              begin
                tmp := byte(pStrMessage[i]);
                result[2*i-1] := HEX_DIGITS[(tmp and $0F)];
                result[2*i]   := HEX_DIGITS[(tmp and $F0) shr 4];
              end;
          end
        else result := '';
    end;

  function TRC4.toBin(pStrMessage : string) : string;
    function htoi(l, h : char) : char;
      var
        l1 : byte;
        h1 : byte;
      begin
        l1 := ord(l);
        if l1 >= ord('A')
          then l1 := 10 + l1 - ord('A')
          else l1 := l1 - ord('0');
        h1 := ord(h);
        if h1 >= ord('A')
          then h1 := 10 + h1 - ord('A')
          else h1 := h1 - ord('0');
        result := char(byte(l1) or (byte(h1) shl 4));
      end;
    var
      len : integer;
      i   : integer;
    begin
      len := Length(pStrMessage);
      if (len > 0) and (len mod 2 = 0)
        then
          begin
            SetLength(result, len div 2);
            for i := 1 to len div 2 do
              result[i] := htoi(pStrMessage[2*i-1], pStrMessage[2*i]);
          end
        else result := '';
    end;

  function TRC4.genKey(len : integer) : string;
    var
      i : integer;
    begin
      SetLength(result, len);
      for i := 1 to len do
        result[i] := char(ord('0') + random(ord('z') - ord('0')));
    end;

{
  procedure TRC4.InitializeCypher;
    var
      lBytJump   : integer;
      lBytIndex  : integer;
      lBytTemp   : byte;
    begin
      // init the array with the a[i]=i
      for lBytIndex := 0 To 255 do
        fBytCypherAry[lBytIndex] := lBytIndex;

      // Switch values of Cypher arround based off of index and Key value
      lBytJump := 0;
      for lBytIndex := 0 to 255 do
        begin
          // Figure index To switch
          lBytJump := (lBytJump + fBytCypherAry[lBytIndex] + fBytKeyAry[lBytIndex]) mod 256;
          // Do the switch
          lBytTemp := fBytCypherAry[lBytIndex];
          fBytCypherAry[lBytIndex] := fBytCypherAry[lBytJump];
          fBytCypherAry[lBytJump]  := lBytTemp;
        end;
    end;


  procedure TRC4.SetKey(pStrKey : string);
    var
      lLngKeyLength : integer;
      lLngIndex     : integer;
    begin
      // if the key is diff and not empty, change it
      if (pStrKey <> '') and (pStrKey <> fStrKey)
        then
          begin
            fStrKey := pStrKey;
            lLngKeyLength := Length(pStrKey);
            // spread the key all over the array
            for lLngIndex := 0 To 255 do
    	      fBytKeyAry[lLngIndex] := byte(pStrKey[lLngIndex mod lLngKeyLength + 1]);
          end;
    end;
}


initialization

  randomize;

end.
