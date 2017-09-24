unit StrUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.
//
// Notes:
//
//    Ported from the UtStr unit.
//
// Old functions:             Use instead:
//
// - UpCaseStr:               UpperCase or AnsiUpperCase ( SysUtils )
// - LoCaseStr:               LowerCase or AnsiLowerCase ( SysUtils )
// - CleanSpaces, CleanStr:   Trim                       ( SysUtils )
// - SkipSpaces:              TrimLeft                   ( SysUtils )
// - CleanTrailingSpaces:     TrimRight                  ( SysUtils )
// - TrimSpaces               PackSpaces                 ( new name )

interface

  uses
    SysUtils, Windows, Classes;

  const
    chBEEP  = ^G;
    chBS    = ^H;
    chTAB   = ^I;
    chLF    = ^J;
    chNP    = ^L;
    chCR    = ^M;
    chSPACE = ' ';

  const
    DefaultTabStop = 8;

  function CleanTabs( const Str : string; TabStop : integer ) : string;         // Converts TABs to Spaces

  function StripHiBit( const Str : string ) : string;                           // Converts 8bit ASCII to 7 bits

  function KillSpaces( const Str : string ) : string;                           // Delete all spaces
  function PackSpaces( const Str : string ) : string;                           // Replaces contiguous spaces by one space

  function LeftAlign( const Str : string; Width : integer ) : string;           // Align left to a field of specified width
  function RightAlign( const Str : string; Width : integer ) : string;          // Align right to a field of specified width
  function CenterAlign( const Str : string; Width : integer ) : string;         // Center in a field of specified width
  function AdjustStr( const Str : string; Width : integer ) : string;           // Trims control characters leading spaces and aligns to left
  function CapitalizeStr( const Str : string ) : string;                        // Guess what: First letter in upper case and the rest in lower case

  function ReplaceChar( const Str : string; Old, New : char ) : string;         // Replaces all "Old" characters by "New"
  function ReplaceStr(  const Str : string; const Old, New : string ) : string; // Replaces all "Old" substrings by "New"

  function Spaces( Count : integer ) : string;                                  // Returns "Count" spaces
  function DupChar( Ch : char; Count : integer ) : string;                      // ( "a",   3 ) => "aaa"
  function DupStr( const Str : string; Count : integer ) : string;              // ( "abc", 3 ) => "abcabcabc"

  function StrEqual( Str1, Str2 : pchar; Len : integer ) : boolean;             // Check for equality between "Str1" & "Str2", ignores #0
  function CharPos( Str : pchar; Ch : char; Len : integer ) : integer;

  function RightStr( const Str : string; Count : integer ) : string;            // Get last "Count" characters
  function LeftStr( const Str : string; Count : integer ) : string;             // Get first "Count" characters

  function FirstNot( const Str : string; Ch : char; At : integer ) : integer;   // First character that is not "Ch", starting at "At"
  function Pos( const SubStr, Str : string; At : integer ) : integer;           // Same as System cousin
  function BackPos( const SubStr, Str : string; At : integer ) : integer;       // Backwards version

  function EndsWith( const SubStr, Str : string ) : boolean;
  function StartsWith( const SubStr, Str : string ) : boolean;

  function BeforeStr( const SubStr, Str : string ) : string;                    // Portion of "Str" before "SubStr"
  function AfterStr ( const SubStr, Str : string ) : string;                    // Portion of "Str" after "SubStr"

  function JoinStrings( Strings : array of string ) : pchar;                    // Return a NUL terminated list of ASCIIZ strings from an array of strings
  function JoinStringList( List : TStrings ) : pchar;                           // Same, but from a string list
  function SplitStringList( StringList : pchar ) : TStrings;                    // Obtain a list of strings from a NUL terminated list

  function SplitStrings( const Strings, Separator : string ) : TStrings;

  function AnsiToAscii( const Str : string ) : string;
  function AsciiToAnsi( const Str : string ) : string;

  // --- Number to string ---

  const
    HexDigits : array[0..15] of char = '0123456789ABCDEF';

  function NumToStr( Num : integer; Base, Width : integer ) : string;  // NumToStr( 8, 9, 2 ) = '10'
  function Hex( Num, Width : integer ) : string;                       // Hex( 31, 4 ) = '001F'

  function IndxOf( const IndxStr, Prefix : string ) : integer;         // IndxOf( 'Foobar01', 'Foobar' ) = 1

  // Internal structure of AnsiString

  type
    StrInternalStruct =
      record
        AllocSize : longint;
        RefCount  : longint;
        Length    : longint;
      end;

  const
    StrSkew     = sizeof( StrInternalStruct );
    StrOverhead = sizeof( StrInternalStruct ) + 1;

implementation

  function IndxOf( const IndxStr, Prefix : string ) : integer; // IndxOf( 'Foobar01', 'Foobar' ) = 1
    begin
      if Prefix = copy( IndxStr, 1, length( Prefix ) )
        then
          try
            Result := StrToInt( copy( IndxStr, length( Prefix ) + 1, MaxInt ) );
          except
            Result := -1;
          end
        else
          Result := -1;
    end;

  // --- Number to string ---

  function Hex( Num, Width : integer ) : string;
    begin
      Result := NumToStr( Num, 16, Width );
    end;

  function NumToStr( Num : integer; Base, Width : integer ) : string;
    begin
      SetLength( Result, Width );
      repeat
        dec( Width );
        pchar(Result)[Width] := HexDigits[Num mod Base];
        Num := Num div Base;
      until Width <= 0;
    end;

  function SplitStrings( const Strings, Separator : string ) : TStrings;
    var
      ChPos  : integer;
      OldPos : integer;
      Len    : integer;
    begin
      OldPos := 0;
      Len    := length( Strings );
      Result := TStringList.Create;
      repeat
        ChPos := Pos( Separator, Strings, OldPos + Len );
        if ChPos <> 0
          then Result.Add( copy( Strings, OldPos + Len, ChPos - OldPos - 1 ) )
          else Result.Add( copy( Strings, OldPos + Len, MaxInt ) );
        OldPos := ChPos;  
      until ChPos = 0;
    end;

  function SplitStringList( StringList : pchar ) : TStrings;
    var
      s : string;
    begin
      Result := TStringList.Create;
      while StrLen( StringList ) <> 0 do
        begin
          s := StringList;
          Result.Add( s );
          Inc( StringList, length( s ) + 1 );
        end;
    end;

  function JoinStrings( Strings : array of string ) : pchar;
    var
      i         : integer;
      ListSize  : integer;
      StrPtr    : pchar;
      CurrStr   : string;
    begin
      ListSize  := 1;
      for i := low( Strings ) to high( Strings ) do
        Inc( ListSize, length( Strings[i] ) + 1 );

      GetMem( Result, ListSize );

      StrPtr := Result;
      for i := low( Strings ) to high( Strings ) do
        begin
          CurrStr := Strings[i];
          System.Move( pchar(CurrStr)[0], StrPtr[0], length( CurrStr ) + 1 );
          Inc( StrPtr, length( CurrStr ) + 1 );
        end;
      StrPtr[0] := #0;
    end;

  function JoinStringList( List : TStrings ) : pchar;
    var
      i         : integer;
      LastIndx  : integer;
      ListSize  : integer;
      StrPtr    : pchar;
      CurrStr   : string;
    begin
      ListSize  := 1;
      with List do
        begin
          LastIndx := List.Count - 1;

          for i := 0 to LastIndx do
            Inc( ListSize, length( Strings[i] ) + 1 );

          GetMem( Result, ListSize );

          StrPtr := Result;
          for i := 0 to LastIndx do
            begin
              CurrStr := Strings[i];
              System.Move( pchar(CurrStr)[0], StrPtr[0], length( CurrStr ) + 1 );
              Inc( StrPtr, length( CurrStr ) + 1 );
            end;
          StrPtr[0] := #0;
        end;
    end;

  function AnsiToAscii( const Str : string ) : string;
    begin
      SetLength( Result, length( Str ) );
      CharToOemA( pchar( Str ), pchar( Result ) );
    end;

  function AsciiToAnsi( const Str : string ) : string;
    begin
      SetLength( Result, length( Str ) );
      OemToCharA( pchar( Str ), pchar( Result ) );
    end;

  function CapitalizeStr( const Str : string ) : string;
    begin
      Result    := LowerCase( Str );
      Result[1] := UpCase( Result[1] );
    end;

  function ReplaceChar( const Str : string; Old, New : char ) : string;
    var
      i : integer;
    begin
      SetString( Result, pchar(Str), length( Str ) );
      for i := 0 to length( Str ) - 1 do
        if pchar( Result )[i] = Old
          then pchar( Result )[i] := New;
    end;

  function ReplaceStr(  const Str : string; const Old, New : string ) : string;
    var
      Indx     : integer;
      LastIndx : integer;
      ResLen   : integer;
      Delta    : integer;
    begin
      // Reserve space for max new length
      if New = ''
        then SetLength( Result, length( Str ) )
        else
          if length( New ) > length( Old ) // Reserve space for max new length
            then SetLength( Result, length(Str) * length( New ) div length( Old ) )
            else SetLength( Result, length(Str) * length( Old ) div length( New ) );

      ResLen := 0;
      Indx   := 1;

      repeat
        LastIndx := Indx;
        Indx := Pos( Old, Str, LastIndx );
        if Indx <> 0
          then
            begin
              Delta := Indx - LastIndx;
              Move( pchar(Str)[LastIndx - 1], pchar(Result)[ResLen], Delta );      // Copy original piece
              Move( pchar(New)[0], pchar(Result)[ResLen + Delta], length( New ) ); // Copy New
              Inc( ResLen, Delta + length( New ) );
              Inc( Indx, length( Old ) );
            end;
      until Indx = 0;
      Move( pchar(Str)[LastIndx - 1], pchar(Result)[ResLen], length(Str) - LastIndx + 1 );  // Copy last piece
      SetLength( Result, ResLen + length(Str) - LastIndx + 1 );
    end;

  function KillSpaces( const Str : string ) : string;
    var
      i, j : integer;
      s    : string;
      Len  : integer;
    begin
      Len := Length( Str );
      SetLength( s, Len );

      j := 0;
      for i := 0 to Len - 1 do
        if pchar( Str )[i] > ' '
          then
            begin
              pchar( s )[j] := pchar( Str )[i];
              Inc( j );
            end;
      SetString( Result, pchar( s ), j );
    end;

  function PackSpaces( const Str : string ) : string;
    var
      i, j : integer;
      Len  : integer;
    begin
      Len := Length( Str );
      SetLength( Result, Len );

      j := 0;
      pchar( Result )[0] := pchar( Str )[0];
      for i := 1 to Len - 1 do
        if ( pchar( Str )[ i - 1 ] > ' ' ) or ( pchar( Str )[i] > ' ' )
          then
            begin
              Inc( j );
              pchar( Result )[j] := pchar( Str )[i];
            end;
      SetLength( Result, j + 1 );
    end;

  function DupStr( const Str : string; Count : integer ) : string;
    var
      i   : integer;
      Len : integer;
    begin
      Len := Length( Str );
      SetLength( Result, Count * length( Str ) );
      for i := 0 to Count - 1 do
        Move( pchar( Str )[0], pchar( Result )[i * Len], Len );
    end;

  function DupChar( Ch : char; Count : integer ) : string;
    begin
      SetLength( Result, Count );
      FillChar( pchar( Result )[0], Count, Ch );
    end;

  function Spaces( Count : integer ) : string;
    begin
      SetLength( Result, Count );
      FillChar( pchar( Result )[0], Count, ' ' );
    end;

  function CleanTabs( const Str : string; TabStop : integer ) : string;   // Converts TABs to Spaces
    var
      ResStr   : pchar;
      CurrLine : pchar;
      i        : integer;
      SpcCount : integer;
    begin
      if TabStop = 0
        then TabStop := DefaultTabStop;

      SetLength( Result, length( Str ) * 8 );              // Worst case!
      CurrLine := pchar(Result);                           // For multi-line strings, see later
      ResStr   := CurrLine;

      for i := 1 to length( Str ) do
        case Str[i] of
          chTAB :
            begin
              SpcCount := ( ResStr - CurrLine ) mod TabStop;
              FillChar( ResStr, SpcCount, ' ' );
              Inc( ResStr, SpcCount );
            end;
          chCR :
            begin
              CurrLine := pchar( Result ) + i; // This function can format a multi-line string: here
                                               // we update CurrLine if we found a new line
              ResStr := pchar(Str) + i - 1;
              Inc( ResStr );
            end;
          else
            begin
              ResStr := pchar(Str) + i - 1;
              Inc( ResStr );
            end;
        end;
      SetLength( Result, ResStr - pchar(Result) );
    end;

  function AdjustStr( const Str : string; Width : integer ) : string;
    begin
      AdjustStr := LeftAlign( TrimLeft( CleanTabs( Str, 0 ) ), Width );
    end;

  function FirstNot( const Str : string; Ch : char; At : integer ) : integer;
    var
      Len : integer;
    begin
      Len    := Length( Str );
      Result := At;
      while ( Str[Result] = Ch ) and ( Result <= Len ) do
        Inc( Result );
      if Result > Len
        then Result := 0;
    end;

  function Pos( const SubStr, Str : string; At : integer ) : integer;
    asm
      test    eax, eax
      je      @@noWork

      test    edx, edx
      je      @@stringEmpty

      push    ebx
      push    esi
      push    edi

      mov     esi, eax                                       // Point ESI to substr
      mov     edi, edx                                       // Point EDI to s
      dec     ecx
      jns     @@offsetOK
      xor     ecx, ecx

    @@offsetOK:
      mov     eax, [edi - StrSkew].StrInternalStruct.Length  // ECX = Length( s )

      push    edi                                            // remember s position to calculate index

      add     edi, ecx
      sub     eax, ecx
      js      @@fail
      mov     ecx, eax

      mov     edx, [esi - StrSkew].StrInternalStruct.Length  // EDX = Length( substr )

      dec     edx                                            // EDX = Length( substr ) - 1
      js      @@fail                                         // < 0 ? return 0
      mov     al, [esi]                                      // AL = first char of substr
      inc     esi                                            // Point ESI to 2'nd char of substr

      sub     ecx, edx                                       // #positions in s to look at
                                                             // = Length( s ) - Length( substr ) + 1
      jle     @@fail

    @@loop:
      repne   scasb
      jne     @@fail
      mov     ebx, ecx                                       // save outer loop counter
      push    esi                                            // save outer loop substr pointer
      push    edi                                            // save outer loop s pointer

      mov     ecx, edx
      repe    cmpsb
      pop     edi                                            // restore outer loop s pointer
      pop     esi                                            // restore outer loop substr pointer
      je      @@found
      mov     ecx, ebx                                       // restore outer loop counter
      jmp     @@loop

    @@fail:
      pop     edx                                            // get rid of saved s pointer
      xor     eax, eax
      jmp     @@exit

    @@stringEmpty:
      xor     eax, eax
      jmp     @@noWork

    @@found:
      pop     edx                                            // restore pointer to first char of s
      mov     eax, edi                                       // EDI points of char after match
      sub     eax, edx                                       // the difference is the correct index

    @@exit:
      pop     edi
      pop     esi
      pop     ebx

    @@noWork:
    end;

  function BackPos( const SubStr, Str : string; At : integer ) : integer;
    var                           // This was coded by Meddy
      MaxPos : integer;
      Len    : integer;
      i, j   : integer;
      Aux    : pchar;
    begin
      Len := length( SubStr );

      assert( ( Len > 0 ) and ( length( Str ) > 0 ), 'Empty string passed in StrUtils.BackPos' );

      if Len = 1
        then      // [JRG opt]
          begin
            if At > 0
              then Result := At
              else Result := Length( Str );
            while ( Str[Result] <> SubStr[1] ) and ( Result > 0 ) do
              dec( Result );
          end
        else      // [Med's stuff]
          begin
            if At = 0
              then MaxPos := length( Str ) - Len
              else MaxPos := At - Len;
            i := 0;
            j := -1;
            Result := -1;
            repeat
              Aux := StrPos( pchar( Str ) + i, pchar( SubStr ) ); // Should be opt'zd [JRG note]
              if ( Aux <> nil ) and ( Aux - pchar( Str ) <= MaxPos )
                then
                  begin
                    j := Aux - pchar( Str );
                    i := j + Len;
                  end
                else Result := j + 1;
            until Result >= 0;
          end
    end;

  function StripHiBit( const Str : string ) : string;
    var
      i : integer;
    begin
      Result := Str;
      for i := 0 to length( Result ) - 1 do
        byte( pchar( Result )[i] ) := byte( pchar( Result )[i] ) and 127;
    end;

  function RightAlign( const Str : string; Width : integer ) : string;
    begin
      if length( Str ) < Width
        then Result := Spaces( Width - length( Str ) ) + Str
        else SetString( Result, pchar( Str ), Width );
    end;

  function LeftAlign( const Str : string; Width : integer ) : string;
    begin
      if length( Str ) < Width
        then Result := Str + Spaces( Width - length( Str ) )
        else SetString( Result, pchar( Str ), Width );
    end;

  function CenterAlign( const Str : string; Width : integer ) : string;
    var
      n : integer;
      s : string;
    begin
      n := ( Width - length( Str ) ) div 2;
      if n > 0
        then
          begin
            s := Spaces( n );
            Result := s + Str + s;
          end
        else Result := Str
    end;

  function CharPos( Str : pchar; Ch : char; Len : integer ) : integer;
    // EAX = Str
    // EDX = Ch
    // ECX = Len
    asm
      push ebx
      mov  ebx, eax              // Str
      mov  eax, ecx              // Backup Len

      or   ecx, ecx
      jz   @@NotFound
      or   ebx, ebx
      jz   @@NotFound

    @@Loop:
      cmp  [ebx], dl
      je   @@Exit
      inc  ebx
      dec  ecx
      jnz  @@Loop

    @@NotFound:
      lea  ecx, [eax + 1] // Not found, make EAX = -1

    @@Exit:
      sub  eax, ecx
      pop  ebx
    end;

  function StrEqual( Str1, Str2 : pchar; Len : integer ) : boolean;
    asm
      push ebx

      or   eax, eax
      jz   @@NoWork
      or   edx, edx
      jz   @@Exit
      or   ecx, ecx
      jz   @@Exit

    @@Loop:
      mov  bl, [eax]
      cmp  bl, [edx]
      jne  @@Exit
      inc  eax
      inc  edx
      dec  ecx
      jnz  @@Loop

    @@Exit:
      setz al
      pop  ebx

    @@NoWork:
    end;

  function StartsWith( const SubStr, Str : string ) : boolean;
    begin
      Result := StrEqual( pchar(SubStr), pchar(Str), length( SubStr ) );
    end;

  function EndsWith( const SubStr, Str : string ) : boolean;
    begin
      Result := StrEqual( pchar(SubStr), pchar(Str)+(length(Str) - length(SubStr)), length( SubStr ) );
    end;

  function RightStr( const Str : string; Count : integer ) : string;
    begin
      if Length( Str ) > Count
        then Result := copy( Str, length( Str ) - Count + 1, Count )
        else Result := Str;
    end;

  function LeftStr( const Str : string; Count : integer ) : string;
    begin
      SetString( Result, pchar( Str ), Count );
    end;

  function BeforeStr( const SubStr, Str : string ) : string;
    begin
      SetString( Result, pchar( Str ), System.Pos( SubStr, Str ) - 1 );
    end;

  function AfterStr( const SubStr, Str : string ) : string;
    var
      pos : integer;
    begin
      pos := System.Pos( SubStr, Str );
      SetString( Result, pchar( Str ) + pos, Length( Str ) - pos );
    end;

end.
