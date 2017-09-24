unit ExcUnmangle;

{$ASSERTIONS OFF}

{$D-,L-,Y-} // turn off all debug-info
{$R-}       // turn off range checking
{$H+}       // huge strings
{$Q-}       // OVERFLOWCHECKS OFF


{ -------------------------- } interface { -------------------------- }

uses
  SysUtils;

const
        UM_UNKNOWN       = $00000000;

        UM_FUNCTION      = $00000001;
        UM_CONSTRUCTOR   = $00000002;
        UM_DESTRUCTOR    = $00000003;
        UM_OPERATOR      = $00000004;
        UM_CONVERSION    = $00000005;

        UM_DATA          = $00000006;
        UM_THUNK         = $00000007;
        UM_TPDSC         = $00000008;
        UM_VTABLE        = $00000009;
        UM_VRDF_THUNK    = $0000000a;

        UM_KINDMASK      = $000000ff;

        (* Modifier (is it a member, template?). *)

        UM_QUALIFIED     = $00000100;
        UM_TEMPLATE      = $00000200;

        UM_VIRDEF_FLAG   = $00000400;
        UM_FRIEND_LIST   = $00000800;
        UM_CTCH_HNDL_TBL = $00001000;
        UM_OBJ_DEST_TBL  = $00002000;
        UM_THROW_LIST    = $00004000;
        UM_EXC_CTXT_TBL  = $00008000;
        UM_LINKER_PROC   = $00010000;
        UM_SPECMASK      = $0001fc00;

        UM_MODMASK       = $00ffff00;

        (* Some kind of error occurred. *)

        UM_BUFOVRFLW     = $01000000;
        UM_HASHTRUNC     = $02000000;
        UM_ERROR         = $04000000;

        UM_ERRMASK       = $3f000000;

        (* This symbol is not a mangled name. *)

        UM_NOT_MANGLED   = $40000000;

function _UnMangle( Src,Dest: PChar; MaxLen: Integer;
                    QualP,BaseP: PChar; doArgs,IsDelphi: Boolean ): Longint;


{ -------------------------- } implementation { -------------------------- }

type
  EFinishUnmangle = class(Exception);

type
  TParamEntry = record
    targpos: PChar;
    len: Integer;
  end;

  trans = record
    name: PChar;
    symbol: PChar;
  end;

const
  DELPHI4_COMPAT 	= 	1;
  MAXBUFFLEN            =       8192;      (* maximum output length *)
  PTABLE_LEN 		=	36;
  CONTENT_LEN 		=	250;
  QUALIFIER : Char	=	'@';
  ARGLIST   : Char	=	'$';
  TMPLCODE  : Char	=	'%';

(* unmangle(src, dest, maxlen, qualP, baseP, doArgs): Longword

   This is the main entry-point for the unmangler code.  To use it, pass
   the following arguments:

      src      the source buffer, NULL terminated, which contains
               the mangled name.  If this pointer is NULL, unmangle()
        will return UM_NOT_MANGLED.

      dest     the destination buffer.  If this pointer is NULL,
               unmangle() will return UM_ERROR.

      maxlen   the maximum number of bytes which should be output
               to the destination buffer.  Remember to account for
        the NULL that will be output at the end of the mangled
        name.

        It is impossible to know beforehand exactly how long a
        mangled name should be, but due to restrictions in the
        length of linker names, imposed by the OMF format, a
        buffer of at least 2K bytes or longer should always be
        sufficient.

        If the size of the buffer is insufficient, unmangle()
        will return with the flag UM_BUFOVRFLW set in the return
        code.  Any other flags set in the return code will
        reflect whatever information unmangle() was able to
        determine before the overflow occurred.

      qualP    if non-NULL, this argument should point to the address
               of a buffer large enough to contain the qualifier part
               of the unmangled name.  For example, if the unmangled
        name is "foo::bar::baz", then the qualifier would be
        "foo::bar".

        Thus, this buffer should always be at least as large as
        the destination buffer, in order to ensure that memory
        overwrites never occur.

      baseP    if non-NULL, this argument should point to the address
               of a buffer large enough to contain the basename part
        of the unmangled name.  For example, if the unmangled
        name is "foo::bar::baz", then the basename would be
        "baz".  See the documentation of "qualP" for further
        notes on the required length of this buffer.

      doArgs   if this argument is non-0 (aka TRUE), it means that
               when unmangling a function name, its arguments should
        also be unmangled as part of the output name.
        Otherwise, only the name will be unmangled, and not
        the arguments.

   The return code of this function contains a series of flags, some of
   which are mutually exclusive, and some of which represent the status
   of the unmangled name.  These flags are:

        UM_NOT_MANGLED   If the return value equals this flag, then
                  it is the only flag which will be set, all
    other values being irrelevant.

      The kind of symbol (mutually exclusive)

 UM_UNKNOWN       Symbol of unknown type
 UM_FUNCTION      Global function, or member function
 UM_CONSTRUCTOR   Class donstructor function
 UM_DESTRUCTOR    Class destructor function
 UM_OPERATOR      Global operator, or member operator
 UM_CONVERSION    Member conversion operator
 UM_DATA          Class static data member
 UM_THUNK         (16-bit only, no longer used)
 UM_TPDSC         Type descriptor object (RTTI)
 UM_VTABLE        Class virtual table
 UM_VRDF_THUNK    Virtual table thunk (special)

 UM_KINDMASK      This mask can be used to exclude all other
                  flags from the return type, except the
    symbol kind.

      Modifiers (not mutually exclusive)

 UM_QUALIFIED     A member symbol, either of a class or of a
                  namespace
 UM_TEMPLATE      A template specialization symbol

      Modifiers (mutually exclusive)

 UM_VIRDEF_FLAG   Virdef flag (special)
 UM_FRIEND_LIST   Friend list (special)
 UM_CTCH_HNDL_TBL Catch handler table (exception handling)
 UM_OBJ_DEST_TBL  Object destructor table (exception handling)
 UM_THROW_LIST    Throw list (exception handling)
 UM_EXC_CTXT_TBL  Exception context table (exception handling)
 UM_LINKER_PROC   Special linker procedure (#pragma package)

 UM_SPECMASK      Special flags mask.  Use this to extract only
                  these special, mutually exclusive, flags.

 UM_MODMASK       This mask can be used to access any of the
                  symbol modifiers, whether mutually exclusive
    or not.

      Error flags (not mutually exclusive)

 UM_BUFOVRFLW     The output buffer has been overflowed
 UM_HASHTRUNC     The input name was truncated by a hash code
 UM_ERROR         Some other error has occurred

 UM_ERRMASK       Use this mask to examine only the error flags

   Note on exceptional conditions: Sometimes a mangled name does not
   have the correct format.  This can happen if garbage code is passed
   in, or a mangled name from a different, or older product, is used.
   In this case, you will notice a number enclosed in curly-braces at
   the point in the name where the fault was detected.

   For example, a false name like "@foo@$z" will generate an error like
   "foo::begin853end...", because "$z" does not represent a valid special
   function name.  In this case, the number 853 represents the line in
   UM.C where the error was found.

   If you are debugging a problem with unmangling in a case where
   examining the mangled name under the debugger is not convenient, you
   can tell the unmangler to output the mangled form of the name in the
   output buffer by setting the environment variable SHOW_TROUBLED_NAME
   to any textual value.  In that case, the output buffer for the
   example above would contain the string "foo::begin853: @foo@$zend".

   Lastly, this code is subject to change at any time.  Although Inprise
   intends to keep the API and function signature intact from release to
   release, nothing is guaranteed.  Making this source code visible in
   no wise implies any guarantee as to its functionality or accuracy.
   Caveat Programmor.
*)

function _UnMangle( src,dest: PChar; maxlen: Integer; qualP,baseP: PChar; doArgs,IsDelphi: Boolean ): Longint;
var
 source: PChar;  (* current input source *)
 srcbase: PChar;  (* beginning of input source *)
 srcindx: Integer;  (* beginning of input source *)
 target: PChar;  (* current output location *)
 targbase: PChar;  (* beginning of output *)
 namebase: PChar;  (* beginning of 'name' *)
 targend: PChar;  (* end of output *)
 qualend: PChar;  (* qualified part of name *)
 prevqual: PChar;  (* qualified part of name *)
 basename: PChar;  (* base part of name *)
 base_end: PChar;  (* end of base name *)
 set_qual: Boolean;  (* setup the qualifier name? *)
 adjust_quals: Boolean; (* adjust the qualifier pos? *)
 kind: Cardinal;
 buff: array[0..MAXBUFFLEN-1] of Char;
 vtbl_flags: array[0..256-1] of Char;

(* The mangler, when mangling argument types, will create backreferences
   if the type has already been seen.  These take the form t?, where ?
   can be either 0-9, or a-z. *)

// ---------------------------------------------------------------------------

function input: Char;
begin
  if srcindx > CONTENT_LEN then
    begin
      kind := kind or UM_HASHTRUNC;
      raise EFinishUnmangle.Create('');
    end
  else
    Result := source^;
end;

function advance: Char;
begin
  Inc(source);
  Inc(srcindx);
  Result := input;
end;

procedure backup;
begin
  Dec(source);
  Dec(srcindx);
end;

procedure _overflow;
begin
  target^ := #0;
  kind := kind or UM_BUFOVRFLW;
  raise EFinishUnmangle.Create('');
end;

procedure copy_char( c: Char );
begin
  if target <> targend then
    begin
      target^ := c;
      Inc(target);
    end
  else
    _overflow;
end;

procedure copy_class_delimiter;
begin
  if IsDelphi then copy_char('.')
  else
    begin
      copy_char(':');
      copy_char(':');
    end;
end;

procedure copy_string( p: PChar; len: Integer );
begin
 if len = 0 then len := strlen(p);

 if len <= targend - target then
   begin
    Move( p^, target^, len );
    Inc(target,len);
   end
 else
   begin
    len := targend - target;
    Move( p^, target^, len );
    Inc(target,len);
    target^ := #0;
    kind := kind or UM_BUFOVRFLW;
    raise EFinishUnmangle.Create('');
   end
end;


const
  table: array[0..42] of trans = (
   (name: 'add';  symbol: '+'  ), (name: 'adr'; symbol: '&' ),  (name: 'and' ; symbol: '&' ),
   (name: 'asg';  symbol: '='  ),  (name: 'land'; symbol: '&&' ), (name: 'lor' ; symbol: '||' ),
   (name: 'call'; symbol: '()' ), (name: 'cmp' ; symbol: '~' ), (name: 'fnc' ; symbol: '()' ),
   (name: 'dec';  symbol: '--' ), (name: 'dele'; symbol: 'delete' ), (name: 'div' ; symbol: '/' ),
   (name: 'eql';  symbol: '==' ), (name: 'geq' ; symbol: '>=' ), (name: 'gtr' ; symbol: '>' ),
   (name: 'inc';  symbol: '++' ), (name: 'ind' ; symbol: '*' ), (name: 'leq' ; symbol: '<=' ),
   (name: 'lsh';  symbol: '<<' ), (name: 'lss' ; symbol: '<' ), (name: 'mod' ; symbol: '%' ),
   (name: 'mul';  symbol: '*'  ), (name: 'neq' ; symbol: '!=' ), (name: 'new' ; symbol: 'new' ),
   (name: 'not';  symbol: '!'  ), (name: 'or'  ; symbol: '|' ), (name: 'rand'; symbol: '&=' ),
   (name: 'rdiv'; symbol: '/=' ), (name: 'rlsh'; symbol: '<<=' ), (name: 'rmin'; symbol: '-=' ),
   (name: 'rmod'; symbol: '%=' ), (name: 'rmul'; symbol: '*=' ), (name: 'ror' ; symbol: '|=' ),
   (name: 'rplu'; symbol: '+=' ), (name: 'rrsh'; symbol: '>>=' ), (name: 'rsh' ; symbol: '>>' ),
   (name: 'rxor'; symbol: '^=' ), (name: 'subs'; symbol: '[]' ), (name: 'sub' ; symbol: '-' ),
   (name: 'xor';  symbol:  '^' ), (name: 'arow'; symbol: '->'),  (name: 'nwa';  symbol: 'new[]' ),
   (name: 'dla'; symbol: 'delete[]' )
  );

procedure copy_op( src: PChar );
var
  i: Integer;
begin
  for i := Low(table) to High(table) do
    if StrComp(table[i].name, src ) = 0 then
      begin
        copy_string(table[i].symbol, 0);
        Exit;
      end;
  (* not found -> error (presumably truncated) *)
  raise EFinishUnmangle.Create('');
end;

procedure copy_until1( end1: Char );
var
  c: Char;
begin
 c := input;
 while (c <> #0) and (c <> end1) do
   begin
     copy_char(c);
     c := advance;
   end;
end;

procedure copy_until2( end1,end2: Char );
var
  c: Char;
begin
 c := input;
 while (c <> #0) and (c <> end1) and (c <> end2) do
   begin
     copy_char(c);
     c := advance;
   end;
end;

procedure copy_name(tmplname: Boolean); forward;
procedure copy_args(argsend: char; tmplargs: Boolean); forward;
procedure copy_type(start: PChar; arglvl: Boolean); forward;

procedure copy_return_type( start,callconv,regconv: PChar; process_return: Integer );
var
  ret_type: PChar;
  ret_len: Integer;
begin
  (* Process the return type of a function, and shuffle the output
     text around so it looks like the return type came first.  *)

  ret_type := target;

  if process_return <> 0 then
    begin
      copy_type(target, False);
      copy_char(' ');
     end;

  if callconv <> nil then copy_string(callconv, 0);
  if regconv  <> nil then copy_string(regconv, 0);

  ret_len  := target - ret_type;

  (* Set up the return type to have a space after it. *)

  assert(ret_len < MAXBUFFLEN);
  StrLCopy(buff, ret_type, ret_len);
  StrMove(start + ret_len, start, ret_type - start);
  StrMove(start, buff, ret_len);

  (* If we are inserting this return type at the very beginning of
           a string, it means the location of all the qualifier names is
           about to move. *)

  if adjust_quals then
    begin
      if namebase <> nil then Inc(namebase, ret_len);
      if qualend  <> nil then Inc(qualend,  ret_len);
      if prevqual <> nil then Inc(prevqual, ret_len);
      if basename <> nil then Inc(basename, ret_len);
      if base_end <> nil then Inc(base_end, ret_len);
    end;
end;

{$HINTS OFF}
procedure copy_type(start: PChar; arglvl: Boolean);
label
  HANDLE_TYPE;
var
 p,name,tname: PChar;
 is_const,is_volatile,is_signed,is_unsigned,maxloop: Integer;
 i,len: Integer;
 c,savechar: char;
 dims: array[0..90-1] of Char;
 callconv,regconv: PChar;
 hasret: Integer;
 save_adjqual: Boolean;
begin
 tname       := nil;
 c           := input;
 is_const    := 0;
 is_volatile := 0;
 is_signed   := 0;
 is_unsigned := 0;
 maxloop     := 100;

 while True do    (* emit type qualifiers *)
   begin
    assert(--maxloop > 0);

    case c of
      'u': is_unsigned := 1;
      'z': is_signed   := 1;
      'x': is_const    := 1;
      'w': is_volatile := 1;

      'y': (* 'y' for closure is followed by 'f' or 'n' *)
          begin
            c := advance;
            assert((c = 'f') or (c = 'n'));
            copy_string('__closure', 9);
          end;
      else
          goto HANDLE_TYPE;
    end;

    c := advance();
   end;

HANDLE_TYPE:
 if c in ['0'..'9'] then   (* enum or class name *)
   begin
     i := 0;
     repeat (* compute length *)
       i := i * 10 + (Ord(c) - Ord('0'));
       c := advance;
     until not(c in ['0'..'9']);

     (* In order to output only the name indicated, we fake
                   the unmangler by making it appear as though there
                   were nothing else left in the string. *)

     p := source;
     for len := i-1 downto 0 do
       begin
         assert(p^ <> #0);
         if (p - srcbase) > CONTENT_LEN then
           begin
             kind := kind or UM_HASHTRUNC;
             raise EFinishUnmangle.Create('');
           end;
         Inc(p);
       end;

  (* Output whether this class name was const or
                   volatile. *)

     if is_const <> 0 then copy_string('const ', 6);
     if is_volatile <> 0 then copy_string('volatile ', 9);

     savechar := (source + i)^;
     (source + i)^ := #0;

     copy_name(False);

     source^ := savechar;

     Exit;
   end;

 savechar := c;

 case c of
   'v': tname := 'void';
   'c': tname := 'char';
   'b': tname := 'wchar_t';
   's': tname := 'short';
   'i': tname := 'int';
   'l': tname := 'long';
   'f': tname := 'float';
   'd': tname := 'double';
   'g': tname := 'long double';
   'j': tname := '__int64';
   'o': tname := 'bool';
   'e': tname := '...';

   'M':    (* member pointer *)
       begin
        name := target;

        (* We call 'copy_type' because it knows how to extract length-prefixed names. *)

        advance;
        copy_type(target, False);

        len := target - name;
        if len > MAXBUFFLEN - 1 then len := MAXBUFFLEN - 1;
        StrLCopy(buff, name, len);
        buff[len] := #0;

        target := name;
       end;

   'r',    (* reference *)
   'p':    (* pointer *)
       begin
        c := advance;

        if c = 'q' then  (* function pointer *)
          begin
           copy_char('(');

           if savechar = 'M' then
             begin
              copy_string(buff, 0);
              copy_class_delimiter;
             end;

           copy_char('*');
           copy_char(')');

           savechar := c;
          end;

        copy_type(start, False);

        case savechar of
          'r': copy_char('&');
          'p':
              begin
               copy_char(' ');
               copy_char('*');
              end;
          'M':
              begin
               copy_char(' ');
               copy_string(buff, 0);
               copy_class_delimiter;
               copy_char('*');
              end;
        end;
       end;

   'a':    (* array *)
       begin
          i := 0;

          repeat
           c := advance();
           dims[i] := '[';
           Inc(i);
           if c = '0' then c := advance; (* 0 size means unspecified *)
           while c <> '$' do (* collect size, up to '$' *)
             begin
               dims[i] := c;
               Inc(i);
               c := advance;
             end;
             assert(c = '$');
             c := advance;
             dims[i] := ']';
             Inc(i);
          until c <> 'a'; (* collect all dimensions *)
          dims[i] := #0;
          copy_type(target, False);
          copy_string(dims, 0);
       end;

   'q':    (* function *)
       begin
          callconv := nil;
          regconv  := nil;

          (* We want the return type first, but find it last. So
                           we emit all but the return type, get the return type,
                           then shuffle to get them in the right place. *)

          while True do
            begin
               if advance <> 'q' then Break;
               case advance of
                 'c': callconv := '__cdecl ';
                 'p': callconv := '__pascal ';
                 'r': callconv := '__fastcall ';
                 'f': callconv := '__fortran ';
                 's': callconv := '__stdcall ';
                 'y': callconv := '__syscall ';
                 'i': callconv := '__interrupt ';
                 'g': regconv  := '__saveregs ';
               end;
            end;

          save_adjqual := adjust_quals;
          adjust_quals := False;

          copy_char('(');
          copy_args('$', False);
          copy_char(')');

          adjust_quals := save_adjqual;

          hasret := Integer(input = '$');
          if (hasret <> 0) then advance;

          if (hasret <> 0) or (callconv <> nil) or (regconv <> nil) then
            copy_return_type(start, callconv, regconv, hasret);
       end;

   else
       raise EFinishUnmangle.Create('Unknown type');
 end; // case

 if tname <> nil then
   begin
      if is_const    <> 0 then copy_string('const ', 6);
      if is_volatile <> 0 then copy_string('volatile ', 9);
      if is_signed   <> 0 then copy_string('signed ', 7);
      if is_unsigned <> 0 then copy_string('unsigned ', 9);

      if (not arglvl) or (savechar <> 'v') then copy_string(tname, 0);

      advance;
   end
 else
   begin
      if is_const <> 0 then copy_string(' const', 6);
      if is_volatile <> 0 then copy_string(' volatile', 9);
   end;
end;
{$HINTS ON}

procedure copy_delphi4args( argsend: Char; tmplargs: Boolean );
var
  c,termchar: char;
  first,i_T_passed: Boolean;
  argsbegin,start: PChar;
begin
 c := input();
 first := True;
 termchar := #0;

 while (c <> #0) and (c <> argsend) do
   begin
     if first then first := False
     else
        begin
          copy_char(',');
          copy_char(' ');
        end;

     argsbegin := source;
     start := target;

     advance;      (* skip the kind character *)

     i_T_passed := False;

     if c = 'T' then
        begin
          copy_string('<type ', 6);
          termchar := '>';
          i_T_passed := True;
        end
     else if c = 'i' then
        if (argsbegin^ = '4') and (StrLComp(argsbegin + 1, 'bool', 4) = 0) then
          begin
            if input = '0' then copy_string('false', 5)
                           else copy_string('true', 4);
            advance;
            i_T_passed := True;
          end;

     if not i_T_passed then
       case c of
         't': copy_type(target, not tmplargs);
         'j',
         'g',
         'e':
             begin
               copy_type(target, not tmplargs);
               target := start;
               assert(input = '$');
               advance();
               copy_until2('$', TMPLCODE);
               if termchar <> #0 then copy_char(termchar);
             end;

         'm':
             begin
               copy_type(target, not tmplargs);
               target := start;
               assert(input = '$');
               advance;
               copy_until1('$');
               copy_class_delimiter;
               copy_char('*');
               copy_until2('$', TMPLCODE);
             end;

         else
               raise EFinishUnmangle.Create('Unknown template arg kind');
       end; // case

     c := input;
     if c <> argsend then
       begin
         assert(c = '$');
         c := advance;
       end;
   end;
end;

procedure copy_args( argsend: Char; tmplargs: Boolean);
var
  c,termchar: Char;
  first,scanned,i_T_passed: Boolean;
  argsbegin,start: PChar;
  startidx,param_index,index: Integer;
  param_table: array[0..PTABLE_LEN-1] of TParamEntry;
begin
 c := input();
 first := True;
 param_index := 0;

 FillChar( param_table, sizeof(TParamEntry) * PTABLE_LEN, 0 );

 while (c <> #0) and (c <> argsend) do
   begin
     if first then first := False
     else
        begin
          copy_char(',');
          copy_char(' ');
        end;

      argsbegin := source;
      startidx  := srcindx;
      start     := target;

      param_table[param_index].targpos := target;

      scanned := False;

      while (c = 'x') or (c = 'w') do
      begin
       scanned := True;
       c := advance;
      end;

      if scanned and (c <> 't') then
      begin
       source  := argsbegin;
       srcindx := startidx;
      end;

      if c <> 't' then copy_type(target, not tmplargs)
      else
        begin
         c := advance;

         if c in ['0'..'9'] then index := Ord(c) - Ord('0')
                            else index := (Ord(c) - Ord('a')) + 10;
         Dec(index);

         assert(param_table[index].targpos <> nil);
         assert(param_table[index].len > 0);

         StrLCopy( buff, param_table[index].targpos, param_table[index].len);
         buff[param_table[index].len] := #0;
         copy_string(buff, 0);
         advance;
        end;

        param_table[param_index].len := target - param_table[param_index].targpos;
        Inc(param_index);

        c := input;

        if tmplargs and (c = '$') then (* non-type template argument *)
        begin
           termchar := #0;

           target := start;

           c := advance;
           advance;

           i_T_passed := False;

           if c = 'T' then
              begin
                copy_string('<type ', 6);
                termchar := '>';
                i_T_passed := True;
              end
           else if c = 'i' then
              if (argsbegin^ = '4') and (StrLComp(argsbegin + 1, 'bool', 4) = 0) then
                begin
                  if input = '0' then copy_string('false', 5)
                                 else copy_string('true', 4);
                  advance;
                  i_T_passed := True;
                end;

           if not i_T_passed then
             case c of
               'j',
               'g',
               'e':
                    begin
                      copy_until1('$');
                      if termchar <> #0 then copy_char(termchar);
                    end;

               'm':
                    begin
                      copy_until1('$');
                      copy_class_delimiter;
                      copy_char('*');
                      copy_until1('$');
                    end;
               else
                    raise EFinishUnmangle.Create('Unknown template arg kind');
             end; // case

           assert(input = '$');
           c := advance;
        end; // if
   end;
end;

{$HINTS OFF}
procedure copy_name( tmplname: Boolean );
label
  AFTER_CASE;
var
  c: Char;
  start: PChar;
  startidx,flags: Integer;
  save_setqual,isDelphi4name: Boolean;
begin
 c := input();
 isDelphi4name := False;

 (* Start outputting the qualifier names and the base name. *)

 while TRUE do
   begin
      if set_qual then basename := target;

      (* Examine the string to see what this is.  Either it's
                       a qualifier name, a member name, a function name, a
                       template name, or a special name.  We wouldn't be
                       here if this were a regular name. *)

      if c in ['0'..'9'] then
        begin
         (* If there's a number at the beginning of a name,
            it could only be a vtable symbol flag. *)

         flags := Ord(c) - Ord('0') + 1;

         vtbl_flags[0] := #0;

         if (flags and $01 <> 0) then StrCat(vtbl_flags, 'huge');

         if (flags and $02 <> 0) then
         begin
          if vtbl_flags[0] <> #0 then strcat(vtbl_flags, ', ');
          strcat(vtbl_flags, 'fastthis');
         end;

         if (flags and $04 <> 0) then
         begin
          if vtbl_flags[0] <> #0 then strcat(vtbl_flags, ', ');
          strcat(vtbl_flags, 'rtti');
         end;

         kind := (kind and not(UM_KINDMASK)) or UM_VTABLE;

         c := advance;
         assert((c = #0) or (c = '$'));
        end;

      case c of
        '@': {QUALIFIER} (* virdef flag or linker proc *)
             begin
               c := advance;
               if c = '$' then
                 begin
                    assert(advance = 'c');
                    assert(advance = 'f');
                    assert(advance = '$');
                    assert(advance = '@');

                    copy_string('__vdflg__ ', 10);
                    advance;
                    copy_name(False);

                    kind := kind or UM_VIRDEF_FLAG;
                 end
               else
                 begin
                    copy_string('__linkproc__ ', 13);
                    copy_name(False);

                    kind := kind or UM_LINKER_PROC;
                 end;
               Exit;
             end;

        '%': {TMPLCODE}  (* template name *)
             begin
               c := advance;
               if (c = 'S') or (c = 'D') then
                 if (StrLComp(source, 'Set$', 4) <> 0) or
                    (StrLComp(source, 'DynamicArray$', 13) <> 0) or
                    (StrLComp(source, 'SmallString$', 12) <> 0) or
                    (StrLComp(source, 'DelphiInterface$', 16) <> 0) then isDelphi4name := True;

               (* Output the base name of the template.  We use
                                       'copy_name' instead of 'copy_until', since
                                       this could be a template constructor name,
                                       for example. *)

               copy_name(True);

               assert(input = ARGLIST);
               advance();

               if (target - 1)^ = '<' then  copy_char(' ');

               copy_char('<');

               (* Copy the template arguments over.  Also, save
                                       the 'set_qual' variable, since we don't want
                                       to mix up the status of the currently known
                                       qualifier name with a name from a template
                                       argument, for example. *)

               save_setqual := set_qual;
               set_qual := False;

               if isDelphi4name then copy_delphi4args(TMPLCODE, True)
                                else copy_args(TMPLCODE, True);

               set_qual := save_setqual;

               if (target - 1)^ = '>' then copy_char(' ');
               copy_char('>');

               assert(input = TMPLCODE);
               advance();

               if input <> QUALIFIER then kind := kind or UM_TEMPLATE;
             end;

        '$': {ARGLIST} (* special name, or arglist *)
             begin
               if tmplname then Exit;

               c := advance;
               if c = 'x' then
                 begin
                  c := advance;
                  if (c = 'p') or (c = 't') then
                    begin
                     assert(advance = ARGLIST);
                     advance;
                     copy_string('__tpdsc__ ', 10);
                     copy_type(target, False);
                     kind := (kind and not(UM_KINDMASK)) or UM_TPDSC;
                     Exit;
                    end
                  else
                     raise EFinishUnmangle.Create('What happened?');
                 end;

               if c = 'b' then
                 begin
                    c := advance;
                    start    := source;
                    startidx := srcindx;

                    if ((c = 'c') or (c = 'd')) and (advance = 't') and (advance = 'r') then
                      begin
                       (* The actual outputting of the name will happen outside of this function,
                          to be sure that we don't include any special name characters. *)

                       //dimus - need one more advance here to skip last special name char
                       advance;

                       if c = 'c' then kind := (kind and (not UM_KINDMASK)) or UM_CONSTRUCTOR
                                  else kind := (kind and (not UM_KINDMASK)) or UM_DESTRUCTOR;

                       goto AFTER_CASE;
                      end;

                    source  := start;
                    srcindx := startidx;

                    copy_string('operator ', 9);

                    start := target;

                    copy_until1(ARGLIST);

                    target^ := #0;
                    target  := start;

                    copy_op(start);

                    kind := (kind and (not UM_KINDMASK)) or UM_OPERATOR;
                 end
               else if c = 'o' then
                 begin
                    advance;
                    copy_string('operator ', 9);
                    save_setqual := set_qual;
                    set_qual := False;
                    copy_type(target, False);
                    set_qual := save_setqual;
                    assert(input = ARGLIST);
                    kind := (kind and (not UM_KINDMASK)) or UM_CONVERSION;
                 end
               else if c = 'v' then
                 begin
                   c := advance;
                   if c = 's' then
                     begin
                       c := advance;
                       assert( (c = 'f') or (c = 'n') );
                       advance;
                       copy_string('__vdthk__', 9);
                       kind := (kind and (not UM_KINDMASK)) or UM_VRDF_THUNK;
                     end
                   else if c = 'c' then
                     begin
                       c := advance;
                       assert(c = '1');
                       c := advance;
                       assert(c = '$');
                       c := advance;

                       copy_string('__thunk__ [', 11);
                       kind := (kind and (not UM_KINDMASK)) or UM_THUNK;

                       copy_char(c);
                       copy_char(',');
                       while True do
                         begin
                           c := advance;
                           if c <> '$' then copy_char(c) else Break;
                         end;
                       copy_char(',');
                       while True do
                         begin
                           c := advance;
                           if c <> '$' then copy_char(c) else Break;
                         end;
                       copy_char(',');
                       while True do
                         begin
                           c := advance;
                           if c <> '$' then copy_char(c) else Break;
                         end;
                       copy_char(']');

                       advance; (* skip last '$' *)
                       Exit;
                     end;
                 end // else if c = 'v' then
               else
                 raise EFinishUnmangle.Create('Unknown special name');
             end;

        '_':
             begin
                 start    := source;
                 startidx := srcindx;

                 if advance = '$' then
                   begin
                     c := advance;

                      (* At the moment there are five kind of special names:

                         frndl  FL   friend list
                         chtbl  CH   catch handler table
                         odtbl  DC   object destructor table
                         thrwl  TL   throw list
                         ectbl  ECT  exception context table
                      *)

                     copy_char('_');
                     copy_char('_');

                      case (Word(source[0]) shl 8) or Word(source[1]) of
                        $464c: (* FL *)
                          begin
                             copy_string('frndl', 5);
                             kind := kind or UM_FRIEND_LIST;
                          end;
                        $4348: (* CH *)
                          begin
                             copy_string('chtbl', 5);
                             kind := kind or UM_CTCH_HNDL_TBL;
                          end;
                        $4443: (* DC *)
                          begin
                             copy_string('odtbl', 5);
                             kind := kind or UM_OBJ_DEST_TBL;
                          end;
                        $544c: (* TL *)
                          begin
                             copy_string('thrwl', 5);
                             kind := kind or UM_THROW_LIST;
                          end;
                        $4543: (* EC(T) *)
                          begin
                             copy_string('ectbl', 5);
                             kind := kind or UM_EXC_CTXT_TBL;
                          end;
                      end; // case

                      copy_char('_');
                      copy_char('_');
                      copy_char(' ');

                      while (c >= 'A') and (c <= 'Z') do c := advance;

                      assert(c = '$');
                      assert(advance = '@');
                      advance;

                      copy_name(False);

                      Exit;
                   end;

                 source  := start;
                 srcindx := startidx;
                 copy_until2(QUALIFIER, ARGLIST);
             end;

        else (* qualifier, member, plain *) // case
           copy_until2(QUALIFIER, ARGLIST);

      end; // big "case c of"

    AFTER_CASE:

      (* If we're processing a template name, then '$' is allowed to end the name. *)

      c := input;

      assert( (c = #0) or (c = QUALIFIER) or (c = ARGLIST) );

      if c = QUALIFIER then
        begin
          c := advance;
          if set_qual then
            begin
              prevqual := qualend;
              qualend  := target;
            end;
          copy_class_delimiter;
          if c = #0 then kind := (kind and (not UM_KINDMASK)) or UM_VTABLE;
        end
      else
        break; // out of big "while TRUE"

   end; // while TRUE

end;
{$HINTS ON}

 { --------------------------- Main function body ------------------------- }

{$HINTS OFF}
label
  NOT_PASCAL,FINISH;
var
  c: Char;
  p: PChar;
  i,len: Integer;
  start: PChar;
begin
  assert(maxlen <= MAXBUFFLEN);

 (* Quick check to see whether this name is even mangled or not. *)

  if src = nil then
    begin
      Result := UM_NOT_MANGLED;
      Exit;
    end;

  if dest = nil then
    begin
      Result := UM_ERROR;
      Exit;
    end;

  if src^ <> '@' then
    begin
      StrLCopy(dest, src, maxlen);
      dest[maxlen - 1] := #0;
      Result := UM_NOT_MANGLED;
      Exit;
    end;

 (* All mangled names begin with an '@' character. *)

  srcbase := src;
  Inc(src);     (* skip the initial '@' *)
  srcindx := 1;

 (* Slightly ugly code for turning an uppercase pascal name into a
           lowercase equivalent. *)

  len := StrLen(src);
  p   := src;
  for i := 0 to len-1 do
    if (p^ >= 'a') and (p^ <= 'z') then goto NOT_PASCAL
                                   else Inc(p);

  StrLower( src );

NOT_PASCAL:

 (* This is at LEAST a member name, if not a fully mangled
           template or function name.  So, begin outputting the
           subnames.  We set up the pointers in globals so that we don't
           have to pass everything around all the time. *)

  kind     := UM_UNKNOWN;
  source   := src;
  prevqual := nil;
  qualend  := nil;
  basename := nil;
  base_end := nil;
  set_qual := True;
  target   := dest;
  targbase := dest;
  targend  := targbase + (maxlen - 1);

 (* If anyone long jumps, it means a hash code was reached, the
           destination buffer reached its end, or the source buffer
           was terminated. *)

  try
     (* Start outputting the qualifier names and the base name. *)

      namebase := target;

      copy_name(False);
      set_qual := False;
      base_end := target;

      if ((kind and UM_KINDMASK) = UM_TPDSC) or ((kind and UM_SPECMASK) <> 0) then
        begin
          p := StrScan(namebase, ' ');
          namebase := p + 1;
        end;

      if ((kind and UM_KINDMASK) = UM_CONSTRUCTOR) or ((kind and UM_KINDMASK) = UM_DESTRUCTOR) then
         begin
            if (kind and UM_KINDMASK) = UM_DESTRUCTOR then copy_char('~');
            if prevqual = nil then start := namebase
                              else start := prevqual + 2;

            len := qualend - start;

            StrLCopy(buff, start, len);
            buff[len] := #0;
            copy_string(buff, len);
         end;

     (* If there's a function argument list, copy it over in expanded
               form. *)

      if (input = ARGLIST) and doArgs then (* function args *)
        begin
          c := advance;
          assert( (c = 'q') or (c = 'x') or (c = 'w') );

          (* Output the function parameters, and return type in
                           the case of template function specializations. *)

          set_qual     := False;
          adjust_quals := True;
          copy_type(namebase, False);

          if (kind and UM_KINDMASK) = UM_UNKNOWN then kind := kind or UM_FUNCTION;
        end
      else if (kind and UM_KINDMASK) = UM_UNKNOWN then kind := kind or UM_DATA
      else if vtbl_flags[0] <> #0 then
        begin
          copy_char(' ');
          copy_char('(');
          copy_string(vtbl_flags, 0);
          copy_char(')');
        end;
  except
      (* If we reached this exit point because the target did not contain enough space,
         or a hash code was reached, then output a trailer to let the user know that there
         was more data in the source string. *)

      if source^ <> #0 then
        begin
          if target + 3 < targend then
            begin
              copy_char('.');
              copy_char('.');
              copy_char('.');
            end
          else
            begin
              Dec(target); target^ := '.';
              Dec(target); target^ := '.';
              Dec(target); target^ := '.';
            end;
        end;
  end;

FINISH:
 (* Put some finishing touches on the kind of this entity. *)
 if qualend <> nil then kind := kind or UM_QUALIFIED;

 (* Put a terminator on the target. *)
 target^ := #0;

 (* If the user wanted the qaulifier and base name saved, then do it now. *)
 if (kind and UM_ERRMASK) = 0 then
   begin
     if qualP <> nil then
       begin
         qualP^ := #0;
         if (qualend <> nil) and (qualend <> nil) then
           begin
             len := qualend - namebase;
             StrLCopy(qualP, namebase, len);
             qualP[len] := #0;
           end;
       end;

     if baseP <> nil then
       begin
         baseP^ := #0;
         if (basename <> nil) and (base_end <> nil) then
           begin
             len := base_end - basename;
             StrLCopy(baseP, basename, len);
             baseP[len] := #0;
           end;
       end;
   end;

   Result := kind;
end;
{$HINTS ON}

end.

