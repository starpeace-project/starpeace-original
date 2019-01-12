unit DirectoryServerProtocol;

interface

  type
    TAccountId = string;

  const
    DIR_NOERROR                    = 0;
    DIR_ERROR_Unknown              = 1;
    DIR_ERROR_AccountAlreadyExists = 2;
    DIR_ERROR_UnexistingAccount    = 3;
    DIR_ERROR_InvalidAlias         = 6;
    DIR_ERROR_InvalidPassword      = 7;
    DIR_ERROR_AccountBlocked       = 8;

  const
    DIR_ACC_RegUser     = 0;
    DIR_ACC_BlockedUser = 2;

  function IsValidAlias( Alias : string ) : boolean;
  function GetUserPath( Alias : string ) : string;
  function GetAliasId( Alias : string ) :  string;


implementation

  uses
    SysUtils;

  function IsValidAlias( Alias : string ) : boolean;
    const
      AlphabetUpper   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      AlphabetLower   = 'abcdefghijklmnopqrstuvwxyz';
      Digits          = '0123456789';
      Specials        = '-_!()[]+=;,';

      ValidChars = AlphabetUpper + AlphabetLower + Digits + Specials;
    var
      i : integer;
    begin
      Alias := Trim( Alias );
      if (Length( Alias ) > 3)
        then
          begin
            i := 2;
            while (i <= Length( Alias )) and (Pos( Alias[i], ValidChars ) > 0) do
              inc( i );
            result := i > Length( Alias );
          end
        else result := false;
    end;

  function GetAliasId( Alias : string ) : string;
    var
      i : integer;
    begin
      result := Trim( Alias );
      for i := 1 to Length( result ) do
        if result[i] = ' '
          then result[i] := '.'
          else result[i] := result[i];
    end;

  function GetUserPath( Alias : string ) : string;
    var
      aID : string;
    begin
      aID    := GetAliasID( Alias );
      result := 'root/users/' + aID[1] + '/' + aID
    end;

end.
