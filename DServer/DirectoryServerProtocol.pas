unit DirectoryServerProtocol;

interface

  type
    TAccountId = string;

  const
    DIR_NOERROR_StillTrial         = -1;
    DIR_NOERROR                    = 0;
    DIR_ERROR_Unknown              = 1;
    DIR_ERROR_AccountAlreadyExists = 2;
    DIR_ERROR_UnexistingAccount    = 3;
    DIR_ERROR_SerialMaxed          = 4;
    DIR_ERROR_InvalidSerial        = 5;
    DIR_ERROR_InvalidAlias         = 6;
    DIR_ERROR_InvalidPassword      = 7;
    DIR_ERROR_AccountBlocked       = 8;
    DIR_ERROR_TrialExpired         = 9;
    DIR_ERROR_SubscriberIdNotFound = 10;

    // SEGA SNAP ERROR CODES   
    DIR_SEGA_NOERROR                 = 0;
    DIR_SEGA_ERROR_UserNotFound      = 1;
    DIR_SEGA_ERROR_BadUserIDFormat   = 2;
    DIR_SEGA_ERROR_UnknownDomain     = 3;
    DIR_SEGA_ERROR_InternalError     = 4;
    DIR_SEGA_ERROR_UserNotAuthorized = 5;
    DIR_SEGA_ERROR_SystemDown        = 6;
    DIR_SEGA_ERROR_NotInitialized    = 7;
    DIR_SEGA_ERROR_TimeOut           = 8;

  const
    DIR_ACC_RegUser     = 0;
    DIR_ACC_TrialUser   = 1;
    DIR_ACC_BlockedUser = 2;
    DIR_ACC_NoAuthUserA = 3;
    DIR_ACC_NoAuthUserB = 33;

  type
    TSerialFamilyId = (famRegular, famTester, famGameMaster, famTutor);

  var
    SerialFamilies : array[TSerialFamilyId] of extended =
      (0.1233, 0.1233, 0.1233, 0.1233);
//    (0.1233, 0.1233, 0.1233, 0.1233);
//    (0.3737, 0.1212, 0.5555, 0.9191);

  function GetFamily( AccountId : TAccountId; out FamilyId : TSerialFamilyId ) : boolean;
  function AuthenticAlias(Alias : string) : boolean;
  function IsValidAlias( Alias : string ) : boolean;
  function GetUserPath( Alias : string ) : string;
  function GetUserMapPath(Alias : string) : string;
  function GetAliasId( Alias : string ) :  string;


implementation

  uses
    SysUtils, GenIdd;

  function GetFamily( AccountId : TAccountId; out FamilyId : TSerialFamilyId ) : boolean;
    var
      EndOfArray : boolean;
    begin
      FamilyId := low(FamilyId);
      repeat
        result := HeavyIddCheck( AccountId, SerialFamilies[FamilyId] );
        EndOfArray := FamilyId = high(FamilyId);
        if not EndOfArray
          then inc( FamilyId );
      until result or EndOfArray;
    end;

  function AuthenticAlias(Alias : string) : boolean;
    const
      reserved : array[0..7] of string = ('oc_', 'gm_', 'sp_', 'oceanus', 'starpeace', 'support', 'gamemaster', 'sega');
    var
      i : integer;
    begin
      result := true;
      Alias  := LowerCase(Alias);
      i      := low(reserved);
      while (i <= high(reserved)) and result do
        begin
          result := result and (pos(reserved[i], Alias) = 0);
          inc(i);
        end;
    end;

  function IsValidAlias( Alias : string ) : boolean;
    const
      Alphabet   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      //ValidChars = Alphabet + '0123456789 -_:?!(){}[]<>+=&@#$%^&|;,';
      ValidChars = Alphabet + '0123456789 -_!()[]+=#;,';
    var
      i : integer;
    begin
      Alias := Trim( Alias );
      if (Length( Alias ) > 0) and (Pos( UpCase( Alias[1] ), Alphabet ) > 0) and AuthenticAlias(Alias)
        then
          begin
            i := 2;
            while (i <= Length( Alias )) and (Pos( UpCase( Alias[i] ), ValidChars ) > 0) do
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
          else result[i] := UpCase( result[i] );
    end;

  function GetUserPath( Alias : string ) : string;
    var
      aID : string;
    begin
      aID    := GetAliasID( Alias );
      result := 'Root/Users/' + aID[1] + '/' + aID
    end;

  function GetUserMapPath(Alias : string) : string;
    var
      aID : string;
    begin
      aID    := GetAliasID( Alias );
      result := 'Root/SegaUsers/' + aID[1] + '/' + aID
    end;
     

end.
