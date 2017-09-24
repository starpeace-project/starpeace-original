unit Protocol;

interface

  type
    TVisualClassId  = word;
    TCompanyId      = word;

  const
    NullObject = 0;

  const
    MapChunkSize = 64;

  const
    RepSeparator        = ':';
    ScopeSeparator      = '::';
    StatusTextSeparator = ':-:';
    LineBreak           = #13#10;
    TabSpace            = #9;

  const
    BINSignature : word = $F0F0;


  // Error codes

  const
    NOERROR                    = 0;
    ERROR_Unknown              = 1;
    ERROR_CannotInstantiate    = 2;
    ERROR_AreaNotClear         = 3;
    ERROR_UnknownClass         = 4;
    ERROR_UnknownCompany       = 5;
    ERROR_UnknownCluster       = 6;
    ERROR_UnknownTycoon        = 7;
    ERROR_CannotCreateTycoon   = 8;
    ERROR_FacilityNotFound     = 9;
    ERROR_TycoonNameNotUnique  = 10;
    ERROR_CompanyNameNotUnique = 11;
    ERROR_InvalidUserName      = 12;
    ERROR_InvalidPassword      = 13;
    ERROR_InvalidCompanyId     = 14;
    ERROR_AccessDenied         = 15;
    ERROR_CannotSetupEvents    = 16;
    ERROR_AccountActive        = 17;
    ERROR_AccountDisabled      = 18;
    ERROR_InvalidLogonData     = 19;
    ERROR_ModelServerIsDown    = 20;
    ERROR_UnknownCircuit       = 21;
    ERROR_CannotCreateSeg      = 22;
    ERROR_CannotBreakSeg       = 23;
    ERROR_LoanNotGranted       = 24;
    ERROR_InvalidMoneyValue    = 25;
    ERROR_InvalidProxy         = 26;
    ERROR_RequestDenied        = 27;
    ERROR_ZoneMissmatch        = 28;
    ERROR_InvalidParameter     = 29;
    ERROR_InsuficientSpace     = 30;
    ERROR_CannotRegisterEvents = 31;
    ERROR_NotEnoughRoom        = 32;
    ERROR_TooManyFacilities    = 33;
    ERROR_BuildingTooClose     = 34;


  // Politics errors

    ERROR_POLITICS_NOTALLOWED = 100;
    ERROR_POLITICS_REJECTED   = 101;
    ERROR_POLITICS_NOTIME     = 102;

  // Logon Error
    ERROR_AccountAlreadyExists  = 110;
    ERROR_UnexistingAccount     = 112;
    ERROR_SerialMaxed           = 113;
    ERROR_InvalidSerial         = 114;
    ERROR_SubscriberIdNotFound  = 115;

  // Account status

  const
    ACCOUNT_Valid           = 0;
    ACCOUNT_UnknownError    = 1;
    ACCOUNT_Unexisting      = 2;
    ACCOUNT_InvalidName     = 3;
    ACCOUNT_InvalidPassword = 4;
    ACCOUNT_Forbiden        = 5;


  // Supported circuits identifiers

  const
    cirRoads     = 1;
    cirRailRoads = 2;


  // Actor pools

  const
    poolIdTrains       = 1;
    poolTrainsInterval = 1000;



  // RDO Hooks

  const
    tidRDOHook_InterfaceServer = 'InterfaceServer';
    tidRDOHook_NewsServer      = 'NewsServer';
    tidRDOHook_InterfaceEvents = 'InterfaceEvents';
    tidRDOHook_Trains          = 'Trains';

  const
    tidLastViewX = 'LastX.';
    tidLastViewY = 'LastY.';

  type
    TStatusKind          = (sttMain, sttSecondary, sttHint);
    TFacilityChange      = (fchStatus, fchStructure, fchDestruction);
    TUserListChange      = (uchInclusion, uchExclusion);
    TMsgCompositionState = (mstIdle, mstComposing, mstAFK);
    TCargoKind           = (carPeople, carLight, carHeavy);
    TErrorCode           = integer;

  const
    CompositionTimeOut = 10000;


  // IRDOInterfaceServerEvents
  // -------------------------
  //
  // Client events interface (method implementation MUST be published)
  // The following interface, IRDOInterfaceServerEvents, can be used to enforce
  // the existence of these methods in the object registered under the
  // tidRDOHook_InterfaceEvents hook.
  //
  // procedure InitClient( Date : TDateTime; Money : widestring; FailureLevel : integer );
  //   :: This is called by the IS after a succesfull log on to initialize data in
  //      the client. Date is the current (virtual) date, Money the current budget
  //      of the user and FailureLevel and indicator of bankrupcy (0-Normal, 1-Warning,
  //      2-ALERT!)
  //
  // procedure RefreshArea( x, y, dx, dy : integer );
  //   :: This is called by the IS to refresh some dirty area in the map.
  //
  // procedure RefreshObject( ObjId, KindOfChange : integer );
  //   :: Indicates that the focused object ObjId has been modified so its image
  //      in the client must be updated. KindOfChange is a TFacilityChange.
  //
  // procedure RefreshTycoon( Money : widestring );
  //   :: The user (or tycoon) data was modified.
  //
  // procedure RefreshDate( Date : TDateTime );
  //   :: Virtual date changed.
  //
  // procedure EndOfPeriod( FailureLevel : integer );
  //   :: Indicates the conclusion of an economic period. Tax collection and other
  //      events occur then. FailureLevel must be updated (see InitClient)      
  //
  // procedure TycoonRetired( FailureLevel : integer );
  //   :: Notifies the user that he or she was widthdrawn from the game. That is,
  //      when he or she loses the game.
  //
  // procedure ChatMsg( From, Msg : widestring );
  //   :: A chat msg was received. From is the name of the user the message comes
  //      from. Msg is the message itself.
  //
  // procedure MoveTo( x, y : integer );
  //   :: The IS commands the client to display the cell (x, y).
  //
  // procedure NotifyCompanionship( Names : widestring );
  //   :: The IS notifies a change in the Companionship list. Companionship are viewing
  //      the same area (or part of it) in the map. The list uses #13#10 as separators.


  // Notification Kinds

  const
    ntkMessageBox   = 0;
    ntkURLFrame     = 1;
    ntkChatMessage  = 2;
    ntkSound        = 3;
    ntkGenericEvent = 4;

  const
    gevnId_RefreshBuildPage = 1;

  // Model status

  const
    mstBusy    = $00000001;
    mstNotBusy = $00000002;
    mstError   = $00000004;

  type
    IRDOInterfaceServerEvents =
      interface
        procedure InitClient( Date : TDateTime; Money : widestring; FailureLevel, TycoonId : integer );
        procedure RefreshArea( x, y, dx, dy : integer; ExtraInfo : widestring );
        procedure RefreshObject( ObjId, KindOfChange : integer; ExtraInfo : widestring );
        procedure RefreshTycoon( Money, NetProfit : widestring; Ranking, FacCount, FacMax : integer );
        procedure RefreshDate( Date : TDateTime );
        procedure RefreshSeason( Season : integer );
        procedure EndOfPeriod( FailureLevel : integer );
        procedure TycoonRetired( FailureLevel : integer );
        procedure ChatMsg( From, Msg : widestring );
        procedure VoiceMsg( From, Msg : widestring; TxId, NewTx : integer );
        procedure VoiceRequestGranted( RequestId : integer );
        procedure NewMail( MsgCount : integer );
        procedure MoveTo( x, y : integer );
        procedure NotifyCompanionship( Names : widestring );
        procedure NotifyUserListChange( Name : widestring; Change : TUserListChange ); 
        procedure NotifyChannelListChange( Name, Password : widestring; Change : TUserListChange );
        procedure NotifyChannelChange( Name : widestring );
        procedure NotifyMsgCompositionState( Name : widestring; State : TMsgCompositionState );
      {$ifDef Train}
        procedure ActorPoolModified( ActorPoolId : integer; Data : widestring );
      {$endif}
        procedure ShowNotification( Kind : integer; Title, Body : widestring; Options : integer );
        procedure ModelStatusChanged( Status : integer ); 
        function  AnswerStatus : olevariant;
      end;


  // InterfaceServer
  // ---------------
  //
  // The following properties and methods can be called via RDO to the Interface Server
  // object (registered under tidRDOHook_InterfaceServer):
  //
  // property WorldName : string
  //   :: Contains the name of the world
  //
  // property WorldURL : string
  //   :: Base URL for this world. All relative URLs are completed by adding this
  //      string to the begining (no "/" is needed.)
  //
  // property WorldXSize : integer
  //   :: Width of the world, in ground cells.
  //
  // property WorldYSize : integer
  //   :: Height of the world, in ground cells.
  //
  // function AccountStatus( UserName, Password : widestring ) : OleVariant;
  //   :: Returns the account status (see ACCOUNT_XXXX)
  //
  // function Logon( UserName, Password : widestring ) : OleVariant;
  //   :: Logs on the user UserName. Returns an Id to a ClientView object.
  //

  // ClientView
  // ----------
  //
  // For each client logged to the IS there is one ClientView object. The Id
  // of the ClientView is obtained as a result of a call to the Logon function
  // to the InterfaceServer object. The following properties and methods can
  // be ivoked to a ClientView via RDO.
  //
  // property UserName : string
  //   :: Name that was used to log on.
  //
  // property x1, y1, x2, y2 : integer
  //   :: Portion of the map the client is currently viewing.
  //
  // procedure SetViewedArea( x, y, dx, dy : integer );
  //   :: Informs the ClientView that the Client has moved to a diferent location
  //      in the map. (x, y) is the origin of the viewport, (dx, dy) is its size.
  //
  // function ObjectsInArea( x, y, dx, dy : integer ) : OleVariant;
  //   :: Returns the objects that are in somre area of the map. The result is a
  //      widestring in the form:
  //
  //      result = obj1 + #13#10 + obj2 + #13#10 + ... + objN
  //
  //      where:
  //        obj1 = classId + #13#10 + companyId + #13#10 + xPos + #13#10 + yPos
  //
  //        where:
  //          classId   : integer(ASCII) :: Id of the visual class of the object
  //          companyId : integer(ASCII) :: Id of the company the object belongs to
  //          xPos      : integer(ASCII) :: x coordinate of the object
  //          yPos      : integer(ASCII) :: y coordinate of the object
  //
  // function ObjectAt( x, y : integer ) : OleVariant;
  //   :: Returns the object Id (integer) of the object at location (x, y).
  //
  // function ObjectStatusText( kind : TStatusKind; Id : TObjId ) : OleVariant;
  //   :: Returns the status text of the object specified in the Id parameter.
  //
  // procedure FocusObject( Id : TObjId );
  //   :: Focuses the object specified in the Id parameter. After calling FocusObject
  //      over one object, the RefreshObject event will occur every time the object is
  //      modified.
  //
  // procedure UnfocusObject( Id : TObjId );
  //   :: Removes the object from the focus list.
  //
  // function SwitchFocus( From : TObjId; toX, toY : integer ) : OleVariant;
  //   :: SwitchFocus encapsulates calls to ObjectAt, FocusObject and UnfocusObject
  //      in the server side. It can be replaced in the client side by:
  //
  //      if Focus <> NullObject
  //        then ClientViewProxy.UnfocusObject( Focus );
  //      Focus := ClientViewProxy.ObjectAt( xTo, yTo );
  //      if Focus <> NullObject
  //        then ClientViewProxy.FocusObject( Focus );
  //
  // function GetCompanyList : OleVariant;
  //   :: Returns a widestring containing a list of the companies the user has.
  //      the result is in the form:
  //
  //      '[' + Company1_Name + ',' + Company1_Id + ']' + ... + '[' + CompanyN_Name + ',' + CompanyN_Id + ']'
  //
  //      where:
  //        Company_Name : string         :: Readable name of the company.
  //        Company_Id   : integer(ASCII) :: Id of the company (same as in ObjectsInArea)
  //
  //      NOTE: An empty company list is '[]'
  //
  // function NewCompany( name, cluster : widestring ) : OleVariant;
  //   :: Creates a new company for the user. The result, if succesfull, is a company pair
  //      is same format as GetCompanyList.
  //
  // function NewFacility( FacilityId : widestring; CompanyId : integer; x, y : integer ) : OleVariant;
  //   :: Creates a new facility on the map. FacilityId is the MetaFacilityId (or ClassId)
  //      of the facility, CompanyId is the Id of the Company that owns the facility,
  //      x and y the position in the map. The result can be one of the following codes:
  //        NOERROR, ERROR_Unkbown, ERROR_AreaNotClear, ERROR_UnknownClass, ERROR_UnknownCompany.
  //
  // function SayThis( Dest, Msg : widestring ) : OleVariant;
  //   :: Sends a chat message to the users listed in Dest. Usernames must be sepparated
  //      by ';'. An empty Dest generates a message that is received by everyone.
  //
  // function Chase( UserName : widestring ) : OleVariant;
  //   :: Starts the chase of one user by linking the client's viewport to this user's
  //      viewport.
  //
  // function StopChase : OleVariant;
  //   :: Stops the chase.
  //
  // function RegisterEvents( ClientAddress : widestring; ClientPort : integer ) : OleVariant;
  //   :: Creates a proxy of the tidRDOHook_InterfaceEvents object running in the Client RDO
  //      server. ClientAddress and ClientPort are the ones of this RDO server.  
  //
  // function  Logoff : OleVariant;
  //   :: Logs off the user.
  //

  type
    TSecurityId = string;

  const
    SecIdItemSeparator = '-';

  function GrantAccess( RequesterId, SecurityId : TSecurityId ) : boolean;

  
  // Building zones

  const
    tidSurface_Zones = 'ZONES';
    tidSurface_Towns = 'TOWNS';

  type
    TZoneType = byte;

  const
    znNone           = 0;
    znReserved       = 1;
    znResidential    = 2;
    znHiResidential  = 3;
    znMidResidential = 4;
    znLoResidential  = 5;
    znIndustrial     = 6;
    znCommercial     = 7;
    znCivics         = 8;
    znOffices        = 9;

  function ZoneMatches( ZoneA, ZoneB : TZoneType ) : boolean;

  // Events

  const
    tidEventField_Date = 'Date';
    tidEventField_Text = 'Text';
    tidEventField_Kind = 'Kind';
    tidEventField_URL  = 'URL';

  const
    tidInvention_WaterQuest = 'WaterQuest';

  // Cookies

  function ComposeLinkCookie( name : string; x, y : integer; select : boolean ) : string;
  function ParseLinkCookie( link : string; out name : string; out x, y : integer; out select : boolean ) : boolean;

  // AccountDesc

  const
    AccDesc_NobMask = $0000FFFF;
    AccDesc_ModMask = $FFFF0000;

  const
    AccMod_UnknownUser = $8000;
    AccMod_Support     = $0001; 
    AccMod_Developer   = $0002; 
    AccMod_Publisher   = $0004;
    AccMod_Ambassador  = $0008;
    AccMod_GameMaster  = $0010;
    AccMod_Trial       = $0020;
    AccMod_Newbie      = $0040;
    AccMod_Veteran     = $0080;

  function  ComposeAccDesc( Nobility, Modifiers : integer ) : integer;
  procedure BreakAccDesc( AccDesc : integer; out Nobility, Modifiers : word );

  function ComposeChatUser(const name : string; const id: cardinal; const AFK: boolean) : string;
  function ParseChatUser(const ChatUser : string; out name : string; out id: cardinal;out AFK: boolean): boolean;

  const
    tidDSId_Nobpoints = 'NobPoints';

implementation

  uses
    SysUtils, CompStringsParser, mr_StrUtils, MathUtils;

  function GrantAccess( RequesterId, SecurityId : TSecurityId ) : boolean;
    begin
      result := system.pos( SecIdItemSeparator + RequesterId + SecIdItemSeparator, SecurityId ) > 0;
    end;

  function ZoneMatches( ZoneA, ZoneB : TZoneType ) : boolean;
    begin
      case ZoneA of
        znNone :
          result := true;
        znReserved :
          result := true;//false;
        znResidential :
          result := ZoneB in [znHiResidential, znMidResidential, znLoResidential];
        else
          result := (ZoneB = ZoneA) or (ZoneB = znNone)
      end;
    end;

  function ComposeLinkCookie( name : string; x, y : integer; select : boolean ) : string;
    begin
      result := name + ',' + IntToStr(x) + ',' + IntToStr(y) + ',' + IntToStr(integer(select));
    end;

  function ParseLinkCookie( link : string; out name : string; out x, y : integer; out select : boolean ) : boolean;
    var
      p : integer;
    begin
      if link <> ''
        then
          try
            p := length(link);
            select := boolean(StrToInt(GetPrevStringUpTo( link, p, ',' )));
            y      := StrToInt(GetPrevStringUpTo( link, p, ',' ));
            x      := StrToInt(GetPrevStringUpTo( link, p, ',' ));
            name   := copy(link, 1, p-1);
            result := true;
          except
            result := false;
          end
        else result := false;
    end;

  function ComposeAccDesc( Nobility, Modifiers : integer ) : integer;
    begin
      Nobility := min( Nobility, AccDesc_NobMask );       
      result := Nobility and AccDesc_NobMask or (Modifiers shl 16 and AccDesc_ModMask);
    end;

  procedure BreakAccDesc( AccDesc : integer; out Nobility, Modifiers : word );
    begin
      Nobility  := AccDesc and AccDesc_NobMask;
      Modifiers := (AccDesc and AccDesc_ModMask) shr 16;
    end;

  function ComposeChatUser(const name : string; const id: cardinal; const AFK: boolean) : string;
    var
      AFKNumber : integer;
    begin
      if AFK
      then
        AFKNumber := 1
      else
        AFKNumber := 0;
      result := format('%s/%d/%d', [name, id, AFKNumber]);
    end;

  function ParseChatUser(const ChatUser : string; out name : string; out id: cardinal;out AFK: boolean): boolean;
    var
      i : integer;
      s : string;
    begin
      if trim(ChatUser)<>''
        then
          begin
            i := system.pos('/', ChatUser);
            if i>0
              then
                begin
                  name := trim(copy(ChatUser, 1, i-1));
                  try
                    s := copy(ChatUser, i+1, length(ChatUser)-i+1);
                    i := system.pos('/', s);
                    if (i>0)
                      then
                        begin
                          id := strtoint(copy(s, 1, i-1));
                          AFK := s[length(s)]='1';
                        end
                      else id := strtoint(s);
                  except
                    id := 0;
                  end;
                end
              else
                begin
                  name := ChatUser;
                  id := 0;
                end;
            result := name<>'';
          end
        else result := false;
    end;


end.

