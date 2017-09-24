unit SessionInterfaces;

interface

  uses
    Classes;

  const
    SESSION_NOERROR                = 0;
    SESSION_ERROR_Unknown          = 1;
    SESSION_ERROR_UnexpectedMember = 2;

  type
    IRDOSessionServer =
      interface
        // Connection handling
        function  Logon( Address : string; Port : integer; SessionId : string; out ErrorCode : integer ) : boolean;
        procedure Logoff( out ErrorCode : integer );
        procedure RegisterEventsHook( Hook : TObject );
        // Member iteration
        function  GetMemberCount( out ErrorCode : integer ) : integer;
        function  GetMemberInfoByIdx( index : integer; out ErrorCode : integer ) : TStringList;
        function  GetMemberInfoByName( Name : string; out ErrorCode : integer ) : TStringList;
        // Data interchange
        procedure SendMessage     ( Id : integer; Dest, Info : string; out ErrorCode : integer );
        procedure SetMemberCookie ( Name : string; Cookie, Value : string; out ErrorCode : integer );
        function  GetMemberCookie ( Name : string; Cookie : string; out ErrorCode : integer ) : string;
        procedure ModifyMemberCash( Name : string; Amount : currency; out ErrorCode : integer );
      end;

    IRDOSessionEvents =
      interface
        procedure RDONotifyMessage      ( Id : integer; Sender, Info : widestring );
        procedure RDONotifyMemberEntered( Name : widestring );
        procedure RDONotifyMemberLeaved ( Name : widestring );
        procedure RDONotifyCashChange   ( Value : currency );
      end;

    const
      tidRDOHook_SessionServer = 'SessionServer';
      tidRDOHook_SessionEvents = 'SessionEvents';
      tidMemberInfo_Name       = 'Name';
      tidMemberInfo_Cash       = 'Cash';

implementation

end.
