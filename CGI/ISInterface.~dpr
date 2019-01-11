library ISInterface;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  SysUtils,
  ComObj,
  ActiveX,
  Classes;

function GetWorldData( const ISAddr : string; ISPort : integer; out WorldName : string; out WorldPop, UserCount, Year, OnlineUsers : integer ) : boolean; export;
  var
    RDOConn : olevariant;
    Obj     : olevariant;
  begin
    RDOConn := CreateOleObject( 'RDOClient.WinSockRDOConnection' );
    RDOConn.Server := ISAddr;
    RDOConn.Port := ISPort;
    if RDOConn.Connect( 5000 )
      then
        begin
          Obj := CreateOleObject( 'RDOClient.RDOObjectProxy' );
          Obj.SetConnection( RDOConn );
          if Obj.BindTo( 'InterfaceServer' )
            then
              begin
                WorldName   := Obj.WorldName;
                WorldPop    := Obj.WorldPopulation;
                UserCount   := Obj.UserCount;
                Year        := Obj.WorldYear;
                OnlineUsers := 12;
                result := true;
              end
            else result := false;
        end
      else result := false
  end;

exports
  GetWorldData;
{$R *.RES}

var
  useless : pointer;
begin
  useless := nil;
  OleInitialize( useless );
end.
