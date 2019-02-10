library Rankings;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  View-Project Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the DELPHIMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using DELPHIMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  SysUtils,
  Classes,
  Daemons in 'Daemons.pas',
  RankingsDaemon in 'RankingsDaemon.pas',
  DirServerSession in 'DirServerSession.pas',
  RankProtocol in '..\Protocol\RankProtocol.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  Logs in '..\Logs\Logs.pas',
  DirectoryServerProtocol in '..\Directory Server\DirectoryServerProtocol.pas',
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  RDOInterfaces in '..\Rdo\Common\RDOInterfaces.pas',
  RDOProtocol in '..\Rdo\Common\RDOProtocol.pas',
  RDOObjectProxy in '..\Rdo\Client\RDOObjectProxy.pas',
  ErrorCodes in '..\Rdo\Common\ErrorCodes.pas',
  RDOMarshalers in '..\Rdo\Client\RDOMarshalers.pas',
  RDOUtils in '..\Rdo\Common\RDOUtils.pas',
  WinsockRDOConnection in '..\Rdo\Client\WinsockRDOConnection.pas',
  SmartThreads in '..\Kernel\SmartThreads.pas',
  SocketComp in '..\Utils\Network\SocketComp.pas',
  Collection in '..\Kernel\Collection.pas',
  SpoolPackets in '..\Utils\Network\SpoolPackets.pas';

exports
  CreateDaemon;

begin
end.
 
