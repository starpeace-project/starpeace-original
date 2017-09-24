program SocketPtch;

uses
  Forms,
  SocketComp in '..\SocketComp.pas',
  ServerSocketFrm in 'ServerSocketFrm.pas' {ServerSocketFrom},
  MathUtils in '..\..\misc\MathUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerSocketFrom, ServerSocketFrom);
  Application.Run;
end.
