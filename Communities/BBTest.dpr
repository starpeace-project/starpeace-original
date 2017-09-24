{$R *.TLB}

{$R *.TLB}

{$R *.TLB}

program BBTest;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  BBoard in 'BBoard.pas';

procedure RunTest;
  var
    BB : TBBoard;
    i  : integer;
    j  : integer;
    s  : string;
  begin
    BB := TBBoard.Create;
    {
    for i := 0 to 9 do
      BB.NewItem( 'Cepero', 'Test1...' + IntToStr(i), 'X:\Forums\', 'I3 = ' + IntToStr(i), true );
    }
    for i := 0 to -1 do
      BB.NewItem( 'Cepero', 'Extra messages' + IntToStr(i), 'X:\Forums\Cepero}Test1...7}CeperoZYCAELDQBHBAEZ.top', 'X3 = ' + IntToStr(i), false );
    //BB.Open( 'S:\Five web\Salsa\Bulletines\Trade\', true );
    BB.Open( 'S:\Five web\Salsa\Bulletines\Trade\Administrator}Other Test}AdministratorZYCAMOHMBAUYEB.topic', false);
    readln;
    writeln( BB.Count );
    for i := 0 to pred(BB.Count) do
      begin
        //s := BB.GetAuthor( i ) + '   ' + BB.GetTitle( i ) + '   ' + BB.GetId( i );
        s := BB.GetAttachment(i);
        writeln( s );
        {
        if i mod 3 = 0
          then
            for j := 1 to 10 do
              BB.NewItem( 'Lola', 'My Message (3)' + IntToStr(j), BB.GetId( i ), 'Message = ' + IntToStr(j), false );
        }
        if (i + 1) mod 23 = 0
          then
            begin
              writeln;
              readln;
            end
      end;
    readln;
  end;


begin
  RunTest;
end.