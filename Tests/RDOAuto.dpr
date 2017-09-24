{$APPTYPE CONSOLE}

program RDOAuto;

uses
  ComObj;

procedure DoThings;
  var
    o : OleVariant;
  begin
    o := CreateOleObject( 'RDOClient.ObjectProxy' );
  end;

begin
  DoThings;
end.
 