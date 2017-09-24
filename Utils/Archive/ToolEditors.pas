unit ToolEditors;

interface

//constructor Create(AComponent: TComponent; ADesigner: TFormDesigner);
//procedure Edit;
//procedure ExecuteVerb(Index: Integer);
//function GetVerb(Index: Integer): string;
//function GetVerbCount: Integer;
//procedure Copy;

  uses
    DsgnIntf;
(*
  type
    TSpeedbarEditor =
      class( TComponentEditor )
        procedure Edit;                                                                               override;
        procedure ExecuteVerb( Indx : integer );                                                      override;
        function  GetVerb(Indx : integer) : string;                                                   override;
        function  GetVerbCount : integer;                                                             override;
      end;

  procedure Register;
*)
implementation

  uses
    StrUtils, SysUtils;

  resourcestring
    SNewButton    = 'New B&utton';
    SNewSeparator = 'New Se&parator';

  const
    Verbs : array[0..1] of string = ( SNewButton, SNewSeparator );
(*
  procedure TSpeedbarEditor.Edit;
    begin

    end;

  procedure TSpeedbarEditor.ExecuteVerb( Indx : integer);
    begin
      case Indx of
        0 :
          CreateNewButton( Component as TSpeedbar, tbsButton );
        1 :
          CreateNewButton( Component as TSpeedbar, tbsSeparator );
      end;
    end;

  function TSpeedbarEditor.GetVerb( Indx : integer ) : string;
    begin
      Result := Verbs[Indx];
    end;

  function TSpeedbarEditor.GetVerbCount : integer;
    begin
      Result := High( Verbs ) - Low( Verbs ) + 1;
    end;

  procedure Register;
    begin
      RegisterComponentEditor( TSpeedbar, TSpeedbarEditor );
    end;
*)
end.
