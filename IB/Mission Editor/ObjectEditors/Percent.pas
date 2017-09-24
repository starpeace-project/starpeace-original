unit Percent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, EditableObjects, Editors;

type
  TPercentEditor =
    class(TInPlaceVisualControl)
        tbValue: TTrackBar;
    procedure tbValueChange(Sender: TObject);
      public
        procedure UpdateObject; override;
      protected
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
    end;

implementation

{$R *.DFM}

  procedure TPercentEditor.UpdateObject;
    begin
      EditableObject.Value := FloatToStr( tbValue.Position/100 );
    end;

  procedure TPercentEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    begin
      inherited;
      try
        tbValue.Position := trunc(100*StrToFloat(EditableObject.Value));
      except
        tbValue.Position := 50;
      end;
      tbValue.Hint := IntToStr(tbValue.Position) + '%';
    end;

  procedure TPercentEditor.tbValueChange(Sender: TObject);
  begin
    tbValue.Hint := IntToStr(tbValue.Position) + '%';
    UpdateObject;
  end;

end.
