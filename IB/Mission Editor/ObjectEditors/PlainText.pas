unit PlainText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Editors, EditableObjects;

type
  TPlainTextEditor =
    class(TInPlaceVisualControl)
        edValue: TEdit;
        procedure FormResize(Sender: TObject);
    procedure edValueChange(Sender: TObject);
      public
        procedure UpdateObject; override;
      public
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
    end;

var
  PlainTextEditor: TPlainTextEditor;

implementation

{$R *.DFM}

  procedure TPlainTextEditor.FormResize(Sender: TObject);
    begin
      edValue.SetBounds( 0, 0, ClientWidth, ClientHeight );
    end;

  procedure TPlainTextEditor.UpdateObject;
    begin
      fEditableObject.Value := edValue.Text;
    end;

  procedure TPlainTextEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    begin
      inherited;
      edValue.Text := fEditableObject.Value;
    end;

  procedure TPlainTextEditor.edValueChange(Sender: TObject);
    begin
      UpdateObject;
    end;

end.
