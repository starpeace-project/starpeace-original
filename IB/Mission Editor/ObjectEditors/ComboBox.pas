unit ComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, EditableObjects, Editors;

type
  TComboBoxEditor =
    class(TInPlaceVisualControl)
        cbValue: TComboBox;
        procedure FormResize(Sender: TObject);
    procedure cbValueChange(Sender: TObject);
      public
        procedure UpdateObject; override;
      public
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
    end;

implementation

  uses
    EditorEvents, Notifications;

{$R *.DFM}

  procedure TComboBoxEditor.FormResize(Sender: TObject);
    begin
      cbValue.SetBounds( 0, 0, ClientWidth, ClientHeight );
    end;

  procedure TComboBoxEditor.UpdateObject;
    begin
      fEditableObject.Value := cbValue.Text;
    end;

  procedure TComboBoxEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    var
      template : TEditableObject;
      Data     : TGetTemplateData;
    begin
      inherited;
      template := fEditableObject.getProperty( 'comboTemplate' );
      if template <> nil
        then
          begin
            Data.Name     := template.Value;
            Data.Template := cbValue.Items;
            DispatchEvent( evGetTemplates, Data );
          end;
      cbValue.Text := fEditableObject.Value;
    end;

  procedure TComboBoxEditor.cbValueChange(Sender: TObject);
    begin
      UpdateObject;
    end;

end.
