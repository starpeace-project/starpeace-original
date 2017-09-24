unit NewStepClass;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Editors, EditableObjects, NativeEdObjects;

type
  TNewStepClassForm =
    class(TObjectCreationWizard)
    Notebook1: TNotebook;
    pnContainer: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    Button3: TButton;
    Button4: TButton;
    Panel5: TPanel;
    Label1: TLabel;
    edName: TEdit;
    Button6: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button6Click(Sender: TObject);
      public
        function CreateObject : TEditableObject; override;
      private
        fResult : TEditableObject;
        fEditor : TObject;
    end;

implementation

{$R *.DFM}

  // TNewStepClassForm

  function TNewStepClassForm.CreateObject : TEditableObject;
    var
      MetaSteps : TMetaEditableObject;
    begin
      fResult   := nil;
      MetaSteps := TheMetaObjectPool.get( 'MetaStep' );
      if MetaSteps <> nil
        then
          begin
            fResult := TNativeMetaEditableObject.Create;
            MetaSteps.Clone(  TNativeMetaEditableObject(fResult) );

            fResult.ObjectEditor := TheMetaObjectPool.getEditor( 'Step' );
            fEditor              := fResult.Edit( VOP_ALLOWPROPERTYEDITING, pnContainer );
            if (fEditor <> nil)
              then ShowModal;
          end;

      result := fResult;
    end;

  procedure TNewStepClassForm.Button2Click(Sender: TObject);
    begin
      if edName.Text <> ''
        then
          begin
            fResult.Name := edName.Text;
            fResult.ObjectEditor.UpdateObject( fEditor, fResult );
            ModalResult := mrOk;
          end;
    end;

  procedure TNewStepClassForm.Button1Click(Sender: TObject);
    begin
      fResult.Free;
      fResult := nil;
      ModalResult := mrCancel;
    end;

  procedure TNewStepClassForm.Button5Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 0;
    end;

  procedure TNewStepClassForm.Button3Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 1;
    end;

procedure TNewStepClassForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fEditor.Free;
end;

procedure TNewStepClassForm.Button6Click(Sender: TObject);
begin
  Notebook1.PageIndex := 0;
end;

end.
