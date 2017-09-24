unit NewStepCreator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Editors, EditableObjects;

type
  TNewStepForm =
    class(TObjectCreationWizard)
        Notebook1: TNotebook;
        Button1: TButton;
        Button2: TButton;
        pnContainer: TPanel;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Label1: TLabel;
        edName: TEdit;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
      public
        function CreateObject : TEditableObject; override;
      private
        fResult       : TEditableObject;
        fEditor       : TObject;
        fObjectEditor : TObjectEditor;
    end;

implementation

  uses
    NativeEdObjects;

{$R *.DFM}

  // TNewStepForm

  function TNewStepForm.CreateObject : TEditableObject;
    var
      MetaSteps : TMetaEditableObject;
    begin
      fResult   := nil;
      MetaSteps := TheMetaObjectPool.get( 'MetaStep' );
      if MetaSteps <> nil
        then
          begin
            fObjectEditor := MetaSteps.ObjectEditor;
            fEditor       := MetaSteps.Edit( VOP_EDITCHILDREN, pnContainer );
            if (fEditor <> nil) and (fObjectEditor <> nil)
              then ShowModal;
          end;

      result := fResult;
    end;

  procedure TNewStepForm.Button1Click(Sender: TObject);
    begin
      fResult     := nil;
      ModalResult := mrCancel;
    end;

  procedure TNewStepForm.Button2Click(Sender: TObject);
    var
      selected : TMetaEditableObject;
      useless  : integer;
    begin
      if fObjectEditor.SendMessage( fEditor, MSG_GETSELOBJECT, selected ) and (selected <> nil)
        then
          begin
            if fResult <> nil
              then fResult.Free;
            fResult := selected.Instantiate( '', '', useless );
            Notebook1.PageIndex := 1;
          end;
    end;

  procedure TNewStepForm.Button5Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 0;
    end;

  procedure TNewStepForm.Button3Click(Sender: TObject);
    begin
      if edName.Text <> ''
        then
          begin
            fResult.Name := edName.Text;
            ModalResult := mrOk;
          end
        else ShowMessage( 'Enter the name of the Step' );
    end;

procedure TNewStepForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fEditor.Free;
end;

end.
