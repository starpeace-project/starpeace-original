unit Editors;

interface

  uses
    EditableObjects, Controls, Forms;

  type
    CInPlaceVisualControl = class of TInPlaceVisualControl;
    TInPlaceVisualControl =
      class(TForm)
        public
          function  ProcessMessage( msg : integer; var data ) : boolean; virtual;
          procedure UpdateObject; virtual;
          procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); virtual;
        protected
          fEditableObject : TEditableObject;
        public
          property EditableObject : TEditableObject read fEditableObject;
      end;

    CObjectCreationWizard = class of TObjectCreationWizard;
    TObjectCreationWizard =
      class(TForm)
        public
          function CreateObject : TEditableObject; virtual; abstract;
      end;

    TNativeObjectEditor =
      class(TObjectEditor)
        public
          constructor Create( aName : string; controlClass : CInPlaceVisualControl; newObjWizardClass : CObjectCreationWizard );
          destructor  Destroy; override;
        public
          function  SendMessage( control : TObject; msg : integer; var data ) : boolean; override;
          procedure HideEditor( const visualcontext; visualEditor : TObject ); override;
          procedure ShowEditor( const visualcontext; visualEditor : TObject ); override;
          function  CreateVisualEditor( options : integer; const visualcontext; obj : TEditableObject ) : TObject; override;
          procedure UpdateObject( VisualEditor : TObject; obj : TObject ); override;
          function  CreateNewObject : TEditableObject; override;
          procedure DestroyEditor( editor : TObject ); override;
        protected
          fControlClass : CInPlaceVisualControl;
          fWizardClass  : CObjectCreationWizard;
          fMainControl  : TInPlaceVisualControl;
      end;

implementation

  // TInPlaceVisualControl

  function TInPlaceVisualControl.ProcessMessage( msg : integer; var data ) : boolean;
    begin
      result := false;
    end;

  procedure TInPlaceVisualControl.UpdateObject;
    begin
    end;

  procedure TInPlaceVisualControl.setEditableObject( aEditableObject : TEditableObject; options : integer );
    begin
      fEditableObject := aEditableObject;
    end;

  // TNativeObjectEditor

  constructor TNativeObjectEditor.Create( aName : string; controlClass : CInPlaceVisualControl; newObjWizardClass : CObjectCreationWizard );
    begin
      inherited Create;
      fWizardClass   := newObjWizardClass;  
      fControlClass  := controlClass;
      fName          := aName;
    end;

  destructor TNativeObjectEditor.Destroy;
    begin
      fMainControl.Free;
      inherited;
    end;

  procedure TNativeObjectEditor.HideEditor( const visualcontext; visualEditor : TObject );
    begin
      TInPlaceVisualControl(visualEditor).Hide;
    end;

  function TNativeObjectEditor.SendMessage( control : TObject; msg : integer; var data ) : boolean;
    begin
      result := TInPlaceVisualControl(control).ProcessMessage( msg, data );
    end;

  procedure TNativeObjectEditor.ShowEditor( const visualcontext; visualEditor : TObject );
    begin
      TInPlaceVisualControl(visualEditor).Show;
    end;

  function TNativeObjectEditor.CreateVisualEditor( options : integer; const visualcontext; obj : TEditableObject ) : TObject;
    var
      Parent  : TWinControl absolute visualcontext;
      Control : TInPlaceVisualControl;
    begin
      if (options and VOP_SHOWINMAINCONTROL) <> 0
        then
          begin
            if fMainControl = nil
              then
                begin
                  fMainControl := fControlClass.CreateParented( Parent.Handle );
                  fMainControl.Parent := Parent;
                  fMainControl.Align  := alClient;
                  fMainControl.Show;
                end;
            fMainControl.setEditableObject( obj, options );
            result := fMainControl;
          end
        else
          begin
            Control := fControlClass.CreateParented( Parent.Handle );
            Control.Parent := Parent;
            Control.Align  := alClient;
            Control.setEditableObject( obj, options );
            Control.Show;
            result := Control;
          end;
    end;

  procedure TNativeObjectEditor.UpdateObject( VisualEditor : TObject; obj : TObject );
    begin
      TInPlaceVisualControl(VisualEditor).UpdateObject;
    end;

  function TNativeObjectEditor.CreateNewObject : TEditableObject;
    var
      wizard : TObjectCreationWizard;
    begin
      result := nil;
      if fWizardClass <> nil
        then
          begin
            wizard := fWizardClass.Create( nil );
            try
              result := wizard.CreateObject;
            finally
              wizard.Free;
            end;
          end;
    end;

  procedure TNativeObjectEditor.DestroyEditor( editor : TObject );
    begin
      TInPlaceVisualControl(editor).Parent := nil;
      editor.Free;
    end;

end.
