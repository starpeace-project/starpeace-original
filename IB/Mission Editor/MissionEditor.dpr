program MissionEditor;

uses
  Forms,
  EditorMain in 'EditorMain.pas' {EditorMainForm},
  EditableObjects in 'EditableObjects.pas',
  Collection in '..\..\Kernel\Collection.pas',
  NativeEdObjects in 'NativeEdObjects.pas',
  FileName in 'ObjectEditors\FileName.pas' {FilenameEditor},
  Percent in 'ObjectEditors\Percent.pas' {PercentEditor},
  ComboBox in 'ObjectEditors\ComboBox.pas' {ComboBoxEditor},
  Editors in 'Editors.pas',
  VisualControls in '..\..\Voyager\Components\VisualControls.pas',
  PlainText in 'ObjectEditors\PlainText.pas' {PlainTextEditor},
  ObjectInspector in 'ObjectEditors\ObjectInspector.pas' {ObjectInspectorEditor},
  ObjectContainer in 'ObjectEditors\ObjectContainer.pas' {ObjectContainerEditor},
  InterfaceCollection in '..\..\Kernel\InterfaceCollection.pas',
  NewStepCreator in 'ObjectEditors\NewStepCreator.pas' {NewStepForm},
  NewProp in 'ObjectEditors\NewProp.pas' {NewPropForm},
  EditorEvents in 'EditorEvents.pas',
  NewStepClass in 'NewStepClass.pas' {NewStepClassForm},
  stringutils in '..\Utils\stringutils.pas',
  XMLFile in '..\Utils\XMLFile.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TEditorMainForm, EditorMainForm);
  Application.Run;
end.
