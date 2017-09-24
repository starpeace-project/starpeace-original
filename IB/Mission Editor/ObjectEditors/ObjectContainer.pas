unit ObjectContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ComCtrls, EditableObjects, Editors;

type
  TObjectContainerEditor =
    class(TInPlaceVisualControl)
        lvItems: TListView;
        Images: TImageList;
        procedure lvItemsDblClick(Sender: TObject);
      public
        procedure UpdateObject; override;
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
        function  ProcessMessage( msg : integer; var data ) : boolean; override;
      private
        fEditingChilds : boolean;
        procedure Clear;
        procedure InsertObject( EditableObject : TEditableObject );
    end;

var
  ObjectContainerEditor: TObjectContainerEditor;

implementation

  uses
    EditorMain, NativeEdObjects;

{$R *.DFM}

  // TObjectContainerEditor

  procedure TObjectContainerEditor.UpdateObject;
    begin
    end;

  procedure TObjectContainerEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    var
      i : integer;
    begin
      inherited;
      Clear;
      if (fEditableObject.Options and OPT_ALLOWNEW) <> 0
        then
          with lvItems.Items.Add do
            begin
              ImageIndex := 0;
              Data       := nil;
              Caption    := 'New...';
            end;

      fEditingChilds := boolean(options and VOP_EDITCHILDREN);
      if not fEditingChilds
        then
          for i := 0 to pred(fEditableObject.Properties.Count) do
            InsertObject( TEditableObject( fEditableObject.Properties[i] ))
        else
          for i := 0 to pred(fEditableObject.Children.Count) do
            InsertObject( TEditableObject( fEditableObject.Children[i] ))
    end;

  function TObjectContainerEditor.ProcessMessage( msg : integer; var data ) : boolean;
    var
      edObject : TEditableObject absolute data;
    begin
      result := true;
      case msg of
        MSG_GETSELOBJECT :
          begin
            edObject := nil;
            if (lvItems.Selected <> nil) and (lvItems.Selected.Data <> nil)
              then edObject := TEditableObject(lvItems.Selected.Data);
          end;
        else result := inherited ProcessMessage( msg, data );
      end;
    end;

  procedure TObjectContainerEditor.Clear;
    begin
      lvItems.Items.Clear;
    end;

  procedure TObjectContainerEditor.InsertObject( EditableObject : TEditableObject );
    begin
      with lvItems.Items.Add do
        begin
          ImageIndex := 0;
          Data       := EditableObject;
          Caption    := EditableObject.Name;
        end;
    end;

  procedure TObjectContainerEditor.lvItemsDblClick(Sender: TObject);
    var
      newObject : TEditableObject;
    begin
      if lvItems.Selected <> nil
        then
          if lvItems.Selected.Data <> nil
            then TEditableObject(lvItems.Selected.Data).Edit( VOP_SHOWINMAINCONTROL, EditorMainForm.pnRight )
            else
              begin
                newObject := fEditableObject.ObjectEditor.CreateNewObject;
                if newObject <> nil
                  then
                    begin
                      if not fEditingChilds
                        then fEditableObject.Properties.Insert( newObject )
                        else
                          begin
                            if newObject is TNativeMetaEditableObject
                              then TNativeMetaEditableObject(newObject).Parent := TMetaEditableobject(fEditableObject);
                            fEditableObject.Children.Insert( newObject );
                          end;
                      InsertObject( newObject );
                    end;
              end;
    end;

end.
