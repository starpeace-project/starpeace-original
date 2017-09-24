unit ObjectInspector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Grids, EditableObjects, Collection, Editors, StdCtrls;

type
  TObjectInspectorEditor =
    class(TInPlaceVisualControl)
    pcContainer: TPageControl;
        TabSheet1: TTabSheet;
        TabSheet2: TTabSheet;
        Panel1: TPanel;
        pnLeft: TPanel;
        Splitter1: TSplitter;
        pnRight: TPanel;
        pnActions: TPanel;
        Button1: TButton;
        Button2: TButton;
        Panel2: TPanel;
        pnEvRight: TPanel;
        Splitter2: TSplitter;
        pnEvLeft: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
      public
        procedure UpdateObject; override;
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
      private
        fProperties : TCollection;
        fOptions    : integer;
        procedure InsertObject( obj : TEditableObject; ParentLeft, ParentRight : TPanel );
        procedure Clear;
  end;

var
  ObjectInspectorEditor: TObjectInspectorEditor;

implementation

  uses
    newProp;

{$R *.DFM}

  type
    TPropertyDescriptor =
      class
        public
          edObject              : TEditableObject;
          LeftPanel, RightPanel : TPanel;
          editor                : TObject;
      end;

  procedure TObjectInspectorEditor.UpdateObject;
    var
      i : integer;
    begin
      for i := 0 to pred(fProperties.Count) do
        begin
          with TPropertyDescriptor(fProperties[i]) do
            edObject.ObjectEditor.UpdateObject( editor, edObject );
        end;
    end;

  procedure TObjectInspectorEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    var
      i : integer;
    begin
      Clear;
      inherited;
      fOptions := options;
      pnActions.Visible := (options and VOP_ALLOWPROPERTYEDITING) <> 0;

      for i := 0 to pred(fEditableObject.Properties.Count) do
        if (TEditableObject( fEditableObject.Properties[i] ).Options and OPT_EVENT) <> 0
          then InsertObject( TEditableObject( fEditableObject.Properties[i]), pnEvLeft, pnEvRight )
          else InsertObject( TEditableObject( fEditableObject.Properties[i]), pnLeft, pnRight );

    end;

  procedure TObjectInspectorEditor.InsertObject( obj : TEditableObject; ParentLeft, ParentRight : TPanel );
    var
      PropertyDescriptor : TPropertyDescriptor;
    begin
      PropertyDescriptor := TPropertyDescriptor.Create;
      with PropertyDescriptor do
        begin
          edObject             := obj;

          LeftPanel            := TPanel.Create( nil );
          LeftPanel.Parent     := ParentLeft;
          LeftPanel.Top        := 15000;
          LeftPanel.Align      := alTop;
          LeftPanel.Alignment  := taLeftJustify;
          LeftPanel.Caption    := edObject.Name;
          LeftPanel.Height     := 22;
          LeftPanel.BevelInner := bvRaised;
          LeftPanel.BevelOuter := bvNone;
          LeftPanel.Tag        := fProperties.Count;

          RightPanel            := TPanel.Create( nil );
          RightPanel.Parent     := ParentRight;
          RightPanel.Top        := 15000;
          RightPanel.Align      := alTop;
          RightPanel.Alignment  := taLeftJustify;
          RightPanel.Caption    := '';
          RightPanel.Height     := 22;
          RightPanel.BevelInner := bvRaised;
          RightPanel.BevelOuter := bvNone;
          RightPanel.Tag        := fProperties.Count;

          editor := edObject.Edit( 0, RightPanel );
        end;
      fProperties.Insert( PropertyDescriptor );
    end;

  procedure TObjectInspectorEditor.Clear;
    var
      i : integer;
    begin
      for i := pred(fProperties.Count) downto 0 do
        begin
          with TPropertyDescriptor(fProperties[i]) do
            begin
              edObject.ObjectEditor.UpdateObject( editor, edObject );

              edObject.ObjectEditor.DestroyEditor( editor );

              RightPanel.Parent := nil;
              RightPanel.Free;
              LeftPanel.Parent := nil;
              LeftPanel.Free;


              fProperties.AtDelete( i );
            end;
        end;

    end;

  procedure TObjectInspectorEditor.FormCreate(Sender: TObject);
    begin
      fProperties := TCollection.Create( 10, rkBelonguer );
    end;

  procedure TObjectInspectorEditor.Button1Click(Sender: TObject);
    var
      newPropForm : TNewPropForm;
      i           : integer;
    begin
      newPropForm := TNewPropForm.Create( nil );
      try
        newPropForm.CreateMetaClass := (fOptions and VOP_ALLOWPROPERTYEDITING) <> 0;
        newPropForm.EdParent        := fEditableObject;
        if newPropForm.ShowModal = mrOk
          then
            begin
              for i := 0 to pred(newPropForm.NewProperty.Count) do
                begin
                  fEditableObject.Properties.Insert( TEditableObject(newPropForm.NewProperty[i]) );
                  InsertObject( TEditableObject(newPropForm.NewProperty[i]), pnLeft, pnRight );
                end;
            end;
      finally
        newPropForm.Free;
      end;
    end;

  procedure TObjectInspectorEditor.FormShow(Sender: TObject);
    begin
      TabSheet2.Enabled := (fOptions and VOP_DONOTSHOWEVENTS) = 0;
    end;

end.
