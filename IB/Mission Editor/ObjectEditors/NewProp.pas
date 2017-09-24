unit NewProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, EditableObjects, ComCtrls, NativeEdObjects, Collection;

const
  MAX_EDITPROP = 2;

type
  TPropsInfo =
    array[0..MAX_EDITPROP-1] of
      record
        NameLabel    : TLabel;
        EditorParent : TPanel;
      end;

  TNewPropForm =
    class(TForm)
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        Label4: TLabel;
        Label5: TLabel;
        lbTemplate: TLabel;
        Label7: TLabel;
        edPropName: TEdit;
        cbPropClass: TComboBox;
        cbTemplates: TComboBox;
        cbValue: TComboBox;
        TabSheet2: TTabSheet;
        pnContainer: TPanel;
        Panel1: TPanel;
        Panel2: TPanel;
        Label1: TLabel;
        cbPropTemplates: TComboBox;
        Panel3: TPanel;
        Button2: TButton;
        Button1: TButton;
        procedure cbPropClassChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure cbTemplatesChange(Sender: TObject);
        procedure cbPropTemplatesChange(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
      public
        NewProperty     : TCollection;
        CreateMetaClass : boolean;
        EdParent        : TEditableObject;
      private
        fTemplate : TNativeMetaEditableObject;
        fEditor   : TObject;
    end;

var
  NewPropForm: TNewPropForm;

implementation

  uses
    Notifications, EditorEvents;

{$R *.DFM}

  procedure TNewPropForm.cbPropClassChange(Sender: TObject);
    begin
      if CompareText( cbPropClass.Text, 'ComboBox' ) = 0
        then
          begin
            lbTemplate.Visible := true;
            cbTemplates.Visible := true;
          end
        else
          begin
            lbTemplate.Visible := false;
            cbTemplates.Visible := false;
          end;
    end;

  procedure TNewPropForm.FormCreate(Sender: TObject);
    var
      MetaClass : TMetaEditableObject;
      Data      : TGetTemplateData;
      i         : integer;
    begin
      NewProperty   := TCollection.Create( 5, rkUse );
      
      Data.Name     := 'Templates';
      Data.Template := cbTemplates.Items;
      DispatchEvent( evGetTemplates, Data );

      MetaClass := TheMetaObjectPool.get( 'MetaProperty' );
      if MetaClass <> nil
        then
          for i := 0 to pred(MetaClass.Children.Count) do
            cbPropClass.Items.Add( TEditableObject(MetaClass.Children[i]).Name );

      MetaClass := TheMetaObjectPool.get( 'PropertyTemplates' );
      if MetaClass <> nil
        then
          for i := 0 to pred(MetaClass.Children.Count) do
            cbPropTemplates.Items.Add( TEditableObject(MetaClass.Children[i]).Name );
    end;

  procedure TNewPropForm.Button2Click(Sender: TObject);
    var
      MetaClass    : TMetaEditableObject;
      temp         : TEditableObject;
      useless      : integer;
      _NewProperty : TEditableObject;
      i            : integer;
    begin
      if PageControl1.ActivePageIndex = 0
        then
          begin
            if (edPropName.Text <> '') and (cbPropClass.Text <> '')
              then
                begin
                  MetaClass := TheMetaObjectPool.get( 'MetaProperty.' + cbPropClass.Text );
                  if MetaClass <> nil
                    then
                      begin
                        if not CreateMetaClass
                          then
                            begin
                              _NewProperty := MetaClass.Instantiate( edPropName.Text, cbValue.Text, useless );
                              _NewProperty.MetaClass := MetaClass;
                              if cbTemplates.Visible
                                then
                                  begin
                                    temp           := TEditableObject.Create;
                                    temp.MetaClass := TheMetaObjectPool.get( 'MetaProperty.PlainText' );
                                    temp.Name      := 'comboTemplate';
                                    temp.Value     := cbTemplates.Text;
                                    _NewProperty.Properties.Insert( temp );
                                  end;
                            end
                          else
                            begin
                              _NewProperty           := TNativeMetaEditableObject.Create;
                              MetaClass.Clone( TNativeMetaEditableObject(_NewProperty) );
                              _NewProperty.MetaClass := MetaClass;
                              _NewProperty.Name      := edPropName.Text;
                              _NewProperty.Value     := cbValue.Text;
                              if cbTemplates.Visible
                                then
                                  begin
                                    temp           := TNativeMetaEditableObject.Create;
                                    temp.Name      := 'comboTemplate';
                                    temp.Value     := cbTemplates.Text;
                                    _NewProperty.Properties.Insert( temp );
                                  end;
                            end;
                        NewProperty.Insert( _NewProperty );
                        ModalResult := mrOk;
                      end;
                end;
          end
        else
          begin
            if (cbPropTemplates.Text <> '') and (fTemplate <> nil)
              then
                begin
                  fTemplate.ObjectEditor.UpdateObject( fEditor, fTemplate );
                  for i := 0 to pred(fTemplate.Properties.Count) do
                    NewProperty.Insert( fTemplate.Properties[i] );
                  fTemplate.Properties.ExtractAll;
                  fTemplate.Free;
                  ModalResult := mrOk;
                end;
          end;
    end;

  procedure TNewPropForm.Button1Click(Sender: TObject);
    begin
      fTemplate.Free;
      NewProperty.DeleteAll;
      ModalResult := mrCancel;
    end;

  procedure TNewPropForm.cbTemplatesChange(Sender: TObject);
    var
      Data : TGetTemplateData;
    begin
      cbValue.Items.Clear;
      Data.Name     := cbTemplates.Text;
      Data.Template := cbValue.Items;
      DispatchEvent( evGetTemplates, Data );
      cbValue.Text := cbValue.Items[0];
    end;

  procedure TNewPropForm.cbPropTemplatesChange(Sender: TObject);
    function getNextNumber( PropName : string ) : integer;
      var
        i : integer;
      begin
        i := 1;
        while EdParent.getProperty( PropName + IntToStr(i) ) <> nil do
          inc( i );
        result := i;
      end;

    var
      OldTemplate : TNativeMetaEditableObject;
      MetaClass   : TMetaEditableObject;
      i           : integer;
    begin
      OldTemplate := fTemplate;

      MetaClass   := TheMetaObjectPool.get( 'PropertyTemplates.' + cbPropTemplates.Text );
      if (MetaClass <> nil) and CreateMetaClass
        then
          begin
            fTemplate := TNativeMetaEditableObject.Create;
            MetaClass.Clone( fTemplate );

            if (MetaClass.Options and OPT_NUMERABLE) <> 0
              then
                begin
                  for i := 0 to pred(fTemplate.Properties.Count) do
                    TEditableObject(fTemplate.Properties[i]).Name := TEditableObject(fTemplate.Properties[i]).Name + IntToStr(getNextNumber(TEditableObject(fTemplate.Properties[i]).Name));
                end;

            fTemplate.ObjectEditor := TheMetaObjectPool.getEditor( 'Step' );
            fEditor := fTemplate.Edit( VOP_DONOTSHOWEVENTS, pnContainer );

            if OldTemplate <> nil
              then OldTemplate.Free;
          end;
    end;

  procedure TNewPropForm.FormDestroy(Sender: TObject);
    begin
      NewProperty.Free;
    end;

end.
