unit EditorMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EditableObjects, Menus, Notifications;

type
  TEditorMainForm =
    class(TForm, IHook)
        pnLeft: TPanel;
        pnRightContainer: TPanel;
        MainMenu1: TMainMenu;
        File1: TMenuItem;
        Save1: TMenuItem;
        Load1: TMenuItem;
        N1: TMenuItem;
        Exit1: TMenuItem;
        SaveDialog1: TSaveDialog;
        OpenDialog1: TOpenDialog;
        Splitter1: TSplitter;
        Panel1: TPanel;
        pnRight: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure Save1Click(Sender: TObject);
        procedure Load1Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure Exit1Click(Sender: TObject);
      private
        fMission   : TEditableObject;
        fTemplates : TStringList;
        procedure RegisterEditors;
        procedure LoadTemplates;
      private
        procedure Handle( EventClass : TEventClass; var Info );
        procedure SaveMission( filename : string );
        procedure LoadMission( filename : string );
    end;

var
  EditorMainForm: TEditorMainForm;

implementation

  uses
    NativeEdObjects, Editors, plainText, ComboBox, FileName, Percent, ObjectContainer, ObjectInspector,
    NewStepCreator, EditorEvents, NewStepClass, XMLFile, IniFiles;

{$R *.DFM}

  procedure TEditorMainForm.FormCreate(Sender: TObject);
    var
      useless : integer;
      AppPath : string;
    begin
      CreateMetaObjectPool;
      RegisterEditors;

      AppPath := ExtractFilePath( Application.ExeName );
      InitMetaObjectPool( AppPath + 'classes.xml' );
      //TheMetaObjectPool.Save( AppPath + 'test2.ini' );

      InitNotificationEngine;

      RegisterEventClass( evGetTemplates, 0 );
      AddNotifier( evGetTemplates, self );

      LoadTemplates;
    end;

  procedure TEditorMainForm.FormShow(Sender: TObject);
    var
      useless : integer;
      general : TEditableObject;
    begin
      fMission := TheMetaObjectPool.get( 'MetaMission' ).Instantiate( 'New Mission', '', useless );

      fMission.Edit( VOP_SHOWINMAINCONTROL, pnLeft );

      //fMission.getProperty( 'General' ).Edit( 0, pnRight );
    end;

  procedure TEditorMainForm.RegisterEditors;
    begin
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'Mission', TObjectContainerEditor, TNewStepForm ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'MetaStep', TObjectContainerEditor, TNewStepClassForm ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'Step', TObjectInspectorEditor, nil ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'PlainText', TPlainTextEditor, nil ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'Percent', TPercentEditor, nil ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'Combobox', TComboBoxEditor, nil ));
      TheMetaObjectPool.registerEditor( TNativeObjectEditor.Create( 'FileName', TFilenameEditor, nil ));
    end;

  procedure TEditorMainForm.LoadTemplates;
    var
      IniFile  : TIniFile;
      sections : TStringList;
      names    : TStringList;
      i        : integer;
      AppPath  : string;
    begin
      fTemplates := TStringList.Create;
      AppPath    := ExtractFilePath( Application.ExeName );
      IniFile    := TIniFile.Create( AppPath + 'templates.ini' );
      try
        sections := TStringList.Create;
        IniFile.ReadSections( sections );
        fTemplates.AddObject( 'Templates', sections );
        for i := 0 to pred(sections.count) do
          begin
            names := TStringList.Create;
            IniFile.ReadSection( sections[i], Names );
            fTemplates.AddObject( sections[i], Names );
          end;
      finally
        IniFile.Free;
      end;
    end;

  procedure TEditorMainForm.Handle( EventClass : TEventClass; var Info );
    var
      GetTemplateData : TGetTemplateData;
      Idx             : integer;
      i               : integer;
    begin
      case EventClass of
        evGetTemplates :
          begin
            GetTemplateData := TGetTemplateData(Info);
            Idx := fTemplates.IndexOf( GetTemplateData.Name );
            if Idx <> -1
              then GetTemplateData.Template.Assign( TStringList(fTemplates.Objects[Idx]) )
              else
                if CompareText( GetTemplateData.Name, 'Steps' ) = 0
                  then
                    for i := 0 to pred(fMission.Properties.Count) do
                      if TEditableObject(fMission.Properties[i]).Name <> 'General'
                        then GetTemplateData.Template.Add( TEditableObject(fMission.Properties[i]).Name );
          end;
      end;
    end;

procedure TEditorMainForm.SaveMission( filename : string );
  const
    MaxRoles = 10;

  var
    Trainings : array[0..MaxRoles] of TStringList;
    Roles     : TStringList;

  procedure getClientInfo;
    var
      i    : integer;
      role : integer;
      prop : TEditableObject;
      tr   : TEditableObject;
      idx  : integer;
    begin
      for i := 0 to pred(fMission.Properties.Count) do
        with TEditableObject(fMission.Properties[i]) do
          begin
            role := 1;
            prop := getProperty( 'Role' + IntToStr(role) );
            while prop <> nil do
              begin
                idx := Roles.IndexOf( prop.Value );
                if idx = -1
                  then idx := Roles.Add( prop.Value );

                tr := getProperty( 'Training' + IntToStr(role) );
                if tr <> nil
                  then
                    begin
                      if Trainings[idx].IndexOf( tr.Value ) = -1
                        then Trainings[idx].Add( tr.Value );
                    end;

                tr := getProperty( 'SecondaryTraining' + IntToStr(role) );
                if tr <> nil
                  then
                    begin
                      if (Trainings[idx].IndexOf( tr.Value ) = -1) and (tr.Value <> 'none')
                        then Trainings[idx].Add( tr.Value );
                    end;

                inc( role );
                prop := getProperty( 'Role' + IntToStr(role) );
              end;
          end;
    end;

  var
    XMLServer : TXMLFile;
    XMLClient : TXMLFile;
    i         : integer;
    j         : integer;
    general   : TEditableObject;
    node      : TXMLNode;
    propNode  : TXMLNode;
    infoNode  : TXMLNode;
    trainNode : TXMLNode;
  begin
    XMLServer := TXMLFile.Create;
    XMLClient := TXMLFile.Create;
    try
      // Save the server side

      // save the general information first

      general   := fMission.getProperty( 'General' );

      node := XMLServer.CreateNode( '', 'General' );
      node.WriteString( 'class', general.MetaClass.getFullName );
      node.WriteString( 'Name', general.Name );
      node.WriteString( 'value', general.Value );
      for j := 0 to pred(general.Properties.Count) do
        begin
          propNode := node.CreateChild( TEditableObject(general.Properties[j]).Name );
          propNode.setValueAsString( TEditableObject(general.Properties[j]).Value );
          propNode.Free;
        end;
      node.Free;

      for i := 0 to pred(fMission.Properties.Count) do
        with TEditableObject(fMission.Properties[i]) do
          begin
            if Name <> 'General'
              then
                begin
                  node := XMLServer.CreateNode( '', 'Property' );
                  node.WriteString( 'class', MetaClass.getFullName );
                  node.WriteString( 'Name', Name );
                  node.WriteString( 'value', Value );
                  for j := 0 to pred(Properties.Count) do
                    begin
                      propNode := node.CreateChild( TEditableObject(Properties[j]).Name );
                      propNode.setValueAsString( TEditableObject(Properties[j]).Value );
                      propNode.Free;
                    end;
                  node.Free;
                end;
          end;

      // save the client side
      Roles := TStringList.Create;
      for i := 0 to pred(MaxRoles) do
        Trainings[i] := TStringList.Create;

      try
        general := fMission.getProperty( 'General' );

        infoNode := XMLClient.CreateNode( '', 'Info' );
        infoNode.WriteString( 'Name', general.getProperty( 'MissionName' ).Value );
        infoNode.WriteString( 'Description', general.getProperty( 'Description' ).Value );
        infoNode.WriteString( 'Id', general.getProperty( 'Id' ).Value );
        infoNode.Free;

        getClientInfo;

        for i := 0 to pred(Roles.Count) do
          begin
            node := XMLClient.CreateNode( '', 'Role' );
            node.setValueAsString( Roles[i] );

            for j := 0 to pred(Trainings[i].Count) do
              begin
                trainNode := node.CreateChild( 'Training' );
                trainNode.setValueAsString( Trainings[i][j] );
                trainNode.Free;
              end;

            node.Free;
          end;

      finally
        Roles.Free;
        for i := 0 to pred(MaxRoles) do
          Trainings[i].Free;

      end;

      XMLServer.Save( filename );
      XMLClient.Save( filename + '.client' );
    finally
      XMLServer.Free;
      XMLClient.Free;
    end;
  end;

procedure TEditorMainForm.LoadMission( filename : string );
  var
    XMLFile  : TXMLFile;
    i, j     : integer;
    prop     : TEditableObject;
    prop1    : TEditableObject;
    classId  : string;
    name     : string;
    value    : string;
    useless  : integer;
    list     : TList;
    proplist : TList;
    node     : TXMLNode;
    propnode : TXMLNode;
  begin
    XMLFile := TXMLFile.Create;
    try
      if XMLFile.Load( filename )
        then
          begin
            if fMission <> nil
              then fMission.Free;

            fMission := TheMetaObjectPool.get( 'MetaMission' ).Instantiate( 'New Mission', '', useless );

            list     := TList.Create;
            proplist := TList.Create;
            try
              XMLFile.getAllNodes( '', list );
              for i := 0 to pred(list.Count) do
                begin
                  node    := TXMLNode(list[i]);
                  classId := node.ReadString( 'class', '' );
                  name    := node.ReadString( 'Name', '' );
                  value   := node.ReadString( 'value', '' );

                  prop    := fMission.getProperty( classId );
                  if prop = nil
                    then
                      begin
                        prop  := TheMetaObjectPool.get( classId ).Instantiate( name, value, useless );
                        fMission.Properties.Insert( prop );
                      end
                    else
                      begin
                        prop.Name  := name;
                        prop.Value := value;
                      end;

                  proplist.Clear;
                  node.getChilds( proplist );
                  for j := 0 to pred(proplist.Count) do
                    begin
                      propnode    := TXMLNode(proplist[j]);

                      prop1       := prop.getProperty( propnode.getName );
                      prop1.Value := propnode.ReadString( 'value', '' );

                      propnode.Free;
                    end;
                end;

              fMission.Edit( VOP_SHOWINMAINCONTROL, pnLeft );
            finally
              list.Free;
              proplist.Free;
            end;
          end
        else ShowMessage( 'Unable to open ' + filename );
    finally
      XMLFile.Free;
    end;
  end;

procedure TEditorMainForm.Save1Click(Sender: TObject);
  var
    AppPath : string;
  begin
    if SaveDialog1.Execute
      then
        begin
          AppPath := ExtractFilePath( Application.ExeName );
          TheMetaObjectPool.Save( AppPath + 'Classes.xml' );
          SaveMission( SaveDialog1.FileName );
        end;
  end;

procedure TEditorMainForm.Load1Click(Sender: TObject);
  begin
    if OpenDialog1.Execute
      then LoadMission( OpenDialog1.FileName );
  end;

procedure TEditorMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    DestroyMetaObjectPool;
  end;

  procedure TEditorMainForm.Exit1Click(Sender: TObject);
    begin
      Close;
    end;

end.
