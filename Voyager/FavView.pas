unit FavView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FramedButton, ComCtrls, GradientBox, StdCtrls, ImgList,
  SyncObjs, Collection, VisualControls, VoyagerInterfaces, VoyagerServerInterfaces,
  InternationalizerComponent;

  type
    TNodeExtraInfo =
      class
        location : string;
        content  : string;
        info     : string;
        kind     : integer;
        defined  : boolean;
        rendered : boolean;
        empty    : boolean;
        constructor Create;
        destructor Destroy; override;
      end;

type
  TFavViewer = class(TVisualControl)
    TreeView: TTreeView;
    Splitter1: TSplitter;
    Panel2: TPanel;
    GradientBox2: TGradientBox;
    Panel3: TPanel;
    ListView: TListView;
    GradientBox3: TGradientBox;
    Panel4: TPanel;
    Image2: TImage;
    FolderName: TLabel;
    Add: TFramedButton;
    NewFolder: TFramedButton;
    Delete: TFramedButton;
    Move: TFramedButton;
    GradientBox1: TGradientBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Panel1: TPanel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure NewFolderClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fClientView : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fCurrLoc : string;
    fReqLoc  : string;
    fEditing : boolean;
  public
    property ClientView : IClientView write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
  private
    function  LocateTreeNode( Location : string; StartNode : TTreeNode ) : TTreeNode;
    procedure RenderFolder( Location : string; UpdateList : boolean; ForceRefresh : boolean = false );
    procedure InvalidateLocation( Location : string; InvalidateStructure : boolean = true );
  private
    procedure threadedGetFolderInfo( const parms : array of const );
    procedure syncGetFolderInfo( const parms : array of const );
    procedure threadedNewFolder( const parms : array of const );
    procedure syncNewFolder( const parms : array of const );
    procedure threadedDelItem( const parms : array of const );
    procedure syncDelItem( const parms : array of const );
    procedure threadedNewItem( const parms : array of const );
    procedure syncNewItem( const parms : array of const );
    procedure threadedMoveItem( const parms : array of const );
    procedure syncMoveItem( const parms : array of const );
    procedure threadedRenameItem( const parms : array of const );
    procedure syncRenameItem( const parms : array of const );
    procedure threadedGetLinkData( const parms : array of const );
  protected
    procedure SetParent( Parent : TWinControl ); override;
  private
    procedure EnableControls( NodeInfo : TNodeExtraInfo );
  end;

type
  TFavoritesHandler =
    class( TInterfacedObject, IMetaURLHandler, IURLHandler )
      public
        constructor Create;
        destructor  Destroy; override;
      private
        fControl            : TFavViewer;
        fMasterURLHandler   : IMasterURLHandler;
        fClientView         : IClientView;
        fLastFacCount       : integer;
      // IMetaURLHandler
      private
        function getName    : string;
        function getOptions : TURLHandlerOptions;
        function getCanHandleURL( URL : TURL ) : THandlingAbility;
        function Instantiate : IURLHandler;
      // IURLHandler
      private
        function  HandleURL( URL : TURL ) : TURLHandlingResult;
        function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
        function  getControl : TControl;
        procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
    end;

const
  tidMetaHandlerName_Favorites = 'Favorites';

implementation

{$R *.DFM}

  uses
    CompStringsParser, FavProtocol, Threads, ServerCnxEvents, MoveFav, Events,
    VCLUtils, Protocol, FiveTypes, mr_StrUtils, VoyagerEvents, CoolSB;

  constructor TFavoritesHandler.Create;
    begin
      inherited Create;
      fLastFacCount := -1;
      fControl := TFavViewer.Create( nil );
    end;

  destructor TFavoritesHandler.Destroy;
    begin
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
      inherited;
    end;

  function TFavoritesHandler.getName : string;
    begin
      result := tidMetaHandlerName_Favorites;
    end;

  function TFavoritesHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopEnabledWhenCached];
    end;

  function TFavoritesHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TFavoritesHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TFavoritesHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlHandled;
    end;

  function TFavoritesHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      RefreshTycoon : TRefreshTycoonInfo absolute info;
      CurrNode      : TNodeExtraInfo;
    begin
      result := evnHandled;
      case EventId of
        evnHandlerExposed :
          begin
            fControl.ClientView := fClientView;
            fControl.MasterURLHandler := fMasterURLHandler;
            fControl.TreeView.Items.Clear;
            with fControl.TreeView.Items.Add( nil, 'Favorites' ) do
              Data := nil;
            fControl.RenderFolder( '', true );
          end;
        evnRefreshTycoon :
          begin
            if fLastFacCount <> RefreshTycoon.FacCount
              then
                begin
                  if fLastFacCount <> -1
                    then
                      begin
                        fControl.InvalidateLocation('', false);
                        if fControl.TreeView.Selected <> nil
                          then
                            begin
                              CurrNode := TNodeExtraInfo(fControl.TreeView.Selected.Data);
                              fControl.RenderFolder(CurrNode.location, true, true);
                            end;
                      end;
                  fLastFacCount := RefreshTycoon.FacCount;
                end;
          end;
        else
          result := evnNotHandled;
      end;
    end;

  function TFavoritesHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TFavoritesHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

  const
    ExtraInfoGarbColl : TCollection = nil;
    shutdown : boolean = false;

  constructor TNodeExtraInfo.Create;
    begin
      inherited Create;
      ExtraInfoGarbColl.Insert( self );
    end;

  destructor TNodeExtraInfo.Destroy;
    begin
      if not shutdown
        then ExtraInfoGarbColl.Extract( self );
      inherited;
    end;

  function TFavViewer.LocateTreeNode( Location : string; StartNode : TTreeNode ) : TTreeNode;
    var
      i : integer;
    begin
      if (StartNode.Data <> nil) and (TNodeExtraInfo(StartNode.Data).location <> Location)
        then
          begin
            result := nil;
            i      := 0;
            while (i < StartNode.Count) and (result = nil) do
              begin
                result := LocateTreeNode( Location, StartNode.Item[i] );
                inc( i );
              end;
          end
        else result := StartNode;
    end;

  procedure TFavViewer.RenderFolder( Location : string; UpdateList : boolean; ForceRefresh : boolean );
    var
      ParentContent : string;
      p             : integer;
      ItemStr       : string;
      Id, Kind, Cnt : integer;
      Name, Info    : string;
      Node, NewNode : TTreeNode;
      wasdefined    : boolean;
      hadchildren   : boolean;
      empty         : boolean;
      locations     : TStringList;
    begin
      Node := LocateTreeNode( Location, TreeView.Items.GetFirstNode );
      if UpdateList
        then ListView.Items.Clear;
      fCurrLoc := Location;
      fReqLoc := Location;
      if (Node.Data = nil) or not TNodeExtraInfo(Node.Data).defined or ForceRefresh
        then
          begin
            if Node.Data = nil
              then
                begin
                  Node.Data := TNodeExtraInfo.Create;
                  TNodeExtraInfo(Node.Data).location := Location;
                end;
            Node.ImageIndex := 2;
            Node.SelectedIndex := 2;
            hadchildren := Node.HasChildren;
            Node.DeleteChildren;
            Node.HasChildren := hadchildren;
            if UpdateList
              then
                with ListView.Items.Add do
                  begin
                    Caption := 'downloading...';
                    ImageIndex := -1;
                  end;
            TNodeExtraInfo(Node.Data).rendered := false;
            locations := TStringList.Create;
            locations.Add( Location );
            Fork( threadedGetFolderInfo, priNormal, [locations, UpdateList, hadchildren] );
            wasdefined := false
          end
        else
          begin
            ParentContent := TNodeExtraInfo(Node.Data).content;
            wasdefined := true;
          end;
      if wasdefined
        then
          begin
            empty := true;
            p := 1;
            ListView.Items.BeginUpdate;
            try
              repeat
                ItemStr := GetNextStringUpTo( ParentContent, p, chrItemSeparator );
                if ItemStr <> ''
                  then
                    begin
                      UnSerializeItemProps( ItemStr, Id, Kind, Name, Info, Cnt );
                      empty := false;
                      if (Kind = fvkFolder) and not TNodeExtraInfo(Node.Data).rendered
                        then
                          begin
                            NewNode := TreeView.Items.AddChild( Node, Name );
                            NewNode.HasChildren := Cnt > 0;
                            NewNode.Data := TNodeExtraInfo.Create;
                            TNodeExtraInfo(NewNode.Data).location := Location + chrPathSeparator + IntToStr(Id);
                            TNodeExtraInfo(NewNode.Data).info := Info;
                            TNodeExtraInfo(NewNode.Data).kind := Kind;
                          end;
                      if UpdateList
                        then
                          with ListView.Items.Add do
                            begin
                              Caption := Name;
                              case Kind of
                                fvkFolder :
                                  ImageIndex := 2;
                                else
                                  ImageIndex := 0;
                              end;
                              Data := TNodeExtraInfo.Create;
                              TNodeExtraInfo(Data).location := Location + chrPathSeparator + IntToStr(Id);
                              TNodeExtraInfo(Data).info := Info;
                              TNodeExtraInfo(Data).kind := Kind;
                            end;
                    end;
              until ItemStr = '';
            finally
              ListView.Items.EndUpdate;
            end;
            if Node.Data <> nil
              then
                begin
                  TNodeExtraInfo(Node.Data).rendered := true;
                  TNodeExtraInfo(Node.Data).empty := empty;
                end;
            if UpdateList
              then
                begin
                  Node.Selected := true;
                  Node.MakeVisible;
                end;
            {
            if empty
              then
                with ListView.Items.Add do
                  begin
                    Caption := 'No items.';
                    ImageIndex := -1;
                  end;
            }
            Node.ImageIndex := 0;
            FolderName.Caption := Node.Text;
          end;
    end;

  { SYNCHRONIC VERSION
  procedure TForm1.RenderFolder( Location : string; UpdateList : boolean );

    function LocateTreeNode( Location : string; StartNode : TTreeNode ) : TTreeNode;
      var
        i : integer;
      begin
        if (StartNode.Data <> nil) and (TNodeExtraInfo(StartNode.Data).location <> Location)
          then
            begin
              result := nil;
              i      := 0;
              while (i < StartNode.Count) and (result = nil) do
                begin
                  result := LocateTreeNode( Location, StartNode.Item[i] );
                  inc( i );
                end;
            end
          else result := StartNode;
      end;

    var
      ParentContent : string;
      p             : integer;
      ItemStr       : string;
      Id, Kind, Cnt : integer;
      Name, Info    : string;
      Node, NewNode : TTreeNode;
      wasdefined    : boolean;
    begin
      Node := LocateTreeNode( Location, TreeView.Items.GetFirstNode );
      if UpdateList
        then ListView.Items.Clear;
      fCurrLoc := Location;
      if (Node.Data = nil) or (TNodeExtraInfo(Node.Data).content = '')
        then
          begin
            Node.DeleteChildren;
            ParentContent := Fav.RDOGetSubItems( fCurrLoc );
            if Node.Data = nil
              then
                begin
                  Node.Data := TNodeExtraInfo.Create;
                  TNodeExtraInfo(Node.Data).location := Location;
                end;
            TNodeExtraInfo(Node.Data).content := ParentContent;
            wasdefined := false
          end
        else
          begin
            ParentContent := TNodeExtraInfo(Node.Data).content;
            wasdefined := true;
          end;
      p := 1;
      repeat
        ItemStr := GetNextStringUpTo( ParentContent, p, chrItemSeparator );
        if ItemStr <> ''
          then
            begin
              UnSerializeItemProps( ItemStr, Id, Kind, Name, Info, Cnt );
              if (Kind = fvkFolder) and not wasdefined
                then
                  begin
                    NewNode := TreeView.Items.AddChild( Node, Name );
                    NewNode.HasChildren := Cnt > 0;
                    NewNode.Data := TNodeExtraInfo.Create;
                    TNodeExtraInfo(NewNode.Data).location := Location + chrPathSeparator + IntToStr(Id);
                  end;
              if UpdateList
                then
                  with ListView.Items.Add do
                    begin
                      Caption := Name;
                      case Kind of
                        fvkFolder :
                          ImageIndex := 2;
                        else
                          ImageIndex := 0;
                      end;
                      Data := TNodeExtraInfo.Create;
                      TNodeExtraInfo(Data).location := Location + chrPathSeparator + IntToStr(Id);
                    end;
            end;
      until ItemStr = '';
      if UpdateList
        then
          begin
            Node.Selected := true;
            Node.MakeVisible;
          end;
    end;
  }

  procedure TFavViewer.InvalidateLocation( Location : string; InvalidateStructure : boolean );
    var
      Node : TTreeNode;
    begin
      Node := LocateTreeNode( Location, TreeView.Items.GetFirstNode );
      if Node <> nil
        then
          begin
            if InvalidateStructure
              then
                begin
                  Node.DeleteChildren;
                  Node.HasChildren := false;
                end;
            TNodeExtraInfo(Node.Data).content := '';
            TNodeExtraInfo(Node.Data).defined := false;
            TNodeExtraInfo(Node.Data).rendered := false;
          end;
    end;

  procedure TFavViewer.threadedGetFolderInfo( const parms : array of const );
    var
      locations   : TStringList absolute parms[0].vObject;
      updateList  : boolean;
      hadchildren : boolean;
      content     : string;
      i           : integer;
    begin
      sleep( 100 + random(300) );
      updateList  := parms[1].vBoolean;
      hadchildren := parms[2].vBoolean;
      for i := 0 to pred(locations.Count) do
        begin
          content := fClientView.FavGetSubItems( locations[i] );
          Join( syncGetFolderInfo, [locations[i], content, updateList, hadchildren] );
        end;
      locations.Free;
    end;

  procedure TFavViewer.syncGetFolderInfo( const parms : array of const );
    var
      location      : string;
      content       : string;
      updateList    : boolean;
      hadchildren   : boolean;
      Node          : TTreeNode;
    begin
      location    := parms[0].vPchar;
      content     := parms[1].vPchar;
      updateList  := parms[2].vBoolean;
      hadchildren := parms[3].vBoolean;
      Node := LocateTreeNode( Location, TreeView.Items.GetFirstNode );
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            TNodeExtraInfo(Node.Data).content := content;
            TNodeExtraInfo(Node.Data).defined := true;
            if location = fReqLoc
              then
                begin
                  RenderFolder( location, updateList );
                  if (location = '') or hadchildren
                    then Node.Expand( false );
                  EnableControls( Node.Data );
                end;
            Node.ImageIndex := 0;
            Node.SelectedIndex := 1;
          end;
    end;

  procedure TFavViewer.threadedNewFolder( const parms : array of const );
    var
      location : string absolute parms[0].vPchar;
      id       : integer;
    begin
      id := fClientView.FavNewItem( location, fvkFolder, 'New Folder', '' );
      Join( syncNewFolder, [location, id] );
    end;

  procedure TFavViewer.syncNewFolder( const parms : array of const );
    var
      location : string;
      Node     : TTreeNode;
    begin
      location := parms[0].vPchar;
      Node := LocateTreeNode( fCurrLoc, TreeView.Items.GetFirstNode );
      TNodeExtraInfo(Node.Data).content := '';
      TNodeExtraInfo(Node.Data).defined := false;
      TNodeExtraInfo(Node.Data).rendered := false;
      if location = fCurrLoc
        then RenderFolder( fCurrLoc, true );
    end;

  procedure TFavViewer.threadedDelItem( const parms : array of const );
    var
      items    : TStringList absolute parms[0].vObject;
      location : string;
      i        : integer;
    begin
      location := parms[1].vPChar;
      for i := 0 to pred(items.Count) do
        begin
          fClientView.FavDelItem( items[i] );
        end;
      items.Free;
      Join( syncDelItem, [location] );
    end;

  procedure TFavViewer.syncDelItem( const parms : array of const );
    var
      location : string absolute parms[0].vPchar;
    begin
      RenderFolder( location, true, true );
    end;

  procedure TFavViewer.threadedNewItem(const parms : array of const);
    var
      location : string absolute parms[0].vPchar;
      info     : TSelObjectInfo;
    begin
      if fMasterURLHandler.HandleEvent(evnGetSelObjInfo, info) = evnHandled
        then Fork( threadedGetLinkData, priNormal, [location, info.obj, info.x, info.y] );
      //id := fClientView.FavNewItem( location, fvkLink, 'Building ' + IntToStr(random(1000)), '100,100' );
      //Join( syncNewItem, [location, id] );
    end;

  procedure TFavViewer.threadedGetLinkData(const parms : array of const);
    var
      loc       : string;
      id        : integer;
      Obj, x, y : integer;
      link      : string;
      StText    : string;
      Name      : string;
      ErrorCode : TErrorCode;
      p         : integer;
    begin
      try
        loc    := parms[0].VPChar;
        Obj    := parms[1].vInteger;
        x      := parms[2].vInteger;
        y      := parms[3].vInteger;
        StText := ReplaceStr(fClientView.ObjectStatusText( sttMain, Obj, ErrorCode ), #10#13, '|');
        if ErrorCode = NOERROR
          then
            try
              p := 1;
              Name := GetNextStringUpTo(StText, p, '|');
              if Name <> ''
                then
                  begin
                    link := Protocol.ComposeLinkCookie(Name, x, y, true);
                    id := fClientView.FavNewItem(loc, fvkLink, Name, link);
                    if id >= 0
                      then Join(syncNewItem, [loc, id]);
                  end;
            except
              fClientView.SayThis( '', IntToStr(id), id );
            end
          else fClientView.SayThis( '', IntToStr(ErrorCode), ErrorCode );
      except
      end;
    end;

  procedure TFavViewer.SetParent( Parent : TWinControl );
    begin
      inherited;
      if InitSkinImage and (Parent <> nil)
        then
          begin
            InitializeCoolSB( ListView.Handle );
            ListView.DoubleBuffered := true;
            InitializeCoolSB( TreeView.Handle );
            TreeView.DoubleBuffered := true;
            if hThemeLib <> 0
              then
                begin
                  SetWindowTheme( ListView.Handle, ' ', ' ' );
                  SetWindowTheme( TreeView.Handle, ' ', ' ' );
                end;
          end;
      fEditing := false;
    end;

  procedure TFavViewer.syncNewItem( const parms : array of const );
    begin
      syncNewFolder(parms);
    end;

  procedure TFavViewer.threadedMoveItem( const parms : array of const );
    var
      items      : TStringList;
      src        : string;
      dest       : string;
      i          : integer;
      folderMove : boolean;
    begin
      items      := TStringList(parms[0].vObject);
      src        := parms[1].vPChar;
      dest       := parms[2].vPChar;
      folderMove := parms[3].vBoolean;
      for i := 0 to pred(items.Count) do
        fClientView.FavMoveItem( items[i], dest );
      items.Free;
      Join( syncMoveItem, [src, dest, folderMove] );
    end;

  procedure TFavViewer.syncMoveItem( const parms : array of const );
    var
      src        : string;
      dest       : string;
      folderMove : boolean;
    begin
      src  := parms[0].vPChar;
      dest := parms[1].vPChar;
      folderMove := parms[2].vBoolean;
      if folderMove
        then RenderFolder( '', true, true )
        else
          begin
            InvalidateLocation( dest, false );
            RenderFolder( src, true, true )
          end;
    end;

  procedure TFavViewer.threadedRenameItem( const parms : array of const );
    var
      location, name : string;
      folder         : boolean;
    begin
      location := parms[0].vPchar;
      name     := parms[1].vPchar;
      folder   := parms[2].vBoolean;
      fClientView.FavRenameItem( location, name );
      Join( syncRenameItem, [location, folder] );
    end;

  procedure TFavViewer.syncRenameItem( const parms : array of const );
    var
      location : string;
      folder         : boolean;
    begin
      location := parms[0].vPchar;
      folder   := parms[1].vBoolean;
      location := ExtractParentLocation(location);
      if folder
        then RenderFolder( location, true, true )
        else InvalidateLocation( location, false );
    end;

  procedure TFavViewer.EnableControls( NodeInfo : TNodeExtraInfo );
    var
      enable : boolean;
      modEnable : boolean;
    begin
      enable := (NodeInfo <> nil) and NodeInfo.defined;
      modEnable := enable and ((ListView.Selected <> nil) or (TreeView.Selected <> nil) and (NodeInfo.location <> ''));
      NewFolder.Enabled := enable;
      Move.Enabled      := modEnable;
      Delete.Enabled    := modEnable;
      Add.Enabled       := enable;
    end;

  procedure TFavViewer.TreeViewChange(Sender: TObject; Node: TTreeNode);
    begin
      EnableControls( Node.Data );
      if Node.Data <> nil
        then RenderFolder( TNodeExtraInfo(Node.Data).location, true );
    end;

  procedure TFavViewer.TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    begin
      if Node.Data <> nil
        then RenderFolder( TNodeExtraInfo(Node.Data).location, false );
    end;

  { SYNCHONIC
  procedure TForm1.NewFolderClick(Sender: TObject);
    var
      Node : TTreeNode;
    begin
      Fav.RDONewItem( fCurrLoc, fvkFolder, 'New Folder', '' );
      Node := LocateTreeNode( fCurrLoc, TreeView.Items.GetFirstNode );
      TNodeExtraInfo(Node.Data).content := '';
      TNodeExtraInfo(Node.Data).defined := false;
      TNodeExtraInfo(Node.Data).rendered := false;
      RenderFolder( fCurrLoc, true );
    end;
  }

  procedure TFavViewer.NewFolderClick(Sender: TObject);
    begin
      Fork( threadedNewFolder, priNormal, [fCurrLoc] );
    end;

  procedure TFavViewer.DeleteClick(Sender: TObject);
    var
      list   : TStringList;
      parent : string;
      i      : integer;
    begin
      list := TStringList.Create;
      if ListView.Focused
        then
          begin
            for i := 0 to pred(ListView.Items.Count) do
              if ListView.Items[i].Selected
                then list.Add( TNodeExtraInfo(ListView.Items[i].Data).location );
            parent := fCurrLoc;
          end
        else
          if fCurrLoc<>''
            then
              begin
                list.Add( fCurrLoc );
                parent := ExtractParentLocation(fCurrLoc);
              end
            else exit;
      InvalidateLocation( parent );
      Fork( threadedDelItem, priNormal, [list, parent] );
    end;

  procedure TFavViewer.AddClick(Sender: TObject);
    begin
      threadedNewItem( [fCurrLoc] );
    end;

  procedure TFavViewer.MoveClick(Sender: TObject);
    var
      list       : TStringList;
      i          : integer;
      src        : string;
      dest       : string;
      folderMove : boolean;
    begin
      MoveFavDlg.ClientView := fClientView;
      Move.Enabled := false;
      if (MoveFavDlg.ShowModal = mrOk) and
         ((MoveFavDlg.TreeView.Selected <> nil) or (ListView.Selected <> nil))
        then
          begin
            folderMove := false;
            list := TStringList.Create;
            if ListView.Focused
              then
                begin
                  for i := 0 to pred(ListView.Items.Count) do
                    if ListView.Items[i].Selected
                      then
                        begin
                          list.Add( TNodeExtraInfo(ListView.Items[i].Data).location );
                          if ListView.Items[i].ImageIndex = 2 // >> a folder...
                            then folderMove := true;
                        end
                end
              else
                if TreeView.Selected <> nil
                  then
                    begin
                      list.Add( TNodeExtraInfo(TreeView.Selected.Data).location );
                      folderMove := true;
                    end;
            if list.Count > 0
              then
                begin
                  src  := fCurrLoc;
                  dest := TNodeExtraInfo(MoveFavDlg.TreeView.Selected.Data).location;
                  Fork( threadedMoveItem, priNormal, [list, src, dest, folderMove] );
                end
              else list.Free;
          end
        else Move.Enabled := true;
    end;

  procedure TFavViewer.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      if TreeView.Selected <> nil
        then EnableControls( TNodeExtraInfo(TreeView.Selected.Data) );
    end;

  procedure TFavViewer.ListViewEdited(Sender: TObject; Item: TListItem; var S: String);
    begin
      if Item.Data <> nil
        then Fork( threadedRenameItem, priNormal, [TNodeExtraInfo(Item.Data).location, S, Item.ImageIndex = 2] );
      fEditing := false;
    end;

  procedure TFavViewer.TreeViewEdited(Sender: TObject; Node: TTreeNode; var S: String);
    begin
      if Node.Data <> nil
        then Fork( threadedRenameItem, priNormal, [TNodeExtraInfo(Node.Data).location, S, true] );
    end;

  procedure TFavViewer.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    begin
      if Item1.ImageIndex = Item2.ImageIndex
        then
          if Item1.Caption > Item2.Caption
            then Compare := -1
            else
              if Item1.Caption < Item2.Caption
                then Compare := 1
                else Compare := 0
        else
          if Item1.ImageIndex = 2
            then Compare := -1
            else Compare := 1;
    end;


  procedure TFavViewer.ListViewDblClick(Sender: TObject);
    var
      x, y   : integer;
      name   : string;
      select : boolean;
    begin
      if (ListView.Selected <> nil) and (ListView.Selected.Data <> nil)
        then
          if TNodeExtraInfo(ListView.Selected.Data).kind = fvkLink
            then
              begin
                ParseLinkCookie( TNodeExtraInfo(ListView.Selected.Data).info, name, x, y, select );
                fMasterURLHandler.HandleURL( '?frame_Id=MapIsoView&frame_Action=SELECT&x=' + IntToStr(x) + '&y=' + IntToStr(y) );
              end
            else
              begin
                EnableControls( ListView.Selected.Data );
                RenderFolder( TNodeExtraInfo(ListView.Selected.Data).location, true );
              end;
    end;

  procedure TFavViewer.Image2Click(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL('?frame_Id=Favorites&frame_Close=YES');
    end;

  procedure TFavViewer.ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      if not fEditing
        then
          case Key of
            vk_Return :
              begin
                ListViewDblClick( self );
              end;
            vk_Delete :
              begin
                DeleteClick( self );
              end;
            vk_F2 :
              begin
                DeleteClick( self );
              end;
          end;
    end;

procedure TFavViewer.ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
  begin
    fEditing := AllowEdit;
  end;

procedure TFavViewer.TreeViewEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
  begin
    fEditing := AllowEdit;
  end;

procedure TFavViewer.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbRight then
   ListView.Selected.EditCaption;
end;

initialization

  ExtraInfoGarbColl := TCollection.Create( 0, rkBelonguer );

finalization

  shutdown := true;
  ExtraInfoGarbColl.Free;

end.




