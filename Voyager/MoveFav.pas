unit MoveFav;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdCtrls, ComCtrls, FramedButton, ExtCtrls, FavView, VoyagerServerInterfaces,
  InternationalizerComponent;

type
  TMoveFavDlg = class(TForm)
    BorderPanel: TPanel;
    Panel2: TPanel;
    BtnsPanel: TPanel;
    bOK: TFramedButton;
    bCancel: TFramedButton;
    CaptionPanel: TPanel;
    TreeView: TTreeView;
    Label8: TLabel;
    ImageList2: TImageList;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormShow(Sender: TObject);
  private
    fClientView : IClientView;
  public
    property ClientView : IClientView write fClientView;
  private
    procedure RenderFolder( Location : string );
    function LocateTreeNode( Location : string; StartNode : TTreeNode ) : TTreeNode;
  private
    procedure threadedGetFolderInfo( const parms : array of const );
    procedure syncGetFolderInfo( const parms : array of const );
  end;

var
  MoveFavDlg: TMoveFavDlg;

implementation

  uses
    Threads, CompStringsParser, FavProtocol;

{$R *.DFM}

  procedure TMoveFavDlg.bOKClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TMoveFavDlg.bCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TMoveFavDlg.RenderFolder( Location : string );
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
      if (Node.Data = nil) or not TNodeExtraInfo(Node.Data).defined
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
            TNodeExtraInfo(Node.Data).rendered := false;
            locations := TStringList.Create;
            locations.Add( Location );
            Fork( threadedGetFolderInfo, priNormal, [locations, false, false] );
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
                        end;
                  end;
            until ItemStr = '';
            TNodeExtraInfo(Node.Data).rendered := true;
            TNodeExtraInfo(Node.Data).empty := empty;
            Node.ImageIndex := 0;
          end;
    end;

  function TMoveFavDlg.LocateTreeNode( Location : string; StartNode : TTreeNode ) : TTreeNode;
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

  procedure TMoveFavDlg.threadedGetFolderInfo( const parms : array of const );
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

  procedure TMoveFavDlg.syncGetFolderInfo( const parms : array of const );
    var
      location      : string;
      content       : string;
      Node          : TTreeNode;
    begin
      location    := parms[0].vPchar;
      content     := parms[1].vPchar;
      Node := LocateTreeNode( Location, TreeView.Items.GetFirstNode );
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            TNodeExtraInfo(Node.Data).content := content;
            TNodeExtraInfo(Node.Data).defined := true;
            RenderFolder( location );
            Node.Expand( false );
          end;
      Node.ImageIndex := 0;
      Node.SelectedIndex := 1;
    end;

  procedure TMoveFavDlg.TreeViewChange(Sender: TObject; Node: TTreeNode);
    begin
      if Node.Data <> nil
        then RenderFolder( TNodeExtraInfo(Node.Data).location );
    end;

  procedure TMoveFavDlg.TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    begin
      if Node.Data <> nil
        then RenderFolder( TNodeExtraInfo(Node.Data).location );
    end;                                                               

  procedure TMoveFavDlg.FormShow(Sender: TObject);
    begin
      TreeView.Items.Clear;
      with TreeView.Items.Add( nil, 'Favorites' ) do
        begin
          HasChildren := true;
          Data := nil;
        end;
      RenderFolder( '' );
    end;

end.
