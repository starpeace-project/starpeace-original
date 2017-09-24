unit InputSearchHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PDTabControl, InfoBook, VisualControls, SHDocVw, CustomWebBrowser,
  VoyagerInterfaces, VoyagerServerInterfaces, ObjectInspectorInterfaces,
  StdCtrls, VisualClassesHandler, InternationalizerComponent, ComCtrls,
  FramedButton;

const
  tidHandlerName_ClientFinder = 'ClientFinder';

type
  TInputSearchViewer = class(TVisualControl)
    SearchPanel: TPanel;
    Panel1: TPanel;
    LeftLine: TShape;
    eCompany: TEdit;
    eTown: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    cbInportWarehouses: TCheckBox;
    cbRegWarehouses: TCheckBox;
    cbStores: TCheckBox;
    cbFactories: TCheckBox;
    btnSearch: TFramedButton;
    Panel2: TPanel;
    lvResults: TListView;
    Panel4: TPanel;
    fbSelectAll: TFramedButton;
    fbUnselectAll: TFramedButton;
    fbBuyFrom: TFramedButton;
    btnClose: TFramedButton;
    Label2: TLabel;
    eMaxLinks: TEdit;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure fbSelectAllClick(Sender: TObject);
    procedure fbUnselectAllClick(Sender: TObject);
    procedure fbBuyFromClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure lvResultsDeletion(Sender: TObject; Item: TListItem);
    procedure lvResultsDblClick(Sender: TObject);
    procedure lvResultsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eMaxLinksKeyPress(Sender: TObject; var Key: Char);
  private
    fMasterURLHandler : IMasterURLHandler;
    fClientView       : IClientView;
    fXPos             : integer;
    fYPos             : integer;
    fWorld            : string;
    fFluid            : string;
  public
    property MasterURLHandler : IMasterURLHandler read fMasterURLHandler write fMasterURLHandler;
  public
    procedure InitViewer(ClientView : IClientView; x, y : integer; world, fluid : string);
  private
    function  GetLinkCoord(const text : string; var p : integer) : TPoint;
    procedure RenderResult(const text : string);
  public
    procedure RenderResults(const fluid, text : string);
  private
    function GetRoles : integer;
  public
    property Roles : integer read GetRoles;
  end;

var
  InputSearchViewer: TInputSearchViewer;

implementation

  uses
    CompStringsParser, SpecialChars, ServerCnxEvents, CacheCommon, Literals;

  {$R *.DFM}

  procedure TInputSearchViewer.fbSelectAllClick(Sender: TObject);
    var
      i : integer;
    begin
      lvResults.SetFocus;
      for i := 0 to pred(lvResults.Items.Count) do
        lvResults.Items[i].Selected := true;
    end;

  procedure TInputSearchViewer.fbUnselectAllClick(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(lvResults.Items.Count) do
        lvResults.Items[i].Selected := false;
    end;

  procedure TInputSearchViewer.fbBuyFromClick(Sender: TObject);
    var
      Item : TListItem;
      i    : integer;
      pP   : PPoint;
      aux  : string;
      url  : string;
    begin
      aux := '';
      for i := 0 to pred(lvResults.Items.Count) do
        begin
          Item := lvResults.Items[i];
          if Item.Selected
            then
              begin
                pP := PPoint(lvResults.Items[i].Data);
                if pP <> nil
                  then aux := aux + IntToStr(pP.x) + ',' + IntToStr(pP.y) + ',';
              end;
        end;
      for i := pred(lvResults.Items.Count) to 0 do
        begin
          Item := lvResults.Items[i];
          if Item.Selected
            then lvResults.Items.Delete(i);
        end;
      if aux <> ''
        then
          begin
            url := 'http://local.asp?frame_Id=ObjectInspector&Cnxs=' + aux + '&frame_Action=Connect';
            fMasterURLHandler.HandleURL(url);
          end;
    end;

  procedure TInputSearchViewer.btnCloseClick(Sender: TObject);
    var
      url : string;
    begin
      url := '?frame_Id=' + tidHandlerName_ClientFinder + '&frame_Close=YES';
      fMasterURLHandler.HandleURL(url);
    end;

  procedure TInputSearchViewer.btnSearchClick(Sender: TObject);
    var
      url : string;
    begin
      lvResults.Items.BeginUpdate;
      try
        lvResults.Items.Clear;
      finally
        lvResults.Items.EndUpdate;
      end;
      url := '?frame_Id=ObjectInspector' +
        '&Fluid=' + fFluid +
        '&Owner=' + eCompany.Text +
        '&Town=' + eTown.Text +
        '&Count=' + eMaxLinks.Text +
        '&Roles=' + IntToStr(Roles) +
        '&frame_Action=FINDCLIENTS';
      fMasterURLHandler.HandleURL(url);
    end;

  procedure TInputSearchViewer.InitViewer(ClientView : IClientView; x, y : integer; world, fluid : string);
    var
      keepResults : boolean;
    begin
      try
        keepResults := (fXPos = x) and (fYPos = y) and (fWorld = world) and (fFluid = fluid);
        fClientView := ClientView;
        fXPos       := x;
        fYPos       := y;
        fWorld      := world;
        fFluid      := fluid;
        cbInportWarehouses.Checked := true;
        cbRegWarehouses.Checked := true;
        cbStores.Checked := true;
        cbFactories.Checked := true;
        eCompany.Text := '';
        eTown.Text := '';
        if not keepResults
          then
            begin
              lvResults.Items.BeginUpdate;
              try
                lvResults.Items.Clear;
              finally
                lvResults.Items.EndUpdate;
              end;
            end;
      except
      end;
    end;

 function TInputSearchViewer.GetLinkCoord(const text : string; var p : integer) : TPoint;
    var
      aux : string;
    begin
      FillChar(result, sizeof(result), 0);
      aux := CompStringsParser.GetNextStringUpTo(text, p, BackslashChar);
      if aux <> ''
        then
          begin
            result.x := StrToInt(aux);
            inc(p);
            aux := CompStringsParser.GetNextStringUpTo(text, p, BackslashChar);
            if aux <> ''
              then result.y := StrToInt(aux)
              else result.y := 0;
          end
        else result.x := 0;
    end;

 procedure TInputSearchViewer.RenderResult(const text : string);
    var
      Item  : TListItem;
      p     : integer;
      aux   : string;
      coord : TPoint;
      pt    : PPoint;
    begin
      p := 1;
      coord := GetLinkCoord(text, p);
      inc(p);
      aux := CompStringsParser.GetNextStringUpTo(text, p, BackslashChar);
      if aux <> ''
        then
          begin
            Item := lvResults.Items.Add;
            Item.Caption := aux;
            new(pt);
            pt^ := coord;
            Item.Data := pt;
            inc(p);
            aux := CompStringsParser.GetNextStringUpTo(text, p, BackslashChar);
            while aux <> '' do
              begin
                Item.SubItems.Add(aux);
                aux := CompStringsParser.GetNextStringUpTo(text, p, BackslashChar);
              end;
          end;
    end;

 procedure TInputSearchViewer.RenderResults(const fluid, text : string);
    var
      List : TStringList;
      i    : integer;
    begin
      if fFluid = fluid
        then
          begin
            List := TStringList.Create;
            try
              List.Text := text;
              if List.Count = 0
                then
                  with lvResults.Items.Add do
                    begin
                      Caption := GetLiteral('Literal70');
                      Data    := nil;
                    end
                else
                  for i := 0 to pred(List.Count) do
                    RenderResult(List[i]);
            finally
              List.Free;
            end;
          end;
    end;

  function TInputSearchViewer.GetRoles : integer;
    var
      rl : TFacilityRoleSet;
    begin
      rl := [];
      if cbInportWarehouses.Checked
        then rl := rl + [rolCompInport];  // result := result or cbExportWarehouses.Tag;
      if cbRegWarehouses.Checked
        then rl := rl + [rolDistributer]; // result := result or cbRegWarehouses.Tag;
      if cbStores.Checked
        then rl := rl + [rolBuyer];    // result := result or cbTradeCenter.Tag;
      if cbFactories.Checked
        then rl := rl + [rolProducer];    // result := result or cbFactories.Tag;
      result := byte(rl);
    end;

  procedure TInputSearchViewer.lvResultsDeletion(Sender: TObject; Item: TListItem);
    begin
      try
        if Item.Data <> nil
          then dispose(Item.Data);
      finally
        Item.Data := nil;
      end;
    end;

  procedure TInputSearchViewer.lvResultsDblClick(Sender: TObject);
    begin
      fbBuyFromClick(Sender);
    end;

  procedure TInputSearchViewer.lvResultsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      i : integer;
    begin
      if Key = VK_DELETE
        then
          for i := pred(lvResults.Items.Count) downto 0 do
            if lvResults.Items[i].Selected
              then lvResults.Items.Delete(i);
    end;

  procedure TInputSearchViewer.eMaxLinksKeyPress(Sender: TObject; var Key: Char);
    begin
      if not(key in ['0'..'9'])
        then Key := #0;
    end;

end.
