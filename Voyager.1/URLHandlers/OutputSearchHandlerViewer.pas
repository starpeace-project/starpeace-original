unit OutputSearchHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PDTabControl, InfoBook, VisualControls, SHDocVw, CustomWebBrowser,
  VoyagerInterfaces, VoyagerServerInterfaces, ObjectInspectorInterfaces,
  StdCtrls, VisualClassesHandler, InternationalizerComponent, ComCtrls,
  FramedButton;

const
  tidHandlerName_SupplyFinder = 'SupplyFinder';

type
  TOutputSearchViewer = class(TVisualControl)
    IECompPanel: TPanel;
    Panel1: TPanel;
    LeftLine: TShape;
    eCompany: TEdit;
    eTown: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    cbExportWarehouses: TCheckBox;
    cbRegWarehouses: TCheckBox;
    cbTradeCenter: TCheckBox;
    cbFactories: TCheckBox;
    btnSearch: TFramedButton;
    Panel2: TPanel;
    lvResults: TListView;
    fbSelectALL: TFramedButton;
    fbClearAll: TFramedButton;
    fbBuyFrom: TFramedButton;
    btnClose: TFramedButton;
    eMaxLinks: TEdit;
    Label2: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btnSearchClick(Sender: TObject);
    procedure fbSelectALLClick(Sender: TObject);
    procedure fbClearAllClick(Sender: TObject);
    procedure fbBuyFromClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lvResultsDeletion(Sender: TObject; Item: TListItem);
    procedure lvResultsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvResultsDblClick(Sender: TObject);
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
  OutputSearchViewer: TOutputSearchViewer;

implementation

  uses
    CompStringsParser, SpecialChars, ServerCnxEvents, CacheCommon, Literals;

  {$R *.DFM}

  type
    TSupplierLinkInfo =
      class
        fX : integer;
        fY : integer;
      end;

  // TOutputSearchViewer

  procedure TOutputSearchViewer.InitViewer(ClientView : IClientView; x, y : integer; world, fluid : string);
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
        cbExportWarehouses.Checked := true;
        cbRegWarehouses.Checked := true;
        cbTradeCenter.Checked := true;
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

  function TOutputSearchViewer.GetLinkCoord(const text : string; var p : integer) : TPoint;
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

  procedure TOutputSearchViewer.RenderResult(const text : string);
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

  procedure TOutputSearchViewer.RenderResults(const fluid, text : string);
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
                      Caption := GetLiteral('Literal93');
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

  procedure TOutputSearchViewer.btnSearchClick(Sender: TObject);
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
        '&frame_Action=FINDSUPPLIERS';
      fMasterURLHandler.HandleURL(url);
    end;

  procedure TOutputSearchViewer.fbSelectALLClick(Sender: TObject);
    var
      i : integer;
    begin
      lvResults.SetFocus;
      for i := 0 to pred(lvResults.Items.Count) do
        lvResults.Items[i].Selected := true;
    end;

  procedure TOutputSearchViewer.fbClearAllClick(Sender: TObject);
    var
      i : integer;
    begin
      for i := 0 to pred(lvResults.Items.Count) do
        lvResults.Items[i].Selected := false;
    end;

  procedure TOutputSearchViewer.fbBuyFromClick(Sender: TObject);
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

  procedure TOutputSearchViewer.btnCloseClick(Sender: TObject);
    var
      url : string;
    begin
      url := '?frame_Id=' + tidHandlerName_SupplyFinder + '&frame_Close=YES';
      fMasterURLHandler.HandleURL(url);
    end;

  function TOutputSearchViewer.GetRoles : integer;
    var
      rl : TFacilityRoleSet;
    begin
      rl := [];
      if cbExportWarehouses.Checked
        then rl := rl + [rolCompExport];  // result := result or cbExportWarehouses.Tag;
      if cbRegWarehouses.Checked
        then rl := rl + [rolDistributer]; // result := result or cbRegWarehouses.Tag;
      if cbTradeCenter.Checked
        then rl := rl + [rolImporter];    // result := result or cbTradeCenter.Tag;
      if cbFactories.Checked
        then rl := rl + [rolProducer];    // result := result or cbFactories.Tag;
      result := byte(rl);
    end;

  procedure TOutputSearchViewer.lvResultsDeletion(Sender: TObject; Item: TListItem);
    begin
      try
        if Item.Data <> nil
          then dispose(Item.Data);
      finally
        Item.Data := nil;
      end;
    end;

  procedure TOutputSearchViewer.lvResultsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      i : integer;
    begin
      if Key = VK_DELETE
        then
          for i := pred(lvResults.Items.Count) downto 0 do
            if lvResults.Items[i].Selected
              then lvResults.Items.Delete(i);
    end;

  procedure TOutputSearchViewer.lvResultsDblClick(Sender: TObject);
    begin
      fbBuyFromClick(Sender);
    end;

  procedure TOutputSearchViewer.eMaxLinksKeyPress(Sender: TObject; var Key: Char);
    begin
      if not(key in ['0'..'9'])
        then Key := #0;
    end;

end.
