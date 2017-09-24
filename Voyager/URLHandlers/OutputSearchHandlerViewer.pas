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
  TOutputSearchViewer =
    class(TVisualControl)
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
        procedure lvResultsColumnClick(Sender: TObject; Column: TListColumn);
        procedure lvResultsCompare(Sender: TObject; Item1, Item2: TListItem;
          Data: Integer; var Compare: Integer);
      private
        fMasterURLHandler : IMasterURLHandler;
        fClientView       : IClientView;
        fXPos             : integer;
        fYPos             : integer;
        fWorld            : string;
        fFluid            : string;
        fActionPage       : string;
      public
        property MasterURLHandler : IMasterURLHandler read fMasterURLHandler write fMasterURLHandler;
      public
        procedure InitViewer(ClientView : IClientView; x, y : integer; world, fluid, actionPage : string);
      private
        function  GetLinkCoord(const text : string; var p : integer) : TPoint;
        procedure RenderResult(const text : string);
      public
        procedure RenderResults(const fluid, text : string);
      private
        function GetRoles : integer;
      public
        property Roles : integer read GetRoles;
        private
      {Private Declarations}
        SortColumn: Integer;
        SortDescending: Boolean;
        procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
      private
        procedure threadedHire( const parms : array of const );
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

var
  OutputSearchViewer: TOutputSearchViewer;

implementation

  uses
    CompStringsParser, SpecialChars, ServerCnxEvents, CacheCommon, Literals, Threads, URLParser, CoolSB;

  {$R *.DFM}

  type
    TSupplierLinkInfo =
      class
        fX : integer;
        fY : integer;
      end;

  // TOutputSearchViewer
  procedure TOutputSearchViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    var
      sum : integer;
      i   : integer;
      dwStyles : DWORD;
    begin
      inherited;
      if (lvResults <> nil) //and (lvResults.Items <> nil)
        then
          begin
            sum := 0;
            for i := 0 to pred(lvResults.Columns.Count-1) do
              inc(sum, lvResults.Columns[i].Width);
            dwStyles := GetWindowLong(lvResults.Handle, GWL_STYLE);
            if (dwStyles and WS_VSCROLL) <> 0
              then Inc(sum, GetSystemMetrics(SM_CXVSCROLL));
            lvResults.Columns[lvResults.Columns.Count-1].Width := lvResults.Width-sum-2;
          end;
    end;

  procedure TOutputSearchViewer.InitViewer(ClientView : IClientView; x, y : integer; world, fluid, actionPage : string);
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
        fActionPage := actionPage;
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
        if fActionPage <> ''
          then
            begin
              cbTradeCenter.Enabled := false;
              cbTradeCenter.Checked := false;
              btnSearchClick( self );
            end
          else cbTradeCenter.Enabled := true;
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
      sum  : integer;
    begin
      if fFluid = fluid
        then
          begin
            lvResults.Items.Clear;
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
              SetBounds(Left,Top,Width,Height);
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
        with lvResults.Items.Add do
          begin
            Caption := GetLiteral('literal482');
            Data    := nil;
          end;
      finally
        lvResults.Items.EndUpdate;
      end;
      url := '?frame_Id=ObjectInspector' +
        '&frame_Class=ObjectInspector' +
        '&frame_Visibility=hidden' +
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
            if fActionPage = ''
              then url := 'http://local.asp?frame_Id=ObjectInspector&Cnxs=' + aux + '&frame_Action=Connect'
              else
                begin
                  DeleteParameter( fActionPage, 'Suppliers' );
                  DeleteParameter( fActionPage, 'Fluid' );
                  url := fActionPage + '&frame_Id=WebMain&Suppliers=' + aux + '&Connect=YES&Fluid=' + fFluid;
                end;
            fMasterURLHandler.HandleURL( url );
            fMasterURLHandler.HandleURL( 'local.asp?frame_Id=' + tidHandlerName_SupplyFinder + '&frame_Close=yes' );
            Fork(threadedHire, priNormal, [0]);
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


procedure TOutputSearchViewer.CMVisibleChanged(var Message: TMessage);
  begin
    inherited;
  end;

procedure TOutputSearchViewer.threadedHire( const parms : array of const );
  var
    ErrorCode : integer;
  begin
    try
      fClientView.SetCookie('Hired', '1', ErrorCode );
    except
    end;
  end;

procedure TOutputSearchViewer.lvResultsColumnClick(Sender: TObject; Column: TListColumn);
  begin
    with Column do
      if SortColumn = Index
        then SortDescending := not SortDescending
        else
          begin
            SortDescending := False;
            SortColumn := Index
          end;
     lvResults.AlphaSort;
  end;

procedure TOutputSearchViewer.lvResultsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
  begin
    if SortColumn = 0 
      then Compare := CompareStr(Item1.Caption, Item2.Caption)
      else Compare := CompareStr(Item1.SubItems[Pred(SortColumn)], Item2.SubItems[Pred(SortColumn)]);
    if SortDescending 
      then Compare := - Compare;
  end;

procedure TOutputSearchViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvResults.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvResults.Handle, ' ', ' ');
          CoolSBEnableBar(lvResults.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvResults );
        end;
  end;

end.
