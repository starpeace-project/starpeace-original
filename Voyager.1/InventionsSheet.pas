unit InventionsSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ComCtrls, ExtCtrls, FingerTabs,
  InternationalizerComponent;

const
  tidSecurityId   = 'SecurityId';
  tidTrouble      = 'Trouble';
  tidCurrBlock    = 'CurrBlock';
  tidResearchKind = 'RsKind';

  hasRsId       = 'hasRsId';
  hasRsName     = 'hasRsName';
  hasRsParent   = 'hasRsParent';
  hasRsCost     = 'hasRsCost';

  devRsId       = 'devRsId';
  devRsName     = 'devRsName';
  devRsParent   = 'devRsParent';

  avlRsId       = 'avlRsId';
  avlRsName     = 'avlRsName';
  avlRsParent   = 'avlRsParent';

  hasCount      = 'hasCount';
  devCount      = 'devCount';
  avlCount      = 'avlCount';

  avlRsEnabled  = 'avlRsEnabled';

const
  facStoppedByTycoon  = $04;

const
  tidNoResearchSelected = 'No research selected.';

const
  isDissabled   = 0;
  isEnabled     = 1;
  isResearching = 2;

type
  TInventionsSheetHandler = class;
  TInventionInfo          = class;

  TInventionsSheetViewer = class(TVisualControl)
    leftPanel: TPanel;
    ftInveKind: TFingerTabs;
    Notebook: TNotebook;
    lvSchedInvention: TListView;
    lbAvlDesc: TLabel;
    btnResearch: TFramedButton;
    lbSchDesc: TLabel;
    btnStop: TFramedButton;
    Shape1: TShape;
    Shape2: TShape;
    ImageList1: TImageList;
    tvAvailInventions: TTreeView;
    Shape4: TShape;
    lbAvlProps: TLabel;
    tvAlrDevInventions: TTreeView;
    Shape6: TShape;
    lbSchProps: TLabel;
    Shape3: TShape;
    lbDevDesc: TLabel;
    btnSell: TFramedButton;
    Shape5: TShape;
    lbDevProps: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure ftInveKindOnFingerChange(Sender: TObject);
    procedure btnResearchClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure lvSchedInventionChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvAvailInventionsDeletion(Sender: TObject; Item: TListItem);
    procedure tvAvailInventionsDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvAvailInventionsChange(Sender: TObject; Node: TTreeNode);
    procedure tvAlrDevInventionsChange(Sender: TObject; Node: TTreeNode);
    procedure btnSellClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    function  AddNode(TreeView : TTreeView; Invention : TInventionInfo; Cache : TStringList) : TTreeNode;
  private
    fHandler    : TInventionsSheetHandler;
    fLastFinger : integer;
  end;


  TInventionInfo =
    class
      public
        constructor Load( Stream : TStream );
        constructor Create(Id, Name, Parent : string);
      private
        fName    : string;
        fDesc    : string;
        fParent  : string;
        fProps   : string;
        fId      : string;
        fEnabled : boolean;
        fLoaded  : boolean;
        fCache   : boolean;
        fValue   : string;
    end;

  TInventionsSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TInventionsSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
        fResearchKind : string;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure RenderPropertiesToList(Properties : TStringList; Update : integer);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedResearch(const parms : array of const);
        procedure threadedStopResearch(const parms : array of const);
        procedure threadedGetResearchInfo(const parms : array of const);
        procedure threadedRenderResearchInfo(const parms : array of const);
        function  QueueResearch(inv : string; priority : integer) : boolean;
        function  CloseResearch(inv : string) : boolean;
        function  LoadInvention(Id, Name, Parent : string) : TInventionInfo;
    end;

var
  InventionsSheetViewer: TInventionsSheetViewer;

  function InventionSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, IniFiles, Protocol, 
    DelphiStreamUtils, SheetUtils, ClassStorage, ClientMLS, Literals;

{$R *.DFM}

  procedure SupressToolTip(Tree : TTreeView);
    const
      TVS_NOTOOLTIPS = $80;
      TVM_SETBKCOLOR = $1100 + 29;
    var
      Msg : TMessage;
    begin
      with Tree do
        SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);
      Msg.Msg := TVM_SETBKCOLOR;
      Msg.WParam := 0;
      Msg.LParam := clWhite;
      Tree.Dispatch( Msg );
    end;

  function IntToMil(val : integer) : string;
    begin
      result := IntToStr(val div 1000000) + ' M';
    end;

  function CvtToInt(str : string) : integer;
    begin
      if str <> ''
        then
          try
            result := StrToInt(str);
          except
            result := 0;
          end
        else result := 0;
    end;

  // TInventionsSheetHandler

  procedure TInventionsSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TInventionsSheetHandler.CreateControl(Owner : TControl) : TControl;

    procedure LoadInventions;
      var
        path   : string;
        Stream : TFileStream;
        count  : integer;
        i      : integer;
        Info   : TInventionInfo;
      begin
        path := fContainer.GetCacheDir + 'Inventions\research.' + ActiveLanguage + '.dat';
        Stream := TFileStream.Create( path, fmOpenRead );
        try
          Stream.Read( count, sizeof(count) );
          for i := 0 to count - 1 do
            try
              Info := TInventionInfo.Load( Stream );
              if Info.fId <> ''
                then TheClassStorage.RegisterClass( 'CachedInventions', Info.fId, Info )
            except
            end;
        finally
          Stream.Free;
        end;
      end;

    begin
      fControl := TInventionsSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
      LoadInventions;
    end;

  function TInventionsSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TInventionsSheetHandler.RenderProperties(Properties : TStringList);
    var
      i    : integer;
      cnt  : integer;
      Item : TListItem;
      iStr : string;
      Info : TInventionInfo;
      RootCache : TStringList;

    function IntToSgnStr(v : integer) : string;
      begin
        if v = 0
          then result := ''
          else
            if v > 0
              then result := '+' + IntToStr(v)
              else result := IntToStr(v);
      end;

    begin
      fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
      fResearchKind := Properties.Values[tidResearchKind];
      fCurrBlock    := CvtToInt(Properties.Values[tidCurrblock]);
      fControl.btnResearch.Enabled := fOwnsFacility;
      fControl.btnStop.Enabled     := fOwnsFacility;

      cnt := CvtToInt(Properties.Values[avlCount]);
      RootCache := TStringList.Create;
      if cnt < 20
        then fControl.tvAvailInventions.Items.BeginUpdate;
      try
        SupressToolTip(fControl.tvAvailInventions);
        fControl.tvAvailInventions.Items.Clear;
        for i := 0 to pred(cnt) do
          begin
            iStr := IntToStr(i);
            Info := LoadInvention(Properties.Values[avlRsId + iStr], Properties.Values[avlRsName + iStr], Properties.Values[avlRsParent + iStr]);
            if Info <> nil
              then
                begin
                  Info.fEnabled := Properties.Values[avlRsEnabled + iStr] = '1';
                  fControl.AddNode(fControl.tvAvailInventions, Info, RootCache);
                end;
            Application.ProcessMessages;
          end;
      finally
        if cnt < 20
          then fControl.tvAvailInventions.Items.EndUpdate;
        RootCache.Free;
      end;

      RootCache := TStringList.Create;
      fControl.tvAlrDevInventions.Items.BeginUpdate;
      try
        SupressToolTip(fControl.tvAlrDevInventions);
        fControl.tvAlrDevInventions.Items.Clear;
        cnt := CvtToInt(Properties.Values[hasCount]);
        for i := 0 to pred(cnt) do
          begin
            iStr := IntToStr(i);
            Info := LoadInvention(Properties.Values[hasRsId + iStr], Properties.Values[hasRsName + iStr], Properties.Values[hasRsParent + iStr]);
            Info.fValue := Properties.Values[hasRsCost + iStr];
            if Info <> nil
              then
                with fControl.AddNode(fControl.tvAlrDevInventions, Info, RootCache) do
                  begin
                    ImageIndex    := isDissabled;
                    SelectedIndex := isDissabled;
                  end;
          end;
      finally
        fControl.tvAlrDevInventions.Items.EndUpdate;
        RootCache.Free;
      end;

      fControl.lvSchedInvention.Items.BeginUpdate;
      try
        fControl.lvSchedInvention.Items.Clear;
        cnt := CvtToInt(Properties.Values[devCount]);
        for i := 0 to pred(cnt) do
          begin
            iStr := IntToStr(i);
            Info := LoadInvention(Properties.Values[devRsId + iStr], Properties.Values[devRsName + iStr], Properties.Values[devRsParent + iStr]);
            Info.fEnabled := true;
            if Info <> nil
              then
                begin
                  Item := fControl.lvSchedInvention.Items.Add;
                  Item.Caption    := Info.fName;
                  Item.StateIndex := isResearching;
                  Item.Data       := Info;
                end;
          end;
      finally
        fControl.lvSchedInvention.Items.EndUpdate;
      end;
      if cnt = 0
        then
          begin
            SheetUtils.ClearListView(fControl.lvSchedInvention);
            with SheetUtils.AddItem(fControl.lvSchedInvention, [GetLiteral('Literal42')]) do
              Data := nil;
          end;
      fControl.ftInveKind.CurrentFinger := fControl.fLastFinger;
    end;

  procedure TInventionsSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            Names.Add(tidSecurityId);
            Names.Add(tidCurrBlock);
            Names.Add(tidResearchKind);
            Names.Add(hasCount);
            Names.Add(devCount);
            Names.Add(avlCount);
            //SheetUtils.AddItem(fControl.lvAvailInventions, ['Downloading...']);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TInventionsSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvSchedInvention);
      //fControl.ftInveKind.CurrentFinger := 0;
      fControl.btnResearch.Enabled := false;
      fControl.btnStop.Enabled     := false;
      fControl.btnSell.Enabled     := false;
      fControl.tvAvailInventions.Items.Clear;
      fControl.tvAvailInventions.Items.Add( nil, GetLiteral('Literal43') );
      fControl.tvAlrDevInventions.Items.Clear;
      fControl.tvAlrDevInventions.Items.Add( nil, GetLiteral('Literal44') );
    end;

  procedure TInventionsSheetHandler.RenderPropertiesToList(Properties : TStringList; Update : integer);
    var
      Proxy : OleVariant;
      List  : TStringList;

    procedure ReadResearchProp(const RsKind : string);
      var
        i     : integer;
        iStr  : string;
        pName : string;
        aux   : string;
        cnt   : integer;
      begin
        if fLastUpdate = Update
          then
            try
              pName := RsKind + 'Count';
              aux   := Properties.Values[pName];
              cnt   := CvtToInt(aux);
              i     := 0;
              while (i < cnt) and (fLastUpdate = Update) do
                begin
                  iStr := IntToStr(i);
                  List.Add(RsKind + 'RsId' + iStr);
                  List.Add(RsKind + 'RsName' + iStr);
                  List.Add(RsKind + 'RsParent' + iStr);
                  if RsKind = 'avl'
                    then List.Add(avlRsEnabled + iStr)
                    else
                      if RsKind = 'has'
                        then List.Add(hasRsCost + iStr);
                  inc(i);
                end;
              if (fLastUpdate = Update) and (List.Count > 0)
                then fContainer.GetPropertyList(Proxy, List, Properties);
              List.Clear;
            except
            end;
      end;

    begin
      Proxy := fContainer.GetCacheObjectProxy;
      if (fLastUpdate = Update) and not VarIsEmpty(Proxy)
        then
          begin
            List := TStringList.Create;
            try
              if fLastUpdate = Update
                then
                  begin
                    ReadResearchProp('avl');
                    ReadResearchProp('dev');
                    ReadResearchProp('has');
                  end;
            finally
              List.Free;
            end;
          end;
    end;

  procedure TInventionsSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      Update := parms[1].vInteger;
      try
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Prop <> nil
          then
            begin
              RenderPropertiesToList(Prop, Update);
              if Update = fLastUpdate
                then Threads.Join(threadedRenderProperties, [Prop, Update])
                else Prop.Free;
            end;
      except
      end;
    end;

  procedure TInventionsSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if fLastUpdate = parms[1].vInteger
            then RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  procedure TInventionsSheetHandler.threadedResearch(const parms : array of const);
    var
      invId : string;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fCurrBlock <> 0) and fOwnsFacility
        then
          begin
            invId := parms[1].vPChar;
            QueueResearch(invId, 10);
          end;
    end;

  procedure TInventionsSheetHandler.threadedStopResearch(const parms : array of const);
    var
      invId : string;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fCurrBlock <> 0) and fOwnsFacility
        then
          begin
            invId := parms[1].vPChar;
            CloseResearch(invId);
          end;
    end;

  procedure TInventionsSheetHandler.threadedGetResearchInfo(const parms : array of const);
    var
      Props : string;
      Desc  : string;
      Proxy : OleVariant;
      id    : WideString;
    begin
      try
        if fLastUpdate = parms[0].vInteger
          then
            begin
              Proxy := fContainer.GetMSProxy;
              if not VarIsEmpty(Proxy)
                then
                  begin
                    id := parms[1].vPChar;
                    Proxy.BindTo(fCurrBlock);
                    Props := Proxy.RDOGetInvProps(id);
                    Proxy.BindTo(fCurrBlock);
                    Desc  := Proxy.RDOGetInvDesc(id);
                    if fLastUpdate = parms[0].vInteger
                      then Threads.Join(threadedRenderResearchInfo, [parms[0].vInteger, parms[2].vPointer, parms[3].vPointer, Props, Desc, parms[4].vPointer]);
                  end;
            end;
      except
      end;
    end;

  procedure TInventionsSheetHandler.threadedRenderResearchInfo(const parms : array of const);
    var
      dsLabel : TLabel;
      ppLabel : TLabel;
      Inv     : TInventionInfo;
    begin
      try
        if fLastUpdate = parms[0].vInteger
          then
            begin
              ppLabel := TLabel(parms[1].vPointer);
              dsLabel := TLabel(parms[2].vPointer);
              ppLabel.Caption := uppercase(parms[3].vPChar);
              dsLabel.Caption := parms[4].vPChar;
              Inv := TInventionInfo(parms[5].vPointer);
              Inv.fProps  := parms[3].vPChar;
              Inv.fDesc   := parms[4].vPChar;
              Inv.fLoaded := true;
            end;
      except
      end;
    end;

  function TInventionsSheetHandler.QueueResearch(inv : string; priority : integer) : boolean;
    var
      Proxy : OleVariant;
    begin
      try
        Proxy := fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy)
          then
            begin
              Proxy.WaitForAnswer := true;
              Proxy.BindTo(fCurrBlock);
              Proxy.RDOQueueResearch(inv, priority);
              result := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TInventionsSheetHandler.CloseResearch(inv : string) : boolean;
    var
      Proxy : OleVariant;
    begin
      try
        Proxy := fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy)
          then
            begin
              Proxy.WaitForAnswer := true;
              Proxy.BindTo(fCurrBlock);
              Proxy.RDOCancelResearch(inv);
              result := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TInventionsSheetHandler.LoadInvention(Id, Name, Parent : string) : TInventionInfo;
    {
    var
      path : string;
    }
    begin
      try
        result := TInventionInfo(TheClassStorage.ClassById['CachedInventions', Id]);
      except
        result := nil;
      end;
      if result = nil
        then result := TInventionInfo.Create(Id, Name, Parent);
      {
      try
        if result = nil
          then
            begin
              path := fContainer.GetCacheDir + 'Inventions\' + Id + '.inv';
              if FileExists(path)
                then
                  begin
                    result     := TInventionInfo.Load(path);
                    result.fId := Id;
                  end
                else result := TInventionInfo.Create(Id, Name, Parent);
              TheClassStorage.RegisterClass( 'CachedInventions', Id, result );
            end;
      except
        result := nil;
      end;
      }
    end;


  // InventionSheetHandlerCreator

  function InventionSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TInventionsSheetHandler.Create;
    end;


  // TInventionsSheetViewer

  procedure TInventionsSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TInventionsSheetViewer.ftInveKindOnFingerChange(Sender: TObject);
    begin
      if ftInveKind.CurrentFinger <> noFinger
        then
          begin
            Notebook.PageIndex := ftInveKind.CurrentFinger;
            fLastFinger := ftInveKind.CurrentFinger;
          end;
    end;

  procedure TInventionsSheetViewer.btnResearchClick(Sender: TObject);
    var
      SNode  : TTreeNode;
      Parent : TTreeNode;
      NItem  : TListItem;
    begin
      SNode := tvAvailInventions.Selected;
      if (SNode <> nil) and (SNode.ImageIndex > 0) and fHandler.fOwnsFacility and (fHandler.fCurrBlock <> 0)  
        then
          try
            Threads.Fork(fHandler.threadedResearch, priNormal, [fHandler.fLastUpdate, TInventionInfo(SNode.Data).fId]);
            if (lvSchedInvention.Items.Count = 1) and (lvSchedInvention.Items[0].Data = nil)
              then NItem := lvSchedInvention.Items[0]
              else NItem := lvSchedInvention.Items.Add;
            NItem.Caption := SNode.Text;
            NItem.StateIndex := isResearching;
            NItem.Data := SNode.Data;
            SNode.Data := nil;
            Parent := SNode.Parent;
            tvAvailInventions.Items.Delete(SNode);
            if (Parent <> nil) and not Parent.HasChildren
              then tvAvailInventions.Items.Delete(Parent);
            tvAvailInventions.SetFocus;
          except
          end;
    end;

  procedure TInventionsSheetViewer.btnStopClick(Sender: TObject);
    var
      SItem : TListItem;
    begin
      SItem := lvSchedInvention.Selected;
      if SItem <> nil
        then
          begin
            Threads.Fork(fHandler.threadedStopResearch, priNormal, [fHandler.fLastUpdate, TInventionInfo(SItem.Data).fId]);
            with AddNode(tvAvailInventions, TInventionInfo(SItem.Data), nil) do
              begin
                ImageIndex    := isEnabled;
                SelectedIndex := isEnabled;
              end;
            SItem.Data := nil;
            lvSchedInvention.Items.Delete(SItem.Index);
            lvSchedInvention.SetFocus;
          end;
    end;

  procedure TInventionsSheetViewer.lvSchedInventionChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    var
      Inv : TInventionInfo;
    begin
      if (Item <> nil) and (Item.Data <> nil)
        then
          begin
            if Change = ctState
              then
                begin
                  Inv := TInventionInfo(Item.Data);
                  if Inv.fCache // >> Used to be fLoaded!
                    then
                      begin
                        lbSchDesc.Caption  := Inv.fDesc;
                        lbSchProps.Caption := Inv.fProps;
                        btnStop.Enabled    := fHandler.fOwnsFacility;
                      end
                    else
                      begin
                        lbSchDesc.Caption  := '';
                        lbSchProps.Caption := '';
                        Threads.Fork(fHandler.threadedGetResearchInfo, priNormal, [fHandler.fLastUpdate, Inv.fId, lbSchDesc, lbSchProps, Inv]);
                      end;
                end;
          end
        else
          begin
            lbSchDesc.Caption  := tidNoResearchSelected;
            lbSchProps.Caption := '';
            btnStop.Enabled    := false;
          end;
    end;

  function TInventionsSheetViewer.AddNode(TreeView : TTreeView; Invention : TInventionInfo; Cache : TStringList) : TTreeNode;
    var
      i      : integer;
      cnt    : integer;
      Parent : TTreeNode;
      PrStr  : string;
      idx    : integer;
    begin
      cnt := TreeView.Items.Count;
      i   := 0;
      if Invention.fParent = ''
        then
          begin
            result := TreeView.Items.AddChildObjectFirst(nil, Invention.fName, Invention);
            result.ImageIndex    := isEnabled;
            result.SelectedIndex := isEnabled;
          end
        else
          begin
            PrStr := Invention.fParent;
            if Cache = nil
              then
                begin
                  while (i < cnt) and (TreeView.Items[i].Text <> PrStr) do
                    inc( i );
                  if i = cnt
                    then Parent := TreeView.Items.AddChildObject(nil, PrStr, nil)
                    else Parent := TreeView.Items[i];
                end
              else
                begin
                  idx := Cache.IndexOf(PrStr);
                  if (idx <> -1)
                    then Parent := TTreeNode(Cache.Objects[idx])
                    else
                      begin
                        Parent := TreeView.Items.AddChildObject(nil, PrStr, nil);
                        Cache.AddObject( PrStr, Parent );
                      end;
                end;
            result := TreeView.Items.AddChildObject(Parent, Invention.fName, Invention);
            if Invention.fEnabled
              then
                begin
                  Parent.ImageIndex    := isEnabled;
                  Parent.SelectedIndex := isEnabled;
                  result.ImageIndex    := isEnabled;
                  result.SelectedIndex := isEnabled;
                end
              else
                begin
                  result.ImageIndex    := isDissabled;
                  result.SelectedIndex := isDissabled;
                end;
          end;
    end;

  procedure TInventionsSheetViewer.tvAvailInventionsDeletion(Sender: TObject; Node: TTreeNode);
    begin
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            // TObject(Node.Data).Free; >> Not needed because of caching!
            Node.Data := nil;
          end;
    end;

  procedure TInventionsSheetViewer.lvAvailInventionsDeletion(Sender: TObject; Item: TListItem);
    begin
      if (Item <> nil) and (Item.Data <> nil)
        then
          begin
            TObject(Item.Data).Free;
            Item.Data := nil;
          end;
    end;

  procedure TInventionsSheetViewer.tvAvailInventionsChange(Sender: TObject; Node: TTreeNode);
    var
      Inv : TInventionInfo;
    begin
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            Inv := TInventionInfo(Node.Data);
            if Inv.fCache // >> Used to be fLoaded!
              then
                begin
                  lbAvlProps.Caption  := uppercase(Inv.fProps);
                  lbAvlDesc.Caption   := Inv.fDesc;
                end
              else
                begin
                  lbAvlProps.Caption := '';
                  lbAvlDesc.Caption  := '';
                  Threads.Fork(fHandler.threadedGetResearchInfo, priNormal, [fHandler.fLastUpdate, Inv.fId, lbAvlProps, lbAvlDesc, Inv]);
                end;
            btnResearch.Enabled := fHandler.fOwnsFacility and Inv.fEnabled;
          end
        else
          begin
            lbAvlProps.Caption  := '';
            lbAvlDesc.Caption   := '';
            btnResearch.Enabled := false;
          end;
    end;

  procedure TInventionsSheetViewer.tvAlrDevInventionsChange(Sender: TObject; Node: TTreeNode);
    var
      Inv : TInventionInfo;
    begin
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            Inv := TInventionInfo(Node.Data);
            //lbSellValue.Caption := GetLiteral('Literal45') + Inv.fValue + ')';
            //lbSellValue.Caption := 'Refund amount: (' + Inv.fValue + ')';
            //btnSell.Text := 'Sell (+' + Inv.fValue + ')';
            btnSell.Text := GetFormattedLiteral('Literal45', [Inv.fValue]);
            btnSell.Enabled := fHandler.fOwnsFacility;
            if Inv.fCache // >> Used to be fLoaded!
              then
                begin
                  lbDevProps.Caption  := uppercase(TInventionInfo(Node.Data).fProps);
                  lbDevDesc.Caption   := TInventionInfo(Node.Data).fDesc;
                end
              else
                begin
                  lbDevProps.Caption  := '';
                  lbDevDesc.Caption   := '';
                  Threads.Fork(fHandler.threadedGetResearchInfo, priNormal, [fHandler.fLastUpdate, Inv.fId, lbDevProps, lbDevDesc, Inv]);
                end;
          end
        else
          begin
            lbDevProps.Caption  := '';
            lbDevDesc.Caption   := '';
            //lbSellValue.Caption := '';
            btnSell.Text := GetLiteral('Literal476');
            btnSell.Enabled     := false;
          end;
    end;


  procedure TInventionsSheetViewer.btnSellClick(Sender: TObject);
    var
      Node   : TTreeNode;
      Parent : TTreeNode;
    begin
      Node := tvAlrDevInventions.Selected;
      if (Node <> nil) and (Node.Data <> nil)
        then
          begin
            Threads.Fork(fHandler.threadedStopResearch, priNormal, [fHandler.fLastUpdate, TInventionInfo(Node.Data).fId]);
            with AddNode(tvAvailInventions, fHandler.LoadInvention(TInventionInfo(Node.Data).fId, TInventionInfo(Node.Data).fName, TInventionInfo(Node.Data).fParent), nil) do
              begin
                ImageIndex    := isEnabled;
                SelectedIndex := isEnabled;
              end;
            Node.Data := nil;
            Parent := Node.Parent;
            tvAlrDevInventions.Items.Delete(Node);
            if (Parent <> nil) and not Parent.HasChildren
              then tvAlrDevInventions.Items.Delete(Parent);
            tvAlrDevInventions.SetFocus;
          end;
    end;


  // TInventionInfo

  constructor TInventionInfo.Load( Stream : TStream );
    begin
      inherited Create;
      DelphiStreamUtils.ReadString(Stream, fId);
      DelphiStreamUtils.ReadString(Stream, fName);
      DelphiStreamUtils.ReadString(Stream, fDesc);
      DelphiStreamUtils.ReadString(Stream, fParent);
      Stream.ReadBuffer( fCache, sizeof(fCache) );
      DelphiStreamUtils.ReadString(Stream, fProps);
      fLoaded := true;
    end;

  constructor TInventionInfo.Create(Id, Name, Parent : string);
    begin
      inherited Create;
      fId     := Id;
      fName   := Name;
      fParent := Parent;
    end;






initialization

  SheetHandlerRegistry.RegisterSheetHandler('hdqInventions', InventionSheetHandlerCreator);

end.
