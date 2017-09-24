unit InventionsSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ComCtrls, ExtCtrls, FingerTabs,
  InternationalizerComponent, ImgList, PDTabControl, Collection;

const
  tidSecurityId   = 'SecurityId';
  tidTrouble      = 'Trouble';
  tidCurrBlock    = 'CurrBlock';
  tidResearchKind = 'RsKind';

  RsId          = 'RsId';
  RsName        = 'RsName';
  RsParent      = 'RsParent';
  has           = 'has';
  RsCost        = 'RsCost';

  Count         = 'Count';
  dev           = 'dev';
  avlCount      = 'avlCount';
  hasCount      = 'hasCount';
  devCount      = 'devCount';

  avl           = 'avl';
  RsEnabled     = 'RsEnabled';

const
  facStoppedByTycoon  = $04;

//const
  //tidNoResearchSelected = 'No research selected.';

const
  isDissabled   = 0;
  isEnabled     = 1;
  isResearching = 2;

type
  TInventionsSheetHandler = class;
  TInventionInfo          = class;

  TInventionsSheetViewer = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Tabs: TPDTabControl;
    leftPanel: TPanel;
    btnClose: TFramedButton;
    ftInveKind: TFingerTabs;
    Notebook: TNotebook;
    Shape1: TShape;
    lbAvlDesc: TLabel;
    btnResearch: TFramedButton;
    Shape4: TShape;
    lbAvlProps: TLabel;
    tvAvailInventions: TTreeView;
    Shape2: TShape;
    lbSchDesc: TLabel;
    btnStop: TFramedButton;
    Shape6: TShape;
    lbSchProps: TLabel;
    lvSchedInvention: TListView;
    Shape3: TShape;
    lbDevDesc: TLabel;
    btnSell: TFramedButton;
    Shape5: TShape;
    lbDevProps: TLabel;
    tvAlrDevInventions: TTreeView;
    ImageList1: TImageList;
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
    procedure btnCloseClick(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabsTabChange(Sender: TObject);
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
        function  AddNode(TreeView : TTreeView; Invention : TInventionInfo; Cache : TStringList) : TTreeNode;
      private
        fHandler    : TInventionsSheetHandler;
        fLastFinger : integer;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;


  TInventionInfo =
    class
      public
        constructor Load( Stream : TStream );
        constructor Create(Id, Name, Parent : string);
      private
        fName    : string;
        fCat     : string;
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
        fTabs         : TStringList;
        fLastTab      : integer;
        fLoadTab      : integer;
        fAvailables   : TCollection;
        fScheduled    : TCollection;
        fDeveloped    : TCollection;
        fLoadedArr    : array[0..9] of boolean;
      public
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
        procedure RenderTab(index : integer);
        procedure GetInventionByTab;
    end;

var
  InventionsSheetViewer: TInventionsSheetViewer;

  function InventionSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, IniFiles, Protocol,
    DelphiStreamUtils, CompStringsParser, SheetUtils, ClassStorage, ClientMLS,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals, CoolSB;

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
        aux    : string;
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
          DelphiStreamUtils.ReadString(Stream, aux);
          fTabs := TStringList.Create;
          fTabs.Text := aux;
          fControl.Tabs.BeginUpdate;
          try
            for i := 0 to pred(fTabs.Count) do
              fControl.Tabs.AddObject(fTabs[i], nil);
          finally
            fControl.Tabs.EndUpdate;
          end;
        finally
          Stream.Free;
        end;
      end;

    begin
      fControl := TInventionsSheetViewer.Create(Owner);
      fControl.fHandler := self;

      if InitSkinImage
        then
          begin
            InitializeCoolSB(fControl.tvAvailInventions.Handle);
            fControl.tvAvailInventions.DoubleBuffered := true;
            InitializeCoolSB(fControl.tvAlrDevInventions.Handle);
            fControl.tvAlrDevInventions.DoubleBuffered := true;
            InitializeCoolSB(fControl.lvSchedInvention.Handle);
            fControl.lvSchedInvention.DoubleBuffered := true;
            if hThemeLib <> 0
              then
                begin
                  SetWindowTheme(fControl.tvAvailInventions.Handle, ' ', ' ');
                  SetWindowTheme(fControl.tvAlrDevInventions.Handle, ' ', ' ');
                  SetWindowTheme(fControl.lvSchedInvention.Handle, ' ', ' ');
                end;
          end;

      //fContainer.ChangeHeight(130);
      result := fControl;
      fControl.tvAvailInventions.Items.Add( nil, GetLiteral('Literal43') );
      fControl.tvAlrDevInventions.Items.Add( nil, GetLiteral('Literal44') );
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
      iStr : string;
      Info : TInventionInfo;
      tab  : integer;

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

      if fAvailables = nil
      then
        fAvailables := TCollection.Create(0, rkUse);
      if fScheduled = nil
      then
        fScheduled  := TCollection.Create(0, rkUse);
      if fDeveloped = nil
      then
        fDeveloped  := TCollection.Create(0, rkUse);

      // Availables
      cnt := CvtToInt(Properties.Values[avl + Count + IntToStr(fLoadTab)]);
      for i := 0 to pred(cnt) do
        begin
          iStr := IntToStr(i);
          Info := LoadInvention(Properties.Values[avl + IntToStr(fLoadTab) + RsId + iStr], Properties.Values[avl + IntToStr(fLoadTab) + RsName + iStr], Properties.Values[avl + IntToStr(fLoadTab) + RsParent + iStr]);
          if Info <> nil
            then
              begin
                Info.fEnabled := Properties.Values[avl + IntToStr(fLoadTab) + RsEnabled + iStr] = '1';
                fAvailables.Insert(Info);
              end;
        end;

      // Developed
      cnt := CvtToInt(Properties.Values[has + Count+ IntToStr(fLoadTab)]);
      for i := 0 to pred(cnt) do
        begin
          iStr := IntToStr(i);
          Info := LoadInvention(Properties.Values[has + IntToStr(fLoadTab) + RsId + iStr], Properties.Values[has + IntToStr(fLoadTab) + RsName + iStr], Properties.Values[has + IntToStr(fLoadTab) + RsParent + iStr]);
          if Info <> nil
            then
              begin
                Info.fValue := Properties.Values[has + IntToStr(fLoadTab) + RsCost + iStr];
                fDeveloped.Insert(Info);
              end;
        end;

      // Scheduled
      cnt := CvtToInt(Properties.Values[dev + Count+ IntToStr(fLoadTab)]);
      for i := 0 to pred(cnt) do
        begin
          iStr := IntToStr(i);
          Info := LoadInvention(Properties.Values[dev + IntToStr(fLoadTab) + RsId + iStr], Properties.Values[dev + IntToStr(fLoadTab) + RsName + iStr], Properties.Values[dev + IntToStr(fLoadTab) + RsParent + iStr]);
          if Info <> nil
            then
              begin
                Info.fEnabled := true;
                fScheduled.Insert(Info);
              end;
        end;

      tab      := fLastTab;
      fLoadedArr[fLoadTab] := true;
      fControl.ftInveKind.CurrentFinger := fControl.fLastFinger;
      //fControl.Tabs.CurrentTab          := noTab;
      //fControl.Tabs.CurrentTab          := tab;
      fLastTab := -1;

      fLoadedArr[fLoadTab] := true;
      RenderTab(fLoadTab);
    end;

  procedure TInventionsSheetHandler.SetFocus;
    begin
      if not fLoadedArr[0]
        then
          begin
            inherited;
            fLastTab := -1;
            fControl.Tabs.CurrentTab := 0;
            //GetInventionByTab;
            //SheetUtils.AddItem(fControl.lvAvailInventions, ['Downloading...']);
          end;
    end;

  procedure TInventionsSheetHandler.GetInventionByTab;
    var
      Names : TStringList;
      index : integer;
    begin
      Names := TStringList.Create;
      Names.Add(tidSecurityId);
      Names.Add(tidCurrBlock);
      Names.Add(tidResearchKind);
      Names.Add(hasCount + IntToStr(fLoadTab));
      Names.Add(devCount + IntToStr(fLoadTab));
      Names.Add(avlCount + IntToStr(fLoadTab));
      Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
    end;

  procedure TInventionsSheetHandler.Clear;
    var
      i : integer;
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
      fAvailables.Free;
      fAvailables := nil;
      fScheduled.Free;
      fScheduled := nil;
      fDeveloped.Free;
      fDeveloped := nil;
      fControl.Tabs.CurrentTab := -1;
      for i := 0 to 9 do
        fLoadedArr[i] := false;
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
              pName := RsKind + 'Count' + IntToStr(fLoadTab);
              aux   := Properties.Values[pName];
              cnt   := CvtToInt(aux);
              i     := 0;
              while (i < cnt) and (fLastUpdate = Update) do
                begin
                  iStr := IntToStr(i);
                  List.Add(RsKind + IntToStr(fLoadTab) + 'RsId' + iStr);
                  List.Add(RsKind + IntToStr(fLoadTab) + 'RsName' + iStr);
                  List.Add(RsKind + IntToStr(fLoadTab) + 'RsParent' + iStr);
                  if RsKind = 'avl'
                    then List.Add(avl + IntToStr(fLoadTab) + RsEnabled + iStr)
                    else
                      if RsKind = 'has'
                        then List.Add(has + IntToStr(fLoadTab) + RsCost + iStr);
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
      Inv   : TInventionInfo;
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
                    Props := Proxy.RDOGetInvPropsByLang(id, ActiveLanguage);
                    Inv := TInventionInfo(parms[4].vPointer);
                    if (Inv <> nil) and (Inv.fDesc = '')
                      then
                        begin
                          Proxy.BindTo(fCurrBlock);
                          try
                            Desc := Proxy.RDOGetInvDescEx(id, ActiveLanguage);
                          except
                            Desc := Proxy.RDOGetInvDesc(id);
                          end;
                        end
                      else Desc := Inv.fDesc;
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
    begin
      try
        result := TInventionInfo(TheClassStorage.ClassById['CachedInventions', Id]);
      except
        result := nil;
      end;
      if result = nil
        then result := TInventionInfo.Create(Id, Name, Parent);
    end;

  procedure TInventionsSheetHandler.RenderTab(index : integer);
    var
      RootCache : TStringList;
      Info      : TInventionInfo;
      Item      : TListItem;
      i, cnt    : integer;
      tName     : string;
    begin
      if fLoadedArr[index]
        then
          begin
            //RenderProperties;
            fLastTab := index;
            if fTabs.Count > index
              then tName := fTabs[index]
              else tName := '';
            cnt := fAvailables.Count;
            RootCache := TStringList.Create;
            if cnt < 20
              then fControl.tvAvailInventions.Items.BeginUpdate;
            try
              SupressToolTip(fControl.tvAvailInventions);
              fControl.tvAvailInventions.Items.Clear;
              for i := 0 to pred(cnt) do
                begin
                  Info := TInventionInfo(fAvailables[i]);
                  if (tName = '') or (Info.fCat = tName)
                    then fControl.AddNode(fControl.tvAvailInventions, Info, RootCache);
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
              cnt := fDeveloped.Count;
              for i := 0 to pred(cnt) do
                begin
                  Info := TInventionInfo(fDeveloped[i]);
                  if (tName = '') or (Info.fCat = tName)
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
              cnt := fScheduled.Count;
              for i := 0 to pred(cnt) do
                begin
                  Info := TInventionInfo(fScheduled[i]);
                  if (tName = '') or (Info.fCat = tName)
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
          end
        else
          begin
            fLoadTab := index;
            fControl.tvAvailInventions.Items.Clear;
            fControl.tvAvailInventions.Items.Add( nil, GetLiteral('Literal43') );
            fControl.tvAlrDevInventions.Items.Clear;
            fControl.tvAlrDevInventions.Items.Add( nil, GetLiteral('Literal44') );
            GetInventionByTab;
          end;

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

            fHandler.fAvailables.Extract(TInventionInfo(SNode.Data));
            fHandler.fScheduled.Insert(TInventionInfo(SNode.Data));

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

            fHandler.fScheduled.Extract(TInventionInfo(SItem.Data));
            fHandler.fAvailables.Insert(TInventionInfo(SItem.Data));

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
                        lbSchProps.Caption := UpperCase(Inv.fProps);
                        btnStop.Enabled    := fHandler.fOwnsFacility;
                      end
                    else
                      begin
                        lbSchDesc.Caption  := '';
                        lbSchProps.Caption := '';
                        Threads.Fork(fHandler.threadedGetResearchInfo, priNormal, [fHandler.fLastUpdate, Inv.fId, lbSchProps, lbSchDesc, Inv]);
                        btnStop.Enabled := fHandler.fOwnsFacility;
                      end;
                end;
          end
        else
          begin
            lbSchDesc.Caption  := '';//tidNoResearchSelected;
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
            if false //Inv.fCache // >> Used to be fLoaded!
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

            fHandler.fDeveloped.Extract(TInventionInfo(Node.Data));
            fHandler.fAvailables.Insert(TInventionInfo(Node.Data));

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

  procedure TInventionsSheetViewer.SetParent(which: TWinControl);
    begin
      inherited;
    end;

// TInventionInfo

  constructor TInventionInfo.Load( Stream : TStream );
    var
      p   : integer;
      aux : string;
    begin
      inherited Create;
      DelphiStreamUtils.ReadString(Stream, fId);
      DelphiStreamUtils.ReadString(Stream, aux);

      p := 1;
      fName := CompStringsParser.GetNextStringUpTo(aux, p, '|');
      fCat  := UpperCase(CompStringsParser.GetNextStringUpTo(aux, p, '|'));

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

  procedure TInventionsSheetViewer.btnCloseClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TInventionsSheetViewer.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    const
      SC_DragMove = $F012;
    begin
      ReleaseCapture;
      perform(WM_SysCommand, SC_DragMove, 0);
    end;

  procedure TInventionsSheetViewer.TabsTabChange(Sender: TObject);
    begin
      if (Tabs.CurrentTab >= 0) and (Tabs.CurrentTab <> fHandler.fLastTab)
        then
          begin
            fHandler.fLastTab := Tabs.CurrentTab;
            fHandler.RenderTab(Tabs.CurrentTab);
          end;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('hdqInventions', InventionSheetHandlerCreator);

end.
