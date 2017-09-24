unit VotesSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PercentEdit, ComCtrls, ExtCtrls, FramedButton, VisualControls,
  ObjectInspectorInterfaces, SheetHandlers, InternationalizerComponent,
  ImgList;

const
  tidSecurityId    = 'SecurityId';
  tidTrouble       = 'Trouble';
  tidCurrBlock     = 'CurrBlock';
  tidCampaignCount = 'CampaignCount';
  tidRulerName     = 'RulerName';
  tidRulerVotes    = 'RulerVotes';
  tidRulerCmpRat   = 'RulerCmpRat';
  tidRulerCmpPnts  = 'RulerCmpPnts';

type
  TVotesSheetHandler = class;

  TVotesSheetViewer =
    class(TVisualControl)
        Panel2: TPanel;
        rightPanel: TPanel;
        lvVotes: TListView;
        btnElect: TFramedButton;
        ImageList1: TImageList;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure btnElectClick(Sender: TObject);
        procedure lvVotesDblClick(Sender: TObject);
      private
        fHandler  : TVotesSheetHandler;
      private
        procedure SetVote(name : string);
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    end;

  TVotesSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TVotesSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderCandidate(const parms : array of const);
        procedure threadedClear(const parms : array of const);
        procedure threadedVote(const parms : array of const);
        procedure threadedGetVote(const parms : array of const);
        procedure threadedRenderVote(const parms : array of const);
    end;

var
  VotesSheetViewer: TVotesSheetViewer;

  function VotesSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  {$R *.DFM}
  uses
    Threads, SheetHandlerRegistry, Protocol, MathUtils, SheetUtils, Literals,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB;

  // TVotesSheetHandler

  procedure TVotesSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TVotesSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TVotesSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TVotesSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TVotesSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            try
              Names.Add(tidSecurityId);
              Names.Add(tidTrouble);
              Names.Add(tidCurrBlock);
              Names.Add(tidCampaignCount);
              Names.Add(tidCampaignCount);
              Names.Add(tidRulerName);
              Names.Add(tidRulerVotes);
              Names.Add(tidRulerCmpRat);
              Names.Add(tidRulerCmpPnts);
              Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
            except
              Names.Free;
            end;
          end;
    end;

  procedure TVotesSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvVotes);
    end;

  procedure TVotesSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names    : TStringList absolute parms[0].vPointer;
      Update   : integer;
      CrBlck   : string;
      Prop     : TStringList;
      Proxy    : OleVariant;
      i        : integer;
      count    : integer;
      iStr     : string;
      cand     : string;
      votes    : string;
      ratings  : string;
      ptStr    : string;
    begin
      Update := parms[1].vInteger;
      // Get Cached properties
      try
        if Update = fLastUpdate
          then Prop := fContainer.GetProperties(Names)
          else Prop := nil;
      finally
        Names.Free;
      end;
      Threads.Join(threadedClear, [Update]);
      if Prop <> nil
        then
          try
            if Update = fLastUpdate
              then
                begin
                  CrBlck := Prop.Values[tidCurrBlock];
                  if CrBlck <> ''
                    then fCurrBlock := StrToInt(CrBlck)
                    else fCurrBlock := 0;
                  fOwnsFacility := Protocol.GrantAccess(fContainer.GetClientView.getSecurityId, Prop.Values[tidSecurityId]);
                  if Prop.Values[tidCampaignCount] <> ''
                    then count := StrToInt(Prop.Values[tidCampaignCount])
                    else count := 0;
                end
              else count := 0;

            // el presidente
            if (Update = fLastUpdate) and (Prop.Values[tidRulerName] <> '') and (Prop.Values[tidRulerCmpPnts] <> '')
              then Threads.Join(threadedRenderCandidate, [Prop.Values[tidRulerName], 1, Prop.Values[tidRulerCmpRat], Prop.Values[tidRulerVotes], StrToInt(Prop.Values[tidRulerCmpPnts]), Update]);

            // the campaings
            if (Update = fLastUpdate) and (count > 0)
              then
                try
                  Proxy := fContainer.GetCacheObjectProxy;
                  if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
                    then
                      begin
                        i := 0;
                        while (Update = fLastUpdate) and (i < count) do
                          begin
                            iStr    := IntToStr(i);
                            cand    := Proxy.Properties('Candidate' + iStr);
                            votes   := Proxy.Properties('Votes' + iStr);
                            ratings := Proxy.Properties('CmpRat' + iStr);
                            ptStr   := Proxy.Properties('CmpPnts' + iStr);
                            if (Update = fLastUpdate) and (cand <> '') and (votes <> '') and (ptStr <> '')
                              then Threads.Join(threadedRenderCandidate, [cand, 0, ratings, votes, StrToInt(ptStr), Update]);
                            inc(i);
                          end;
                        end;
                except
                end;

            // Get who is he voting for here..    
            Threads.Fork(threadedGetVote, priNormal, [fLastUpdate]);

          finally
            fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
            Prop.Free;
          end;
    end;

  procedure TVotesSheetHandler.threadedRenderCandidate(const parms : array of const); // Name, Type, Rating, Votes, Points, LastUpdate
    var
      Item   : TListItem;
      i      : integer;
      points : integer;
    begin
      if parms[5].vInteger = fLastUpdate
        then
          try
            points := parms[4].vInteger;
            i := 0;
            while (i < fControl.lvVotes.Items.Count) and (integer(fControl.lvVotes.Items[i].Data) >= points) do
              inc(i);
            if i = fControl.lvVotes.Items.Count
              then Item := fControl.lvVotes.Items.Add
              else Item := fControl.lvVotes.Items.Insert(i);
            Item.Caption := parms[0].vPChar;
            Item.SubItems.Add(parms[2].vPChar + '%');
            Item.SubItems.Add(parms[3].vPChar);
            Item.SubItems.Add(IntToStr(points));
            case parms[1].vInteger of
              0: Item.ImageIndex := -1;
              1: Item.ImageIndex := 1;
              else
                Item.ImageIndex := 0;
            end;
            Item.Data := pointer(points);
          except
          end;
    end;

  procedure TVotesSheetHandler.threadedClear(const parms : array of const);
    begin
      if parms[0].vInteger = fLastUpdate
        then SheetUtils.ClearListView(fControl.lvVotes);
    end;

  procedure TVotesSheetHandler.threadedVote(const parms : array of const);
    var
      MSProxy : OleVariant;
      voter   : string;
      votee   : string;
    begin
      if fLastUpdate = parms[0].vInteger
        then
          try
            voter := GetContainer.GetClientView.getUserName;
            votee := parms[1].vPChar;
            MSProxy := fContainer.GetMSProxy;
            MSProxy.BindTo(fCurrBlock);
            MSProxy.RDOVote(voter, votee);
          except
          end;
    end;

  procedure TVotesSheetHandler.threadedGetVote(const parms : array of const);
    var
      MSProxy : OleVariant;
      voter   : string;
      votee   : string;
    begin
      if fLastUpdate = parms[0].vInteger
        then
          try
            voter := GetContainer.GetClientView.getUserName;
            MSProxy := fContainer.GetMSProxy;
            MSProxy.BindTo(fCurrBlock);
            votee := MSProxy.RDOVoteOf(voter);
            if votee <> ''
              then Threads.Join(threadedRenderVote, [fLastUpdate, votee]);
          except
          end;
    end;

  procedure TVotesSheetHandler.threadedRenderVote(const parms : array of const);
    begin
      if fLastUpdate = parms[0].vInteger
        then fControl.SetVote(parms[1].vPChar);
    end;


  // VotesSheetHandlerCreator

  function VotesSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TVotesSheetHandler.Create;
    end;


  // TVotesSheetViewer

  procedure TVotesSheetViewer.btnElectClick(Sender: TObject);
    var
      i : integer;
    begin
      if lvVotes.Selected <> nil
        then
          begin
            lvVotes.Items[0].ImageIndex := 1;
            for i := 1 to pred(lvVotes.Items.Count) do
              if lvVotes.Items[i] <> lvVotes.Selected
                then lvVotes.Items[i].ImageIndex := -1;
            lvVotes.Selected.ImageIndex := 0;
            Threads.Fork(fHandler.threadedVote, priHigher, [fHandler.fLastUpdate, lvVotes.Selected.Caption]);
          end;
    end;

  procedure TVotesSheetViewer.SetVote(name : string);
    var
      i    : integer;
      Item : TListItem;
      flg  : boolean;
    begin
      flg  := false;
      Item := nil;
      for i := 0 to pred(lvVotes.Items.Count) do
        begin
          if CompareText(lvVotes.Items[i].Caption, name) = 0
            then Item := lvVotes.Items[i];
          flg := flg or (lvVotes.Items[i].ImageIndex = 0);
        end;
      if not flg and (Item <> nil)
        then Item.ImageIndex := 0;
    end;


  procedure TVotesSheetViewer.lvVotesDblClick(Sender: TObject);
    begin
      btnElectClick(Sender);
    end;

procedure TVotesSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
    dwStyles : DWORD;
  begin
    inherited;
    if (lvVotes <> nil)// and (lvVotes.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(lvVotes.Columns.Count-1) do
            inc(sum, lvVotes.Columns[i].Width);
          dwStyles := GetWindowLong(lvVotes.Handle, GWL_STYLE);
          if (dwStyles and WS_VSCROLL) <> 0
            then Inc(sum, GetSystemMetrics(SM_CXVSCROLL));
          lvVotes.Columns[lvVotes.Columns.Count-1].Width := lvVotes.Width-sum-2;
        end;
  end;

procedure TVotesSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvVotes.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvVotes.Handle, ' ', ' ');
          CoolSBEnableBar(lvVotes.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvVotes );
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Votes', VotesSheetHandlerCreator);

end.
