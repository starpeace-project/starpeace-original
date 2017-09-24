unit BankGeneralSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  //tidOwner      = 'Owner';
  tidEstLoan    = 'EstLoan';
  tidBudget     = 'Budget';
  tidInterest   = 'Interest';
  tidTerm       = 'Term';

type
  TBankRequestResult = (brqApproved, brqRejected, brqNotEnoughFunds, brqError);

const
  facStoppedByTycoon  = $04;

type
  TBankGeneralSheetHandler = class;

  TBankGeneralSheetViewer = class(TVisualControl)
    xfer_Name: TEdit;
    peBankBudget: TPercentEdit;
    Label7: TLabel;
    lbBudget: TLabel;
    Label12: TLabel;
    peInterest: TPercentEdit;
    lbInterest: TLabel;
    NameLabel: TLabel;
    cbLendNeutral: TCheckBox;
    Label5: TLabel;
    cbLendEnemy: TCheckBox;
    Label10: TLabel;
    Label2: TLabel;
    peTerm: TPercentEdit;
    lbTerm: TLabel;
    eBorrow: TEdit;
    fbRequest: TFramedButton;
    btnDemolish: TFramedButton;
    btnClose: TFramedButton;
    Label3: TLabel;
    Label4: TLabel;
    Shape2: TShape;
    Label1: TLabel;
    LoanResult: TPanel;
    lbBankResult: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure peBankBudgetChange(Sender: TObject);
    procedure peInterestChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure peBankBudgetMoveBar(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure fbRequestClick(Sender: TObject);
    procedure peTermMoveBar(Sender: TObject);
    procedure peTermChange(Sender: TObject);
    procedure eBorrowEnter(Sender: TObject);
    procedure eBorrowKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler    : TBankGeneralSheetHandler;
  end;


  TBankGeneralSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TBankGeneralSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure SetName(str : string);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  BankGeneralSheetViewer: TBankGeneralSheetViewer;

  function BankGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, MathUtils, StrUtils,
    Protocol, SheetUtils, Literals;

{$R *.DFM}

  // TBankGeneralSheetHandler

  procedure TBankGeneralSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TBankGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TBankGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TBankGeneralSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TBankGeneralSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
    begin
      FiveViewUtils.SetViewProp(fControl, Properties);
      trouble := Properties.Values[tidTrouble];
      fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
      if fOwnsFacility
        then
          begin
           fControl.NameLabel.Visible := false;
           fControl.xfer_Name.Visible := true;
           fControl.xfer_Name.Enabled := true;
          end
        else
          begin
            fControl.xfer_Name.Visible := false;
            fControl.NameLabel.Caption := fControl.xfer_Name.Text;
            fControl.NameLabel.Visible := true;
          end;
      try
        if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
          then fControl.btnClose.Text := GetLiteral('Literal4')
          else fControl.btnClose.Text := GetLiteral('Literal5');
      except
      end;
      fControl.btnClose.Enabled := fOwnsFacility;
      fControl.btnDemolish.Enabled := fOwnsFacility;
      fControl.fbRequest.Enabled := not fOwnsFacility;
      fControl.peInterest.Enabled := fOwnsFacility;
      fControl.peTerm.Enabled := fOwnsFacility;
      fControl.peBankBudget.Enabled := fOwnsFacility;
      fControl.cbLendNeutral.Enabled := fOwnsFacility;
      fControl.cbLendEnemy.Enabled := fOwnsFacility;
      fControl.eBorrow.Enabled := not fOwnsFacility;
      if not fOwnsFacility
        then fControl.eBorrow.Text := Properties.Values[tidEstLoan];
      try
        fControl.peBankBudget.Value := StrToInt(Properties.Values[tidBudget]);
      except
      end;
      try
        fControl.peInterest.Value := StrToInt(Properties.Values[tidInterest]);
      except
      end;
      try
        fControl.peTerm.Value := StrToInt(Properties.Values[tidTerm]);
      except
      end;
    end;

  procedure TBankGeneralSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Names.Add(tidSecurityId);
            Names.Add(tidTrouble);
            Names.Add(tidCurrBlock);
            //Names.Add(tidOwner);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TBankGeneralSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption    := NA;
      fControl.xfer_Name.Text       := '';
      fControl.NameLabel.Caption    := '';
      fControl.eBorrow.Text         := '';
      fControl.peBankBudget.Value   := 0;
      fControl.peInterest.Value     := 0;
      fControl.peTerm.Value         := 0;
      fControl.peBankBudget.Enabled := false;
      fControl.peInterest.Enabled   := false;
      fControl.peTerm.Enabled       := false;
      fControl.eBorrow.Enabled      := false;
      fControl.xfer_Name.Enabled    := false;
    end;

  procedure TBankGeneralSheetHandler.SetName(str : string);
    var
      MSProxy : OleVariant;
    begin
      try
        try
          fControl.Cursor := crHourGlass;
          MSProxy := fContainer.GetMSProxy;
          if not VarIsEmpty(MSProxy) and fOwnsFacility and CacheCommon.ValidName(str)
            then MSProxy.Name := str
            else Beep;
        finally
          fControl.Cursor := crDefault;
        end;
      except
      end;
    end;

  procedure TBankGeneralSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names    : TStringList absolute parms[0].vPointer;
      Update   : integer;
      Prop     : TStringList;
      aux      : string;
      MSProxy  : OleVariant;
      EstLoan  : currency;
      Budget   : integer;
      Interest : integer;
      Term     : integer;
    begin
      Update := parms[1].vInteger;
      try
        // Get Cached properties
        try
          Prop := fContainer.GetProperties(Names);
        finally
          Names.Free;
        end;
        // Get Model propeties
        aux := Prop.Values[tidCurrBlock];
        if aux <> ''
          then fCurrBlock := StrToInt(aux)
          else fCurrBlock := 0;
        if (fCurrBlock <> 0) and (Update = fLastUpdate)
          then
            begin
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              EstLoan  := realmax(0, MSProxy.RDOEstimateLoan(fContainer.GetClientView.getTycoonId)); // >>
              Budget   := MSProxy.BudgetPerc;
              Interest := MSProxy.Interest;
              Term     := MSProxy.Term;
              Prop.Values[tidEstLoan]  := Format( '%.0n', [int(EstLoan)] );
              Prop.Values[tidBudget]   := IntToStr(Budget);
              Prop.Values[tidInterest] := IntToStr(Interest);
              Prop.Values[tidTerm]     := IntToStr(Term);
            end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TBankGeneralSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      if fLastUpdate = parms[1].vInteger
        then
          try
            try
              RenderProperties(Prop);
            finally
              Prop.Free;
            end;
          except
          end;
    end;


  // BankGeneralSheetHandlerCreator

  function BankGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TBankGeneralSheetHandler.Create;
    end;

procedure TBankGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then fHandler.SetName(xfer_Name.Text)
      else
        if Key in NotAllowedChars
          then Key := #0;
  end;

procedure TBankGeneralSheetViewer.peBankBudgetMoveBar(Sender: TObject);
  begin
    lbBudget.Caption := IntToStr(peBankBudget.Value) + '%';
  end;

procedure TBankGeneralSheetViewer.peTermMoveBar(Sender: TObject);
  begin
    lbTerm.Caption := IntToStr(peTerm.Value);
  end;

procedure TBankGeneralSheetViewer.peBankBudgetChange(Sender: TObject);
  var
    Proxy  : OleVariant;
    wfansw : boolean;
  begin
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
      then
        try
          Proxy  := fHandler.fContainer.GetMSProxy;
          wfansw := Proxy.WaitForAnswer;
          Proxy.BindTo(fHandler.fCurrBlock);
          Proxy.RDOSetLoanPerc(peBankBudget.Value);
          Proxy.WaitForAnswer := wfansw;
        except
        end;
  end;

procedure TBankGeneralSheetViewer.peInterestChange(Sender: TObject);
  var
    Proxy : OleVariant;
    wfansw : boolean;
  begin
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
      then
        try
          Proxy  := fHandler.fContainer.GetMSProxy;
          Proxy.BindTo(fHandler.fCurrBlock);
          wfansw := Proxy.WaitForAnswer;
          Proxy.Interest := peInterest.Value;
          Proxy.WaitForAnswer := wfansw;
        except
        end;
  end;

procedure TBankGeneralSheetViewer.peTermChange(Sender: TObject);
  var
    Proxy : OleVariant;
    wfansw : boolean;
  begin
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
      then
        try
          Proxy  := fHandler.fContainer.GetMSProxy;
          wfansw := Proxy.WaitForAnswer;
          Proxy.BindTo(fHandler.fCurrBlock);
          Proxy.Term := peInterest.Value;
          Proxy.WaitForAnswer := wfansw;
        except
        end;
  end;

procedure TBankGeneralSheetViewer.btnCloseClick(Sender: TObject);
  begin
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
      then
        if btnClose.Text = GetLiteral('Literal6')
          then
            begin
              fHandler.fContainer.GetMSProxy.Stopped := true;
              btnClose.Text := GetLiteral('Literal7');
            end
          else
            begin
              fHandler.fContainer.GetMSProxy.Stopped := false;
              btnClose.Text := GetLiteral('Literal8');
            end;
  end;

procedure TBankGeneralSheetViewer.btnDemolishClick(Sender: TObject);
  var
    Proxy : OleVariant;
  begin
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
      then
        try
          Proxy := fHandler.fContainer.GetMSProxy;
          if not VarIsEmpty(Proxy)
            then
              begin
                Proxy.BindTo('World');
                if Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos) <> NOERROR
                  then Beep;
              end;
        except
        end;
  end;

procedure TBankGeneralSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TBankGeneralSheetViewer.fbRequestClick(Sender: TObject);
  var
    Proxy  : OleVariant;
    Amount : string;
    Answ   : TBankRequestResult;
    SecId  : string;
  begin
    if (fHandler.fCurrBlock <> 0) and not fHandler.fOwnsFacility
      then
        begin
          try
            Proxy  := fHandler.fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(fHandler.fCurrBlock);
                  Amount := KillSpaces( ReplaceChar( eBorrow.Text, ',', ' ' ));
                  SecId  := fHandler.fContainer.GetClientView.getSecurityId;
                  if SecId <> ''
                    then Answ := Proxy.RDOAskLoan(StrToInt(SecId), Amount)
                    else Answ := brqError;
                end
              else Answ := brqError;
          except
            Answ := brqError;
          end;
          case Answ of
            brqApproved :
              begin
                lbBankResult.Caption := GetLiteral('Literal9');
                LoanResult.Color := clGreen;
              end;
            brqRejected :
              begin
                lbBankResult.Caption := GetLiteral('Literal10');
                LoanResult.Color := clMaroon;
              end;
            brqNotEnoughFunds :
              begin
                lbBankResult.Caption := GetLiteral('Literal11');
                LoanResult.Color := clMaroon;
              end;
            brqError :
              begin
                lbBankResult.Caption := GetLiteral('Literal12');
                LoanResult.Color := clMaroon;
              end;
          end;
          LoanResult.Visible := true;
        end;
  end;

procedure TBankGeneralSheetViewer.eBorrowEnter(Sender: TObject);
  begin
    LoanResult.Visible := false;
  end;

procedure TBankGeneralSheetViewer.eBorrowKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    LoanResult.Visible := false;
  end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('BankGeneral', BankGeneralSheetHandlerCreator);

end.
