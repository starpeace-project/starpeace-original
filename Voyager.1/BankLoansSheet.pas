unit BankLoansSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, VisualControls, ComCtrls, ObjectInspectorInterfaces, SheetHandlers,
  InternationalizerComponent;


const
  tidCurrBlock   = 'CurrBlock';
  tidSecurityId  = 'SecurityId';
  tidLoanCount   = 'LoanCount';
  tidDebtor      = 'Debtor';
  tidInterest    = 'Interest';
  tidAmount      = 'Amount';
  tidTerm        = 'Term';

const
  facStoppedByTycoon  = $04;

type
  TBankLoansSheetHandler = class;

  TBankLoansSheetViewer = class(TVisualControl)
    Panel1: TPanel;
    lvLoans: TListView;
    InternationalizerComponent1: TInternationalizerComponent;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TBankLoansSheetHandler;
  end;

  TBankLoansSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl    : TBankLoansSheetViewer;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderLoan(const parms : array of const);
        procedure threadedRenderEmpty(const parms : array of const);
    end;

  function BankLoansSheetHandlerCreator : IPropertySheetHandler; stdcall;

var
  BankLoansSheetViewer: TBankLoansSheetViewer;

implementation

  uses
    Threads, SheetHandlerRegistry, MathUtils, SheetUtils, Literals;

{$R *.DFM}

  type
    PLoanInfoRec = ^TLoanInfoRec;
    TLoanInfoRec =
      record
        Debtor   : string;
        Amount   : string;
        Interest : string;
        Term     : string;
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

  // TBankLoansSheetHandler

  procedure TBankLoansSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TBankLoansSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TBankLoansSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TBankLoansSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TBankLoansSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            SheetUtils.ClearListView(fControl.lvLoans);
            SheetUtils.AddItem(fControl.lvLoans, [GetLiteral('Literal13')]).Data := self;
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TBankLoansSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvLoans);
    end;

  procedure TBankLoansSheetHandler.threadedRenderLoan(const parms : array of const);
    var
      Info : PLoanInfoRec absolute parms[0].vPointer;
    begin
      if fLastUpdate = parms[1].vInteger
        then
          begin
            if (fControl.lvLoans.Items.Count = 1) and (fControl.lvLoans.Items[0].Data = self)
              then fControl.lvLoans.Items.Clear;
            with fControl.lvLoans.Items.Add, Info^ do
              begin
                Caption := Debtor;
                SubItems.Add(Amount);
                SubItems.Add(Interest);
                SubItems.Add(Term);
              end;
          end;
    end;

  procedure TBankLoansSheetHandler.threadedRenderEmpty(const parms : array of const);
    begin
      if fLastUpdate = parms[0].vInteger
        then
          begin
            SheetUtils.ClearListView(fControl.lvLoans);
            SheetUtils.AddItem(fControl.lvLoans, [GetLiteral('Literal14')]);
          end;
    end;

  procedure TBankLoansSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Update : integer absolute parms[0].vInteger;
      Proxy  : OleVariant;
      Info   : TLoanInfoRec;
      count  : integer;
      i      : integer;
      iStr   : string;
      Values : TStringList;
    begin
      count := 0;
      try
        Proxy := fContainer.GetCacheObjectProxy;
        if not VarIsEmpty(Proxy) and (Update = fLastUpdate)
          then
            begin
              Values := TStringList.Create;
              try
                count := CvtToInt(Proxy.Properties(tidLoanCount));
                i := 0;
                while (i < count) and (Update = fLastUpdate) do
                  begin
                    iStr := IntToStr(i);
                    fContainer.GetPropertyArray(Proxy, [tidDebtor + iStr,
                      tidAmount + iStr,
                      tidInterest + iStr,
                      tidTerm + iStr],
                      Values);
                    if Update = fLastUpdate
                      then
                        begin
                          Info.Debtor   :=  Values.Values[tidDebtor + iStr];
                          Info.Amount   :=  FormatMoneyStr(Values.Values[tidAmount + iStr]);
                          Info.Interest :=  Values.Values[tidInterest + iStr];
                          Info.Term     :=  Values.Values[tidTerm + iStr];
                          Join(threadedRenderLoan, [@Info, Update]);
                        end;
                    inc( i );
                    Values.Clear;
                  end;
              finally
                Values.Free;
              end;
            end;
        if (Update = fLastUpdate) and (count = 0)
          then Join(threadedRenderEmpty, [Update]);
      except
      end;
    end;


  // TBankLoanSheetViewer

  procedure TBankLoansSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;


  // BankLoansSheetHandlerCreator

  function BankLoansSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TBankLoansSheetHandler.Create;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('BankLoans', BankLoansSheetHandlerCreator);

end.
