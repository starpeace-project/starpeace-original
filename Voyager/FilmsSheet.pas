unit FilmsSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidCurrBlock  = 'CurrBlock';
  tidInProd     = 'InProd';
  tidAutoRel    = 'AutoRel';
  tidAutoProd   = 'AutoProd';
  tidFilmDone   = 'FilmDone';

const
  facStoppedByTycoon  = $04;

type
  TFilmsSheetHandler = class;

  TFilmsSheetViewer = class(TVisualControl)
    xfer_FilmName: TEdit;
    Label1: TLabel;
    btnLaunch: TFramedButton;
    NameLabel: TLabel;
    Label2: TLabel;
    xfer_FilmBudget: TEdit;
    Label3: TLabel;
    cbAutoRelease: TCheckBox;
    btnCancelFilm: TFramedButton;
    btnReleaseFilm: TFramedButton;
    xfer_FilmTime: TEdit;
    InternationalizerComponent1: TInternationalizerComponent;
    cbAutoProduction: TCheckBox;
    procedure btnLaunchClick(Sender: TObject);
    procedure xfer_FilmTimeKeyPress(Sender: TObject; var Key: Char);
    procedure xfer_FilmBudgetKeyPress(Sender: TObject; var Key: Char);
    procedure xfer_FilmBudgetChange(Sender: TObject);
    procedure btnCancelFilmClick(Sender: TObject);
    procedure btnReleaseFilmClick(Sender: TObject);
    procedure xfer_FilmBudgetExit(Sender: TObject);
    procedure xfer_FilmTimeExit(Sender: TObject);
    procedure cbAutoProductionClick(Sender: TObject);
    procedure cbAutoProductionMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbAutoProductionMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler   : TFilmsSheetHandler;
    fBudgetStr : string;
    fMouseOp   : boolean;
  end;


  TFilmsSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TFilmsSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedLaunch(const parms : array of const);
        procedure threadedCancel(const parms : array of const);
        procedure threadedRelease(const parms : array of const);
        procedure threadedChangeAutoProd(const parms : array of const);
    end;

var
  FilmsSheetViewer: TFilmsSheetViewer;

  function FilmsSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, Protocol, MathUtils,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    SheetUtils, Literals;

{$R *.DFM}

  function CheckMoneyStr(str : string) : boolean;
    var
      i : integer;
    begin
      str := trim(str);
      if str <> ''
        then
          begin
            result := str[1] in ['$', '0'..'9'];
            i := 2;
            while result and (i < length(str)) do
              begin
                result := str[i] in [',', '0'..'9'];
                inc(i);
              end;
          end
        else result := false;
    end;

  function MoneyStrToStr(str : string) : string;
    var
      res : string;
      i   : integer;
    begin
      res := '';
      for i := 1 to length(str) do
        if str[i] in ['0'..'9']
          then res := res + str[i];
      result := res;
    end;

  function MoneyStrToCur(str : string) : currency;
    var
      res : string;
    begin
      res := MoneyStrToStr(str);
      if res <> ''
        then result := StrToCurr(res)
        else result := 0;
    end;


  // TFilmsSheetHandler

  procedure TFilmsSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TFilmsSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TFilmsSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TFilmsSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TFilmsSheetHandler.RenderProperties(Properties : TStringList);
    var
      inProd : boolean;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        inProd  := Properties.Values[tidInProd] <> '';
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if fOwnsFacility
          then
            try
              fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
            except
              fCurrBlock := 0;
            end
          else fCurrBlock := 0;
        if fOwnsFacility and not inProd
          then
            begin
             fControl.NameLabel.Visible := false;
             fControl.xfer_FilmName.Text := '';
             fControl.xfer_FilmName.Visible := true;
             fControl.xfer_FilmName.Enabled := true;
             fControl.btnLaunch.Enabled := true;
             fControl.cbAutoRelease.Enabled := true;
             fControl.xfer_FilmBudget.Enabled := true;
             fControl.xfer_FilmTime.Enabled := true;
             fControl.xfer_FilmTime.Text := '12';
            end
          else
            begin
              fControl.xfer_FilmName.Visible := false;
              fControl.NameLabel.Caption := fControl.xfer_FilmName.Text;
              fControl.NameLabel.Visible := true;
              fControl.btnLaunch.Enabled := false;
              fControl.cbAutoRelease.Enabled := false;
              fControl.xfer_FilmBudget.Enabled := false;
              fControl.xfer_FilmTime.Enabled := false;
            end;
        fControl.btnReleaseFilm.Enabled := fOwnsFacility and (Properties.Values[tidFilmDone] = 'YES');
        fControl.btnCancelFilm.Enabled  := fOwnsFacility and inProd;
        fControl.cbAutoRelease.Checked  := Properties.Values[tidAutoRel] <> 'NO';
        fControl.fMouseOp := false;
        fControl.cbAutoProduction.Enabled := fOwnsFacility;
        fControl.cbAutoProduction.Checked := Properties.Values[tidAutoProd] = 'YES';
        if fControl.xfer_FilmBudget.Text = ''
          then fControl.xfer_FilmBudget.Text := '$10,000,000';
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TFilmsSheetHandler.SetFocus;
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
            Names.Add(tidCurrBlock);
            Names.Add(tidInProd);
            Names.Add(tidAutoRel);
            Names.Add(tidAutoProd);
            Names.Add(tidFilmDone);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TFilmsSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption     := NA;
      fControl.xfer_FilmName.Text    := '';
      fControl.xfer_FilmName.Enabled := false;
      fControl.btnLaunch.Enabled     := false;
      fControl.btnCancelFilm.Enabled := false;
      fControl.cbAutoRelease.Enabled := false;
      fControl.cbAutoRelease.Checked := false;
      fControl.fMouseOp := false;
      fControl.cbAutoProduction.Enabled := false;
      fControl.cbAutoProduction.Checked := false;
    end;

  procedure TFilmsSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TFilmsSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TFilmsSheetHandler.threadedLaunch(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
      theName : widestring;
      months : integer;
      budget : double;
      autoRel : word; //WordBool;
    begin
      block   := parms[0].vInteger;
      theName := parms[1].vPChar;
      budget  := StrToFloat(parms[2].vPChar);
      months  := parms[3].vInteger;
      if parms[4].VBoolean
        then autoRel := $01
        else autoRel := $00;
      if parms[5].VBoolean
        then autoRel := autoRel or $02;
      //autoRel := parms[4].VBoolean;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.RDOLaunchMovie(theName, budget, months, autoRel);
                end;
          except
          end;
    end;

  procedure TFilmsSheetHandler.threadedCancel(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
    begin
      block := parms[0].vInteger;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.RDOCancelMovie(0);
                end;
          except
          end;
    end;

  procedure TFilmsSheetHandler.threadedRelease(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
    begin
      block := parms[0].vInteger;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.RDOReleaseMovie(0);
                end;
          except
          end;
    end;

  procedure TFilmsSheetHandler.threadedChangeAutoProd(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
      value : WordBool;
    begin
      block := parms[0].vInteger;
      value := parms[1].vBoolean;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.RDOAutoProduce(value);
                end;
          except
          end;
    end;

  // TFilmsSheetViewer

  procedure TFilmsSheetViewer.btnLaunchClick(Sender: TObject);
    begin
      if CheckMoneyStr(xfer_FilmBudget.Text)
        then Threads.Fork(fHandler.threadedLaunch, priNormal, [fHandler.fCurrBlock, xfer_FilmName.Text, MoneyStrToStr(xfer_FilmBudget.Text), StrToInt(xfer_FilmTime.Text), cbAutoRelease.Checked, cbAutoProduction.Checked])
        else Beep;
    end;

  procedure TFilmsSheetViewer.btnCancelFilmClick(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedCancel, priNormal, [fHandler.fCurrBlock]);
    end;

  procedure TFilmsSheetViewer.btnReleaseFilmClick(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedRelease, priNormal, [fHandler.fCurrBlock]);
    end;

  procedure TFilmsSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // FilmsSheetHandlerCreator

  function FilmsSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TFilmsSheetHandler.Create;
    end;


  procedure TFilmsSheetViewer.xfer_FilmTimeKeyPress(Sender: TObject; var Key: Char);
    begin
      if not (Key in [#9, #13, '0'..'9'])
        then Key := #0;
    end;

  procedure TFilmsSheetViewer.xfer_FilmBudgetKeyPress(Sender: TObject; var Key: Char);
    begin
      if not (Key in ['$', #9, #13, ',', '0'..'9'])
        then Key := #0;
    end;

  procedure TFilmsSheetViewer.xfer_FilmBudgetChange(Sender: TObject);
    begin
      if CheckMoneyStr(xfer_FilmBudget.Text)
        then fBudgetStr := xfer_FilmBudget.Text
        else xfer_FilmBudget.Text := fBudgetStr;
    end;

  procedure TFilmsSheetViewer.xfer_FilmBudgetExit(Sender: TObject);
    begin
      xfer_FilmBudget.Text := FormatMoney(MoneyStrToCur(xfer_FilmBudget.Text));
    end;

  procedure TFilmsSheetViewer.xfer_FilmTimeExit(Sender: TObject);
    var
      aux : integer;
    begin
      try
        aux := StrToInt(xfer_FilmTime.Text);
      except
        aux := 6;
      end;
      if aux < 6
        then aux := 6
        else
          if aux > 30
            then aux := 30;
      xfer_FilmTime.Text := IntToStr(aux);
    end;

  procedure TFilmsSheetViewer.cbAutoProductionClick(Sender: TObject);
    begin
      if fMouseOp and (Sender = cbAutoProduction)
        then
          begin
            Threads.Fork(fHandler.threadedChangeAutoProd, priNormal, [fHandler.fCurrBlock, cbAutoProduction.Checked]);
            fMouseOp := false;
          end;
    end;

  procedure TFilmsSheetViewer.cbAutoProductionMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMouseOp := cbAutoProduction.Enabled;
    end;

  procedure TFilmsSheetViewer.cbAutoProductionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMouseOp := cbAutoProduction.Enabled;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Films', FilmsSheetHandlerCreator);

end.
