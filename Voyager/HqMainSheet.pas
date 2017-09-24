unit HqMainSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  tidCost       = 'Cost';
  tidROI        = 'ROI';

const
  facStoppedByTycoon  = $04;

type
  THqSheetHandler = class;

  THqSheetViewer = class(TVisualControl)
    xfer_Name: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Creator: TLabel;
    Label6: TLabel;
    xfer_Years: TLabel;
    NameLabel: TLabel;
    btnClose: TFramedButton;
    btnDemolish: TFramedButton;
    btnConnect: TFramedButton;
    Label8: TLabel;
    lbCost: TLabel;
    Label9: TLabel;
    lbROI: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    fbResearches: TFramedButton;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure fbResearchesClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler    : THqSheetHandler;
    fInvHandler : IPropertySheetHandler;
    fLstUpd : integer;
  end;


  THqSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : THqSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Refresh; override;
        procedure Clear; override;
      private
        procedure SetName(str : string);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  HqSheetViewer: THqSheetViewer;

  function HqSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, Protocol, MathUtils,
    SheetUtils, MessageBox,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    InventionsSheet,
    Literals;

{$R *.DFM}

  // THqSheetHandler

  function THqSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := THqSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function THqSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure THqSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
      roi     : integer;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if fOwnsFacility
          then
            try
              fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
            except
              fCurrBlock := 0;
            end
          else fCurrBlock := 0;
        try
          if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
            then fControl.btnClose.Text := GetLiteral('Literal128')
            else fControl.btnClose.Text := GetLiteral('Literal129');
        except
        end;
        fControl.NameLabel.Caption   := fControl.xfer_Name.Text;
        fControl.NameLabel.Visible   := not fOwnsFacility;
        fControl.xfer_Name.Enabled   := fOwnsFacility;
        fControl.xfer_Name.Visible   := fOwnsFacility;
        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);
        fControl.btnClose.Enabled    := fOwnsFacility;
        fControl.btnDemolish.Enabled := fOwnsFacility;
        fControl.btnConnect.Enabled  := true;

        fControl.lbCost.Caption := MathUtils.FormatMoneyStr(Properties.Values[tidCost]);
        try
          if Properties.Values[tidROI] <> ''
            then roi := round(StrToFloat(Properties.Values[tidROI]))
            else roi := 0;
          if roi = 0
            then fControl.lbROI.Caption  := GetLiteral('Literal130')
            else
              if roi > 0
                then fControl.lbROI.Caption  := GetFormattedLiteral('Literal131', [Properties.Values[tidROI]])
                else fControl.lbROI.Caption  := GetLiteral('Literal132');
        except
          fControl.lbROI.Caption  := GetLiteral('Literal133');
        end;
      // If the research window is open then refresh it
      if (fControl.fInvHandler <> nil) and (fControl.fInvHandler.GetControl.Visible)
        then fControl.fInvHandler.Refresh;
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure THqSheetHandler.SetFocus;
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
            Names.Add(tidCost);
            Names.Add(tidROI);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure THqSheetHandler.Refresh;
    begin
      inherited;
    end;

  procedure THqSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption             := NA;
      fControl.xfer_Name.Text                := '';
      fControl.xfer_Name.Enabled             := false;
      fControl.xfer_Years.Caption            := NA;
      fControl.xfer_Creator.Caption          := NA;
      fControl.lbCost.Caption                := NA;
      fControl.lbROI.Caption                 := NA;
      fControl.btnClose.Enabled              := false;
      fControl.btnConnect.Enabled            := false;
      fControl.btnDemolish.Enabled           := false;
      if fControl.fInvHandler <> nil
        then fControl.fInvHandler.Clear;
    end;

  procedure THqSheetHandler.SetName(str : string);
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

  procedure THqSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      Update := parms[1].vInteger;
      try
        try
          Prop := fContainer.GetProperties(Names);
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure THqSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if parms[1].vInteger = fLastUpdate
            then RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
      end;
    end;


  // HqSheetHandlerCreator

  function HqSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := THqSheetHandler.Create;
    end;


  // THqSheetViewer

  procedure THqSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fHandler.SetName(xfer_Name.Text)
        else
          if Key in NotAllowedChars
            then Key := #0;
    end;

  procedure THqSheetViewer.btnCloseClick(Sender: TObject);
    begin
      if btnClose.Text = GetLiteral('Literal134')
        then
          begin
            fHandler.fContainer.GetMSProxy.Stopped := true;
            btnClose.Text := GetLiteral('Literal135');
          end
        else
          begin
            fHandler.fContainer.GetMSProxy.Stopped := false;
            btnClose.Text := GetLiteral('Literal136');
          end
    end;

  procedure THqSheetViewer.btnRepairClick(Sender: TObject);
    var
      Proxy : OleVariant;
    begin
      if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
        then
          try
            Proxy := fHandler.fContainer.GetMSProxy;
            Proxy.BindTo(fHandler.fCurrBlock);
            Proxy.RdoRepair;
          except
          end;
    end;

  procedure THqSheetViewer.btnDemolishClick(Sender: TObject);
    var
      Proxy : OleVariant;
      s     : string;
    begin
      s := Format(GetLiteral('Literal497'),[xfer_Name.text]);
      if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility and (ShowMsgBoxYN(GetLiteral('Literal498'), s, 1, true, true)=mrOk)
        then
          try
            Proxy := fHandler.GetContainer.GetMSProxy;
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

  procedure THqSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure THqSheetViewer.btnConnectClick(Sender: TObject);
    var
      url : string;
    begin
      url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
      fHandler.GetContainer.HandleURL(url, false);
    end;

  procedure THqSheetViewer.fbResearchesClick(Sender: TObject);
    var
      Ctrl : TControl;
    begin
      if fInvHandler = nil
        then
          begin
            fInvHandler := SheetHandlerRegistry.GetSheetHandler('hdqInventions');
            if fInvHandler <> nil
              then
                begin
                  fInvHandler.SetContainer(fHandler.GetContainer);
                  Ctrl := fInvHandler.CreateControl(self);
                end
              else Ctrl := nil;
          end
        else Ctrl := fInvHandler.GetControl;
      if Ctrl <> nil
        then
          begin
            if fLstUpd <> fHandler.fLastUpdate
              then
                begin
                  fLstUpd := fHandler.fLastUpdate;
                  fInvHandler.SetFocus;
                end;
            TForm(Ctrl).ShowModal;
          end;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('HqGeneral', HqSheetHandlerCreator);

end.

