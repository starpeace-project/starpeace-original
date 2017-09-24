unit IndustrySheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers;


const
  AllowedChars = ['0'..'9', 'a'..'z', 'A'..'Z', '.', ' ', #8, '-', '_', ',', ';', '(', ')', '{', '}'];

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';

const
  facStoppedByTycoon  = $04;

type
  TResidentialSheetHandler = class;

  TResidentialSheetViewer = class(TVisualControl)
    xfer_Name: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Rent: TPercentEdit;
    xfer_Creator: TLabel;
    Label4: TLabel;
    xfer_CrimeRes: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    xfer_PollutionRes: TLabel;
    xfer_GeneralQ: TLabel;
    Label8: TLabel;
    xfer_Cost: TLabel;
    Label9: TLabel;
    xfer_Cluster: TLabel;
    Label10: TLabel;
    xfer_Capacity: TLabel;
    Label6: TLabel;
    xfer_MetaFacilityName: TLabel;
    Label7: TLabel;
    Rent: TLabel;
    Label12: TLabel;
    xfer_Maintenance: TPercentEdit;
    Maintenance: TLabel;
    btnClose: TFramedButton;
    btnRepair: TFramedButton;
    btnDemolish: TFramedButton;
    btnSell: TFramedButton;
    NameLabel: TLabel;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure xfer_RentChange(Sender: TObject);
    procedure xfer_MaintenanceChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure xfer_RentMoveBar(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler    : TResidentialSheetHandler;
  end;


  TResidentialSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TResidentialSheetViewer;
        fSecurityId   : string;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
      private
        procedure SetName(str : string);
    end;

var
  ResidentialSheetViewer: TResidentialSheetViewer;

  function ResidentialSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, Protocol, MessageBox, Literal;

{$R *.DFM}

  // TResidentialSheetHandler

  procedure TResidentialSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
      fSecurityId := fContainer.GetClientView.getSecurityId;
    end;

  function TResidentialSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TResidentialSheetViewer.Create(Owner);
      fControl.fHandler := self;
      fContainer.ChangeHeight(150 - fContainer.GetMinHeight);
      result := fControl;
    end;

  procedure TResidentialSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
    begin
      FiveViewUtils.SetViewProp(fControl, Properties);
      trouble := Properties.Values[tidTrouble];
      fOwnsFacility := GrantAccess( fSecurityId, Properties.Values[tidSecurityId] );
      if fOwnsFacility
        then
          begin
            try
              fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
            except
              fCurrBlock := 0;
            end;
           fControl.NameLabel.Visible := false;
           fControl.xfer_Name.Visible := true;
           fControl.xfer_Name.SetFocus;
          end
        else
          begin
            fCurrBlock := 0;
            fControl.xfer_Name.Visible := false;
            fControl.NameLabel.Caption := fControl.xfer_Name.Text;
            fControl.NameLabel.Visible := true;
          end;
      try
        if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
          then fControl.btnClose.Text := GetLiteral('Literal413')
          else fControl.btnClose.Text := GetLiteral('Literal414');
      except
      end;
      fControl.xfer_Rent.Enabled := fOwnsFacility;
      fControl.xfer_Maintenance.Enabled := fOwnsFacility;
      fControl.btnClose.Enabled := fOwnsFacility;
      fControl.btnDemolish.Enabled := fOwnsFacility;
      fControl.btnRepair.Enabled := fOwnsFacility;
      fControl.btnSell.Enabled := fOwnsFacility;
      Properties.Free;
    end;

  procedure TResidentialSheetHandler.SetFocus;
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
            RenderProperties(fContainer.GetProperties(Names));
            Names.Free;
          end;
    end;

  procedure TResidentialSheetHandler.SetName(str : string);
    var
      MSProxy : OleVariant;
      CHProxy : OleVariant;
      SecId   : string;
      ObjId   : string;
    begin
      try
        try
          fControl.Cursor := crHourGlass;
          MSProxy := fContainer.GetMSProxy;
          CHProxy := fContainer.GetCacheObjectProxy;
          if not VarIsEmpty(MSProxy) and not VarIsEmpty(CHProxy)
            then
              begin
                SecId := CHProxy.Properties('SecurityId');
                ObjId := fContainer.GetClientView.getSecurityId;
                if SecId = ObjId
                  then MSProxy.Name := str
                  else
                    begin
                      fControl.xfer_Name.Text := CHProxy.Properties('Name');
                      Beep;
                    end;
              end;
        finally
          fControl.Cursor := crDefault;
        end;
      except
      end;
    end;

  function ResidentialSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TResidentialSheetHandler.Create;
    end;


procedure TResidentialSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then fHandler.SetName(xfer_Name.Text);
    if not(Key in AllowedChars)
      then Key := #0;
  end;

procedure TResidentialSheetViewer.xfer_RentMoveBar(Sender: TObject);
  begin
    Rent.Caption := IntToStr(xfer_Rent.Value) + '%';
  end;

procedure TResidentialSheetViewer.xfer_RentChange(Sender: TObject);
  var
    Proxy : OleVariant;
  begin
    if fHandler.fCurrBlock <> 0
      then
        try
          Proxy  := fHandler.fContainer.GetMSProxy;
          Proxy.BindTo(fHandler.fCurrBlock);
          Proxy.Rent := xfer_Rent.Value;
        except
        end;
  end;

procedure TResidentialSheetViewer.xfer_MaintenanceChange(Sender: TObject);
  var
    Proxy  : OleVariant;
  begin
    if fHandler.fCurrBlock <> 0
      then
        try
          Proxy  := fHandler.fContainer.GetMSProxy;
          Proxy.BindTo(fHandler.fCurrBlock);
          Proxy.Maintenance := xfer_Maintenance.Value;
        except
        end;
  end;

procedure TResidentialSheetViewer.btnCloseClick(Sender: TObject);
  begin
    if btnClose.Text = GetLiteral('Literal415')
      then
        begin
          fHandler.fContainer.GetMSProxy.Stopped := true;
          btnClose.Text := GetLiteral('Literal416');
        end
      else
        begin
          fHandler.fContainer.GetMSProxy.Stopped := false;
          btnClose.Text := GetLiteral('Literal417');
        end
  end;

procedure TResidentialSheetViewer.btnRepairClick(Sender: TObject);
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

procedure TResidentialSheetViewer.btnDemolishClick(Sender: TObject);
  var
    Proxy : OleVariant;
    s     : string;
  begin
    s := Format(GetLiteral('Literal497'),[xfer_Name.text]);
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility and (ShowMsgBoxYN(GetLiteral('Literal498'), s, 1, true, true)=mrOk)
      then
        try
          Proxy := fHandler.fContainer.GetMSProxy;
          if not VarIsEmpty(Proxy)
            then
              begin
                Proxy.BindTo('World');
                if not Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos)
                  then Beep;
              end;
        except
        end;
  end;

procedure TResidentialSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;


begin

  SheetHandlerRegistry.RegisterSheetHandler('ResGeneral', ResidentialSheetHandlerCreator);


end.
