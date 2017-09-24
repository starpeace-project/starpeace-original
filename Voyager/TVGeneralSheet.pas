unit TVGeneralSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  tidHoursOnAir = 'HoursOnAir';
  tidComercials = 'Comercials';
  tidCost       = 'Cost';
  tidROI        = 'ROI';
  
const
  facStoppedByTycoon  = $04;

type
  TTVGeneralSheetHandler = class;

  TTVGeneralSheetViewer = class(TVisualControl)
    xfer_Name: TEdit;
    Label1: TLabel;
    peAdvertisement: TPercentEdit;
    btnClose: TFramedButton;
    btnDemolish: TFramedButton;
    NameLabel: TLabel;
    lbHourOnAir: TLabel;
    peHoursOnAir: TPercentEdit;
    lbAdPerc: TLabel;
    btnConnect: TFramedButton;
    Label2: TLabel;
    xfer_Creator: TLabel;
    Label6: TLabel;
    xfer_Years: TLabel;
    Label8: TLabel;
    lbCost: TLabel;
    Label9: TLabel;
    lbROI: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure peHoursOnAirChange(Sender: TObject);
    procedure peHoursOnAirMoveBar(Sender: TObject);
    procedure peAdvertisementChange(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TTVGeneralSheetHandler;
  end;


  TTVGeneralSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TTVGeneralSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure SetName(str : string);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedChgHourOnAir(const parms : array of const);
        procedure threadedChgHourAds(const parms : array of const);
    end;

  function TVGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, Protocol, MathUtils,
    SheetUtils,MessageBox, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals;

{$R *.DFM}

  // TTVGeneralSheetHandler

  function TTVGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTVGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TTVGeneralSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTVGeneralSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
      roi     : integer;
      hours   : integer;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);

        trouble       := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess(fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId]); // Properties.Values[tidSecurityId] = fContainer.GetClientView.getSecurityId;
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

        fControl.btnConnect.Enabled      := true;
        fControl.btnClose.Enabled        := fOwnsFacility;
        fControl.btnDemolish.Enabled     := fOwnsFacility;
        fControl.peHoursOnAir.Enabled    := fOwnsFacility;
        fControl.peAdvertisement.Enabled := fOwnsFacility;

        try
          hours := StrToInt(Properties.Values[tidHoursOnAir]);
        except
          hours := 0;
        end;
        fControl.peHoursOnAir.Value := hours;

        try
          hours := StrToInt(Properties.Values[tidComercials]);
        except
          hours := 0;
        end;
        fControl.peAdvertisement.Value := hours;

        try
          if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
            then fControl.btnClose.Text := GetLiteral('Literal119')
            else fControl.btnClose.Text := GetLiteral('Literal120');
        except
        end;

        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);

        fControl.lbCost.Caption := MathUtils.FormatMoneyStr(Properties.Values[tidCost]);
        try
          roi := round(StrToFloat(Properties.Values[tidROI]));
          if roi = 0
            then fControl.lbROI.Caption  := GetLiteral('Literal121')
            else
              if roi > 0
                then fControl.lbROI.Caption  := GetFormattedLiteral('Literal122', [Properties.Values[tidROI]])
                else fControl.lbROI.Caption  := GetLiteral('Literal123');
        except
          fControl.lbROI.Caption  := GetLiteral('Literal124');
        end;
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TTVGeneralSheetHandler.SetFocus;
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

  procedure TTVGeneralSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption             := NA;
      fControl.xfer_Name.Text                := '';
      fControl.xfer_Name.Enabled             := false;
      fControl.xfer_Years.Caption := NA;
      fControl.xfer_Creator.Caption          := NA;
      fControl.lbCost.Caption                := NA;
      fControl.lbROI.Caption                 := NA;
      fControl.btnClose.Enabled              := false;
      fControl.btnDemolish.Enabled           := false;
      fControl.btnConnect.Enabled            := false;
      fControl.peHoursOnAir.Value            := 0;
      fControl.peHoursOnAir.Enabled          := false;
      fControl.peAdvertisement.Value         := 0;
      fControl.peAdvertisement.Enabled       := false;
      fControl.lbHourOnAir.Caption           := NA;
      fControl.lbAdPerc.Caption              := NA;
    end;

  procedure TTVGeneralSheetHandler.SetName(str : string);
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

  procedure TTVGeneralSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names   : TStringList absolute parms[0].vPointer;
      Update  : integer;
      Prop    : TStringList;
      CrBlck  : string;
      MSProxy : OleVariant;
      hrs     : integer;
      cmm     : integer;
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
        // Get Model propeties
        if Update = fLastUpdate
          then
            begin
              CrBlck := Prop.Values[tidCurrBlock];
              if CrBlck <> ''
                then fCurrBlock := StrToInt(CrBlck)
                else fCurrBlock := 0;
            end
          else fCurrBlock := 0;
        if (fCurrBlock <> 0) and (Update = fLastUpdate)
          then
            begin
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              hrs := MSProxy.HoursOnAir;
              Prop.Values[tidHoursOnAir] := IntToStr(hrs);
              MSProxy.BindTo(fCurrBlock);
              cmm := MSProxy.Commercials;
              Prop.Values[tidComercials] := IntToStr(cmm);
            end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TTVGeneralSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TTVGeneralSheetHandler.threadedChgHourOnAir(const parms : array of const);
    var
      Proxy : OleVariant;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fCurrBlock <> 0) and fOwnsFacility
        then
          try
            Proxy := fContainer.GetMSProxy;
            Proxy.BindTo(fCurrBlock);
            Proxy.HoursOnAir := parms[1].vInteger;
          except
          end
    end;

  procedure TTVGeneralSheetHandler.threadedChgHourAds(const parms : array of const);
    var
      Proxy : OleVariant;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fCurrBlock <> 0) and fOwnsFacility
        then
          try
            Proxy := fContainer.GetMSProxy;
            Proxy.BindTo(fCurrBlock);
            Proxy.Commercials := parms[1].vInteger;
          except
          end
    end;


  // TVGeneralSheetHandlerCreator

  function TVGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTVGeneralSheetHandler.Create;
    end;

  procedure TTVGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fHandler.SetName(xfer_Name.Text)
        else
          if Key in NotAllowedChars
            then Key := #0;
    end;

  procedure TTVGeneralSheetViewer.btnCloseClick(Sender: TObject);
    begin
      if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
        then
          if btnClose.Text = GetLiteral('Literal125')
            then
              begin
                fHandler.fContainer.GetMSProxy.Stopped := true;
                btnClose.Text := GetLiteral('Literal126');
              end
            else
              begin
                fHandler.fContainer.GetMSProxy.Stopped := false;
                btnClose.Text := GetLiteral('Literal127');
              end;
    end;

  procedure TTVGeneralSheetViewer.btnDemolishClick(Sender: TObject);
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
                  if Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos) <> NOERROR
                    then Beep;
                end;
          except
          end;
    end;

  procedure TTVGeneralSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TTVGeneralSheetViewer.peHoursOnAirChange(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedChgHourOnAir, priNormal, [fHandler.fLastUpdate, peHoursOnAir.Value]);
    end;

  procedure TTVGeneralSheetViewer.peHoursOnAirMoveBar(Sender: TObject);
    begin
      lbHourOnAir.Caption := IntToStr(peHoursOnAir.Value);
    end;

  procedure TTVGeneralSheetViewer.peAdvertisementChange(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedChgHourAds, priNormal, [fHandler.fLastUpdate, peAdvertisement.Value]);
    end;

  procedure TTVGeneralSheetViewer.btnConnectClick(Sender: TObject);
    var
      url : string;
    begin
      url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
      fHandler.GetContainer.HandleURL(url, false);
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('TVGeneral', TVGeneralSheetHandlerCreator);

end.
