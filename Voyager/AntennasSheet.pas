unit AntennasSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, VisualControls, ComCtrls, ObjectInspectorInterfaces, SheetHandlers,
  InternationalizerComponent, ImgList;


const
  tidCurrBlock   = 'CurrBlock';
  tidSecurityId  = 'SecurityId';
  tidAntCount    = 'antCount';
  tidAntName     = 'antName';
  tidAntTown     = 'antTown';
  tidAntX        = 'antX';
  tidAntY        = 'antY';
  tidAntActive   = 'antActive';
  tidAntViewers  = 'antViewers';

const
  facStoppedByTycoon  = $04;

type
  TAntennasSheetHandler = class;

  TAntennasSheetViewer =
    class(TVisualControl)
        Panel1: TPanel;
        lvAntennas: TListView;
        ImageList1: TImageList;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure lvAntennasDeletion(Sender: TObject; Item: TListItem);
        procedure lvAntennasDblClick(Sender: TObject);
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      private
        fHandler : TAntennasSheetHandler;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    end;

  TAntennasSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TAntennasSheetViewer;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderAntenna(const parms : array of const);
        procedure threadedRenderEmpty(const parms : array of const);
    end;

  function AntennasSheetHandlerCreator : IPropertySheetHandler; stdcall;

var
  AntennasSheetViewer: TAntennasSheetViewer;

implementation

  uses
    Threads, SheetHandlerRegistry, MathUtils, SheetUtils, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals, CoolSB;

{$R *.DFM}

  type
    PAntennaInfoRec = ^TAntennaInfoRec;
    TAntennaInfoRec =
      record
        Name    : string;
        Town    : string;
        X, Y    : integer;
        Active  : boolean;
        Viewers : integer;
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

  // TAntennasSheetHandler

  procedure TAntennasSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TAntennasSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TAntennasSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TAntennasSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TAntennasSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            SheetUtils.ClearListView(fControl.lvAntennas);
            SheetUtils.AddItem(fControl.lvAntennas, [GetLiteral('Literal13')]).Data := nil;
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TAntennasSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvAntennas);
    end;

  procedure TAntennasSheetHandler.threadedRenderAntenna(const parms : array of const);
    var
      Info : PAntennaInfoRec absolute parms[0].vPointer;
      pInf : PAntennaInfoRec;
    begin
      if fLastUpdate = parms[1].vInteger
        then
          begin
            if (fControl.lvAntennas.Items.Count = 1) and (fControl.lvAntennas.Items[0].Data = nil)
              then fControl.lvAntennas.Items.Clear;
            with fControl.lvAntennas.Items.Add, Info^ do
              begin
                new(pInf);

                pInf.Name    := Info.Name;
                pInf.Town    := Info.Town;
                pInf.X       := Info.X;
                pInf.Y       := Info.Y;
                pInf.Active  := Info.Active;
                pInf.Viewers := Info.Viewers;

                Caption := pInf.Name;
                SubItems.Add(pInf.Town);
                if pInf.Active
                  then ImageIndex := 1
                  else ImageIndex := 0;

                SubItems.Add(IntToStr(pInf.Viewers));
                Data := pInf;
              end;
          end;
    end;

  procedure TAntennasSheetHandler.threadedRenderEmpty(const parms : array of const);
    begin
      if fLastUpdate = parms[0].vInteger
        then
          begin
            SheetUtils.ClearListView(fControl.lvAntennas);
            SheetUtils.AddItem(fControl.lvAntennas, [GetLiteral('Literal108')]);
          end;
    end;

  procedure TAntennasSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Update : integer absolute parms[0].vInteger;
      Proxy  : OleVariant;
      Info   : TAntennaInfoRec;
      count  : integer;
      i      : integer;
      iStr   : string;
      Values : TStringList;
      aux    : string;
    begin
      count := 0;
      try
        Proxy := fContainer.GetCacheObjectProxy;
        if not VarIsEmpty(Proxy) and (Update = fLastUpdate)
          then
            begin
              Values := TStringList.Create;
              try
                count := CvtToInt(Proxy.Properties(tidAntCount));
                i := 0;
                while (i < count) and (Update = fLastUpdate) do
                  begin
                    iStr := IntToStr(i);
                    fContainer.GetPropertyArray(Proxy, [tidAntName + iStr,
                      tidAntTown + iStr,
                      tidAntX + iStr,
                      tidAntY + iStr,
                      tidAntActive + iStr,
                      tidAntViewers + iStr],
                      Values);
                    if Update = fLastUpdate
                      then
                        begin
                          Info.Name := Values.Values[tidAntName + iStr];
                          Info.Town := Values.Values[tidAntTown + iStr];

                          aux := Values.Values[tidAntX + iStr];
                          if aux <> ''
                            then Info.X := StrToInt(aux)
                            else Info.X := 0;


                          aux := Values.Values[tidAntY + iStr];
                          if aux <> ''
                            then Info.Y := StrToInt(aux)
                            else Info.Y := 0;

                          Info.Active := Values.Values[tidAntActive + iStr] = 'YES';

                          aux := Values.Values[tidAntViewers + iStr];
                          if aux <> ''
                            then Info.Viewers := StrToInt(aux);

                          Join(threadedRenderAntenna, [@Info, Update]);
                        end;
                    inc(i);
                    Values.Clear;
                  end;
              finally
                fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
                Values.Free;
              end;
            end;
        if (Update = fLastUpdate) and (count = 0)
          then Join(threadedRenderEmpty, [Update]);
      except
      end;
    end;


  // TAntennasheetViewer

  procedure TAntennasSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TAntennasSheetViewer.lvAntennasDeletion(Sender: TObject; Item: TListItem);
    var
      aux : PAntennaInfoRec;
    begin
      if Item.Data <> nil
        then
          begin
            aux := PAntennaInfoRec(Item.Data);
            Item.Data := nil;
            dispose(aux);
          end;
    end;

  procedure TAntennasSheetViewer.lvAntennasDblClick(Sender: TObject);
    var
      Info : PAntennaInfoRec;
    begin
      if (lvAntennas.Selected <> nil) and (lvAntennas.Selected.Data <> nil)
        then
          begin
            Info := PAntennaInfoRec(lvAntennas.Selected.Data);
            fHandler.GetContainer.HandleURL( '?frame_Id=MapIsoView&frame_Action=MoveTo&x=' + IntToStr(Info.X) + '&y=' + IntToStr(Info.Y), false );
          end;
    end;


  // AntennasSheetHandlerCreator

  function AntennasSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TAntennasSheetHandler.Create;
    end;


procedure TAntennasSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
    dwStyles : DWORD;
  begin
    inherited;
    if (lvAntennas<> nil)// and (lvAntennas.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(lvAntennas.Columns.Count-1) do
            inc(sum, lvAntennas.Columns[i].Width);
          dwStyles := GetWindowLong(lvAntennas.Handle, GWL_STYLE);
          if (dwStyles and WS_VSCROLL) <> 0
            then Inc(sum, GetSystemMetrics(SM_CXVSCROLL));
          lvAntennas.Columns[lvAntennas.Columns.Count-1].Width := lvAntennas.Width-sum-2;
        end;
  end;


procedure TAntennasSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvAntennas.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvAntennas.Handle, ' ', ' ');
          CoolSBEnableBar(lvAntennas.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvAntennas );
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Antennas', AntennasSheetHandlerCreator);

end.
