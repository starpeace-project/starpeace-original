unit MausoleumSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidWordsOfWisdom = 'WordsOfWisdom';
  tidOwnerName     = 'OwnerName';
  tidCurrBlock     = 'CurrBlock';
  tidTranscended   = 'Transcended';

const
  facStoppedByTycoon  = $04;

type
  TMausoleumSheetHandler = class;

  TMausoleumSheetViewer = class(TVisualControl)
    btnSetWords: TFramedButton;
    Panel1: TPanel;
    InternationalizerComponent1: TInternationalizerComponent;
    lbWords: TLabel;
    eWordsOfWisdom: TMemo;
    btnCancel: TFramedButton;
    procedure btnSetWordsClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TMausoleumSheetHandler;
  end;


  TMausoleumSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl   : TMausoleumSheetViewer;
        fOwnFac    : boolean;
        fCurrBlock : integer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedSendWords(const parms : array of const);
        procedure threadedCacncelTransc(const parms : array of const);
    end;

var
  MausoleumSheetViewer: TMausoleumSheetViewer;

  function MausoleumSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, VoyagerServerInterfaces,
    ObjectInspectorHandleViewer, ObjectInspectorHandler, Literals, ClientMLS,
     {$IFDEF VER140}
      Variants,
    {$ENDIF}
   CompStringsParser;

{$R *.DFM}

  const
    ParagraphSep = '|';

  function EncodeParagraph(para : TStrings) : string;
    var
      i : integer;
    begin
      if para.Count > 0
        then
          begin
            result := para[0];
            for i := 1 to pred(para.Count) do
              result := result + ParagraphSep + para[i];
          end
        else result := '';
    end;

  function DecodeParagraph(para : string) : string;
    var
      p : integer;
      aux : string;
    begin
      p := 1;
      result := '';
      aux := CompStringsParser.GetNextStringUpTo(para, p, ParagraphSep);
      while aux <> '' do
        begin
          result := result + aux + ^M^J;
          inc(p);
          aux := CompStringsParser.GetNextStringUpTo(para, p, ParagraphSep);
        end;
    end;

  // TMausoleumSheetHandler

  function TMausoleumSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TMausoleumSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TMausoleumSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMausoleumSheetHandler.RenderProperties(Properties : TStringList);
    var
      words : string;
    begin
      //FiveViewUtils.SetViewProp(fControl, Properties);
      if (UpperCase(Properties.Values[tidOwnerName]) = UpperCase(fContainer.GetClientView.getUserName))
        then
          begin
            fOwnFac := true;
            try
              fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
            except
              fCurrBlock := 0;
            end;
          end
        else fOwnFac := false;
      with fControl.eWordsOfWisdom  do
        begin
          fControl.btnSetWords.Enabled := fOwnFac;
          ReadOnly := not fOwnFac;
          Enabled := fOwnFac;
          words := DecodeParagraph(Properties.Values[tidWordsOfWisdom]);
          Text := words;
          fControl.btnCancel.Enabled := fOwnFac and (Properties.Values[tidTranscended] <> '1');
          SelectAll;
          //SetFocus;
        end;
    end;

  procedure TMausoleumSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            fControl.btnSetWords.Enabled := false;
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TMausoleumSheetHandler.Clear;
    begin
      inherited;
      fControl.btnSetWords.Enabled := false;
      fControl.eWordsOfWisdom.Text := '';
    end;

  procedure TMausoleumSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        //FiveViewUtils.GetViewPropNames(fControl, Names);
        Names.Add(tidWordsOfWisdom);
        Names.Add(tidOwnerName);
        Names.Add(tidCurrBlock);
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

  procedure TMausoleumSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TMausoleumSheetHandler.threadedSendWords(const parms : array of const);
    var
      MSProxy : OleVariant;
      Words   : string;
    begin
      Words := parms[0].VPChar;
      try
        if (fLastUpdate = parms[1].VInteger) and (fCurrBlock <> 0)
          then
            begin
              MSProxy := GetContainer.GetMSProxy;
              if not VarIsEmpty(MSProxy) and MSProxy.BindTo(fCurrBlock)
                then MSProxy.RDOSetWordsOfWisdom(Words);
            end;
      except
      end;
    end;

  procedure TMausoleumSheetHandler.threadedCacncelTransc(const parms : array of const);
    var
      MSProxy : OleVariant;
    begin
      try
        if (fLastUpdate = parms[0].VInteger) and (fCurrBlock <> 0)
          then
            begin
              MSProxy := GetContainer.GetMSProxy;
              if not VarIsEmpty(MSProxy) and MSProxy.BindTo(fCurrBlock)
                then MSProxy.RDOCacncelTransc;
            end;
      except
      end;
    end;


  // MausoleumSheetHandlerCreator

  function MausoleumSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TMausoleumSheetHandler.Create;
    end;

  // TCapitolSheetViewer

  procedure TMausoleumSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TMausoleumSheetViewer.btnSetWordsClick(Sender: TObject);
    var
      aux : string;
    begin
      aux := EncodeParagraph(eWordsOfWisdom.Lines);
      Threads.Fork(fHandler.threadedSendWords, priNormal, [aux, fHandler.fLastUpdate]);
    end;

  procedure TMausoleumSheetViewer.btnCancelClick(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedCacncelTransc, priNormal, [fHandler.fLastUpdate]);
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Mausoleum', MausoleumSheetHandlerCreator);

end.
