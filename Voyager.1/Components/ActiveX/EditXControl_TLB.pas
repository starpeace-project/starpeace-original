unit EditXControl_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ EditXControl Library }
{ Version 1.0 }

{ Conversion log:
  Hint: Class is not registered.  Ambient properties cannot be determined.
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_EditXControl: TGUID = '{C9EE07C0-B606-11D1-ABC3-008029EC1811}';

const

{ TxBorderStyle }

  bsNone = 0;
  bsSingle = 1;

{ TxEditCharCase }

  ecNormal = 0;
  ecUpperCase = 1;
  ecLowerCase = 2;

{ TxDragMode }

  dmManual = 0;
  dmAutomatic = 1;

{ TxImeMode }

  imDisable = 0;
  imClose = 1;
  imOpen = 2;
  imDontCare = 3;
  imSAlpha = 4;
  imAlpha = 5;
  imHira = 6;
  imSKata = 7;
  imKata = 8;
  imChinese = 9;
  imSHanguel = 10;
  imHanguel = 11;

{ TxMouseButton }

  mbLeft = 0;
  mbRight = 1;
  mbMiddle = 2;

const

{ Component class GUIDs }
  Class_EditX: TGUID = '{C9EE07C3-B606-11D1-ABC3-008029EC1811}';

type

{ Forward declarations: Interfaces }
  IEditX = interface;
  IEditXDisp = dispinterface;
  IEditXEvents = dispinterface;

{ Forward declarations: CoClasses }
  EditX = IEditX;

{ Forward declarations: Enums }
  TxBorderStyle = TOleEnum;
  TxEditCharCase = TOleEnum;
  TxDragMode = TOleEnum;
  TxImeMode = TOleEnum;
  TxMouseButton = TOleEnum;

{ Dispatch interface for EditX Control }

  IEditX = interface(IDispatch)
    ['{C9EE07C1-B606-11D1-ABC3-008029EC1811}']
    function Get_AutoSelect: WordBool; safecall;
    procedure Set_AutoSelect(Value: WordBool); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_BorderStyle: TxBorderStyle; safecall;
    procedure Set_BorderStyle(Value: TxBorderStyle); safecall;
    function Get_CharCase: TxEditCharCase; safecall;
    procedure Set_CharCase(Value: TxEditCharCase); safecall;
    function Get_Color: Integer; safecall;
    procedure Set_Color(Value: Integer); safecall;
    function Get_Ctl3D: WordBool; safecall;
    procedure Set_Ctl3D(Value: WordBool); safecall;
    function Get_DragCursor: Smallint; safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    function Get_DragMode: TxDragMode; safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_HideSelection: WordBool; safecall;
    procedure Set_HideSelection(Value: WordBool); safecall;
    function Get_ImeMode: TxImeMode; safecall;
    procedure Set_ImeMode(Value: TxImeMode); safecall;
    function Get_ImeName: WideString; safecall;
    procedure Set_ImeName(const Value: WideString); safecall;
    function Get_MaxLength: Integer; safecall;
    procedure Set_MaxLength(Value: Integer); safecall;
    function Get_OEMConvert: WordBool; safecall;
    procedure Set_OEMConvert(Value: WordBool); safecall;
    function Get_ParentColor: WordBool; safecall;
    procedure Set_ParentColor(Value: WordBool); safecall;
    function Get_ParentCtl3D: WordBool; safecall;
    procedure Set_ParentCtl3D(Value: WordBool); safecall;
    function Get_PasswordChar: Smallint; safecall;
    procedure Set_PasswordChar(Value: Smallint); safecall;
    function Get_ReadOnly: WordBool; safecall;
    procedure Set_ReadOnly(Value: WordBool); safecall;
    function Get_Text: WideString; safecall;
    procedure Set_Text(const Value: WideString); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure Clear; safecall;
    procedure ClearSelection; safecall;
    procedure CopyToClipboard; safecall;
    procedure CutToClipboard; safecall;
    procedure PasteFromClipboard; safecall;
    procedure SelectAll; safecall;
    function Get_Modified: WordBool; safecall;
    procedure Set_Modified(Value: WordBool); safecall;
    function Get_SelLength: Integer; safecall;
    procedure Set_SelLength(Value: Integer); safecall;
    function Get_SelStart: Integer; safecall;
    procedure Set_SelStart(Value: Integer); safecall;
    function Get_SelText: WideString; safecall;
    procedure Set_SelText(const Value: WideString); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    property AutoSelect: WordBool read Get_AutoSelect write Set_AutoSelect;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property BorderStyle: TxBorderStyle read Get_BorderStyle write Set_BorderStyle;
    property CharCase: TxEditCharCase read Get_CharCase write Set_CharCase;
    property Color: Integer read Get_Color write Set_Color;
    property Ctl3D: WordBool read Get_Ctl3D write Set_Ctl3D;
    property DragCursor: Smallint read Get_DragCursor write Set_DragCursor;
    property DragMode: TxDragMode read Get_DragMode write Set_DragMode;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property HideSelection: WordBool read Get_HideSelection write Set_HideSelection;
    property ImeMode: TxImeMode read Get_ImeMode write Set_ImeMode;
    property ImeName: WideString read Get_ImeName write Set_ImeName;
    property MaxLength: Integer read Get_MaxLength write Set_MaxLength;
    property OEMConvert: WordBool read Get_OEMConvert write Set_OEMConvert;
    property ParentColor: WordBool read Get_ParentColor write Set_ParentColor;
    property ParentCtl3D: WordBool read Get_ParentCtl3D write Set_ParentCtl3D;
    property PasswordChar: Smallint read Get_PasswordChar write Set_PasswordChar;
    property ReadOnly: WordBool read Get_ReadOnly write Set_ReadOnly;
    property Text: WideString read Get_Text write Set_Text;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Modified: WordBool read Get_Modified write Set_Modified;
    property SelLength: Integer read Get_SelLength write Set_SelLength;
    property SelStart: Integer read Get_SelStart write Set_SelStart;
    property SelText: WideString read Get_SelText write Set_SelText;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
  end;

{ DispInterface declaration for Dual Interface IEditX }

  IEditXDisp = dispinterface
    ['{C9EE07C1-B606-11D1-ABC3-008029EC1811}']
    property AutoSelect: WordBool dispid 1;
    property AutoSize: WordBool dispid 2;
    property BorderStyle: TxBorderStyle dispid 3;
    property CharCase: TxEditCharCase dispid 4;
    property Color: Integer dispid 5;
    property Ctl3D: WordBool dispid 6;
    property DragCursor: Smallint dispid 7;
    property DragMode: TxDragMode dispid 8;
    property Enabled: WordBool dispid 9;
    property HideSelection: WordBool dispid 10;
    property ImeMode: TxImeMode dispid 11;
    property ImeName: WideString dispid 12;
    property MaxLength: Integer dispid 13;
    property OEMConvert: WordBool dispid 14;
    property ParentColor: WordBool dispid 15;
    property ParentCtl3D: WordBool dispid 16;
    property PasswordChar: Smallint dispid 17;
    property ReadOnly: WordBool dispid 18;
    property Text: WideString dispid 19;
    property Visible: WordBool dispid 20;
    procedure Clear; dispid 21;
    procedure ClearSelection; dispid 22;
    procedure CopyToClipboard; dispid 23;
    procedure CutToClipboard; dispid 24;
    procedure PasteFromClipboard; dispid 25;
    procedure SelectAll; dispid 26;
    property Modified: WordBool dispid 27;
    property SelLength: Integer dispid 28;
    property SelStart: Integer dispid 29;
    property SelText: WideString dispid 30;
    property Cursor: Smallint dispid 31;
  end;

{ Events interface for EditX Control }

  IEditXEvents = dispinterface
    ['{C9EE07C2-B606-11D1-ABC3-008029EC1811}']
    procedure OnChange; dispid 1;
    procedure OnClick; dispid 2;
    procedure OnDblClick; dispid 3;
    procedure OnKeyPress(var Key: Smallint); dispid 4;
  end;

{ EditXControl }

  TEditXOnKeyPress = procedure(Sender: TObject; var Key: Smallint) of object;

  TEditX = class(TOleControl)
  private
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnKeyPress: TEditXOnKeyPress;
    FIntf: IEditX;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    procedure Clear;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    property ControlInterface: IEditX read FIntf;
  published
    property AutoSelect: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 2 read GetWordBoolProp write SetWordBoolProp stored False;
    property BorderStyle: TxBorderStyle index 3 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property CharCase: TxEditCharCase index 4 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Color: Integer index 5 read GetIntegerProp write SetIntegerProp stored False;
    property Ctl3D: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property DragCursor: Smallint index 7 read GetSmallintProp write SetSmallintProp stored False;
    property DragMode: TxDragMode index 8 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property HideSelection: WordBool index 10 read GetWordBoolProp write SetWordBoolProp stored False;
    property ImeMode: TxImeMode index 11 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property ImeName: WideString index 12 read GetWideStringProp write SetWideStringProp stored False;
    property MaxLength: Integer index 13 read GetIntegerProp write SetIntegerProp stored False;
    property OEMConvert: WordBool index 14 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentColor: WordBool index 15 read GetWordBoolProp write SetWordBoolProp stored False;
    property ParentCtl3D: WordBool index 16 read GetWordBoolProp write SetWordBoolProp stored False;
    property PasswordChar: Smallint index 17 read GetSmallintProp write SetSmallintProp stored False;
    property ReadOnly: WordBool index 18 read GetWordBoolProp write SetWordBoolProp stored False;
    property Text: WideString index 19 read GetWideStringProp write SetWideStringProp stored False;
    property Visible: WordBool index 20 read GetWordBoolProp write SetWordBoolProp stored False;
    property Modified: WordBool index 27 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelLength: Integer index 28 read GetIntegerProp write SetIntegerProp stored False;
    property SelStart: Integer index 29 read GetIntegerProp write SetIntegerProp stored False;
    property SelText: WideString index 30 read GetWideStringProp write SetWideStringProp stored False;
    property Cursor: Smallint index 31 read GetSmallintProp write SetSmallintProp stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnKeyPress: TEditXOnKeyPress read FOnKeyPress write FOnKeyPress;
  end;

procedure Register;

implementation

uses ComObj;

procedure TEditX.InitControlData;
const
  CEventDispIDs: array[0..3] of Integer = (
    $00000001, $00000002, $00000003, $00000004);
  CControlData: TControlData = (
    ClassID: '{C9EE07C3-B606-11D1-ABC3-008029EC1811}';
    EventIID: '{C9EE07C2-B606-11D1-ABC3-008029EC1811}';
    EventCount: 4;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TEditX.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IEditX;
end;

procedure TEditX.Clear;
begin
  ControlInterface.Clear;
end;

procedure TEditX.ClearSelection;
begin
  ControlInterface.ClearSelection;
end;

procedure TEditX.CopyToClipboard;
begin
  ControlInterface.CopyToClipboard;
end;

procedure TEditX.CutToClipboard;
begin
  ControlInterface.CutToClipboard;
end;

procedure TEditX.PasteFromClipboard;
begin
  ControlInterface.PasteFromClipboard;
end;

procedure TEditX.SelectAll;
begin
  ControlInterface.SelectAll;
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TEditX]);
end;

end.
