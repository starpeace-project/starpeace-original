unit TestLib_TLB;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ TestLib Library }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_TestLib: TGUID = '{BE68CD61-B6B1-11D1-ABC5-008029EC1811}';

const

{ TxDragMode }

  dmManual = 0;
  dmAutomatic = 1;

{ TxMouseButton }

  mbLeft = 0;
  mbRight = 1;
  mbMiddle = 2;

const

{ Component class GUIDs }
  Class_ProgressBarX: TGUID = '{BE68CD64-B6B1-11D1-ABC5-008029EC1811}';

type

{ Forward declarations: Interfaces }
  IProgressBarX = interface;
  IProgressBarXDisp = dispinterface;
  IProgressBarXEvents = dispinterface;

{ Forward declarations: CoClasses }
  ProgressBarX = IProgressBarX;

{ Forward declarations: Enums }
  TxDragMode = TOleEnum;
  TxMouseButton = TOleEnum;

{ Dispatch interface for ProgressBarX Control }

  IProgressBarX = interface(IDispatch)
    ['{BE68CD62-B6B1-11D1-ABC5-008029EC1811}']
    procedure StepIt; safecall;
    procedure StepBy(Delta: Integer); safecall;
    function Get_DragCursor: Smallint; safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    function Get_DragMode: TxDragMode; safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Min: Integer; safecall;
    procedure Set_Min(Value: Integer); safecall;
    function Get_Max: Integer; safecall;
    procedure Set_Max(Value: Integer); safecall;
    function Get_Position: Integer; safecall;
    procedure Set_Position(Value: Integer); safecall;
    function Get_Step: Integer; safecall;
    procedure Set_Step(Value: Integer); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    property DragCursor: Smallint read Get_DragCursor write Set_DragCursor;
    property DragMode: TxDragMode read Get_DragMode write Set_DragMode;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Min: Integer read Get_Min write Set_Min;
    property Max: Integer read Get_Max write Set_Max;
    property Position: Integer read Get_Position write Set_Position;
    property Step: Integer read Get_Step write Set_Step;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
  end;

{ DispInterface declaration for Dual Interface IProgressBarX }

  IProgressBarXDisp = dispinterface
    ['{BE68CD62-B6B1-11D1-ABC5-008029EC1811}']
    procedure StepIt; dispid 1;
    procedure StepBy(Delta: Integer); dispid 2;
    property DragCursor: Smallint dispid 3;
    property DragMode: TxDragMode dispid 4;
    property Enabled: WordBool dispid 5;
    property Min: Integer dispid 6;
    property Max: Integer dispid 7;
    property Position: Integer dispid 8;
    property Step: Integer dispid 9;
    property Visible: WordBool dispid 10;
    property Cursor: Smallint dispid 11;
  end;

{ Events interface for ProgressBarX Control }

  IProgressBarXEvents = dispinterface
    ['{BE68CD63-B6B1-11D1-ABC5-008029EC1811}']
  end;

{ ProgressBarXControl }

  TProgressBarX = class(TOleControl)
  private
    FIntf: IProgressBarX;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    procedure StepIt;
    procedure StepBy(Delta: Integer);
    property ControlInterface: IProgressBarX read FIntf;
  published
    property TabStop;
    property Align;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property DragCursor: Smallint index 3 read GetSmallintProp write SetSmallintProp stored False;
    property DragMode: TxDragMode index 4 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property Min: Integer index 6 read GetIntegerProp write SetIntegerProp stored False;
    property Max: Integer index 7 read GetIntegerProp write SetIntegerProp stored False;
    property Position: Integer index 8 read GetIntegerProp write SetIntegerProp stored False;
    property Step: Integer index 9 read GetIntegerProp write SetIntegerProp stored False;
    property Visible: WordBool index 10 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 11 read GetSmallintProp write SetSmallintProp stored False;
  end;

procedure Register;

implementation

uses ComObj;

procedure TProgressBarX.InitControlData;
const
  CControlData: TControlData = (
    ClassID: '{BE68CD64-B6B1-11D1-ABC5-008029EC1811}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TProgressBarX.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IProgressBarX;
end;

procedure TProgressBarX.StepIt;
begin
  ControlInterface.StepIt;
end;

procedure TProgressBarX.StepBy(Delta: Integer);
begin
  ControlInterface.StepBy(Delta);
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TProgressBarX]);
end;

end.
