unit WebBrowser;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Microsoft Internet Controls }
{ Version 1.0 }

{ Conversion log:
  Warning: 'Type' is a reserved word. IWebBrowser.Type changed to Type_
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_WebBrowser : TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

const

{ Constants for WebBrowser navigation flags }

{ BrowserNavConstants }

  navOpenInNewWindow = 1;
  navNoHistory = 2;
  navNoReadFromCache = 4;
  navNoWriteToCache = 8;

{ Constants for Refresh }

{ RefreshConstants }

  REFRESH_NORMAL = 0;
  REFRESH_IFEXPIRED = 1;
  REFRESH_COMPLETELY = 3;

{ Constants for WebBrowser CommandStateChange }

{ CommandStateChangeConstants }

  CSC_UPDATECOMMANDS = -1;
  CSC_NAVIGATEFORWARD = 1;
  CSC_NAVIGATEBACK = 2;

const

{ Component class GUIDs }
  Class_WebBrowser: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
  Class_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';

type

{ Forward declarations: Interfaces }
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;

{ Forward declarations: CoClasses }
  WebBrowserX = IWebBrowser;
  InternetExplorer = IWebBrowserApp;

{ Forward declarations: Enums }
  BrowserNavConstants = TOleEnum;
  RefreshConstants = TOleEnum;
  CommandStateChangeConstants = TOleEnum;

{ Web Browser interface }

  IWebBrowser = interface(IDispatch)
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; safecall;
    procedure GoForward; safecall;
    procedure GoHome; safecall;
    procedure GoSearch; safecall;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); safecall;
    procedure Refresh; safecall;
    procedure Refresh2(var Level: OleVariant); safecall;
    procedure Stop; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Container: IDispatch; safecall;
    function Get_Document: IDispatch; safecall;
    function Get_TopLevelContainer: WordBool; safecall;
    function Get_Type_: WideString; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(Value: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(Value: Integer); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(Value: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(Value: Integer); safecall;
    function Get_LocationName: WideString; safecall;
    function Get_LocationURL: WideString; safecall;
    function Get_Busy: WordBool; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property Type_: WideString read Get_Type_;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
  end;

{ DispInterface declaration for Dual Interface IWebBrowser }

  IWebBrowserDisp = dispinterface
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

{ Event interface for Web Browser Control }

  DWebBrowserEvents = dispinterface
    ['{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure BeforeNavigate(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool); dispid 100;
    procedure NavigateComplete(const URL: WideString); dispid 101;
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress, ProgressMax: Integer); dispid 108;
    procedure DownloadComplete; dispid 104;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure NewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 107;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure FrameBeforeNavigate(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool); dispid 200;
    procedure FrameNavigateComplete(const URL: WideString); dispid 201;
    procedure FrameNewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 204;
    procedure Quit(var Cancel: WordBool); dispid 103;
    procedure WindowMove; dispid 109;
    procedure WindowResize; dispid 110;
    procedure WindowActivate; dispid 111;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
  end;

{ Web Browser Application Interface. }

  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx, pcy: SYSINT); safecall;
    procedure PutProperty(const szProperty: WideString; vtValue: OleVariant); safecall;
    function GetProperty(const szProperty: WideString): OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_HWND: Integer; safecall;
    function Get_FullName: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_StatusBar: WordBool; safecall;
    procedure Set_StatusBar(Value: WordBool); safecall;
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const Value: WideString); safecall;
    function Get_ToolBar: SYSINT; safecall;
    procedure Set_ToolBar(Value: SYSINT); safecall;
    function Get_MenuBar: WordBool; safecall;
    procedure Set_MenuBar(Value: WordBool); safecall;
    function Get_FullScreen: WordBool; safecall;
    procedure Set_FullScreen(Value: WordBool); safecall;
    property Name: WideString read Get_Name;
    property HWND: Integer read Get_HWND;
    property FullName: WideString read Get_FullName;
    property Path: WideString read Get_Path;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
  end;

{ DispInterface declaration for Dual Interface IWebBrowserApp }

  IWebBrowserAppDisp = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property Type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx, pcy: SYSINT); dispid 301;
    procedure PutProperty(const szProperty: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const szProperty: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
  end;

{ WebBrowser Control }

  TWebBrowserBeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowserNavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowserStatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowserCommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserNewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowserTitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserFrameBeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowserFrameNavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowserFrameNewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowserQuit = procedure(Sender: TObject; var Cancel: WordBool) of object;
  TWebBrowserPropertyChange = procedure(Sender: TObject; const szProperty: WideString) of object;

  TWebBrowser = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowserBeforeNavigate;
    FOnNavigateComplete: TWebBrowserNavigateComplete;
    FOnStatusTextChange: TWebBrowserStatusTextChange;
    FOnProgressChange: TWebBrowserProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowserCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowserNewWindow;
    FOnTitleChange: TWebBrowserTitleChange;
    FOnFrameBeforeNavigate: TWebBrowserFrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowserFrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowserFrameNewWindow;
    FOnQuit: TWebBrowserQuit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowserPropertyChange;
    FIntf: IWebBrowser;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
    procedure Refresh;
    procedure Refresh2(var Level: OleVariant);
    procedure Stop;
    property ControlInterface: IWebBrowser read FIntf;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
  published
    property TabStop;
    property Align;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnBeforeNavigate: TWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowserStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowserCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowserNewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowserFrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowserFrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowserFrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowserQuit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowserPropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;

type  
  TExplorer = TWebBrowser;

procedure Register;

implementation

uses ComObj;

procedure TWebBrowser.InitControlData;
const
  CEventDispIDs: array[0..16] of Integer = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData = (
    ClassID: '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
    EventIID: '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TWebBrowser.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IWebBrowser;
end;

procedure TWebBrowser.GoBack;
begin
  ControlInterface.GoBack;
end;

procedure TWebBrowser.GoForward;
begin
  ControlInterface.GoForward;
end;

procedure TWebBrowser.GoHome;
begin
  ControlInterface.GoHome;
end;

procedure TWebBrowser.GoSearch;
begin
  ControlInterface.GoSearch;
end;

procedure TWebBrowser.Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  ControlInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser.Refresh;
begin
  ControlInterface.Refresh;
end;

procedure TWebBrowser.Refresh2(var Level: OleVariant);
begin
  ControlInterface.Refresh2(Level);
end;

procedure TWebBrowser.Stop;
begin
  ControlInterface.Stop;
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TWebBrowser]);
end;

end.
