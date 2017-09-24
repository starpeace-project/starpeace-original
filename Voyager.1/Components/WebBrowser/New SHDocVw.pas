unit SHDocVw;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Microsoft Internet Controls }
{ Version 1.1 }

{ Conversion log:
  Warning: 'Type' is a reserved word. IWebBrowser.Type changed to Type_
  Warning: 'Property' is a reserved word. Parameter 'Property' in DWebBrowserEvents.PropertyChange changed to 'Property_'
  Warning: 'Property' is a reserved word. Parameter 'Property' in IWebBrowserApp.PutProperty changed to 'Property_'
  Warning: 'Property' is a reserved word. Parameter 'Property' in IWebBrowserApp.GetProperty changed to 'Property_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in IShellUIHelper.AddDesktopComponent changed to 'Type_'
  Warning: 'var' is a reserved word. Parameter 'var' in IShellNameSpace.Expand changed to 'var_'
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_SHDocVw: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

const

{ Constants for WebBrowser CommandStateChange }

{ CommandStateChangeConstants }

  CSC_UPDATECOMMANDS = -1;
  CSC_NAVIGATEFORWARD = 1;
  CSC_NAVIGATEBACK = 2;

{ OLECMDID }

  OLECMDID_OPEN = 1;
  OLECMDID_NEW = 2;
  OLECMDID_SAVE = 3;
  OLECMDID_SAVEAS = 4;
  OLECMDID_SAVECOPYAS = 5;
  OLECMDID_PRINT = 6;
  OLECMDID_PRINTPREVIEW = 7;
  OLECMDID_PAGESETUP = 8;
  OLECMDID_SPELL = 9;
  OLECMDID_PROPERTIES = 10;
  OLECMDID_CUT = 11;
  OLECMDID_COPY = 12;
  OLECMDID_PASTE = 13;
  OLECMDID_PASTESPECIAL = 14;
  OLECMDID_UNDO = 15;
  OLECMDID_REDO = 16;
  OLECMDID_SELECTALL = 17;
  OLECMDID_CLEARSELECTION = 18;
  OLECMDID_ZOOM = 19;
  OLECMDID_GETZOOMRANGE = 20;
  OLECMDID_UPDATECOMMANDS = 21;
  OLECMDID_REFRESH = 22;
  OLECMDID_STOP = 23;
  OLECMDID_HIDETOOLBARS = 24;
  OLECMDID_SETPROGRESSMAX = 25;
  OLECMDID_SETPROGRESSPOS = 26;
  OLECMDID_SETPROGRESSTEXT = 27;
  OLECMDID_SETTITLE = 28;
  OLECMDID_SETDOWNLOADSTATE = 29;
  OLECMDID_STOPDOWNLOAD = 30;
  OLECMDID_ONTOOLBARACTIVATED = 31;
  OLECMDID_FIND = 32;
  OLECMDID_DELETE = 33;
  OLECMDID_HTTPEQUIV = 34;
  OLECMDID_HTTPEQUIV_DONE = 35;
  OLECMDID_ENABLE_INTERACTION = 36;
  OLECMDID_ONUNLOAD = 37;
  OLECMDID_PROPERTYBAG2 = 38;
  OLECMDID_PREREFRESH = 39;
  OLECMDID_SHOWSCRIPTERROR = 40;
  OLECMDID_SHOWMESSAGE = 41;
  OLECMDID_SHOWFIND = 42;
  OLECMDID_SHOWPAGESETUP = 43;
  OLECMDID_SHOWPRINT = 44;
  OLECMDID_CLOSE = 45;
  OLECMDID_ALLOWUILESSSAVEAS = 46;
  OLECMDID_DONTDOWNLOADCSS = 47;
  OLECMDID_UPDATEPAGESTATUS = 48;

{ OLECMDF }

  OLECMDF_SUPPORTED = 1;
  OLECMDF_ENABLED = 2;
  OLECMDF_LATCHED = 4;
  OLECMDF_NINCHED = 8;
  OLECMDF_INVISIBLE = 16;
  OLECMDF_DEFHIDEONCTXTMENU = 32;

{ OLECMDEXECOPT }

  OLECMDEXECOPT_DODEFAULT = 0;
  OLECMDEXECOPT_PROMPTUSER = 1;
  OLECMDEXECOPT_DONTPROMPTUSER = 2;
  OLECMDEXECOPT_SHOWHELP = 3;

{ tagREADYSTATE }

  READYSTATE_UNINITIALIZED = 0;
  READYSTATE_LOADING = 1;
  READYSTATE_LOADED = 2;
  READYSTATE_INTERACTIVE = 3;
  READYSTATE_COMPLETE = 4;

{ Constants for WebBrowser security icon notification }

{ SecureLockIconConstants }

  secureLockIconUnsecure = 0;
  secureLockIconMixed = 1;
  secureLockIconSecureUnknownBits = 2;
  secureLockIconSecure40Bit = 3;
  secureLockIconSecure56Bit = 4;
  secureLockIconSecureFortezza = 5;
  secureLockIconSecure128Bit = 6;

{ Constants for ShellWindows registration }

{ ShellWindowTypeConstants }

  SWC_EXPLORER = 0;
  SWC_BROWSER = 1;
  SWC_3RDPARTY = 2;
  SWC_CALLBACK = 4;

{ Options for ShellWindows FindWindow }

{ ShellWindowFindWindowOptions }

  SWFO_NEEDDISPATCH = 1;
  SWFO_INCLUDEPENDING = 2;
  SWFO_COOKIEPASSED = 4;

const

{ Component class GUIDs }
  Class_WebBrowser_V1: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
  Class_WebBrowser: TGUID = '{8856F961-340A-11D0-A96B-00C04FD705A2}';
  Class_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';
  Class_ShellBrowserWindow: TGUID = '{C08AFD90-F2A1-11D1-8455-00A0C91F3880}';
  Class_ShellWindows: TGUID = '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
  Class_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
  Class_ShellNameSpace: TGUID = '{55136805-B2DE-11D1-B9F2-00A0C98BC547}';
  Class_CScriptErrorList: TGUID = '{EFD01300-160F-11D2-BB2E-00805FF7EFCA}';
  Class_SearchAssistantOC: TGUID = '{B45FF030-4447-11D2-85DE-00C04FA35C89}';

type

{ Forward declarations: Interfaces }
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;
  IWebBrowser2 = interface;
  IWebBrowser2Disp = dispinterface;
  DWebBrowserEvents2 = dispinterface;
  DShellWindowsEvents = dispinterface;
  IShellWindows = interface;
  IShellWindowsDisp = dispinterface;
  IShellUIHelper = interface;
  IShellUIHelperDisp = dispinterface;
  DShellNameSpaceEvents = dispinterface;
  IShellFavoritesNameSpace = interface;
  IShellFavoritesNameSpaceDisp = dispinterface;
  IShellNameSpace = interface;
  IShellNameSpaceDisp = dispinterface;
  IScriptErrorList = interface;
  IScriptErrorListDisp = dispinterface;
  ISearch = interface;
  ISearchDisp = dispinterface;
  ISearches = interface;
  ISearchesDisp = dispinterface;
  ISearchAssistantOC = interface;
  ISearchAssistantOCDisp = dispinterface;
  ISearchAssistantOC2 = interface;
  ISearchAssistantOC2Disp = dispinterface;
  _SearchAssistantEvents = dispinterface;

{ Forward declarations: CoClasses }
  WebBrowser_V1 = IWebBrowser;
  WebBrowser = IWebBrowser2;
  InternetExplorer = IWebBrowser2;
  ShellBrowserWindow = IWebBrowser2;
  ShellWindows = IShellWindows;
  ShellUIHelper = IShellUIHelper;
  ShellNameSpace = IShellNameSpace;
  CScriptErrorList = IScriptErrorList;
  SearchAssistantOC = ISearchAssistantOC2;

{ Forward declarations: Enums }
  CommandStateChangeConstants = TOleEnum;
  OLECMDID = TOleEnum;
  OLECMDF = TOleEnum;
  OLECMDEXECOPT = TOleEnum;
  tagREADYSTATE = TOleEnum;
  SecureLockIconConstants = TOleEnum;
  ShellWindowTypeConstants = TOleEnum;
  ShellWindowFindWindowOptions = TOleEnum;

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

{ Web Browser Control Events (old) }

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
    procedure PropertyChange(const Property_: WideString); dispid 112;
  end;

{ Web Browser Application Interface. }

  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx, pcy: SYSINT); safecall;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); safecall;
    function GetProperty(const Property_: WideString): OleVariant; safecall;
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
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
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

{ Web Browser Interface for IE4. }

  IWebBrowser2 = interface(IWebBrowserApp)
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant); safecall;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; safecall;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant); safecall;
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant); safecall;
    function Get_ReadyState: tagREADYSTATE; safecall;
    function Get_Offline: WordBool; safecall;
    procedure Set_Offline(Value: WordBool); safecall;
    function Get_Silent: WordBool; safecall;
    procedure Set_Silent(Value: WordBool); safecall;
    function Get_RegisterAsBrowser: WordBool; safecall;
    procedure Set_RegisterAsBrowser(Value: WordBool); safecall;
    function Get_RegisterAsDropTarget: WordBool; safecall;
    procedure Set_RegisterAsDropTarget(Value: WordBool); safecall;
    function Get_TheaterMode: WordBool; safecall;
    procedure Set_TheaterMode(Value: WordBool); safecall;
    function Get_AddressBar: WordBool; safecall;
    procedure Set_AddressBar(Value: WordBool); safecall;
    function Get_Resizable: WordBool; safecall;
    procedure Set_Resizable(Value: WordBool); safecall;
    property ReadyState: tagREADYSTATE read Get_ReadyState;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  end;

{ DispInterface declaration for Dual Interface IWebBrowser2 }

  IWebBrowser2Disp = dispinterface
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
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
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
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
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant); dispid 500;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; dispid 501;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant); dispid 502;
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant); dispid 503;
    property ReadyState: tagREADYSTATE readonly dispid -525;
    property Offline: WordBool dispid 550;
    property Silent: WordBool dispid 551;
    property RegisterAsBrowser: WordBool dispid 552;
    property RegisterAsDropTarget: WordBool dispid 553;
    property TheaterMode: WordBool dispid 554;
    property AddressBar: WordBool dispid 555;
    property Resizable: WordBool dispid 556;
  end;

{ Web Browser Control events interface }

  DWebBrowserEvents2 = dispinterface
    ['{34A715A0-6587-11D0-924A-0020AFC7AC4D}']
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress, ProgressMax: Integer); dispid 108;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure DownloadComplete; dispid 104;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
    procedure BeforeNavigate2(pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool); dispid 250;
    procedure NewWindow2(var ppDisp: IDispatch; var Cancel: WordBool); dispid 251;
    procedure NavigateComplete2(pDisp: IDispatch; var URL: OleVariant); dispid 252;
    procedure DocumentComplete(pDisp: IDispatch; var URL: OleVariant); dispid 259;
    procedure OnQuit; dispid 253;
    procedure OnVisible(Visible: WordBool); dispid 254;
    procedure OnToolBar(ToolBar: WordBool); dispid 255;
    procedure OnMenuBar(MenuBar: WordBool); dispid 256;
    procedure OnStatusBar(StatusBar: WordBool); dispid 257;
    procedure OnFullScreen(FullScreen: WordBool); dispid 258;
    procedure OnTheaterMode(TheaterMode: WordBool); dispid 260;
    procedure WindowSetResizable(Resizable: WordBool); dispid 262;
    procedure WindowSetLeft(Left: Integer); dispid 264;
    procedure WindowSetTop(Top: Integer); dispid 265;
    procedure WindowSetWidth(Width: Integer); dispid 266;
    procedure WindowSetHeight(Height: Integer); dispid 267;
    procedure WindowClosing(IsChildWindow: WordBool; var Cancel: WordBool); dispid 263;
    procedure ClientToHostWindow(var CX, CY: Integer); dispid 268;
    procedure SetSecureLockIcon(SecureLockIcon: Integer); dispid 269;
    procedure FileDownload(var Cancel: WordBool); dispid 270;
  end;

{ Event interface for IShellWindows }

  DShellWindowsEvents = dispinterface
    ['{FE4106E0-399A-11D0-A48C-00A0C90A8F39}']
    procedure WindowRegistered(lCookie: Integer); dispid 200;
    procedure WindowRevoked(lCookie: Integer); dispid 201;
  end;

{ Definition of interface IShellWindows }

  IShellWindows = interface(IDispatch)
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    function Get_Count: Integer; safecall;
    function Item(index: OleVariant): IDispatch; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Register(pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure RegisterPending(lThreadId: Integer; var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure Revoke(lCookie: Integer); safecall;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); safecall;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); safecall;
    function FindWindowSW(var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out pHWND: Integer; swfwOptions: SYSINT): IDispatch; safecall;
    procedure OnCreated(lCookie: Integer; punk: IUnknown); safecall;
    procedure ProcessAttachDetach(fAttach: WordBool); safecall;
    property Count: Integer read Get_Count;
  end;

{ DispInterface declaration for Dual Interface IShellWindows }

  IShellWindowsDisp = dispinterface
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly;
    function Item(index: OleVariant): IDispatch; dispid 0;
    function _NewEnum: IUnknown; dispid -4;
    procedure Register(pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer);
    procedure RegisterPending(lThreadId: Integer; var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer);
    procedure Revoke(lCookie: Integer);
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant);
    procedure OnActivated(lCookie: Integer; fActive: WordBool);
    function FindWindowSW(var pvarloc, pvarlocRoot: OleVariant; swClass: SYSINT; out pHWND: Integer; swfwOptions: SYSINT): IDispatch;
    procedure OnCreated(lCookie: Integer; punk: IUnknown);
    procedure ProcessAttachDetach(fAttach: WordBool);
  end;

{ Shell UI Helper Control Interface }

  IShellUIHelper = interface(IDispatch)
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; safecall;
    procedure ResetSafeMode; safecall;
    procedure RefreshOfflineDesktop; safecall;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); safecall;
    procedure AddChannel(const URL: WideString); safecall;
    procedure AddDesktopComponent(const URL, Type_: WideString; var Left, Top, Width, Height: OleVariant); safecall;
    function IsSubscribed(const URL: WideString): WordBool; safecall;
    procedure NavigateAndFind(const URL, strQuery: WideString; var varTargetFrame: OleVariant); safecall;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); safecall;
    procedure AutoCompleteSaveForm(var Form: OleVariant); safecall;
    procedure AutoScan(const strSearch, strFailureUrl: WideString; var pvarTargetFrame: OleVariant); safecall;
    procedure AutoCompleteAttach(var Reserved: OleVariant); safecall;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; safecall;
  end;

{ DispInterface declaration for Dual Interface IShellUIHelper }

  IShellUIHelperDisp = dispinterface
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL, Type_: WideString; var Left, Top, Width, Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL, strQuery: WideString; var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch, strFailureUrl: WideString; var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

  DShellNameSpaceEvents = dispinterface
    ['{55136806-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure FavoritesSelectionChange(cItems, hItem: Integer; const strName, strUrl: WideString; cVisits: Integer; const strDate: WideString; fAvailableOffline: Integer); dispid 1;
    procedure SelectionChange; dispid 2;
    procedure DoubleClick; dispid 3;
    procedure Initialized; dispid 4;
  end;

{ IShellFavoritesNameSpace Interface }

  IShellFavoritesNameSpace = interface(IDispatch)
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; safecall;
    procedure MoveSelectionDown; safecall;
    procedure ResetSort; safecall;
    procedure NewFolder; safecall;
    procedure Synchronize; safecall;
    procedure Import; safecall;
    procedure Export; safecall;
    procedure InvokeContextMenuCommand(const strCommand: WideString); safecall;
    procedure MoveSelectionTo; safecall;
    function Get_SubscriptionsEnabled: WordBool; safecall;
    function CreateSubscriptionForSelection: WordBool; safecall;
    function DeleteSubscriptionForSelection: WordBool; safecall;
    procedure SetRoot(const bstrFullPath: WideString); safecall;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
  end;

{ DispInterface declaration for Dual Interface IShellFavoritesNameSpace }

  IShellFavoritesNameSpaceDisp = dispinterface
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

{ IShellNameSpace Interface }

  IShellNameSpace = interface(IShellFavoritesNameSpace)
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    function Get_EnumOptions: Integer; safecall;
    procedure Set_EnumOptions(Value: Integer); safecall;
    function Get_SelectedItem: IDispatch; safecall;
    procedure Set_SelectedItem(Value: IDispatch); safecall;
    function Get_Root: OleVariant; safecall;
    procedure Set_Root(Value: OleVariant); safecall;
    function Get_Depth: SYSINT; safecall;
    procedure Set_Depth(Value: SYSINT); safecall;
    function Get_Mode: SYSUINT; safecall;
    procedure Set_Mode(Value: SYSUINT); safecall;
    function Get_Flags: UINT; safecall;
    procedure Set_Flags(Value: UINT); safecall;
    procedure Set_TVFlags(Value: UINT); safecall;
    function Get_TVFlags: UINT; safecall;
    function Get_Columns: WideString; safecall;
    procedure Set_Columns(const Value: WideString); safecall;
    function Get_CountViewTypes: SYSINT; safecall;
    procedure SetViewType(iType: SYSINT); safecall;
    function SelectedItems: IDispatch; safecall;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); safecall;
    procedure UnselectAll; safecall;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: UINT read Get_Flags write Set_Flags;
    property TVFlags: UINT read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
  end;

{ DispInterface declaration for Dual Interface IShellNameSpace }

  IShellNameSpaceDisp = dispinterface
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
    property EnumOptions: Integer dispid 14;
    property SelectedItem: IDispatch dispid 15;
    property Root: OleVariant dispid 16;
    property Depth: SYSINT dispid 17;
    property Mode: SYSUINT dispid 18;
    property Flags: UINT dispid 19;
    property TVFlags: UINT dispid 20;
    property Columns: WideString dispid 21;
    property CountViewTypes: SYSINT readonly dispid 22;
    procedure SetViewType(iType: SYSINT); dispid 23;
    function SelectedItems: IDispatch; dispid 24;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); dispid 25;
    procedure UnselectAll; dispid 26;
  end;

{ Script Error List Interface }

  IScriptErrorList = interface(IDispatch)
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; safecall;
    procedure retreatError; safecall;
    function canAdvanceError: Integer; safecall;
    function canRetreatError: Integer; safecall;
    function getErrorLine: Integer; safecall;
    function getErrorChar: Integer; safecall;
    function getErrorCode: Integer; safecall;
    function getErrorMsg: WideString; safecall;
    function getErrorUrl: WideString; safecall;
    function getAlwaysShowLockState: Integer; safecall;
    function getDetailsPaneOpen: Integer; safecall;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); safecall;
    function getPerErrorDisplay: Integer; safecall;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); safecall;
  end;

{ DispInterface declaration for Dual Interface IScriptErrorList }

  IScriptErrorListDisp = dispinterface
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; dispid 10;
    procedure retreatError; dispid 11;
    function canAdvanceError: Integer; dispid 12;
    function canRetreatError: Integer; dispid 13;
    function getErrorLine: Integer; dispid 14;
    function getErrorChar: Integer; dispid 15;
    function getErrorCode: Integer; dispid 16;
    function getErrorMsg: WideString; dispid 17;
    function getErrorUrl: WideString; dispid 18;
    function getAlwaysShowLockState: Integer; dispid 23;
    function getDetailsPaneOpen: Integer; dispid 19;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); dispid 20;
    function getPerErrorDisplay: Integer; dispid 21;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); dispid 22;
  end;

{ Enumerated Search }

  ISearch = interface(IDispatch)
    ['{BA9239A4-3DD5-11D2-BF8B-00C04FB93661}']
    function Get_Title: WideString; safecall;
    function Get_Id: WideString; safecall;
    function Get_URL: WideString; safecall;
    property Title: WideString read Get_Title;
    property Id: WideString read Get_Id;
    property URL: WideString read Get_URL;
  end;

{ DispInterface declaration for Dual Interface ISearch }

  ISearchDisp = dispinterface
    ['{BA9239A4-3DD5-11D2-BF8B-00C04FB93661}']
    property Title: WideString readonly;
    property Id: WideString readonly;
    property URL: WideString readonly;
  end;

{ Searches Enum }

  ISearches = interface(IDispatch)
    ['{47C922A2-3DD5-11D2-BF8B-00C04FB93661}']
    function Get_Count: Integer; safecall;
    function Get_Default: WideString; safecall;
    function Item(index: OleVariant): ISearch; safecall;
    function _NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Default: WideString read Get_Default;
  end;

{ DispInterface declaration for Dual Interface ISearches }

  ISearchesDisp = dispinterface
    ['{47C922A2-3DD5-11D2-BF8B-00C04FB93661}']
    property Count: Integer readonly;
    property Default: WideString readonly;
    function Item(index: OleVariant): ISearch;
    function _NewEnum: IUnknown; dispid -4;
  end;

{ ISearchAssistantOC Interface }

  ISearchAssistantOC = interface(IDispatch)
    ['{72423E8F-8011-11D2-BE79-00A0C9A83DA1}']
    procedure AddNextMenuItem(const bstrText: WideString; idItem: Integer); safecall;
    procedure SetDefaultSearchUrl(const bstrUrl: WideString); safecall;
    procedure NavigateToDefaultSearch; safecall;
    function IsRestricted(const bstrGuid: WideString): WordBool; safecall;
    function Get_ShellFeaturesEnabled: WordBool; safecall;
    function Get_SearchAssistantDefault: WordBool; safecall;
    function Get_Searches: ISearches; safecall;
    function Get_InWebFolder: WordBool; safecall;
    procedure PutProperty(bPerLocale: WordBool; const bstrName, bstrValue: WideString); safecall;
    function GetProperty(bPerLocale: WordBool; const bstrName: WideString): WideString; safecall;
    procedure Set_EventHandled(Value: WordBool); safecall;
    procedure ResetNextMenu; safecall;
    procedure FindOnWeb; safecall;
    procedure FindFilesOrFolders; safecall;
    procedure FindComputer; safecall;
    procedure FindPrinter; safecall;
    procedure FindPeople; safecall;
    function GetSearchAssistantURL(bSubstitute, bCustomize: WordBool): WideString; safecall;
    procedure NotifySearchSettingsChanged; safecall;
    procedure Set_ASProvider(const Value: WideString); safecall;
    function Get_ASProvider: WideString; safecall;
    procedure Set_ASSetting(Value: SYSINT); safecall;
    function Get_ASSetting: SYSINT; safecall;
    procedure NETDetectNextNavigate; safecall;
    procedure PutFindText(const FindText: WideString); safecall;
    function Get_Version: SYSINT; safecall;
    function EncodeString(const bstrValue, bstrCharSet: WideString; bUseUTF8: WordBool): WideString; safecall;
    property ShellFeaturesEnabled: WordBool read Get_ShellFeaturesEnabled;
    property SearchAssistantDefault: WordBool read Get_SearchAssistantDefault;
    property Searches: ISearches read Get_Searches;
    property InWebFolder: WordBool read Get_InWebFolder;
    property EventHandled: WordBool write Set_EventHandled;
    property ASProvider: WideString read Get_ASProvider write Set_ASProvider;
    property ASSetting: SYSINT read Get_ASSetting write Set_ASSetting;
    property Version: SYSINT read Get_Version;
  end;

{ DispInterface declaration for Dual Interface ISearchAssistantOC }

  ISearchAssistantOCDisp = dispinterface
    ['{72423E8F-8011-11D2-BE79-00A0C9A83DA1}']
    procedure AddNextMenuItem(const bstrText: WideString; idItem: Integer); dispid 1;
    procedure SetDefaultSearchUrl(const bstrUrl: WideString); dispid 2;
    procedure NavigateToDefaultSearch; dispid 3;
    function IsRestricted(const bstrGuid: WideString): WordBool; dispid 4;
    property ShellFeaturesEnabled: WordBool readonly dispid 5;
    property SearchAssistantDefault: WordBool readonly dispid 6;
    property Searches: ISearches readonly dispid 7;
    property InWebFolder: WordBool readonly dispid 8;
    procedure PutProperty(bPerLocale: WordBool; const bstrName, bstrValue: WideString); dispid 9;
    function GetProperty(bPerLocale: WordBool; const bstrName: WideString): WideString; dispid 10;
    property EventHandled: WordBool writeonly dispid 11;
    procedure ResetNextMenu; dispid 12;
    procedure FindOnWeb; dispid 13;
    procedure FindFilesOrFolders; dispid 14;
    procedure FindComputer; dispid 15;
    procedure FindPrinter; dispid 16;
    procedure FindPeople; dispid 17;
    function GetSearchAssistantURL(bSubstitute, bCustomize: WordBool): WideString; dispid 18;
    procedure NotifySearchSettingsChanged; dispid 19;
    property ASProvider: WideString dispid 20;
    property ASSetting: SYSINT dispid 21;
    procedure NETDetectNextNavigate; dispid 22;
    procedure PutFindText(const FindText: WideString); dispid 23;
    property Version: SYSINT readonly dispid 24;
    function EncodeString(const bstrValue, bstrCharSet: WideString; bUseUTF8: WordBool): WideString; dispid 25;
  end;

{ ISearchAssistantOC2 Interface }

  ISearchAssistantOC2 = interface(ISearchAssistantOC)
    ['{72423E8F-8011-11D2-BE79-00A0C9A83DA2}']
    function Get_ShowFindPrinter: WordBool; safecall;
    property ShowFindPrinter: WordBool read Get_ShowFindPrinter;
  end;

{ DispInterface declaration for Dual Interface ISearchAssistantOC2 }

  ISearchAssistantOC2Disp = dispinterface
    ['{72423E8F-8011-11D2-BE79-00A0C9A83DA2}']
    procedure AddNextMenuItem(const bstrText: WideString; idItem: Integer); dispid 1;
    procedure SetDefaultSearchUrl(const bstrUrl: WideString); dispid 2;
    procedure NavigateToDefaultSearch; dispid 3;
    function IsRestricted(const bstrGuid: WideString): WordBool; dispid 4;
    property ShellFeaturesEnabled: WordBool readonly dispid 5;
    property SearchAssistantDefault: WordBool readonly dispid 6;
    property Searches: ISearches readonly dispid 7;
    property InWebFolder: WordBool readonly dispid 8;
    procedure PutProperty(bPerLocale: WordBool; const bstrName, bstrValue: WideString); dispid 9;
    function GetProperty(bPerLocale: WordBool; const bstrName: WideString): WideString; dispid 10;
    property EventHandled: WordBool writeonly dispid 11;
    procedure ResetNextMenu; dispid 12;
    procedure FindOnWeb; dispid 13;
    procedure FindFilesOrFolders; dispid 14;
    procedure FindComputer; dispid 15;
    procedure FindPrinter; dispid 16;
    procedure FindPeople; dispid 17;
    function GetSearchAssistantURL(bSubstitute, bCustomize: WordBool): WideString; dispid 18;
    procedure NotifySearchSettingsChanged; dispid 19;
    property ASProvider: WideString dispid 20;
    property ASSetting: SYSINT dispid 21;
    procedure NETDetectNextNavigate; dispid 22;
    procedure PutFindText(const FindText: WideString); dispid 23;
    property Version: SYSINT readonly dispid 24;
    function EncodeString(const bstrValue, bstrCharSet: WideString; bUseUTF8: WordBool): WideString; dispid 25;
    property ShowFindPrinter: WordBool readonly dispid 26;
  end;

  _SearchAssistantEvents = dispinterface
    ['{1611FDDA-445B-11D2-85DE-00C04FA35C89}']
    procedure OnNextMenuSelect(idItem: Integer); dispid 1;
    procedure OnNewSearch; dispid 2;
  end;

{ WebBrowser Control }

  TWebBrowser_V1BeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowser_V1NavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1StatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1ProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowser_V1CommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowser_V1NewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowser_V1TitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowser_V1FrameBeforeNavigate = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool) of object;
  TWebBrowser_V1FrameNavigateComplete = procedure(Sender: TObject; const URL: WideString) of object;
  TWebBrowser_V1FrameNewWindow = procedure(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Processed: WordBool) of object;
  TWebBrowser_V1Quit = procedure(Sender: TObject; var Cancel: WordBool) of object;
  TWebBrowser_V1PropertyChange = procedure(Sender: TObject; const Property_: WideString) of object;

  TWebBrowser_V1 = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowser_V1BeforeNavigate;
    FOnNavigateComplete: TWebBrowser_V1NavigateComplete;
    FOnStatusTextChange: TWebBrowser_V1StatusTextChange;
    FOnProgressChange: TWebBrowser_V1ProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowser_V1CommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowser_V1NewWindow;
    FOnTitleChange: TWebBrowser_V1TitleChange;
    FOnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowser_V1FrameNewWindow;
    FOnQuit: TWebBrowser_V1Quit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowser_V1PropertyChange;
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
    property OnBeforeNavigate: TWebBrowser_V1BeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowser_V1NavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowser_V1StatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowser_V1ProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowser_V1CommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowser_V1NewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowser_V1TitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowser_V1FrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowser_V1FrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowser_V1FrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowser_V1Quit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowser_V1PropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;

{ WebBrowser Control }

  TWebBrowserStatusTextChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowserCommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserTitleChange = procedure(Sender: TObject; const Text: WideString) of object;
  TWebBrowserPropertyChange = procedure(Sender: TObject; const szProperty: WideString) of object;
  TWebBrowserBeforeNavigate2 = procedure(Sender: TObject; pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool) of object;
  TWebBrowserNewWindow2 = procedure(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool) of object;
  TWebBrowserNavigateComplete2 = procedure(Sender: TObject; pDisp: IDispatch; var URL: OleVariant) of object;
  TWebBrowserDocumentComplete = procedure(Sender: TObject; pDisp: IDispatch; var URL: OleVariant) of object;
  TWebBrowserOnVisible = procedure(Sender: TObject; Visible: WordBool) of object;
  TWebBrowserOnToolBar = procedure(Sender: TObject; ToolBar: WordBool) of object;
  TWebBrowserOnMenuBar = procedure(Sender: TObject; MenuBar: WordBool) of object;
  TWebBrowserOnStatusBar = procedure(Sender: TObject; StatusBar: WordBool) of object;
  TWebBrowserOnFullScreen = procedure(Sender: TObject; FullScreen: WordBool) of object;
  TWebBrowserOnTheaterMode = procedure(Sender: TObject; TheaterMode: WordBool) of object;
  TWebBrowserWindowSetResizable = procedure(Sender: TObject; Resizable: WordBool) of object;
  TWebBrowserWindowSetLeft = procedure(Sender: TObject; Left: Integer) of object;
  TWebBrowserWindowSetTop = procedure(Sender: TObject; Top: Integer) of object;
  TWebBrowserWindowSetWidth = procedure(Sender: TObject; Width: Integer) of object;
  TWebBrowserWindowSetHeight = procedure(Sender: TObject; Height: Integer) of object;
  TWebBrowserWindowClosing = procedure(Sender: TObject; IsChildWindow: WordBool; var Cancel: WordBool) of object;
  TWebBrowserClientToHostWindow = procedure(Sender: TObject; var CX, CY: Integer) of object;
  TWebBrowserSetSecureLockIcon = procedure(Sender: TObject; SecureLockIcon: Integer) of object;
  TWebBrowserFileDownload = procedure(Sender: TObject; var Cancel: WordBool) of object;

  TWebBrowser = class(TOleControl)
  private
    FOnStatusTextChange: TWebBrowserStatusTextChange;
    FOnProgressChange: TWebBrowserProgressChange;
    FOnCommandStateChange: TWebBrowserCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TWebBrowserTitleChange;
    FOnPropertyChange: TWebBrowserPropertyChange;
    FOnBeforeNavigate2: TWebBrowserBeforeNavigate2;
    FOnNewWindow2: TWebBrowserNewWindow2;
    FOnNavigateComplete2: TWebBrowserNavigateComplete2;
    FOnDocumentComplete: TWebBrowserDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TWebBrowserOnVisible;
    FOnToolBar: TWebBrowserOnToolBar;
    FOnMenuBar: TWebBrowserOnMenuBar;
    FOnStatusBar: TWebBrowserOnStatusBar;
    FOnFullScreen: TWebBrowserOnFullScreen;
    FOnTheaterMode: TWebBrowserOnTheaterMode;
    FOnWindowSetResizable: TWebBrowserWindowSetResizable;
    FOnWindowSetLeft: TWebBrowserWindowSetLeft;
    FOnWindowSetTop: TWebBrowserWindowSetTop;
    FOnWindowSetWidth: TWebBrowserWindowSetWidth;
    FOnWindowSetHeight: TWebBrowserWindowSetHeight;
    FOnWindowClosing: TWebBrowserWindowClosing;
    FOnClientToHostWindow: TWebBrowserClientToHostWindow;
    FOnSetSecureLockIcon: TWebBrowserSetSecureLockIcon;
    FOnFileDownload: TWebBrowserFileDownload;
    FIntf: IWebBrowser2;
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
    procedure Quit;
    procedure ClientToWindow(var pcx, pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant);
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant);
    procedure ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant);
    property ControlInterface: IWebBrowser2 read FIntf;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property Type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
    property Name: WideString index 0 read GetWideStringProp;
    property HWND: Integer index -515 read GetIntegerProp;
    property FullName: WideString index 400 read GetWideStringProp;
    property Path: WideString index 401 read GetWideStringProp;
    property ReadyState: tagREADYSTATE index -525 read GetTOleEnumProp;
  published
    property TabStop;
    property Align;
    property DragCursor;
    property DragMode;
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
    property Visible: WordBool index 402 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusBar: WordBool index 403 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusText: WideString index 404 read GetWideStringProp write SetWideStringProp stored False;
    property ToolBar: SYSINT index 405 read GetIntegerProp write SetIntegerProp stored False;
    property MenuBar: WordBool index 406 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 407 read GetWordBoolProp write SetWordBoolProp stored False;
    property Offline: WordBool index 550 read GetWordBoolProp write SetWordBoolProp stored False;
    property Silent: WordBool index 551 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsBrowser: WordBool index 552 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsDropTarget: WordBool index 553 read GetWordBoolProp write SetWordBoolProp stored False;
    property TheaterMode: WordBool index 554 read GetWordBoolProp write SetWordBoolProp stored False;
    property AddressBar: WordBool index 555 read GetWordBoolProp write SetWordBoolProp stored False;
    property Resizable: WordBool index 556 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnStatusTextChange: TWebBrowserStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TWebBrowserCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TWebBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TWebBrowserPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TWebBrowserBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TWebBrowserNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TWebBrowserNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TWebBrowserDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TWebBrowserOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TWebBrowserOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TWebBrowserOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TWebBrowserOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TWebBrowserOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TWebBrowserOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
    property OnWindowSetResizable: TWebBrowserWindowSetResizable read FOnWindowSetResizable write FOnWindowSetResizable;
    property OnWindowSetLeft: TWebBrowserWindowSetLeft read FOnWindowSetLeft write FOnWindowSetLeft;
    property OnWindowSetTop: TWebBrowserWindowSetTop read FOnWindowSetTop write FOnWindowSetTop;
    property OnWindowSetWidth: TWebBrowserWindowSetWidth read FOnWindowSetWidth write FOnWindowSetWidth;
    property OnWindowSetHeight: TWebBrowserWindowSetHeight read FOnWindowSetHeight write FOnWindowSetHeight;
    property OnWindowClosing: TWebBrowserWindowClosing read FOnWindowClosing write FOnWindowClosing;
    property OnClientToHostWindow: TWebBrowserClientToHostWindow read FOnClientToHostWindow write FOnClientToHostWindow;
    property OnSetSecureLockIcon: TWebBrowserSetSecureLockIcon read FOnSetSecureLockIcon write FOnSetSecureLockIcon;
    property OnFileDownload: TWebBrowserFileDownload read FOnFileDownload write FOnFileDownload;
  end;

procedure Register;

implementation

uses ComObj;

procedure TWebBrowser_V1.InitControlData;
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

procedure TWebBrowser_V1.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IWebBrowser;
end;

procedure TWebBrowser_V1.GoBack;
begin
  ControlInterface.GoBack;
end;

procedure TWebBrowser_V1.GoForward;
begin
  ControlInterface.GoForward;
end;

procedure TWebBrowser_V1.GoHome;
begin
  ControlInterface.GoHome;
end;

procedure TWebBrowser_V1.GoSearch;
begin
  ControlInterface.GoSearch;
end;

procedure TWebBrowser_V1.Navigate(const URL: WideString; var Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  ControlInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowser_V1.Refresh;
begin
  ControlInterface.Refresh;
end;

procedure TWebBrowser_V1.Refresh2(var Level: OleVariant);
begin
  ControlInterface.Refresh2(Level);
end;

procedure TWebBrowser_V1.Stop;
begin
  ControlInterface.Stop;
end;


procedure TWebBrowser.InitControlData;
const
  CEventDispIDs: array[0..26] of Integer = (
    $00000066, $0000006C, $00000069, $0000006A, $00000068, $00000071,
    $00000070, $000000FA, $000000FB, $000000FC, $00000103, $000000FD,
    $000000FE, $000000FF, $00000100, $00000101, $00000102, $00000104,
    $00000106, $00000108, $00000109, $0000010A, $0000010B, $00000107,
    $0000010C, $0000010D, $0000010E);
  CControlData: TControlData = (
    ClassID: '{8856F961-340A-11D0-A96B-00C04FD705A2}';
    EventIID: '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    EventCount: 27;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TWebBrowser.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IWebBrowser2;
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

procedure TWebBrowser.Quit;
begin
  ControlInterface.Quit;
end;

procedure TWebBrowser.ClientToWindow(var pcx, pcy: SYSINT);
begin
  ControlInterface.ClientToWindow(pcx, pcy);
end;

procedure TWebBrowser.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  ControlInterface.PutProperty(Property_, vtValue);
end;

function TWebBrowser.GetProperty(const Property_: WideString): OleVariant;
begin
  Result := ControlInterface.GetProperty(Property_);
end;

procedure TWebBrowser.Navigate2(var URL, Flags, TargetFrameName, PostData, Headers: OleVariant);
begin
  ControlInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function TWebBrowser.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  Result := ControlInterface.QueryStatusWB(cmdID);
end;

procedure TWebBrowser.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn, pvaOut: OleVariant);
begin
  ControlInterface.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TWebBrowser.ShowBrowserBar(var pvaClsid, pvarShow, pvarSize: OleVariant);
begin
  ControlInterface.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TWebBrowser_V1, TWebBrowser]);
end;

end.
