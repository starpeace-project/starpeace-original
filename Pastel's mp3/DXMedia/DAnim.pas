unit DAnim;

interface

{$Z4}
{$A+}
{$WEAKPACKAGEUNIT}

uses Windows, ActiveX, DDraw, DShow;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       danim.h
 *
 ***************************************************************************)

const
  CLSID_DABehavior: TGUID = '{283807B8-2C60-11d0-A31D-00AA00B92C03}';
  CLSID_DABoolean: TGUID = '{C46C1BC1-3C52-11d0-9200-848C1D000000}';
  CLSID_DACamera: TGUID = '{C46C1BE2-3C52-11d0-9200-848C1D000000}';
  CLSID_DAColor: TGUID = '{C46C1BC6-3C52-11d0-9200-848C1D000000}';
  CLSID_DAGeometry: TGUID = '{C46C1BE0-3C52-11d0-9200-848C1D000000}';
  CLSID_DAImage: TGUID = '{C46C1BD4-3C52-11d0-9200-848C1D000000}';
  CLSID_DAMatte: TGUID = '{C46C1BD2-3C52-11d0-9200-848C1D000000}';
  CLSID_DAMicrophone: TGUID = '{C46C1BE6-3C52-11d0-9200-848C1D000000}';
  CLSID_DAMontage: TGUID = '{C46C1BD6-3C52-11d0-9200-848C1D000000}';
  CLSID_DANumber: TGUID = '{9CDE7341-3C20-11d0-A330-00AA00B92C03}';
  CLSID_DAPath2: TGUID = '{C46C1BD0-3C52-11d0-9200-848C1D000000}';
  CLSID_DAPoint2: TGUID = '{C46C1BC8-3C52-11d0-9200-848C1D000000}';
  CLSID_DAPoint3: TGUID = '{C46C1BD8-3C52-11d0-9200-848C1D000000}';
  CLSID_DASound: TGUID = '{C46C1BE4-3C52-11d0-9200-848C1D000000}';
  CLSID_DAString: TGUID = '{C46C1BC4-3C52-11d0-9200-848C1D000000}';
  CLSID_DATransform2: TGUID = '{C46C1BCC-3C52-11d0-9200-848C1D000000}';
  CLSID_DATransform3: TGUID = '{C46C1BDC-3C52-11d0-9200-848C1D000000}';
  CLSID_DAVector2: TGUID = '{C46C1BCA-3C52-11d0-9200-848C1D000000}';
  CLSID_DAVector3: TGUID = '{C46C1BDA-3C52-11d0-9200-848C1D000000}';
  CLSID_DAFontStyle: TGUID = '{25B0F91C-D23D-11d0-9B85-00C04FC2F51D}';
  CLSID_DALineStyle: TGUID = '{C46C1BF2-3C52-11d0-9200-848C1D000000}';
  CLSID_DAEndStyle: TGUID = '{C46C1BEC-3C52-11d0-9200-848C1D000000}';
  CLSID_DAJoinStyle: TGUID = '{C46C1BEE-3C52-11d0-9200-848C1D000000}';
  CLSID_DADashStyle: TGUID = '{C46C1BF0-3C52-11d0-9200-848C1D000000}';
  CLSID_DABbox2: TGUID = '{C46C1BCE-3C52-11d0-9200-848C1D000000}';
  CLSID_DABbox3: TGUID = '{C46C1BDE-3C52-11d0-9200-848C1D000000}';
  CLSID_DAPair: TGUID = '{C46C1BF4-3C52-11d0-9200-848C1D000000}';
  CLSID_DAEvent: TGUID = '{50B4791F-4731-11d0-8912-00C04FC2A0CA}';
  CLSID_DAArray: TGUID = '{D17506C3-6B26-11d0-8914-00C04FC2A0CA}';
  CLSID_DATuple: TGUID = '{5DFB2651-9668-11d0-B17B-00C04FC2A0CA}';
  CLSID_DAUserData: TGUID = '{AF868304-AB0B-11d0-876A-00C04FC29D46}';
  CLSID_DAView: TGUID = '{283807B5-2C60-11d0-A31D-00AA00B92C03}';
  CLSID_DAImportationResult: TGUID = '{BCBB1F75-E384-11d0-9B99-00C04FC2F51D}';
  CLSID_DAPickableResult: TGUID = '{BCBB1F74-E384-11d0-9B99-00C04FC2F51D}';
  CLSID_DAStatics: TGUID = '{542FB453-5003-11cf-92A2-00AA00B8A733}';
  CLSID_DAViewerControl: TGUID = '{B6FFC24C-7E13-11D0-9B47-00C04FC2F51D}';
  CLSID_DAViewerControlWindowed: TGUID = '{69AD90EF-1C20-11d1-8801-00C04FC29D46}';

  IID_IDAViewSite: TGUID = '{283807B3-2C60-11d0-A31D-00AA00B92C03}';
  IID_IDAView: TGUID = '{283807B4-2C60-11d0-A31D-00AA00B92C03}';
  IID_IDABehavior: TGUID = '{283807B7-2C60-11d0-A31D-00AA00B92C03}';
  IID_IDANumber: TGUID = '{9CDE7340-3C20-11d0-A330-00AA00B92C03}';
  IID_IDABvrHook: TGUID = '{3E2487C4-8709-11d0-B177-00C04FC2A0CA}';
  IID_IDADrawingSurface: TGUID = '{BC0BFD34-D21D-11d0-9385-00C04FB6BD36}';
  IID_IDAPickableResult: TGUID = '{4A933703-E36F-11d0-9B99-00C04FC2F51D}';
  IID_IDAUntilNotifier: TGUID = '{3F3DA01A-4705-11d0-8710-00C04FC29D46}';
  IID_IDABoolean: TGUID = '{C46C1BC0-3C52-11d0-9200-848C1D000000}';
  IID_IDACamera: TGUID = '{C46C1BE1-3C52-11d0-9200-848C1D000000}';
  IID_IDAColor: TGUID = '{C46C1BC5-3C52-11d0-9200-848C1D000000}';
  IID_IDAGeometry: TGUID = '{C46C1BDF-3C52-11d0-9200-848C1D000000}';
  IID_IDAImage: TGUID = '{C46C1BD3-3C52-11d0-9200-848C1D000000}';
  IID_IDAMatte: TGUID = '{C46C1BD1-3C52-11d0-9200-848C1D000000}';
  IID_IDAMicrophone: TGUID = '{C46C1BE5-3C52-11d0-9200-848C1D000000}';
  IID_IDAMontage: TGUID = '{C46C1BD5-3C52-11d0-9200-848C1D000000}';
  IID_IDAPath2: TGUID = '{C46C1BCF-3C52-11d0-9200-848C1D000000}';
  IID_IDAPoint2: TGUID = '{C46C1BC7-3C52-11d0-9200-848C1D000000}';
  IID_IDAPoint3: TGUID = '{C46C1BD7-3C52-11d0-9200-848C1D000000}';
  IID_IDASound: TGUID = '{C46C1BE3-3C52-11d0-9200-848C1D000000}';
  IID_IDAString: TGUID = '{C46C1BC3-3C52-11d0-9200-848C1D000000}';
  IID_IDATransform2: TGUID = '{C46C1BCB-3C52-11d0-9200-848C1D000000}';
  IID_IDATransform3: TGUID = '{C46C1BDB-3C52-11d0-9200-848C1D000000}';
  IID_IDAVector2: TGUID = '{C46C1BC9-3C52-11d0-9200-848C1D000000}';
  IID_IDAVector3: TGUID = '{C46C1BD9-3C52-11d0-9200-848C1D000000}';
  IID_IDAFontStyle: TGUID = '{25B0F91D-D23D-11d0-9B85-00C04FC2F51D}';
  IID_IDALineStyle: TGUID = '{C46C1BF1-3C52-11d0-9200-848C1D000000}';
  IID_IDAEndStyle: TGUID = '{C46C1BEB-3C52-11d0-9200-848C1D000000}';
  IID_IDAJoinStyle: TGUID = '{C46C1BED-3C52-11d0-9200-848C1D000000}';
  IID_IDADashStyle: TGUID = '{C46C1BEF-3C52-11d0-9200-848C1D000000}';
  IID_IDABbox2: TGUID = '{C46C1BCD-3C52-11d0-9200-848C1D000000}';
  IID_IDABbox3: TGUID = '{C46C1BDD-3C52-11d0-9200-848C1D000000}';
  IID_IDAPair: TGUID = '{C46C1BF3-3C52-11d0-9200-848C1D000000}';
  IID_IDAEvent: TGUID = '{50B4791E-4731-11d0-8912-00C04FC2A0CA}';
  IID_IDAArray: TGUID = '{D17506C2-6B26-11d0-8914-00C04FC2A0CA}';
  IID_IDATuple: TGUID = '{5DFB2650-9668-11d0-B17B-00C04FC2A0CA}';
  IID_IDAUserData: TGUID = '{AF868305-AB0B-11d0-876A-00C04FC29D46}';
  IID_IDAPreferences: TGUID = '{69B5BC70-9B19-11d0-9B60-00C04FC2F51D}';
  IID_IDASite: TGUID = '{45393DF0-54B9-11cf-92A2-00AA00B8A733}';
  IID_IDAImportationResult: TGUID = '{4A933702-E36F-11d0-9B99-00C04FC2F51D}';
  IID_IDAStatics: TGUID = '{542FB452-5003-11cf-92A2-00AA00B8A733}';
  IID_IDAViewerControl: TGUID = '{0E41257B-812D-11D0-9B4A-00C04FC2F51D}';
  IID_IDAViewerControlWindowed: TGUID = '{BA8B033E-1E91-11d1-8809-00C04FC29D46}';

type
  TDA_Dash_Style = (
    DAEmpty,
    DASolid,
    DADash
  );

  TDA_End_Style = (
    DAEndFlat,
    DAEndSquare,
    DAEndRound
  );

  TDA_Join_Style = (
    DAJoinBevel,
    DAJoinRound,
    DAJoinMiter
  );

  TDA_Timer_Source = (
    DAMultimediaTimer,
    DAContainerTimer,
    DAWMTimer
  );

type
  IDABehavior = interface;
  IDAImage = interface;
  IDASound = interface;
  IDAViewSite = interface;
  IDAPreferences = interface;

  IDAViewSite = interface(IDispatch)
    ['{283807B3-2C60-11d0-A31D-00AA00B92C03}']
    function SetStatusText(StatusText: TBSTR): HRESULT; stdcall;
  end;

  IDAView = interface(IDispatch)
    ['{283807B4-2C60-11d0-A31D-00AA00B92C03}']
    function get_SimulationTime(var simTime: double): HRESULT; stdcall;
    function Tick(simTime: double; var needToRender: WordBool): HRESULT; stdcall;
    function Render: HRESULT; stdcall;
    function AddBvrToRun(bvr: IDABehavior; var pId: Longint): HRESULT; stdcall;
    function RemoveRunningBvr(id: Longint): HRESULT; stdcall;
    function StartModel(pImage: IDAImage; pSound: IDASound; startTime: double): HRESULT; stdcall;
    function StopModel: HRESULT; stdcall;
    function get_Window(var hwnd: HWND): HRESULT; stdcall;
    function put_Window(hwnd: HWND): HRESULT; stdcall;
    function get_IDirectDrawSurface(out ddsurf: IDirectDrawSurface): HRESULT; stdcall;
    function put_IDirectDrawSurface(ddsurf: IDirectDrawSurface): HRESULT; stdcall;
    function get_DC(var dc: HDC): HRESULT; stdcall;
    function put_DC(dc: HDC): HRESULT; stdcall;
    function get_CompositeDirectlyToTarget(var composeToTarget: WordBool): HRESULT; stdcall;
    function put_CompositeDirectlyToTarget(composeToTarget: WordBool): HRESULT; stdcall;
    function SetViewport(xPos, yPos, w, h: Longint): HRESULT; stdcall;
    function SetClipRect(xPos, yPos, w, h: Longint): HRESULT; stdcall;
    function RePaint(xPos, yPos, w, h: Longint): HRESULT; stdcall;
    function PaletteChanged(bNew: WordBool): HRESULT; stdcall;
    function get_Site(out pViewSite: IDAViewSite): HRESULT; stdcall;
    function put_Site(pViewSite: IDAViewSite): HRESULT; stdcall;
    function put_ClientSite(pClientSite: IOleClientSite): HRESULT; stdcall;
    function get_ClientSite(out pClientSite: IOleClientSite): HRESULT; stdcall;
    function OnMouseMove(when: double; xPos, yPos: Longint; modifiers: Byte): HRESULT; stdcall;
    function OnMouseButton(when: double; xPos, yPos: Longint; button: Byte;
        bPressed: WordBool; modifiers: Byte): HRESULT; stdcall;
    function OnKey(when: double; key: Longint; bPressed: WordBool;
        modifiers: Byte): HRESULT; stdcall;
    function OnFocus(bHasFocus: WordBool): HRESULT; stdcall;
    function get_Preferences(out prefs: IDAPreferences): HRESULT; stdcall;
    function QueryHitPoint(dwAspect: DWORD; const prcBounds: TRect;
        ptLoc: TPoint; lCloseHint: Longint; var pHitResult: DWORD): HRESULT; stdcall;
  end;

  IDANumber = interface;
  IDABvrHook = interface;

  IDABehavior = interface(IDispatch)
    ['{283807B7-2C60-11d0-A31D-00AA00B92C03}']
    function GetClassName(var pClassName: TBSTR): HRESULT; stdcall;
    function Init(toBvr: IDABehavior): HRESULT; stdcall;
    function Importance(relativeImportance: double; out ppBvr: IDABehavior): HRESULT; stdcall;
    function RunOnce(out bvr: IDABehavior): HRESULT; stdcall;
    function SubstituteTime(xform: IDANumber; out bvr: IDABehavior): HRESULT; stdcall;
    function SwitchTo(switchTo: IDABehavior): HRESULT; stdcall;
    function SwitchToNumber(numToSwitchTo: double): HRESULT; stdcall;
    function SwitchToString(strToSwitchTo: TBSTR): HRESULT; stdcall;
    function Hook(notifier: IDABvrHook; out pBvr: IDABehavior): HRESULT; stdcall;
    function Duration(duration: double; out bvr: IDABehavior): HRESULT; stdcall;
    function DurationAnim(duration: IDANumber; out bvr: IDABehavior): HRESULT; stdcall;
    function Repeat_(count: Longint; out bvr: IDABehavior): HRESULT; stdcall;
    function RepeatForever(out bvr: IDABehavior): HRESULT; stdcall;
    function IsReady(bBlock: WordBool; var b: WordBool): HRESULT; stdcall;
  end;

  IDAString = interface;

  IDANumber = interface(IDABehavior)
    ['{9CDE7340-3C20-11d0-A330-00AA00B92C03}']
    function Extract(var ret_0: double): HRESULT; stdcall;
    function AnimateProperty(propertyPath_0, scriptingLanguage_1: TBSTR;
        invokeAsMethod_2: WordBool; minUpdateInterval_3: double;
        out ret_4: IDANumber): HRESULT; stdcall;
    function ToStringAnim(precision_0: IDANumber; out ret_1: IDAString): HRESULT; stdcall;
    function ToString(precision_0: double; out ret_1: IDAString): HRESULT; stdcall;
  end;

  IDABvrHook = interface(IUnknown)
    ['{3E2487C4-8709-11d0-B177-00C04FC2A0CA}']
    function Notify(id: Longint; startingPerformance: WordBool;
        startTime, gTime, lTime: double; sampleVal, curRunningBvr: IDABehavior;
        out ppBvr: IDABehavior): HRESULT; stdcall;
  end;

  IDALineStyle = interface;
  IDAFontStyle = interface;
  IDAMatte = interface;
  IDAPoint2 = interface;
  IDATransform2 = interface;
  IDAColor = interface;
  IDAPath2 = interface;

  IDADrawingSurface = interface(IDispatch)
    ['{BC0BFD34-D21D-11d0-9385-00C04FB6BD36}']
    function get_Image(out img: IDAImage): HRESULT; stdcall;
    function put_LineStyle(ls: IDALineStyle): HRESULT; stdcall;
    function put_BorderStyle(bs: IDALineStyle): HRESULT; stdcall;
    function put_FontStyle(fs: IDAFontStyle): HRESULT; stdcall;
    function put_ClipMatte(matte: IDAMatte): HRESULT; stdcall;
    function put_MouseEventsEnabled(on: WordBool): HRESULT; stdcall;
    function put_HatchFillTransparent(fillOff: WordBool): HRESULT; stdcall;
    function get_LocalContextImage(img: IDAImage): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
    function SaveGraphicsState: HRESULT; stdcall;
    function RestoreGraphicsState: HRESULT; stdcall;
    function Opacity(opac: double): HRESULT; stdcall;
    function OpacityAnim(opac: IDANumber): HRESULT; stdcall;
    function Crop(lowerLeftX, lowerLeftY, upperRightX, upperRightY: double): HRESULT; stdcall;
    function CropPoints(min, max: IDAPoint2): HRESULT; stdcall;
    function Transform(xform: IDATransform2): HRESULT; stdcall;
    function LineColor(clr: IDAColor): HRESULT; stdcall;
    function LineWidth(w: double): HRESULT; stdcall;
    function LineDashStyle(id: TDA_Dash_Style): HRESULT; stdcall;
    function LineEndStyle(id: TDA_End_Style): HRESULT; stdcall;
    function LineJoinStyle(id: TDA_Join_Style): HRESULT; stdcall;
    function BorderColor(clr: IDAColor): HRESULT; stdcall;
    function BorderWidth(w: double): HRESULT; stdcall;
    function BorderDashStyle(id: TDA_Dash_Style): HRESULT; stdcall;
    function BorderEndStyle(obsolete: TDA_End_Style): HRESULT; stdcall;
    function BorderJoinStyle(id: TDA_Join_Style): HRESULT; stdcall;
    function Font(FontFace: TBSTR; sizeInPoints: Longint; Bold, italic,
        underline, strikethrough: WordBool): HRESULT; stdcall;
    function TextureFill(obsolete1: IDAImage; obsolete2, obsolete3: double): HRESULT; stdcall;
    function ImageFill(obsolete1: IDAImage; obsolete2, obsolete3: double): HRESULT; stdcall;
    function FillTexture(img: IDAImage): HRESULT; stdcall;
    function FillImage(img: IDAImage): HRESULT; stdcall;
    function FillStyle(ID: Integer): HRESULT; stdcall;
    function FillColor(foreground: IDAColor): HRESULT; stdcall;
    function SecondaryFillColor(val: IDAColor): HRESULT; stdcall;
    function GradientShape(pts: VARIANT): HRESULT; stdcall;
    function GradientExtent(startx, starty, finishx, finishy: double): HRESULT; stdcall;
    function GradientExtentPoints(startColor, stopColor: IDAPoint2): HRESULT; stdcall;
    function GradientRolloffPower(power: double): HRESULT; stdcall;
    function GradientRolloffPowerAnim(power: IDANumber): HRESULT; stdcall;
    function FixedFillScale: HRESULT; stdcall;
    function HorizontalFillScale: HRESULT; stdcall;
    function VerticalFillScale: HRESULT; stdcall;
    function AutoSizeFillScale: HRESULT; stdcall;
    function PolylineEx(numPts: Longint; pts: IDAPoint2): HRESULT; stdcall;
    function Polyline(v: VARIANT): HRESULT; stdcall;
    function PolygonEx(numPts: Longint; pts: IDAPoint2): HRESULT; stdcall;
    function Polygon(v: VARIANT): HRESULT; stdcall;
    function LinePoints(p1, p2: IDAPoint2): HRESULT; stdcall;
    function Line(startX, startY, endX, endY: double): HRESULT; stdcall;
    function ArcRadians(xPos, yPos, startAngle, endAngle, arcWidth, arcHeight: double): HRESULT; stdcall;
    function ArcDegrees(xPos, yPos, startAngle, endAngle, arcWidth, arcHeight: double): HRESULT; stdcall;
    function Oval(xPos, yPos, w, h: double): HRESULT; stdcall;
    function Rect(xPos, yPos, w, h: double): HRESULT; stdcall;
    function RoundRect(xPos, yPos, w, h, arcWidth, arcHeight: double): HRESULT; stdcall;
    function PieRadians(xPos, yPos, startAngle, endAngle, arcWidth, arcHeight: double): HRESULT; stdcall;
    function PieDegrees(xPos, yPos, startAngle, endAngle, arcWidth, arcHeight: double): HRESULT; stdcall;
    function Text(str: TBSTR; xPos, yPos: double): HRESULT; stdcall;
    function TextPoint(str: TBSTR; point: IDAPoint2): HRESULT; stdcall;
    function FillPath(path: IDAPath2): HRESULT; stdcall;
    function DrawPath(path: IDAPath2): HRESULT; stdcall;
    function OverlayImage(img: IDAImage): HRESULT; stdcall;
  end;

  IDAGeometry = interface;
  IDAEvent = interface;

  IDAPickableResult = interface(IDispatch)
    ['{4A933703-E36F-11d0-9B99-00C04FC2F51D}']
    function get_Image(out ppImage: IDAImage): HRESULT; stdcall;
    function get_Geometry(out ppGeometry: IDAGeometry): HRESULT; stdcall;
    function get_PickEvent(out ppPickEvent: IDAEvent): HRESULT; stdcall;
  end;

  IDAUntilNotifier = interface(IDispatch)
    ['{3F3DA01A-4705-11d0-8710-00C04FC29D46}']
    function Notify(eventData, curRunningBvr: IDABehavior; curView: IDAView;
        out ppBvr: IDABehavior): HRESULT; stdcall;
  end;

  IDABoolean = interface(IDABehavior)
    ['{C46C1BC0-3C52-11d0-9200-848C1D000000}']
    function Extract(var ret_0: WordBool): HRESULT; stdcall;
  end;

  IDATransform3 = interface;

  IDACamera = interface(IDABehavior)
    ['{C46C1BE1-3C52-11d0-9200-848C1D000000}']
    function Transform(xf_0: IDATransform3; out ret_1: IDACamera): HRESULT; stdcall;
    function Depth(depth_0: double; out ret_1: IDACamera): HRESULT; stdcall;
    function DepthAnim(depth_0: IDANumber; out ret_1: IDACamera): HRESULT; stdcall;
    function DepthResolution(resolution_0: double; out ret_1: IDACamera): HRESULT; stdcall;
    function DepthResolutionAnim(resolution_0: IDANumber; out ret_1: IDACamera): HRESULT; stdcall;
  end;

  IDAColor = interface(IDABehavior)
    ['{C46C1BC5-3C52-11d0-9200-848C1D000000}']
    function get_Red(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Green(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Blue(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Hue(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Saturation(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Lightness(out ret_0: IDANumber): HRESULT; stdcall;
  end;

  IDAMicrophone = interface;
  IDABbox3 =  interface;

  IDAGeometry = interface(IDABehavior)
    ['{C46C1BDF-3C52-11d0-9200-848C1D000000}']
    function RenderSound(mic_0: IDAMicrophone; out ret_1: IDASound): HRESULT; stdcall;
    function Pickable(out ret_0: IDAPickableResult): HRESULT; stdcall;
    function PickableOccluded(out ret_0: IDAPickableResult): HRESULT; stdcall;
    function Undetectable(out ret_0: IDAGeometry): HRESULT; stdcall;
    function EmissiveColor(col_0: IDAColor; out ret_1: IDAGeometry): HRESULT; stdcall;
    function DiffuseColor(col_0: IDAColor; out ret_1: IDAGeometry): HRESULT; stdcall;
    function SpecularColor(col_0: IDAColor; out ret_1: IDAGeometry): HRESULT; stdcall;
    function SpecularExponent(power_0: double; out ret_1: IDAGeometry): HRESULT; stdcall;
    function SpecularExponentAnim(power_0: IDANumber; out ret_1: IDAGeometry): HRESULT; stdcall;
    function Texture(texture_0: IDAImage; out ret_1: IDAGeometry): HRESULT; stdcall;
    function Opacity(level_0: double; out ret_1: IDAGeometry): HRESULT; stdcall;
    function OpacityAnim(level_0: IDANumber; out ret_1: IDAGeometry): HRESULT; stdcall;
    function Transform(xf_0: IDATransform3; out ret_1: IDAGeometry): HRESULT; stdcall;
    function get_BoundingBox(out ret_0: IDABbox3): HRESULT; stdcall;
    function Render(cam_0: IDACamera; out ret_1: IDAImage): HRESULT; stdcall;
    function LightColor(color_0: IDAColor; out ret_1: IDAGeometry): HRESULT; stdcall;
    function LightAttenuationAnim(constant_0, linear_1, quadratic_2: IDANumber;
        out ret_3: IDAGeometry): HRESULT; stdcall;
    function LightAttenuation(constant_0, linear_1, quadratic_2: double;
        out ret_3: IDAGeometry): HRESULT; stdcall;
  end;

  IDABbox2 = interface;

  IDAImage = interface(IDABehavior)
    ['{C46C1BD3-3C52-11d0-9200-848C1D000000}']
    function Pickable(out ret_0: IDAPickableResult): HRESULT; stdcall;
    function PickableOccluded(out ret_0: IDAPickableResult): HRESULT; stdcall;
    function ApplyBitmapEffect(effectToApply_0: IUnknown; firesWhenChanged_1: IDAEvent;
        out ret_2: IDAImage): HRESULT; stdcall;
    function get_BoundingBox(out ret_0: IDABbox2): HRESULT; stdcall;
    function Crop(min_0, max_1: IDAPoint2; out ret_2: IDAImage): HRESULT; stdcall;
    function Transform(xf_0: IDATransform2; out ret_1: IDAImage): HRESULT; stdcall;
    function OpacityAnim(opacity_0: IDANumber; out ret_1: IDAImage): HRESULT; stdcall;
    function Opacity(opacity_0: double; out ret_1: IDAImage): HRESULT; stdcall;
    function Undetectable(out ret_0: IDAImage): HRESULT; stdcall;
    function Tile(out ret_0: IDAImage): HRESULT; stdcall;
    function Clip(m_0: IDAMatte; out ret_1: IDAImage): HRESULT; stdcall;
    function MapToUnitSquare(out ret_0: IDAImage): HRESULT; stdcall;
    function ClipPolygonImageEx(points_0size: Longint; points_0: IDAPoint2;
        out ret_1: IDAImage): HRESULT; stdcall;
    function ClipPolygonImage(points_0: Variant; out ret_1: IDAImage): HRESULT; stdcall;
  end;

  IDAMatte = interface(IDABehavior)
    ['{C46C1BD1-3C52-11d0-9200-848C1D000000}']
    function Transform(xf_0: IDATransform2; out ret_1: IDAMatte): HRESULT; stdcall;
  end;

  IDAMicrophone = interface(IDABehavior)
    ['{C46C1BE5-3C52-11d0-9200-848C1D000000}']
    function Transform(xf_0: IDATransform3; out ret_1: IDAMicrophone): HRESULT; stdcall;
  end;

  IDAMontage = interface(IDABehavior)
    ['{C46C1BD5-3C52-11d0-9200-848C1D000000}']
    function Render(out ret_0: IDAImage): HRESULT; stdcall;
  end;

  IDAPath2 = interface(IDABehavior)
    ['{C46C1BCF-3C52-11d0-9200-848C1D000000}']
    function Transform(xf_0: IDATransform2; out ret_1: IDAPath2): HRESULT; stdcall;
    function BoundingBox(style_0: IDALineStyle; out ret_1: IDABbox2): HRESULT; stdcall;
    function Fill(border_0: IDALineStyle; fill_1: IDAImage; out ret_2: IDAImage): HRESULT; stdcall;
    function Draw(border_0: IDALineStyle; out ret_2: IDAImage): HRESULT; stdcall;
    function Close(out ret_0: IDAPath2): HRESULT; stdcall;
  end;

  IDAPoint2 = interface(IDABehavior)
    ['{C46C1BC7-3C52-11d0-9200-848C1D000000}']
    function AnimateControlPosition(propertyPath_0, scriptingLanguage_1: TBSTR;
        invokeAsMethod_2: WordBool; minUpdateInterval_3: double; out ret_4: IDAPoint2): HRESULT; stdcall;
    function AnimateControlPositionPixel(propertyPath_0, scriptingLanguage_1: TBSTR;
        invokeAsMethod_2: WordBool; minUpdateInterval_3: double; out ret_4: IDAPoint2): HRESULT; stdcall;
    function get_X(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Y(out ret_0: IDANumber): HRESULT; stdcall;
    function get_PolarCoordAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_PolarCoordLength(out ret_0: IDANumber): HRESULT; stdcall;
    function Transform(xf_0: IDATransform2; out ret_1: IDAPoint2): HRESULT; stdcall;
  end;

  IDAPoint3 = interface(IDABehavior)
    ['{C46C1BD7-3C52-11d0-9200-848C1D000000}']
    function Project(cam_0: IDACamera; out ret_1: IDAPoint2): HRESULT; stdcall;
    function get_X(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Y(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Z(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordXYAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordYZAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordLength(out ret_0: IDANumber): HRESULT; stdcall;
    function Transform(xf_0: IDATransform3; out ret_1: IDAPoint3): HRESULT; stdcall;
  end;

  IDASound = interface(IDABehavior)
    ['{C46C1BE3-3C52-11d0-9200-848C1D000000}']
    function PhaseAnim(phaseAmt_0: IDANumber; out ret_1: IDASound): HRESULT; stdcall;
    function Phase(phaseAmt_0: double; out ret_1: IDASound): HRESULT; stdcall;
    function RateAnim(pitchShift_0: IDANumber; out ret_1: IDASound): HRESULT; stdcall;
    function Rate(pitchShift_0: double; out ret_1: IDASound): HRESULT; stdcall;
    function PanAnim(panAmt_0: IDANumber; out ret_1: IDASound): HRESULT; stdcall;
    function Pan(panAmt_0: double; out ret_1: IDASound): HRESULT; stdcall;
    function GainAnim(gainAmt_0: IDANumber; out ret_1: IDASound): HRESULT; stdcall;
    function Gain(gainAmt_0: double; out ret_1: IDASound): HRESULT; stdcall;
    function Loop(out ret_0: IDASound): HRESULT; stdcall;
  end;

  IDAString = interface(IDABehavior)
    ['{C46C1BC3-3C52-11d0-9200-848C1D000000}']
    function Extract(var ret_0: TBSTR): HRESULT; stdcall;
    function AnimateProperty(propertyPath_0, scriptingLanguage_1: TBSTR;
        invokeAsMethod_2: WordBool; minUpdateInterval_3: double; out ret_4: IDAString): HRESULT; stdcall;
  end;

  IDATransform2 = interface(IDABehavior)
    ['{C46C1BCB-3C52-11d0-9200-848C1D000000}']
    function Inverse(out ret_0: IDATransform2): HRESULT; stdcall;
    function get_IsSingular(out ret_0: IDABoolean): HRESULT; stdcall;
  end;

  IDATransform3 = interface(IDABehavior)
    ['{C46C1BDB-3C52-11d0-9200-848C1D000000}']
    function Inverse(out ret_0: IDATransform3): HRESULT; stdcall;
    function get_IsSingular(out ret_0: IDABoolean): HRESULT; stdcall;
    function ParallelTransform2(out ret_0: IDATransform2): HRESULT; stdcall;
  end;

  IDAVector2 = interface(IDABehavior)
    ['{C46C1BC9-3C52-11d0-9200-848C1D000000}']
    function get_Length(out ret_0: IDANumber): HRESULT; stdcall;
    function get_LengthSquared(out ret_0: IDANumber): HRESULT; stdcall;
    function Normalize(out ret_0: IDAVector2): HRESULT; stdcall;
    function MulAnim(scalar_0: IDANumber; out ret_1: IDAVector2): HRESULT; stdcall;
    function Mul(scalar_0: double; out ret_1: IDAVector2): HRESULT; stdcall;
    function DivAnim(scalar_0: IDANumber; out ret_1: IDAVector2): HRESULT; stdcall;
    function Div_(scalar_0: double; out ret_1: IDAVector2): HRESULT; stdcall;
    function get_X(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Y(out ret_0: IDANumber): HRESULT; stdcall;
    function get_PolarCoordAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_PolarCoordLength(out ret_0: IDANumber): HRESULT; stdcall;
    function Transform(xf_0: IDATransform2; out ret_1: IDAVector2): HRESULT; stdcall;
  end;

  IDAVector3 = interface(IDABehavior)
    ['{C46C1BD9-3C52-11d0-9200-848C1D000000}']
    function get_Length(out ret_0: IDANumber): HRESULT; stdcall;
    function get_LengthSquared(out ret_0: IDANumber): HRESULT; stdcall;
    function Normalize(out ret_0: IDAVector3): HRESULT; stdcall;
    function MulAnim(scalar_0: IDANumber; out ret_1: IDAVector3): HRESULT; stdcall;
    function Mul(scalar_0: double; out ret_1: IDAVector3): HRESULT; stdcall;
    function DivAnim(scalar_0: IDANumber; out ret_1: IDAVector3): HRESULT; stdcall;
    function Div_(scalar_0: double; out ret_1: IDAVector3): HRESULT; stdcall;
    function get_X(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Y(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Z(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordXYAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordYZAngle(out ret_0: IDANumber): HRESULT; stdcall;
    function get_SphericalCoordLength(out ret_0: IDANumber): HRESULT; stdcall;
    function Transform(xf_0: IDATransform3; out ret_1: IDAVector3): HRESULT; stdcall;
  end;

  IDAFontStyle = interface(IDABehavior)
    ['{25B0F91D-D23D-11d0-9B85-00C04FC2F51D}']
    function Bold(out ret_0: IDAFontStyle): HRESULT; stdcall;
    function Italic(out ret_0: IDAFontStyle): HRESULT; stdcall;
    function Underline(out ret_0: IDAFontStyle): HRESULT; stdcall;
    function Strikethrough(out ret_0: IDAFontStyle): HRESULT; stdcall;
    function AntiAliasing(aaStyle_0: double; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function Color(col_0: IDAColor; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function FamilyAnim(face_0: IDAString; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function Family(face_0: TBSTR; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function SizeAnim(size_0: IDANumber; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function Size(size_0: double; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function Weight(weight_0: double; out ret_1: IDAFontStyle): HRESULT; stdcall;
    function WeightAnim(weight_0: IDANumber; out ret_1: IDAFontStyle): HRESULT; stdcall;
  end;

  IDAEndStyle = interface;
  IDAJoinStyle = interface;
  IDADashStyle = interface;

  IDALineStyle = interface(IDABehavior)
    ['{C46C1BF1-3C52-11d0-9200-848C1D000000}']
    function End_(sty_0: IDAEndStyle; out ret_1: IDALineStyle): HRESULT; stdcall;
    function Join(sty_0: IDAJoinStyle; out ret_1: IDALineStyle): HRESULT; stdcall;
    function Dash(sty_0: IDADashStyle; out ret_1: IDALineStyle): HRESULT; stdcall;
    function WidthAnim(sty_0: IDANumber; out ret_1: IDALineStyle): HRESULT; stdcall;
    function width(sty_0: double; out ret_1: IDALineStyle): HRESULT; stdcall;
    function AntiAliasing(aaStyle_0: double; out ret_1: IDALineStyle): HRESULT; stdcall;
    function Detail(out ret_0: IDALineStyle): HRESULT; stdcall;
    function Color(clr_0: IDAColor; out ret_1: IDALineStyle): HRESULT; stdcall;
  end;

  IDAEndStyle = interface(IDABehavior)
    ['{C46C1BEB-3C52-11d0-9200-848C1D000000}']
  end;

  IDAJoinStyle = interface(IDABehavior)
    ['{C46C1BED-3C52-11d0-9200-848C1D000000}']
  end;

  IDADashStyle = interface(IDABehavior)
    ['{C46C1BEF-3C52-11d0-9200-848C1D000000}']
  end;

  IDABbox2 = interface(IDABehavior)
    ['{C46C1BCD-3C52-11d0-9200-848C1D000000}']
    function get_Min(out ret_0: IDAPoint2): HRESULT; stdcall;
    function get_Max(out ret_0: IDAPoint2): HRESULT; stdcall;
  end;

  IDABbox3 = interface(IDABehavior)
    ['{C46C1BDD-3C52-11d0-9200-848C1D000000}']
    function get_Min(out ret_0: IDAPoint3): HRESULT; stdcall;
    function get_Max(out ret_0: IDAPoint3): HRESULT; stdcall;
  end;

  IDAPair = interface(IDABehavior)
    ['{C46C1BF3-3C52-11d0-9200-848C1D000000}']
    function get_First(out ret_0: IDABehavior): HRESULT; stdcall;
    function get_Second(out ret_0: IDABehavior): HRESULT; stdcall;
  end;

  IDAEvent = interface(IDABehavior)
    ['{50B4791E-4731-11d0-8912-00C04FC2A0CA}']
    function Notify(notifier_0: IDAUntilNotifier; out ret_1: IDAEvent): HRESULT; stdcall;
    function Snapshot(b_0: IDABehavior; out ret_1: IDAEvent): HRESULT; stdcall;
    function AttachData(data_0: IDABehavior; out ret_1: IDAEvent): HRESULT; stdcall;
    function ScriptCallback(scriptlet_0, language_1: TBSTR; out ret_2: IDAEvent): HRESULT; stdcall;
  end;

  IDAArray = interface(IDABehavior)
    ['{D17506C2-6B26-11d0-8914-00C04FC2A0CA}']
    function NthAnim(index_0: IDANumber; out ret_1: IDABehavior): HRESULT; stdcall;
    function Length(out ret_0: IDANumber): HRESULT; stdcall;
  end;

  IDATuple = interface(IDABehavior)
    ['{5DFB2650-9668-11d0-B17B-00C04FC2A0CA}']
    function Nth(index_0: Longint; out ret_1: IDABehavior): HRESULT; stdcall;
    function get_Length(var ret_0: Longint): HRESULT; stdcall;
  end;

  IDAUserData = interface(IDABehavior)
    ['{AF868305-AB0B-11d0-876A-00C04FC29D46}']
    function get_Data(out ret_0: IUnknown): HRESULT; stdcall;
  end;

  IDAPreferences = interface(IDispatch)
    ['{69B5BC70-9B19-11d0-9B60-00C04FC2F51D}']
    function PutPreference(preferenceName: TBSTR; value: Variant): HRESULT; stdcall;
    function GetPreference(preferenceName: TBSTR; var value: Variant): HRESULT; stdcall;
    function Propagate: HRESULT; stdcall;
  end;

  IDASite = interface(IDispatch)
    ['{45393DF0-54B9-11cf-92A2-00AA00B8A733}']
    function SetStatusText(StatusText: TBSTR): HRESULT; stdcall;
    function ReportError(hr: HRESULT; ErrorText: TBSTR): HRESULT; stdcall;
    function ReportGC(bStarting: WordBool): HRESULT; stdcall;
  end;

  IDAImportationResult = interface(IDispatch)
    ['{4A933702-E36F-11d0-9B99-00C04FC2F51D}']
    function get_Image(out ppImage: IDAImage): HRESULT; stdcall;
    function get_Sound(out ppSound: IDASound): HRESULT; stdcall;
    function get_Geometry(out ppGeometry: IDAGeometry): HRESULT; stdcall;
    function get_Duration(out ppDuration: IDANumber): HRESULT; stdcall;
    function get_CompletionEvent(out ppCompletionEvent: IDAEvent): HRESULT; stdcall;
    function get_Progress(out ppProgress: IDANumber): HRESULT; stdcall;
    function get_Size(out ppSizeInBytes: IDANumber): HRESULT; stdcall;
  end;

  IDAStatics = interface(IDispatch)
    ['{542FB452-5003-11cf-92A2-00AA00B8A733}']
    function get_VersionString(var str: TBSTR): HRESULT; stdcall;
    function get_Site(out pSite: IDASite): HRESULT; stdcall;
    function put_Site(pSite: IDASite): HRESULT; stdcall;
    function put_ClientSite(pClientSite: IOleClientSite): HRESULT; stdcall;
    function get_ClientSite(out pClientSite: IOleClientSite): HRESULT; stdcall;
    function put_PixelConstructionMode(bMode: WordBool): HRESULT; stdcall;
    function get_PixelConstructionMode(var bMode: WordBool): HRESULT; stdcall;
    function TriggerEvent(event: IDAEvent; data: IDABehavior): HRESULT; stdcall;
    function NewDrawingSurface(out pds: IDADrawingSurface): HRESULT; stdcall;
    function ImportMovie(url: TBSTR; out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportMovieAsync(url: TBSTR; pImageStandIn: IDAImage; pSoundStandIn: IDASound;
        out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportImage(url: TBSTR; out ppImage: IDAImage): HRESULT; stdcall;
    function ImportImageAsync(url: TBSTR; pImageStandIn: IDAImage;
        out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportImageColorKey(url: TBSTR; colorKeyRed, colorKeyGreen, colorKeyBlue: Byte;
        out ppImage: IDAImage): HRESULT; stdcall;
    function ImportImageAsyncColorKey(url: TBSTR; pImageStandIn: IDAImage;
        colorKeyRed, colorKeyGreen, colorKeyBlue: Byte;
        out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportSound(url: TBSTR; out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportSoundAsync(url: TBSTR; pSoundStandIn: IDASound;
        out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportGeometry(url: TBSTR; out ppGeometry: IDAGeometry): HRESULT; stdcall;
    function ImportGeometryAsync(url: TBSTR; pGeoStandIn: IDAGeometry;
        out ppResult: IDAImportationResult): HRESULT; stdcall;
    function ImportDirectDrawSurface(dds: IDirectDrawSurface; updateEvent: IDAEvent;
        out ppImage: IDAImage): HRESULT; stdcall;
    function Cond(c, i, e: IDABoolean; out pCondBvr: IDABoolean): HRESULT; stdcall;
    function DAArrayEx(s: Longint; pBvrs: IDABehavior; out bvr: IDAArray): HRESULT; stdcall;
    function DAArray(bvrs: Variant; out bvr: IDAArray): HRESULT; stdcall;
    function DATupleEx(s: Longint; pBvrs: IDABehavior; out bvr: IDATuple): HRESULT; stdcall;
    function DATuple(bvrs: Variant; out bvr: IDATuple): HRESULT; stdcall;
    function ModifiableBehavior(orig: IDABehavior; out bvr: IDABehavior): HRESULT; stdcall;
    function UninitializedArray(typeTmp: IDAArray; out bvr: IDAArray): HRESULT; stdcall;
    function UninitializedTuple(typeTmp: IDATuple; out bvr: IDATuple): HRESULT; stdcall;
    function NumberBSplineEx(degree: Integer; numKnots: Longint; knots: IDANumber;
        numPts: Longint; ctrlPts: IDANumber; numWts: Longint; weights: IDANumber;
        evaluator: IDANumber; out bvr: IDANumber): HRESULT; stdcall;
    function NumberBSpline(degree: Integer; knots, CtrlPts, weights: Variant;
        evaluator: IDANumber; out bvr: IDANumber): HRESULT; stdcall;
    function Point2BSplineEx(degree: Integer; numKnots: Longint; knots: IDANumber;
        numPts: Longint; ctrlPts: IDAPoint2; numWts: Longint; weights: IDANumber;
        evaluator: IDANumber; out bvr: IDAPoint2): HRESULT; stdcall;
    function Point2BSpline(degree: Integer; knots, CtrlPts, weights: Variant;
        evaluator: IDANumber; out bvr: IDAPoint2): HRESULT; stdcall;
    function Point3BSplineEx(degree: Integer; numKnots: Longint; knots: IDANumber;
        numPts: Longint; ctrlPts: IDAPoint3; numWts: Longint; weights: IDANumber;
        evaluator: IDANumber; out bvr: IDAPoint3): HRESULT; stdcall;
    function Point3BSpline(degree: Integer; knots, CtrlPts, weights: Variant;
        evaluator: IDANumber; out bvr: IDAPoint3): HRESULT; stdcall;
    function Vector2BSplineEx(degree: Integer; numKnots: Longint; knots: IDANumber;
        numPts: Longint; ctrlPts: IDAVector2; numWts: Longint; weights: IDANumber;
        evaluator: IDANumber; out bvr: IDAVector2): HRESULT; stdcall;
    function Vector2BSpline(degree: Integer; knots, CtrlPts, weights: Variant;
        evaluator: IDANumber; out bvr: IDAVector2): HRESULT; stdcall;
    function Vector3BSplineEx(degree: Integer; numKnots: Longint; knots: IDANumber;
        numPts: Longint; ctrlPts: IDAVector3; numWts: Longint; weights: IDANumber;
        evaluator: IDANumber; out bvr: IDAVector3): HRESULT; stdcall;
    function Vector3BSpline(degree: Integer; knots, CtrlPts, weights: Variant;
        evaluator: IDANumber; out bvr: IDAVector3): HRESULT; stdcall;
    function Pow(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Abs(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Sqrt(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Floor(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Round(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Ceiling(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Asin(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Acos(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Atan(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Sin(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Cos(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Tan(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Exp(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Ln(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Log10(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function ToDegrees(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function ToRadians(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Mod_(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Atan2(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Add(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Sub(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Mul(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function Div_(a_0, b_1: IDANumber; out ret_2: IDANumber): HRESULT; stdcall;
    function LT(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function LTE(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function GT(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function GTE(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function EQ(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function NE(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function Neg(a_0, b_1: IDANumber; out ret_2: IDABoolean): HRESULT; stdcall;
    function InterpolateAnim(from_0, to_1, duration_2: IDANumber; out ret_3: IDANumber): HRESULT; stdcall;
    function Interpolate(from_0, to_1, duration_2: double; out ret_3: IDANumber): HRESULT; stdcall;
    function SlowInSlowOutAnim(from_0, to_1, duration_2, sharpness_3: IDANumber; out ret_3: IDANumber): HRESULT; stdcall;
    function SlowInSlowOut(from_0, to_1, duration_2, sharpness_3: double; out ret_3: IDANumber): HRESULT; stdcall;
    function SoundSource(snd_0: IDASound; out ret_1: IDAGeometry): HRESULT; stdcall;
    function Mix(left_0, right_1: IDASound; out ret_2: IDASound): HRESULT; stdcall;
    function And_(a_0, b_1: IDABoolean; out ret_2: IDABoolean): HRESULT; stdcall;
    function Or_(a_0, b_1: IDABoolean; out ret_2: IDABoolean): HRESULT; stdcall;
    function Not_(a_0: IDABoolean; out ret_1: IDABoolean): HRESULT; stdcall;
    function Integral(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function Derivative(a_0: IDANumber; out ret_1: IDANumber): HRESULT; stdcall;
    function IntegralVector2(v_0: IDAVector2; out ret_1: IDAVector2): HRESULT; stdcall;
    function IntegralVector3(v_0: IDAVector3; out ret_1: IDAVector3): HRESULT; stdcall;
    function DerivativeVector2(v_0: IDAVector2; out ret_1: IDAVector2): HRESULT; stdcall;
    function DerivativeVector3(v_0: IDAVector3; out ret_1: IDAVector3): HRESULT; stdcall;
    function DerivativePoint2(v_0: IDAVector2; out ret_1: IDAVector2): HRESULT; stdcall;
    function DerivativePoint3(v_0: IDAVector3; out ret_1: IDAVector3): HRESULT; stdcall;
    function KeyState(n_0: IDANumber; out ret_1: IDABoolean): HRESULT; stdcall;
    function KeyUp(arg_0: Longint; out ret_1: IDAEvent): HRESULT; stdcall;
    function KeyDown(arg_0: Longint; out ret_1: IDAEvent): HRESULT; stdcall;
    function DANumber(num_0: Longint; out ret_1: IDANumber): HRESULT; stdcall;
    function DAString(str_0: TBSTR; out ret_1: IDAString): HRESULT; stdcall;
    function DABoolean(num_0: WordBool; out ret_1: IDABoolean): HRESULT; stdcall;
    function SeededRandom(arg_0: double; out ret_1: IDANumber): HRESULT; stdcall;
    function get_MousePosition(out ret_0: IDAPoint2): HRESULT; stdcall;
    function get_LeftButtonState(out ret_0: IDABoolean): HRESULT; stdcall;
    function get_RightButtonState(out ret_0: IDABoolean): HRESULT; stdcall;
    function get_DATrue(out ret_0: IDABoolean): HRESULT; stdcall;
    function get_DAFalse(out ret_0: IDABoolean): HRESULT; stdcall;
    function get_LocalTime(out ret_0: IDANumber): HRESULT; stdcall;
    function get_GlobalTime(out ret_0: IDANumber): HRESULT; stdcall;
    function get_Pixel(out ret_0: IDANumber): HRESULT; stdcall;
    function UserData(data_0: IUnknown; out ret_1: IDAUserData): HRESULT; stdcall;
    function UntilNotify(b0_0: IDABehavior; event_1: IDAEvent; notifier_2: IDAUntilNotifier;
        out ret_3: IDABehavior): HRESULT; stdcall;
    function Until_(b0_0: IDABehavior; event_1: IDAEvent; notifier_2: IDAUntilNotifier;
        out ret_3: IDABehavior): HRESULT; stdcall;
    function UntilEx(b0_0: IDABehavior; event_1: IDAEvent; out ret_2: IDABehavior): HRESULT; stdcall;
    function Sequence(s1_0, s2_1: IDABehavior; out ret_2: IDABehavior): HRESULT; stdcall;
    function FollowPath(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngle(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngleUpright(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathEval(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngleEval(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngleUprightEval(path_0: IDAPath2; duration_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAnim(obsoleted1_0: IDAPath2; obsoleted2_1: IDANumber; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngleAnim(obsoleted1_0: IDAPath2; obsoleted2_1: IDANumber; out ret_2: IDATransform2): HRESULT; stdcall;
    function FollowPathAngleUprightAnim(obsoleted1_0: IDAPath2; obsoleted2_1: IDANumber; out ret_2: IDATransform2): HRESULT; stdcall;
    function ConcatString(s1_0, s2_1: IDAString; out ret_2: IDAString): HRESULT; stdcall;
    function PerspectiveCamera(focalDist_0, nearClip_1: double; out ret_2: IDACamera): HRESULT; stdcall;
    function PerspectiveCameraAnim(focalDist_0, nearClip_1: IDANumber; out ret_2: IDACamera): HRESULT; stdcall;
    function ParallelCamera(nearClip_0: double; out ret_1: IDACamera): HRESULT; stdcall;
    function ParallelCameraAnim(nearClip_0: IDANumber; out ret_1: IDACamera): HRESULT; stdcall;
    function ColorRgbAnim(red_0, green_1, blue_2: IDANumber; out ret_3: IDAColor): HRESULT; stdcall;
    function ColorRgb(red_0, green_1, blue_2: double; out ret_3: IDAColor): HRESULT; stdcall;
    function ColorRgb255(red_0, green_1, blue_2: SmallInt; out ret_3: IDAColor): HRESULT; stdcall;
    function ColorHsl(hue_0, saturation_1, lum_2: double; out ret_3: IDAColor): HRESULT; stdcall;
    function ColorHslAnim(hue_0, saturation_1, lum_2: IDANumber; out ret_3: IDAColor): HRESULT; stdcall;
    function get_Red(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Green(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Blue(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Cyan(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Magenta(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Yellow(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Black(out ret_0: IDAColor): HRESULT; stdcall;
    function get_White(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Aqua(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Fuchsia(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Gray(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Lime(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Maroon(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Navy(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Olive(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Purple(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Silver(out ret_0: IDAColor): HRESULT; stdcall;
    function get_Teal(out ret_0: IDAColor): HRESULT; stdcall;
    function Predicate(b_0: IDABoolean; out ret_1: IDAEvent): HRESULT; stdcall;
    function NotEvent(event_0: IDAEvent; out ret_1: IDAEvent): HRESULT; stdcall;
    function AndEvent(e1_0, e2_1: IDAEvent; out ret_2: IDAEvent): HRESULT; stdcall;
    function OrEvent(e1_0, e2_1: IDAEvent; out ret_2: IDAEvent): HRESULT; stdcall;
    function ThenEvent(e1_0, e2_1: IDAEvent; out ret_2: IDAEvent): HRESULT; stdcall;
    function get_LeftButtonDown(out ret_0: IDAEvent): HRESULT; stdcall;
    function get_LeftButtonUp(out ret_0: IDAEvent): HRESULT; stdcall;
    function get_RightButtonDown(out ret_0: IDAEvent): HRESULT; stdcall;
    function get_RightButtonUp(out ret_0: IDAEvent): HRESULT; stdcall;
    function get_Always(out ret_0: IDAEvent): HRESULT; stdcall;
    function get_Never(out ret_0: IDAEvent): HRESULT; stdcall;
    function TimerAnim(n_0: IDANumber; out ret_1: IDAEvent): HRESULT; stdcall;
    function Timer(n_0: double; out ret_1: IDAEvent): HRESULT; stdcall;
    function AppTriggeredEvent(out ret_0: IDAEvent): HRESULT; stdcall;
    function ScriptCallback(obsolete1_0: TBSTR; obsolete2_1: IDAEvent; obsolete3_2: TBSTR; out ret_3: IDAEvent): HRESULT; stdcall;
    function get_EmptyGeometry(out ret_0: IDAGeometry): HRESULT; stdcall;
    function UnionGeometry(g1_0, g2_1: IDAGeometry; out ret_2: IDAGeometry): HRESULT; stdcall;
    function UnionGeometryArrayEx(imgs_0size: Longint; imgs_0: IDAGeometry; out ret_1: IDAGeometry): HRESULT; stdcall;
    function UnionGeometryArray(imgs_0: Variant; out ret_1: IDAGeometry): HRESULT; stdcall;
    function get_EmptyImage(out ret_0: IDAImage): HRESULT; stdcall;
    function get_DetectableEmptyImage(out ret_0: IDAImage): HRESULT; stdcall;
    function SolidColorImage(col_0: IDAColor; out ret_1: IDAImage): HRESULT; stdcall;
    function GradientPolygonEx(points_0size: Longint; points_0: IDAPoint2;
        colors_1size: Longint; colors_1: IDAColor; out ret_2: IDAImage): HRESULT; stdcall;
    function GradientPolygon(points_0, colors_1: Variant; out ret_2: IDAImage): HRESULT; stdcall;
    function RadialGradientPolygonEx(inner_0, outer_1: IDAColor; points_2size: Longint;
        points_2 : IDAPoint2; fallOff_3: double; out ret_4: IDAImage): HRESULT; stdcall;
    function RadialGradientPolygon(inner_0, outer_1: IDAColor; points_2: Variant; fallOff_3: double;
        out ret_4: IDAImage): HRESULT; stdcall;
    function RadialGradientPolygonAnimEx(inner_0, outer_1: IDAColor; points_2size: Longint; points_2: IDAPoint2; fallOff_3: double;
        out ret_4: IDAImage): HRESULT; stdcall;
    function RadialGradientPolygonAnim(inner_0, outer_1: IDAColor; points_2: Variant; fallOff_3: double;
        out ret_4: IDAImage): HRESULT; stdcall;
    function GradientSquare(lowerLeft_0, upperLeft_1, upperRight_2, lowerRight_3: IDAColor;
        out ret_4: IDAImage): HRESULT; stdcall;
    function RadialGradientSquare(inner_0, outer_1: IDAColor; fallOff_2: double; out ret_3: IDAImage): HRESULT; stdcall;
    function RadialGradientSquareAnim(inner_0, outer_1: IDAColor; fallOff_2: IDANumber; out ret_3: IDAImage): HRESULT; stdcall;
    function RadialGradientRegularPoly(inner_0, outer_1: IDAColor; numEdges_2, fallOff_3: double; out ret_4: IDAImage): HRESULT; stdcall;
    function RadialGradientRegularPolyAnim(inner_0, outer_1: IDAColor; numEdges_2, fallOff_3: IDANumber; out ret_4: IDAImage): HRESULT; stdcall;
    function GradientHorizontal(start_0, stop_1: IDAColor; fallOff_2: double; out ret_3: IDAImage): HRESULT; stdcall;
    function GradientHorizontalAnim(start_0, stop_1: IDAColor; fallOff_2: IDANumber; out ret_3: IDAImage): HRESULT; stdcall;
    function HatchHorizontal(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchHorizontalAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchVertical(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchVerticalAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchForwardDiagonal(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchForwardDiagonalAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchBackwardDiagonal(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchBackwardDiagonalAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchCross(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchCrossAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchDiagonalCross(lineClr_0: IDAColor; spacing_1: double; out ret_2: IDAImage): HRESULT; stdcall;
    function HatchDiagonalCrossAnim(lineClr_0: IDAColor; spacing_1: IDANumber; out ret_2: IDAImage): HRESULT; stdcall;
    function Overlay(top_0, bottom_1: IDAImage; out ret_2: IDAImage): HRESULT; stdcall;
    function OverlayArrayEx(imgs_0size: Longint; imgs_0: IDAImage; out ret_1: IDAImage): HRESULT; stdcall;
    function OverlayArray(imgs_0: Variant; ret_1: IDAImage): HRESULT; stdcall;
    function get_AmbientLight(out ret_0: IDAGeometry): HRESULT; stdcall;
    function get_DirectionalLight(out ret_0: IDAGeometry): HRESULT; stdcall;
    function get_PointLight(out ret_0: IDAGeometry): HRESULT; stdcall;
    function SpotLightAnim(fullcone_0, cutoff_1: IDANumber; out ret_2: IDAGeometry): HRESULT; stdcall;
    function SpotLight(fullcone_0: IDANumber; cutoff_1: double; out ret_2: IDAGeometry): HRESULT; stdcall;
    function get_DefaultLineStyle(out ret_0: IDALineStyle): HRESULT; stdcall;
    function get_EmptyLineStyle(out ret_0: IDALineStyle): HRESULT; stdcall;
    function get_JoinStyleBevel(out ret_0: IDAJoinStyle): HRESULT; stdcall;
    function get_JoinStyleRound(out ret_0: IDAJoinStyle): HRESULT; stdcall;
    function get_JoinStyleMiter(out ret_0: IDAJoinStyle): HRESULT; stdcall;
    function get_EndStyleFlat(out ret_0: IDAEndStyle): HRESULT; stdcall;
    function get_EndStyleSquare(out ret_0: IDAEndStyle): HRESULT; stdcall;
    function get_EndStyleRound(out ret_0: IDAEndStyle): HRESULT; stdcall;
    function get_DashStyleSolid(out ret_0: IDADashStyle): HRESULT; stdcall;
    function get_DashStyleDashed(out ret_0: IDADashStyle): HRESULT; stdcall;
    function get_DefaultMicrophone(out ret_0: IDAMicrophone): HRESULT; stdcall;
    function get_OpaqueMatte(out ret_0: IDAMatte): HRESULT; stdcall;
    function get_ClearMatte(out ret_0: IDAMatte): HRESULT; stdcall;
    function UnionMatte(m1_0, m2_1: IDAMatte; out ret_2: IDAMatte): HRESULT; stdcall;
    function IntersectMatte(m1_0, m2_1: IDAMatte; out ret_2: IDAMatte): HRESULT; stdcall;
    function DifferenceMatte(m1_0, m2_1: IDAMatte; out ret_2: IDAMatte): HRESULT; stdcall;
    function FillMatte(p_0: IDAPath2; out ret_1: IDAMatte): HRESULT; stdcall;
    function TextMatte(str_0: IDAString; fs_1: IDAFontStyle; out ret_2: IDAMatte): HRESULT; stdcall;
    function get_EmptyMontage(out ret_0: IDAMontage): HRESULT; stdcall;
    function ImageMontage(im_0: IDAImage; depth_1: double; out ret_2: IDAMontage): HRESULT; stdcall;
    function ImageMontageAnim(im_0: IDAImage; depth_1: IDANumber; out ret_2: IDAMontage): HRESULT; stdcall;
    function UnionMontage(m1_0, m2_1: IDAMontage; out ret_2: IDAMontage): HRESULT; stdcall;
    function Concat(p1_0, p2_1: IDAPath2; out ret_2: IDAPath2): HRESULT; stdcall;
    function ConcatArrayEx(paths_0size: Longint; paths_0: IDAPath2; out ret_1: IDAPath2): HRESULT; stdcall;
    function ConcatArray(paths_0: Variant; out ret_1: IDAPath2): HRESULT; stdcall;
    function Line(p1_0, p2_1: IDAPoint2; out ret_2: IDAPath2): HRESULT; stdcall;
    function Ray(pt_0: IDAPoint2; out ret_1: IDAPath2): HRESULT; stdcall;
    function StringPathAnim(str_0: IDAString; fs_1: IDAFontStyle; out ret_2: IDAPath2): HRESULT; stdcall;
    function StringPath(str_0: TBSTR; fs_1: IDAFontStyle; out ret_2: IDAPath2): HRESULT; stdcall;
    function PolylineEx(points_0size: Longint; points_0: IDAPoint2; out ret_1: IDAPath2): HRESULT; stdcall;
    function Polyline(points_0: Variant; out ret_1: IDAPath2): HRESULT; stdcall;
    function PolydrawPathEx(points_0size: Longint; points_0: IDAPoint2; codes_1size: Longint; codes_1: IDANumber; out ret_2: IDAPath2): HRESULT; stdcall;
    function PolydrawPath(points_0, codes_1: Variant; out ret_2: IDAPath2): HRESULT; stdcall;
    function ArcRadians(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: double; out ret_4: IDAPath2): HRESULT; stdcall;
    function ArcRadiansAnim(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: IDANumber; out ret_4: IDAPath2): HRESULT; stdcall;
    function ArcDegrees(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: double; out ret_4: IDAPath2): HRESULT; stdcall;
    function PieRadians(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: double; out ret_4: IDAPath2): HRESULT; stdcall;
    function PieRadiansAnim(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: IDANumber; out ret_4: IDAPath2): HRESULT; stdcall;
    function PieDegrees(startAngle_0, endAngle_1, arcWidth_2, arcHeight_3: double; out ret_4: IDAPath2): HRESULT; stdcall;
    function Oval(width_0, height_1: double; out ret_2: IDAPath2): HRESULT; stdcall;
    function OvalAnim(width_0, height_1: IDANumber; out ret_2: IDAPath2): HRESULT; stdcall;
    function Rect(width_0, height_1: double; out ret_2: IDAPath2): HRESULT; stdcall;
    function RectAnim(width_0, height_1: IDANumber; out ret_2: IDAPath2): HRESULT; stdcall;
    function RoundRect(width_0, height_1, cornerArcWidth_2, cornerArcHeight_3: double; out ret_4: IDAPath2): HRESULT; stdcall;
    function RoundRectAnim(width_0, height_1, cornerArcWidth_2, cornerArcHeight_3: IDANumber; out ret_4: IDAPath2): HRESULT; stdcall;
    function CubicBSplinePathEx(points_0size: Longint; points_0: IDAPoint2; knots_1size: Longint; knots_1: IDANumber; out ret_2: IDAPath2): HRESULT; stdcall;
    function CubicBSplinePath(points_0, knots_1: Variant; out ret_2: IDAPath2): HRESULT; stdcall;
    function TextPath(obsolete1_0: IDAString; obsolete2_1: IDAFontStyle; out ret_2: IDAPath2): HRESULT; stdcall;
    function get_Silence(out ret_0: IDASound): HRESULT; stdcall;
    function MixArrayEx(snds_0size: Longint; snds_0: IDASound; out ret_1: IDASound): HRESULT; stdcall;
    function MixArray(snds_0: Variant; out ret_1: IDASound): HRESULT; stdcall;
    function get_SinSynth(out ret_0: IDASound): HRESULT; stdcall;
    function get_DefaultFont(out ret_0: IDAFontStyle): HRESULT; stdcall;
    function FontAnim(str_0: IDAString; size_1: IDANumber; col_2: IDAColor; out ret_3: IDAFontStyle): HRESULT; stdcall;
    function Font(str_0: TBSTR; size_1: double; col_2: IDAColor; out ret_3: IDAFontStyle): HRESULT; stdcall;
    function StringImageAnim(str_0: IDAString; fs_1: IDAFontStyle; out ret_2: IDAImage): HRESULT; stdcall;
    function StringImage(str_0: TBSTR; fs_1: IDAFontStyle; out ret_2: IDAImage): HRESULT; stdcall;
    function TextImageAnim(obsoleted1_0: IDAString; obsoleted2_1: IDAFontStyle; out ret_2: IDAImage): HRESULT; stdcall;
    function TextImage(obsoleted1_0: TBSTR; obsoleted2_1: IDAFontStyle; out ret_2: IDAImage): HRESULT; stdcall;
    function get_XVector2(out ret_0: IDAVector2): HRESULT; stdcall;
    function get_YVector2(out ret_0: IDAVector2): HRESULT; stdcall;
    function get_ZeroVector2(out ret_0: IDAVector2): HRESULT; stdcall;
    function get_Origin2(out ret_0: IDAVector2): HRESULT; stdcall;
    function Vector2Anim(x_0, y_1: IDANumber; out ret_2: IDAVector2): HRESULT; stdcall;
    function Vector2(x_0, y_1: double; out ret_2: IDAVector2): HRESULT; stdcall;
    function Point2Anim(x_0, y_1: IDANumber; out ret_2: IDAPoint2): HRESULT; stdcall;
    function Point2(x_0, y_1: double; out ret_2: IDAPoint2): HRESULT; stdcall;
    function Vector2PolarAnim(theta_0, radius_1: IDANumber; out ret_2: IDAVector2): HRESULT; stdcall;
    function Vector2Polar(theta_0, radius_1: double; out ret_2: IDAVector2): HRESULT; stdcall;
    function Vector2PolarDegrees(theta_0, radius_1: double; out ret_2: IDAVector2): HRESULT; stdcall;
    function Point2PolarAnim(theta_0, radius_1: IDANumber; out ret_2: IDAVector2): HRESULT; stdcall;
    function Point2Polar(theta_0, radius_1: double; out ret_2: IDAPoint2): HRESULT; stdcall;
    function DotVector2(v_0, u_1: IDAVector2; out ret_2: IDANumber): HRESULT; stdcall;
    function NegVector2(v_0: IDAVector2; out ret_1: IDAVector2): HRESULT; stdcall;
    function SubVector2(v1_0, v2_1: IDAVector2; out ret_2: IDAVector2): HRESULT; stdcall;
    function AddVector2(v1_0, v2_1: IDAVector2; out ret_2: IDAVector2): HRESULT; stdcall;
    function AddPoint2Vector(p_0: IDAPoint2; v2_1: IDAVector2; out ret_2: IDAPoint2): HRESULT; stdcall;
    function SubPoint2Vector(p_0: IDAPoint2; v2_1: IDAVector2; out ret_2: IDAPoint2): HRESULT; stdcall;
    function SubPoint2(p_0, p_1: IDAPoint2; out ret_2: IDAVector2): HRESULT; stdcall;
    function DistancePoint2(p_0, q_1: IDAPoint2; out ret_2: IDANumber): HRESULT; stdcall;
    function DistanceSquaredPoint2(p_0, q_1: IDAPoint2; out ret_2: IDANumber): HRESULT; stdcall;
    function get_XVector3(out ret_0: IDAVector3): HRESULT; stdcall;
    function get_YVector3(out ret_0: IDAVector3): HRESULT; stdcall;
    function get_ZVector3(out ret_0: IDAVector3): HRESULT; stdcall;
    function get_ZeroVector3(out ret_0: IDAVector3): HRESULT; stdcall;
    function get_Origin3(out ret_0: IDAPoint3): HRESULT; stdcall;
    function Vector3Anim(x_0, y_1, z_2: IDANumber; out ret_3: IDAVector3): HRESULT; stdcall;
    function Vector3(x_0, y_1, z_2: double; out ret_3: IDAVector3): HRESULT; stdcall;
    function Point3Anim(x_0, y_1, z_2: IDANumber; out ret_3: IDAPoint3): HRESULT; stdcall;
    function Point3(x_0, y_1, z_2: double; out ret_3: IDAPoint3): HRESULT; stdcall;
    function Vector3SphericalAnim(xyAngle_0, yzAngle_1, radius_2: IDANumber; out ret_3: IDAVector3): HRESULT; stdcall;
    function Vector3Spherical(xyAngle_0, yzAngle_1, radius_2: double; out ret_3: IDAVector3): HRESULT; stdcall;
    function Point3SphericalAnim(xyAngle_0, yzAngle_1, radius_2: IDANumber; out ret_3: IDAPoint3): HRESULT; stdcall;
    function Point3Spherical(xyAngle_0, yzAngle_1, radius_2: double; out ret_3: IDAPoint3): HRESULT; stdcall;
    function DotVector3(v_0, u_1: IDAVector3; out ret_2: IDANumber): HRESULT; stdcall;
    function CrossVector3(v_0, u_1: IDAVector3; out ret_2: IDAVector3): HRESULT; stdcall;
    function NegVector3(v_0: IDAVector3; out ret_1: IDAVector3): HRESULT; stdcall;
    function SubVector3(v1_0, v2_1: IDAVector3; out ret_2: IDAVector3): HRESULT; stdcall;
    function AddVector3(v1_0, v2_1: IDAVector3; out ret_2: IDAVector3): HRESULT; stdcall;
    function AddPoint3Vector(p_0: IDAPoint3; v_1: IDAVector3; out ret_2: IDAPoint3): HRESULT; stdcall;
    function SubPoint3Vector(p_0: IDAPoint3; v_1: IDAVector3; out ret_2: IDAPoint3): HRESULT; stdcall;
    function SubPoint3(p1_0, p2_1: IDAPoint3; out ret_2: IDAVector3): HRESULT; stdcall;
    function DistancePoint3(p1_0, p2_1: IDAPoint3; out ret_2: IDANumber): HRESULT; stdcall;
    function DistanceSquaredPoint3(p_0, q_1: IDAPoint3; out ret_2: IDANumber): HRESULT; stdcall;
    function get_IdentityTransform3(out ret_0: IDATransform3): HRESULT; stdcall;
    function Translate3Anim(tx_0, ty_1, tz_2: IDANumber; out ret_3: IDATransform3): HRESULT; stdcall;
    function Translate3(tx_0, ty_1, tz_2: double; out ret_3: IDATransform3): HRESULT; stdcall;
    function Translate3Rate(tx_0, ty_1, tz_2: double; out ret_3: IDATransform3): HRESULT; stdcall;
    function Translate3Vector(delta_0: IDAVector3; out ret_1: IDATransform3): HRESULT; stdcall;
    function Translate3Point(new_origin_0: IDAPoint3; out ret_1: IDATransform3): HRESULT; stdcall;
    function Scale3Anim(x_0, y_1, z_2: IDANumber; out ret_3: IDATransform3): HRESULT; stdcall;
    function Scale3(x_0, y_1, z_2: double; out ret_3: IDATransform3): HRESULT; stdcall;
    function Scale3Rate(x_0, y_1, z_2: double; out ret_3: IDATransform3): HRESULT; stdcall;
    function Scale3Vector(scale_vec_0: IDAVector3; out ret_1: IDATransform3): HRESULT; stdcall;
    function Scale3UniformAnim(uniform_scale_0: IDANumber; out ret_1: IDATransform3): HRESULT; stdcall;
    function Scale3Uniform(uniform_scale_0: double; out ret_1: IDATransform3): HRESULT; stdcall;
    function Scale3UniformRate(uniform_scale_0: double; out ret_1: IDATransform3): HRESULT; stdcall;
    function Rotate3Anim(axis_0: IDAVector3; angle_1: IDANumber; out ret_2: IDATransform3): HRESULT; stdcall;
    function Rotate3(axis_0: IDAVector3; angle_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function Rotate3Rate(axis_0: IDAVector3; angle_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function Rotate3Degrees(axis_0: IDAVector3; angle_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function Rotate3RateDegrees(axis_0: IDAVector3; angle_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function XShear3Anim(a_0, b_1: IDANumber; out ret_2: IDATransform3): HRESULT; stdcall;
    function XShear3(a_0, b_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function XShear3Rate(a_0, b_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function YShear3Anim(a_0, b_1: IDANumber; out ret_2: IDATransform3): HRESULT; stdcall;
    function YShear3(c_0, d_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function YShear3Rate(c_0, d_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function ZShear3Anim(e_0, f_1: IDANumber; out ret_2: IDATransform3): HRESULT; stdcall;
    function ZShear3(e_0, f_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function ZShear3Rate(e_0, f_1: double; out ret_2: IDATransform3): HRESULT; stdcall;
    function Transform4x4AnimEx(m_0size: Longint; m_0: IDANumber; out ret_1: IDATransform3): HRESULT; stdcall;
    function Transform4x4Anim(m_0: Variant; out ret_1: IDATransform3): HRESULT; stdcall;
    function Compose3(a_0, b_1: IDATransform3; out ret_2: IDATransform3): HRESULT; stdcall;
    function Compose3ArrayEx(xfs_0size: Longint; xfs_0: IDATransform3; out ret_1: IDATransform3): HRESULT; stdcall;
    function Compose3Array(xfs_0: Variant; out ret_1: IDATransform3): HRESULT; stdcall;
    function LookAtFrom(to_0, from_1: IDAPoint3; up_2: IDAVector3; out ret_3: IDATransform3): HRESULT; stdcall;
    function get_IdentityTransform2(out ret_0: IDATransform2): HRESULT; stdcall;
    function Translate2Anim(Tx_0, Ty_1: IDANumber; out ret_2: IDATransform2): HRESULT; stdcall;
    function Translate2(Tx_0, Ty_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function Translate2Rate(Tx_0, Ty_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function Translate2Vector(delta_0: IDAVector2; out ret_1: IDATransform2): HRESULT; stdcall;
    function Translate2Point(pos_0: IDAPoint2; out ret_1: IDATransform2): HRESULT; stdcall;
    function Scale2Anim(x_0, y_1: IDANumber; out ret_2: IDATransform2): HRESULT; stdcall;
    function Scale2(x_0, y_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function Scale2Rate(x_0, y_1: double; out ret_2: IDATransform2): HRESULT; stdcall;
    function Scale2Vector2(obsoleteMethod_0: IDAVector2; out ret_1: IDATransform2): HRESULT; stdcall;
    function Scale2Vector(scale_vec_0: IDAVector2; out ret_1: IDATransform2): HRESULT; stdcall;
    function Scale2UniformAnim(uniform_scale_0: IDANumber; out ret_1: IDATransform2): HRESULT; stdcall;
    function Scale2Uniform(uniform_scale_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Scale2UniformRate(uniform_scale_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Rotate2Anim(angle_0: IDANumber; out ret_1: IDATransform2): HRESULT; stdcall;
    function Rotate2(angle_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Rotate2Rate(angle_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Rotate2Degrees(angle_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Rotate2RateDegrees(angle_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function XShear2Anim(arg_0: IDANumber; out ret_1: IDATransform2): HRESULT; stdcall;
    function XShear2(arg_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function XShear2Rate(arg_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function YShear2Anim(arg_0: IDANumber; out ret_1: IDATransform2): HRESULT; stdcall;
    function YShear2(arg_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function YShear2Rate(arg_0: double; out ret_1: IDATransform2): HRESULT; stdcall;
    function Transform3x2AnimEx(m_0size: Longint; m_0: IDANumber; out ret_1: IDATransform2): HRESULT; stdcall;
    function Transform3x2Anim(m_0: Variant; out ret_1: IDATransform2): HRESULT; stdcall;
    function Compose2(a_0, b_1: IDATransform2; out ret_2: IDATransform2): HRESULT; stdcall;
    function Compose2ArrayEx(xfs_0size: Longint; xfs_0: IDATransform2; out ret_1: IDATransform2): HRESULT; stdcall;
    function Compose2Array(xfs_0: Variant; out ret_1: IDATransform2): HRESULT; stdcall;
    function Tuple(obsolete1: Variant; out obsolete2: IDATuple): HRESULT; stdcall;
    function Array_(obsolete1: Variant; out obsolete2: IDAArray): HRESULT; stdcall;
    function get_AreBlockingImportsComplete(var bComplete: WordBool): HRESULT; stdcall;
  end;

  IDAViewerControl = interface(IDispatch)
    ['{0E41257B-812D-11D0-9B4A-00C04FC2F51D}']
    function get_UpdateInterval(var pVal: double): HRESULT; stdcall;
    function put_UpdateInterval(newVal: double): HRESULT; stdcall;
    function get_View(out ppView: IDAView): HRESULT; stdcall;
    function put_View(pView: IDAView): HRESULT; stdcall;
    function get_Image(out ppImage: IDAImage): HRESULT; stdcall;
    function put_Image(pImage: IDAImage): HRESULT; stdcall;
    function get_BackgroundImage(out ppImage: IDAImage): HRESULT; stdcall;
    function put_BackgroundImage(pImage: IDAImage): HRESULT; stdcall;
    function get_Sound(out ppSound: IDASound): HRESULT; stdcall;
    function put_Sound(pSound: IDASound): HRESULT; stdcall;
    function get_PixelLibrary(out ppStatics: IDAStatics): HRESULT; stdcall;
    function get_MeterLibrary(out ppStatics: IDAStatics): HRESULT; stdcall;
    function AddBehaviorToRun(pBehavior: IDABehavior): HRESULT; stdcall;
    function Start: HRESULT; stdcall;
    function get_InputImage(out pVal: IDAImage): HRESULT; stdcall;
    function get_OpaqueForHitDetect(var b: WordBool): HRESULT; stdcall;
    function put_OpaqueForHitDetect(b: WordBool): HRESULT; stdcall;
    function get_TimerSource(var ts: TDA_Timer_Source): HRESULT; stdcall;
    function put_TimerSource(ts: TDA_Timer_Source): HRESULT; stdcall;
  end;

  IDAViewerControlWindowed = interface(IDAViewerControl)
    ['{BA8B033E-1E91-11d1-8809-00C04FC29D46}']
  end;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       effect.h
 *
 ***************************************************************************)

const
  CATID_BitmapEffect: TGUID = (D1:$1F9DDD20;D2:$4146;D3:$11D0;D4:($BD,$C2,$00,$A0,$C9,$08,$DB,$96));
  IID_IBitmapEffect: TGUID = (D1:$ACEA25C0;D2:$415B;D3:$11D0;D4:($BD,$C2,$00,$A0,$C9,$08,$DB,$96));

const
  BITMAP_EFFECT_INPLACE             = $00000001;
  BITMAP_EFFECT_REALTIME            = $00000002;
  BITMAP_EFFECT_DIRECTDRAW          = $00000004;
  BITMAP_EFFECT_SUPPORTS_INVALIDATE = $00000008;

type
  IBitmapEffect = interface(IUnknown)
    function SetSite(pUnk: IUnknown): HRESULT; stdcall;
    function GetMiscStatusBits(var pdwFlags: DWORD): HRESULT; stdcall;
    function GetSupportedFormatsCount(var pcFormats: DWORD): HRESULT; stdcall;
    function GetSupportedFormats(var pcFormats: DWORD;
        var pdwColorDepths: DWORD): HRESULT; stdcall;
    function Begin_(dwColorDepth: DWORD; var psizeEffect: TSize): HRESULT; stdcall;
    function End_: HRESULT; stdcall;
    function DoEffect(pbsIn, pbsOut: IDirectDrawSurface;
        const prcFull, prcInvalid: TRect): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       ddrawex.h
 *
 ***************************************************************************)

const
  CLSID_IDirectDrawFactory: TGUID = (D1:$4FD2A832;D2:$86C8;D3:$11D0;D4:($8F,$CA,$00,$C0,$4F,$D9,$18,$9D));
  IID_IDirectDraw3: TGUID = (D1:$618F8AD4;D2:$8B7A;D3:$11D0;D4:($8F,$CC,$00,$C0,$4F,$D9,$18,$9D));
  IID_IDirectDrawFactory: TGUID = (D1:$4FD2A833;D2:$86C8;D3:$11D0;D4:($8F,$CA,$00,$C0,$4F,$D9,$18,$9D));

type

{ IDirectDraw3 Interface }

  IDirectDraw3 = interface(IUnknown)
    ['{618F8AD4-8B7A-11D0-8FCC-00C04FD9189D}']
    (*** IDirectDraw methods ***)
    function Compact: HRESULT; stdcall;
    function CreateClipper(dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function CreatePalette(dwFlags: DWORD; lpColorTable: PPaletteEntry;
        out lplpDDPalette: IDirectDrawPalette; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function CreateSurface(const lpDDSurfaceDesc: TDDSurfaceDesc;
        out lplpDDSurface: IDirectDrawSurface; pUnkOuter: IUnknown): HRESULT;
        stdcall;
    function DuplicateSurface(lpDDSurface: IDirectDrawSurface;
        out lplpDupDDSurface: IDirectDrawSurface): HRESULT; stdcall;
    function EnumDisplayModes(dwFlags: DWORD;
        const lpDDSurfaceDesc: TDDSurfaceDesc; lpContext: Pointer;
        var EnumModesCallback: TDDEnumModesCallback): HRESULT; stdcall;
    function EnumSurfaces(dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc;
        lpContext: Pointer; var EnumCallback: TDDEnumSurfacesCallback): HRESULT;
        stdcall;
    function FlipToGDISurface: HRESULT; stdcall;
    function GetCaps(var lpDDDriverCaps: TDDCaps; var lpDDHELCaps: TDDCaps):
        HRESULT; stdcall;
    function GetDisplayMode(var lpDDSurfaceDesc: TDDSurfaceDesc): HRESULT;
        stdcall;
    function GetFourCCCodes(var lpNumCodes, lpCodes: DWORD): HRESULT; stdcall;
    function GetGDISurface(out lplpGDIDDSSurface: IDirectDrawSurface): HRESULT;
        stdcall;
    function GetMonitorFrequency(var lpdwFrequency: DWORD): HRESULT; stdcall;
    function GetScanLine(var lpdwScanLine: DWORD): HRESULT; stdcall;
    function GetVerticalBlankStatus(var lpbIsInVB: BOOL): HRESULT; stdcall;
    function Initialize(lpGUID: PGUID): HRESULT; stdcall;
    function RestoreDisplayMode: HRESULT; stdcall;
    function SetCooperativeLevel(hWnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
    function SetDisplayMode(dwWidth, dwHeight, dwBPP, dwRefreshRate: DWORD;
        dwFlags: DWORD): HRESULT; stdcall;
    function WaitForVerticalBlank(dwFlags: DWORD; hEvent: THandle): HRESULT;
        stdcall;
    (*** IDirectDraw2 methods ***)
    function GetAvailableVidMem(var lpDDSCaps: TDDSCaps;
        var lpdwTotal, lpdwFree: DWORD): HRESULT; stdcall;
    (*** IDirectDraw3 methods ***)
    function GetSurfaceFromDC(hdc: HDC; out ppSurface: IDirectDrawSurface): HRESULT;
        stdcall;
  end;

{ IDirectDrawFactory Interface }

  IDirectDrawFactory = interface(IUnknown)
    ['{4FD2A833-86C8-11D0-8FCA-00C04FD9189D}']
    (*** IDirectDrawFactory methods ***)
    function CreateDirectDraw(pGUID: PGUID; hWnd: HWND;
        dwCoopLevelFlags: DWORD; dwReserved: DWORD; pUnkOuter: IUnknown;
        out ppDirectDraw: IDirectDraw): HRESULT; stdcall;
    function DirectDrawEnumerate(var Callback: TDDEnumCallback;
        lpContext: Pointer): HRESULT; stdcall;
  end;

const
  DDSD_LPSURFACE          = $00000800;     // from ddrawex.h

  DDSCAPS_DATAEXCHANGE    = DDSCAPS_SYSTEMMEMORY or DDSCAPS_VIDEOMEMORY;

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       ocmm.h
 *
 ***************************************************************************)

const
  IID_IViewFilter: TGUID = '{3050F2F1-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_IViewTransition: TGUID = '{3050F372-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_IViewFilterSite: TGUID = '{3050F2F4-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_IViewTransitionSite: TGUID = '{3050F373-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_ITimerService: TGUID = '{3050F35F-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_ITimer: TGUID ='{3050F360-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_ITimerSink: TGUID = '{3050F361-98B5-11CF-BB82-00AA00BDCE0B}';
  IID_IMapMIMEToCLSID: TGUID = '{D9E89500-30FA-11D0-B724-00AA006C1A01}';
  IID_IImageDecodeFilter: TGUID = '{A3CCEDF3-2DE2-11D0-86F4-00A0C913F750}';
  IID_IImageDecodeEventSink: TGUID = '{BAA342A0-2DED-11D0-86F4-00A0C913F750}';

const
// GetStatusBits Flags
  FILTER_STATUS_OPAQUE    = $000000001;
  FILTER_STATUS_INVISIBLE = $000000002;
  FILTER_STATUS_SURFACE   = $000000004;
  FILTER_STATUS_3DSURFACE = $000000008;

type
  IViewFilterSite = interface;

  IViewFilter = interface(IUnknown)
    ['{3050F2F1-98B5-11CF-BB82-00AA00BDCE0B}']
    function SetSource(pFilter: IViewFilter): HRESULT; stdcall;
    function GetSource(out ppFilter: IViewFilter): HRESULT; stdcall;
    function SetSite(pSink: IViewFilterSite): HRESULT; stdcall;
    function GetSite(out ppSink: IViewFilterSite): HRESULT; stdcall;
    function SetPosition(const prc: TRect): HRESULT; stdcall;
    function Draw(hdc: HDC; const prcBounds: TRect): HRESULT; stdcall;
    function GetStatusBits(var pdwFlags: DWORD): HRESULT; stdcall;
  end;

  IViewTransition = interface(IViewFilter)
    ['{3050F372-98B5-11CF-BB82-00AA00BDCE0B}']
    function Initialize(hdc: HDC; const prc: TRect): HRESULT; stdcall;
    function Go(vtimeDuration: VARIANT): HRESULT; stdcall;
  end;

  IViewFilterSite = interface(IUnknown)
    ['{3050F2F4-98B5-11CF-BB82-00AA00BDCE0B}']
    function GetDC(const prc: TRect; dwFlags: DWORD; var phdc: HDC): HRESULT; stdcall;
    function ReleaseDC(hdc: HDC): HRESULT; stdcall;
    function InvalidateRect(const prc: TRect; fErase: BOOL): HRESULT; stdcall;
    function InvalidateRgn(hrgn: HRGN; fErase: BOOL): HRESULT; stdcall;
    function OnStatusBitsChange(dwFlags: DWORD): HRESULT; stdcall;
  end;

  IViewTransitionSite = interface(IViewFilterSite)
    ['{3050F373-98B5-11CF-BB82-00AA00BDCE0B}']
    function OnComplete: HRESULT; stdcall;
  end;

const
  SURFACE_LOCK_EXCLUSIVE        = $01;
  SURFACE_LOCK_ALLOW_DISCARD    = $02;
  SURFACE_LOCK_WAIT             = $04;

  E_SURFACE_NOSURFACE             = $8000C000;
  E_SURFACE_UNKNOWN_FORMAT        = $8000C001;
  E_SURFACE_NOTMYPOINTER          = $8000C002;
  E_SURFACE_DISCARDED             = $8000C003;
  E_SURFACE_NODC                  = $8000C004;
  E_SURFACE_NOTMYDC               = $8000C005;
  S_SURFACE_DISCARDED             = $0000C003;

type
  BFID = TGUID;
{
EXTERN_C const GUID BFID_MONOCHROME;
EXTERN_C const GUID BFID_RGB_4;
EXTERN_C const GUID BFID_RGB_8;
EXTERN_C const GUID BFID_RGB_555;
EXTERN_C const GUID BFID_RGB_565;
EXTERN_C const GUID BFID_RGB_24;
EXTERN_C const GUID BFID_RGB_32;
EXTERN_C const GUID BFID_RGBA_32;
EXTERN_C const GUID BFID_GRAY_8;
EXTERN_C const GUID BFID_GRAY_16;
}

const
  SID_SDirectDraw3: TGUID = (D1:$618F8AD4;D2:$8B7A;D3:$11D0;D4:($8F,$CC,$00,$C0,$4F,$D9,$18,$9D));

  COLOR_NO_TRANSPARENT = $FFFFFFFF;

  IMGDECODE_EVENT_PROGRESS = $01;
  IMGDECODE_EVENT_PALETTE = $02;
  IMGDECODE_EVENT_BEGINBITS = $04;
  IMGDECODE_EVENT_BITSCOMPLETE = $08;
  IMGDECODE_EVENT_USEDDRAW = $10;

  IMGDECODE_HINT_TOPDOWN = $01;
  IMGDECODE_HINT_BOTTOMUP = $02;
  IMGDECODE_HINT_FULLWIDTH = $04;

  MAPMIME_DEFAULT = 0;
  MAPMIME_CLSID  = 1;
  MAPMIME_DISABLE = 2;
  MAPMIME_DEFAULT_ALWAYS = 3;

{
  BFID_INDEXED_RGB_8 = BFID_RGB_8;
  BFID_INDEXED_RGB_4 = BFID_RGB_4;
  BFID_INDEXED_RGB_1 = BFID_MONOCHROME;

EXTERN_C const GUID CLSID_IImageDecodeFilter;

EXTERN_C const GUID NAMEDTIMER_DRAW;
}

type
  ITimer = interface;
  ITimerSink = interface;
  IImageDecodeEventSink = interface;

  ITimerService = interface(IUnknown)
    ['{3050F35F-98B5-11CF-BB82-00AA00BDCE0B}']
    function CreateTimer(pReferenceTimer: ITimer; out ppNewTimer: ITimer): HRESULT; stdcall;
    function GetNamedTimer(const rguidName: TGUID; out ppTimer: ITimer): HRESULT; stdcall;
    function SetNamedTimerReference(const rguidName: TGUID; pReferenceTimer: ITimer): HRESULT; stdcall;
  end;

  ITimer = interface(IUnknown)
    ['{3050F360-98B5-11CF-BB82-00AA00BDCE0B}']
    function Advise(vtimeMin, vtimeMax, vtimeInterval: Variant; dwFlags: DWORD;
        pTimerSink: ITimerSink; var pdwCookie: DWORD): HRESULT; stdcall;
    function Unadvise(dwCookie: DWORD): HRESULT; stdcall;
    function Freeze(fFreeze: BOOL): HRESULT; stdcall;
    function GetTime(var pvtime: Variant): HRESULT; stdcall;
  end;

  ITimerSink = interface(IUnknown)
    ['{3050F361-98B5-11CF-BB82-00AA00BDCE0B}']
    function OnTimer(vtimeAdvise: Variant): HRESULT; stdcall;
  end;

  IMapMIMEToCLSID = interface(IUnknown)
    ['{D9E89500-30FA-11D0-B724-00AA006C1A01}']
    function EnableDefaultMappings(bEnable: BOOL): HRESULT; stdcall;
    function MapMIMEToCLSID(pszMIMEType: PWCHAR; var pCLSID: TGUID): HRESULT; stdcall;
    function SetMapping(pszMIMEType: PWCHAR; dwMapMode: DWORD;
        const clsid: TGUID): HRESULT; stdcall;
  end;

  IImageDecodeFilter = interface(IUnknown)
    ['{A3CCEDF3-2DE2-11D0-86F4-00A0C913F750}']
    function Initialize(pEventSink: IImageDecodeEventSink): HRESULT; stdcall;
    function Process(pStream: IStream): HRESULT; stdcall;
    function Terminate(hrStatus: HRESULT): HRESULT; stdcall;
  end;

  IImageDecodeEventSink = interface(IUnknown)
    ['{BAA342A0-2DED-11D0-86F4-00A0C913F750}']
    function GetSurface(nWidth, nHeight: Longint; const bfid: TGUID;
        nPasses: ULONG; dwHints: DWORD; out ppSurface: IUnknown): HRESULT; stdcall;
    function OnBeginDecode(var pdwEvents: DWORD; var pnFormats: ULONG;
        var ppFormats: BFID): HRESULT; stdcall;
    function OnBitsComplete: HRESULT; stdcall;
    function OnDecodeComplete(hrStatus: HRESULT): HRESULT; stdcall;
    function OnPalette: HRESULT; stdcall;
    function OnProgress(const pBounds: TRect; bComplete: BOOL): HRESULT; stdcall;
  end;

implementation

end.


