unit ObjectInspectorHandleViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PDTabControl, InfoBook, VisualControls, SHDocVw, CustomWebBrowser,
  VoyagerInterfaces, VoyagerServerInterfaces, ObjectInspectorInterfaces,
  StdCtrls, VisualClassesHandler, InternationalizerComponent, VCLUtils;

const
  tidObjectInspectorSection = 'InspectorInfo';
  tidTabCount               = 'TabCount';
  tidTabName                = 'TabName';
  tidTabHandler             = 'TabHandler';

const
  CacheConnectionTimeOut = 1*60*1000;
  MSConnectionTimeOut    = 1*60*1000;
  RDOProxyTimeOut        = 1*60*1000;

const
  htmlAction_VisitWebSite = 'VISITWEBSITE';
  tidParmName_Access      = 'Access';
  urlAction_FindSuppliers = 'FINDSUPPLIERS';
  urlAction_FindClients   = 'FINDCLIENTS';

const
  MaxSheetHandlers = 128;
  noSheet          = -1;

const
  DefLinkCount = 20;

type
  TPropertySheetHandlerArray = array[0..MaxSheetHandlers-1] of IPropertySheetHandler;

type
  TObjectInspectorHandlerViewer = class(TVisualControl)
    IECompPanel: TPanel;
    Panel1: TPanel;
    Tabs: TPDTabControl;
    BottomLine: TShape;
    RightLine: TShape;
    Pages: TInfoBook;
    KeepAlive: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure TabsTabChange(Sender: TObject);
    procedure KeepAliveTimer(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    fIEControl          : TCustomWebBrowser;
    fPropSheetContInit  : IPropertySheetContainerHandlerInit;
    fPropSheetContainer : IPropertySheetContainerHandler;
    fWorldURL           : string;
    fClassId            : integer;
    fMasterURLHandler   : IMasterURLHandler;
    fLastHandlers       : TStringList;
    fSameClass          : boolean;
    fURLHadler          : IURLHandler;
    fExposed            : boolean;
    fLastSheet          : string;
  public
    procedure ChangeObject(ClientView : IClientView; xPos, yPos, ClassId, ObjectId : integer);
    function  AddSheet(Name, SheetHandler : string) : integer;
    procedure BeginUpdateTabs;
    procedure EndUpdateTabs;
    procedure ShowImage(WorldName : string; xPos, yPos : integer);
    procedure AddControlTabs;
    function  GetValue(ppSection, ppName : string; aType : TValueType) : TVisualClassItem;
    procedure Exposed;
    procedure Unexposed;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    property URLHadler          : IURLHandler read fURLHadler write fURLHadler;
    property MasterURLHandler   : IMasterURLHandler read fMasterURLHandler write fMasterURLHandler;
    property PropSheetContainer : IPropertySheetContainerHandler read fPropSheetContainer;
  public
    procedure InitSheetContainer(ClientView : IClientView);
  end;

var
  ObjectInspectorHandlerViewer: TObjectInspectorHandlerViewer;

implementation

  uses
    Collection, ActiveX, SheetHandlerRegistry, SyncObjs, CacheCommon,
    RDOObjectProxy, RDOInterfaces, WinsockRDOConnection, ServerCnxHandler, URLParser,
    ObjectInspectorHandler, MathUtils, Events, CompStringsParser, SheetUtils, Threads,
    tabNamesMLS,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ServerCnxEvents;

  {$R *.DFM}

  const
    DefViewerHeight = 82;

  type
    TObjectInspectorContainer = class;

    TObjectInspectorContainer =
      class(TInterfacedObject, IPropertySheetContainerHandlerInit, IPropertySheetContainerHandler)
        public
          constructor Create(aControl : TObjectInspectorHandlerViewer);
          destructor  Destroy; override;
        private
          fControl    : TObjectInspectorHandlerViewer;
          fClientView : IClientView;
          fClassId    : integer;
          fObjectId   : integer;
          fXPos       : integer;
          fYPos       : integer;
          fSheets     : TPropertySheetHandlerArray;
          fSheetCount : integer;
          fLock       : TCriticalSection;
          fCacheConn  : IRDOConnectionInit;
          fMSConn     : IRDOConnectionInit;
          fCacheSrv   : OleVariant;
          fCacheObj   : OleVariant;
          fMSObj      : OleVariant;
          fObjChanged : boolean;
          fCurSheet   : integer;
          fWaitCounter : integer;
        private
          procedure OnCacheDisconnect(const Connection : IRDOConnection);
          procedure OnMSDisconnect(const Connection : IRDOConnection);
        protected
          //procedure ThreadTerminate(Sender : TObject);
          function  GetCacheConnection : IRDOConnectionInit;
          function  GetMSConnection : IRDOConnectionInit;
          //procedure TerminateThreads;
          procedure ReleaseCacheObject;
        private
          procedure SetClientView(ClientView : IClientView);
          //procedure SetClassId(ClassId : integer);
          //procedure SetObjectId(ObjectId : integer);
          procedure SetObjectData(ClassId, ObjectId : integer);
          procedure SetXPos(xPos : integer);
          procedure SetYPos(yPos : integer);
          procedure ClearSheets(release : boolean);
          procedure StartNavigator;
          procedure StopNavigator;
        private
          procedure Lock;
          procedure UnLock;
          function  GetClientView : IClientView;
          function  GetClassId : integer;
          function  GetObjectId : integer;
          function  GetXPos : integer;
          function  GetYPos : integer;
          //function  GetMinHeight : integer;
          //procedure ChangeHeight(delta : integer);
          //procedure ChangeWidth(delta : integer);
          function  GetProperties(Names : TStringList) : TStringList;
          function  GetPropertyList (Proxy : OleVariant; Names, Results : TStringList) : boolean;
          function  GetPropertyArray(Proxy : OleVariant; Names : array of string; Results : TStringList) : boolean;
          function  SheetCount : integer;
          procedure FocusSheet(index : integer);
          procedure Refresh;
          function  GetFocusedSheet : IPropertySheetHandler;
          function  GetCacheServerProxy : OleVariant;
          function  CreateCacheObjectProxy : OleVariant;
          function  GetCacheObjectProxy : OleVariant;
          function  GetMSProxy : OleVariant;
          function  HandleURL(URL : TURL; incoming : boolean) : TURLHandlingResult;
        private
          fCacheSheetNames : TStringList;
          fCachedSheets    : TPropertySheetHandlerArray;
        private
          function  AddSheet(Handler : string) : integer;
          function  GetCachedSheet(name : string; var sheet : IPropertySheetHandler) : integer;
          procedure KeepAlive;
          function  GetCacheDir : string;
          procedure ClearConnections; // NEW!
        private
          //procedure threadedDisconnect( const parms : array of const );
          procedure threadedFindSuppliers( const parms : array of const );
          procedure threadedRenderSuppliers( const parms : array of const );
          procedure threadedFindClients( const parms : array of const );
          procedure threadedRenderClients( const parms : array of const );
          procedure ResetWait;
          procedure StartWait;
          procedure StopWait;
      end;


  // TObjectInspectorContainer

  constructor TObjectInspectorContainer.Create(aControl : TObjectInspectorHandlerViewer);
    begin
      inherited Create;
      fControl  := aControl;
      fLock     := TCriticalSection.Create;
      fCurSheet := noSheet;
      fCacheSheetNames := TStringList.Create;
    end;

  destructor TObjectInspectorContainer.Destroy;
    begin
      ClearSheets(true);
      ReleaseCacheObject;
      fCacheSheetNames.Free;
      inherited;
    end;

  procedure TObjectInspectorContainer.Lock;
    begin
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TObjectInspectorContainer.UnLock;
    begin
      if fLock <> nil
        then fLock.Leave;
    end;

  procedure TObjectInspectorContainer.OnCacheDisconnect(const Connection : IRDOConnection);
    begin
      fCacheConn := nil;
      fCacheSrv  := Unassigned;
      fCacheObj  := Unassigned;
    end;

  procedure TObjectInspectorContainer.OnMSDisconnect(const Connection : IRDOConnection);
    begin
      fMSConn := nil;
      fMSObj  := Unassigned;
    end;

  function TObjectInspectorContainer.GetCacheConnection : IRDOConnectionInit;
    var
      Connection : TWinSockRDOConnection;
    begin
      if (fCacheConn = nil) or not (fCacheConn as IRDOConnection).Alive
        then
          begin
            Connection := TWinSockRDOConnection.Create('Cache Server');
            fCacheConn := Connection;
            fCacheConn.Server := fClientView.getCacheAddr;
            fCacheConn.Port   := fClientView.getCachePort;
            (fCacheConn as IRDOConnection).OnDisconnect := OnCacheDisconnect;
          end;
      if fCacheConn.Connect(CacheConnectionTimeOut)
        then result := fCacheConn
        else
          begin
            result := nil;
            fCacheConn := nil;
          end;
      result := fCacheConn;
    end;

  function TObjectInspectorContainer.GetMSConnection : IRDOConnectionInit;
    begin
      if (fMSConn = nil) or not (fMSConn as IRDOConnection).Alive
        then
          begin
            fMSConn        := TWinSockRDOConnection.Create('Model Server');
            fMSConn.Server := fClientView.getDAAddr;
            fMSConn.Port   := fClientView.getDALockPort;
            (fMSConn as IRDOConnection).OnDisconnect := OnMSDisconnect;
            if fMSConn.Connect(MSConnectionTimeOut)
              then result := fMSConn
              else
                begin
                  result := nil;
                  fMSConn := nil;
                end;
          end
        else result := fMSConn;
    end;

  procedure TObjectInspectorContainer.ReleaseCacheObject;
    begin
      try
        if not VarIsEmpty(fCacheSrv) and not VarIsEmpty(fCacheObj)
          then fCacheSrv.CloseObject(fCacheObj.RemoteObjectID);
      except
      end;
    end;

  procedure TObjectInspectorContainer.SetClientView(ClientView : IClientView);
    begin
      fClientView := ClientView;
    end;

  {procedure TObjectInspectorContainer.SetClassId(ClassId : integer);
    begin
      fObjChanged := fClassId <> ClassId;
      fClassId    := ClassId;
    end;

  procedure TObjectInspectorContainer.SetObjectId(ObjectId : integer);
    begin
      fObjChanged := fObjectId <> ObjectId;
      fObjectId   := ObjectId;
    end;}

  procedure TObjectInspectorContainer.SetObjectData(ClassId, ObjectId : integer);
    begin
      fObjChanged := (fClassId <> ClassId) or (fObjectId <> ObjectId);
      fClassId  := ClassId;
      fObjectId := ObjectId;
    end;

  procedure TObjectInspectorContainer.SetXPos(xPos : integer);
    begin
      fXPos := xPos;
    end;

  procedure TObjectInspectorContainer.SetYPos(yPos : integer);
    begin
      fYPos := yPos;
    end;

  procedure TObjectInspectorContainer.ClearSheets(release : boolean);
    var
      i : integer;
    begin
      for i := 0 to pred(fSheetCount) do
        begin
          if fSheets[i].Exposed
            then fSheets[i].Clear;
          if release
            then fSheets[i] := nil;
        end;
      if release
        then fSheetCount := 0;
   end;

  function TObjectInspectorContainer.GetClientView : IClientView;
    begin
      result := fClientView;
    end;

  function TObjectInspectorContainer.GetClassId : integer;
    begin
      result := fClassId;
    end;

  function TObjectInspectorContainer.GetObjectId : integer;
    begin
      result := fObjectId;
    end;

  function TObjectInspectorContainer.GetXPos : integer;
    begin
      result := fXPos;
    end;

  function TObjectInspectorContainer.GetYPos : integer;
    begin
      result := fYPos;
    end;

  procedure TObjectInspectorContainer.StartNavigator;
    var
      TaskStartInfo : TEvnTaskStartInfo;
    begin
      TaskStartInfo.Carrier  := fControl.URLHadler;
      TaskStartInfo.TaskName := 'CachePropertyReader';
      TaskStartInfo.TaskDesc := 'Gathering Facilty Info';
      fControl.MasterURLHandler.HandleEvent( evnTaskStart, TaskStartInfo );
    end;

  procedure TObjectInspectorContainer.StopNavigator;
    var
      TaskEndInfo : TEvnTaskStartInfo;
    begin
      TaskEndInfo.Carrier  := fControl.URLHadler;
      TaskEndInfo.TaskName := 'CachePropertyReader';
      fControl.MasterURLHandler.HandleEvent( evnTaskEnd, TaskEndInfo );
    end;

  function TObjectInspectorContainer.GetProperties(Names : TStringList) : TStringList;
    var
      Proxy : OleVariant;
    begin
      result := TStringList.Create;
      try
        //Lock;
        StartNavigator;
        try
          Proxy := GetCacheObjectProxy;
          if not VarIsEmpty(Proxy) and (Names.Count > 0)
            then SheetUtils.GetProperties(Proxy, Names, Result);
        finally
          StopNavigator;
          //Unlock;
        end;
      except
      end;
    end;

  function TObjectInspectorContainer.GetPropertyList(Proxy : OleVariant; Names, Results : TStringList) : boolean;
    begin
      try
        //Lock;
        StartNavigator;
        try
          if not VarIsEmpty(Proxy) and (Names.Count > 0)
            then result := SheetUtils.GetProperties(Proxy, Names, Results)
            else result := false;
        finally
          StopNavigator;
          //Unlock;
        end;
      except
        result := false;
      end;
    end;

  function TObjectInspectorContainer.GetPropertyArray(Proxy : OleVariant; Names : array of string; Results : TStringList) : boolean;
    begin
      try
        //Lock;
        StartNavigator;
        try
          if not VarIsEmpty(Proxy)
            then result := SheetUtils.GetPropertyArray(Proxy, Names, Results)
            else result := false;
        finally
          StopNavigator;
          //Unlock;
        end;
      except
        result := false;
      end;
    end;

  function TObjectInspectorContainer.SheetCount : integer;
    begin
      result := fSheetCount;
    end;

  procedure TObjectInspectorContainer.Refresh;
    var
      Sheet : IPropertySheetHandler;
      i     : integer;
    begin
      if not VarIsEmpty(fCacheObj)
        then
          begin
            fCacheObj.WaitForAnswer := true;
            fCacheObj.Refresh;
            fCacheObj.WaitForAnswer := false;
          end;
      // Clear all the loaded forms
      for i := 0 to pred(fSheetCount) do
        if (fSheets[i] <> nil) and fSheets[i].Loaded
          then fSheets[i].Clear;
      // Refocus the focused sheet
      Sheet := GetFocusedSheet;
      if Sheet <> nil
        then Sheet.Refresh;
      ResetWait;  
    end;

  function TObjectInspectorContainer.GetFocusedSheet : IPropertySheetHandler;
    begin
      if fCurSheet <> noSheet
        then result := fSheets[fCurSheet]
        else result := nil;
    end;

  procedure TObjectInspectorContainer.FocusSheet(index : integer);
    begin
      if (index >= 0) and (index < fSheetCount)
        then
          begin
            if (fCurSheet >= 0) and (fSheets[fCurSheet] <> nil)
              then fSheets[fCurSheet].LostFocus;
            fSheets[index].SetFocus;
            fCurSheet  := index;
          end
        else fCurSheet := noSheet;
    end;

  function TObjectInspectorContainer.GetCacheServerProxy : OleVariant;
    var
      Connection : IRDOConnectionInit;
      tmpProxy   : OleVariant;
    begin
      Lock;
      try
        if VarIsEmpty(fCacheSrv)
          then
            begin
              Connection := GetCacheConnection;
              if Connection <> nil
                then
                  try
                    tmpProxy := TRDOObjectProxy.Create as IDispatch;
                    tmpProxy.SetConnection(Connection);
                    tmpProxy.Timeout := RDOProxyTimeOut;
                    tmpProxy.BindTo(WSObjectCacherName);
                    fCacheSrv := tmpProxy;
                  except
                    fCacheSrv := Unassigned;
                  end;
            end;
        finally
          Unlock;
          result := fCacheSrv;
        end;
    end;

  function TObjectInspectorContainer.CreateCacheObjectProxy : OleVariant;
    var
      Connection : IRDOConnectionInit;
      srvProxy   : OleVariant;
      tmpProxy   : OleVariant;
      WStr       : WideString;
      NewObj     : integer;
    begin
      //Lock;
      try
        srvProxy := GetCacheServerProxy;
        if not VarIsEmpty(srvProxy)
          then
            try
              Connection := GetCacheConnection;
              WStr := fClientView.getWorldName;
              NewObj := srvProxy.CreateObject(WStr);
              if NewObj <> 0
                then
                  try
                    tmpProxy := TRDOObjectProxy.Create as IDispatch;
                    tmpProxy.SetConnection(Connection);
                    tmpProxy.Timeout := RDOProxyTimeOut;
                    tmpProxy.BindTo(NewObj);
                    result := tmpProxy;
                  except
                    result := Unassigned;
                  end;
            except
              result := Unassigned;
            end
          else result := Unassigned;
      finally
        //Unlock;
      end;
    end;

  function TObjectInspectorContainer.GetCacheObjectProxy : OleVariant;
    var
      tmpProxy : OleVariant;
      currObj  : integer;
    begin
      try
        currObj := fObjectId;
        //Lock;
        try
          if VarIsEmpty(fCacheObj)
            then
              begin
                tmpProxy := CreateCacheObjectProxy;
                if not VarIsEmpty(tmpProxy)
                  then
                    begin
                      fCacheObj := tmpProxy;
                      if tmpProxy.SetObject(fXPos, fYPos)
                        then
                          begin
                            result := tmpProxy;
                            fObjChanged := false;
                          end
                        else result := Unassigned;
                    end
                  else result := Unassigned;
              end
            else
              if fObjChanged
                then
                  if fCacheObj.SetObject(fXPos, fYPos)
                    then
                      begin
                        fObjChanged := currObj <> fObjectId;
                        result := fCacheObj;
                      end
                    else result := Unassigned
              else result := fCacheObj;
        finally
          //Unlock;
        end;
      except
        result    := Unassigned;
        fCacheObj := Unassigned;
      end;
    end;

  function TObjectInspectorContainer.GetMSProxy : OleVariant;
    var
      Connection : IRDOConnectionInit;
      tmpProxy   : OleVariant;
    begin
      Lock;
      try
        if VarIsEmpty(fMSObj)
          then
            begin
              Connection := GetMSConnection;
              if Connection <> nil
                then
                  begin
                    tmpProxy := TRDOObjectProxy.Create as IDispatch;
                    tmpProxy.SetConnection(Connection);
                    tmpProxy.Timeout := RDOProxyTimeOut;
                    if tmpProxy.BindTo('World')
                      then tmpProxy.RDOLogonClient(fClientView.getUserName, fClientView.getUserPassword);
                    tmpProxy.BindTo(fObjectId);
                    fMSObj := tmpProxy;
                    result := fMSObj;
                  end
                else result := Unassigned;
            end
          else
            begin
              fMSObj.BindTo(fObjectId);
              result := fMSObj;
            end;
      finally
        Unlock;
      end;
    end;

  function TObjectInspectorContainer.HandleURL(URL : TURL; incoming : boolean) : TURLHandlingResult;
    var
      NewUrl : string;
      Action : string;
      Output : string;
      Town   : string;
      Name   : string;
      count  : integer;
      Roles  : integer;
      Frame  : string;
    begin
      case incoming of
        false:
          begin
            Frame  := GetParmValue(URL, 'frame_Id');
            if Frame <> tidHandlerName_ObjInspector
              then result := fControl.MasterURLHandler.HandleURL(URL)
              else
                begin
                  Action := GetURLAction(URL);
                  if Action = htmlAction_VisitWebSite
                    then
                      begin
                        NewUrl := fClientView.getWorldURL +
                          'Visual/Clusters/WebLoader.asp?Page=Home&x=' + IntToStr(fXPos) +
                          '&y=' + IntToStr(fYPos) +
                          '&WorldName=' + fClientView.getWorldName +
                          '&DAAddr=' + fClientView.getDAAddr +
                          '&DAPort=' + IntToStr(fClientView.getDALockPort) +
                          '&frame_Id=FacilitySiteView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client';
                          if GetParmValue(URL, tidParmName_Access) <> ''
                            then NewUrl := NewUrl + '&Access=MODIFY';
                          URL := NewURL + '::' + '?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=YES';
                        result := fControl.MasterURLHandler.HandleURL(URL);
                      end
                    else
                      if Action = urlAction_FindSuppliers
                        then
                          begin
                            Output := GetParmValue(URL, 'Fluid');
                            Town   := GetParmValue(URL, 'Town');
                            Name   := GetParmValue(URL, 'Owner');
                            try
                              count  := StrToInt(GetParmValue(URL, 'Count'));
                            except
                              count  := DefLinkCount;
                            end;
                            try
                              Roles := StrToInt(GetParmValue(URL, 'Roles'));
                            except
                              Roles := 1;
                            end;
                            Threads.Fork(threadedFindSuppliers, priNormal, [Output, Town, Name, count, Roles]);
                            result := urlHandled;
                          end
                        else
                          if Action = urlAction_FindClients
                            then
                              begin
                                Output := GetParmValue(URL, 'Fluid');
                                Town   := GetParmValue(URL, 'Town');
                                Name   := GetParmValue(URL, 'Owner');
                                try
                                  count  := StrToInt(GetParmValue(URL, 'Count'));
                                except
                                  count  := DefLinkCount;
                                end;
                                try
                                  Roles := StrToInt(GetParmValue(URL, 'Roles'));
                                except
                                  Roles := 1;
                                end;
                                Threads.Fork(threadedFindClients, priNormal, [Output, Town, Name, count, Roles]);
                                result := urlHandled;
                              end
                            else result := fControl.MasterURLHandler.HandleURL(URL);
                end;
          end;
        true:
          begin
            if fCurSheet <> noSheet
              then result := fSheets[fCurSheet].HandleURL(URL)
              else result := urlNotHandled;
          end;
        else result := urlNotHandled;
      end;
    end;

  function TObjectInspectorContainer.AddSheet(Handler : string) : integer;
    var
      Shdl  : IPropertySheetHandler;
      Ctrl  : TControl;
      Page  : TInfoBookPage;
      index : integer;
    begin
      try
        index := GetCachedSheet(Handler, Shdl);
        if Shdl = nil
          then
            begin
              Shdl := SheetHandlerRegistry.GetSheetHandler(Handler);
              if Shdl <> nil
                then
                  begin
                    Shdl.SetContainer(Self);

                    Ctrl := Shdl.CreateControl(fControl);
                    Page := fControl.Pages.AddPage;
                    Page.ParentColor := true;
                    Ctrl.Parent := Page;
                    Ctrl.Align := alClient;
                    Ctrl.Visible := true;

                    fCacheSheetNames.AddObject(Handler, Page);
                    fCachedSheets[pred(fCacheSheetNames.Count)] := Shdl;
                    fSheets[fSheetCount] := Shdl;
                    inc(fSheetCount);
                    result := pred(fCacheSheetNames.Count);
                  end
                else result := noSheet;
            end
          else
            begin
              fSheets[fSheetCount] := Shdl;
              inc(fSheetCount);
              result := index;
            end;
      except
        result := noSheet;
      end;
    end;

  function TObjectInspectorContainer.GetCachedSheet(name : string; var sheet : IPropertySheetHandler) : integer;
    var
      index : integer;
    begin
      try
        if fCacheSheetNames <> nil
          then
            begin
              index := fCacheSheetNames.IndexOf(UpperCase(name));
              if index >= 0
                then sheet := fCachedSheets[index]
                else sheet := nil;
              result := index;
            end
          else
            begin
              result := noSheet;
              sheet  := nil;
            end;
      except
        result := noSheet;
        sheet  := nil;
      end;
    end;

  procedure TObjectInspectorContainer.KeepAlive;
    begin
      if not VarIsEmpty(fCacheObj)
        then fCacheObj.KeepAlive;
    end;

  function TObjectInspectorContainer.GetCacheDir : string;
    begin
      fControl.MasterURLHandler.HandleEvent( evnAnswerPrivateCache, result );
    end;

  procedure TObjectInspectorContainer.ClearConnections; // NEW!
    begin
      {
      if fCacheConn <> nil
        then fCacheConn._AddRef;
      if fMSConn <> nil
        then fMSConn._AddRef;
      Fork( threadedDisconnect, priNormal, [pointer(fCacheConn), pointer(fMSConn)] );
      }
      if fCacheConn <> nil
        then
          begin
            //fCacheConn.Disconnect;
            fCacheConn := nil;
            fCacheSrv := Unassigned;
            fCacheObj := Unassigned;
          end;
      if fMSConn <> nil
        then
          begin
            //fMSConn.Disconnect;
            fMSConn := nil;
            fMSObj  := Unassigned;
          end;

    end;

  {
  procedure TObjectInspectorContainer.threadedDisconnect( const parms : array of const );
    var
      Cnx1, Cnx2 : IRDOConnectionInit;
    begin
      try
        Cnx1 := IRDOConnectionInit(parms[0].vPointer);
        Cnx2 := IRDOConnectionInit(parms[1].vPointer);
        if Cnx1 <> nil
          then
            begin
              Cnx1.Disconnect;
              Cnx1._Release;
            end;
        if Cnx2 <> nil
          then
            begin
              Cnx2.Disconnect;
              Cnx2._Release;
            end;
      except
      end;
    end;
  }

  procedure TObjectInspectorContainer.threadedFindSuppliers(const parms : array of const);
    var
      Proxy  : OleVariant;
      res    : string;
      Output : string;
      World  : string;
      Town   : string;
      Name   : string;
      count  : integer;
      Roles  : integer;
      msg    : TLinkSearchMsg;
    begin
      res := '';
      try
        Output := parms[0].VPChar;
        World  := fClientView.getWorldName;
        Town   := parms[1].VPChar;
        Name   := parms[2].VPChar;
        count  := parms[3].VInteger;
        Roles  := parms[4].VInteger;
        try
          msg.Fluid := Output;
          Proxy := GetCacheServerProxy;
          if not VarIsEmpty(Proxy)         //Ouput World town name count X Y SortMode Roles
            then res := Proxy.FindSuppliers(Output, World, Town, Name, count, fXPos, fYPos, 1, Roles)
            else res := '';
        finally
          msg.Result := res;
          Threads.Join(threadedRenderSuppliers, [@msg]);
        end;
      except
      end;
    end;

  procedure TObjectInspectorContainer.threadedRenderSuppliers(const parms : array of const);
    var
      pmsg : ^TLinkSearchMsg absolute parms[0];
    begin
      fControl.MasterURLHandler.HandleEvent(evnRenderInputSuppliers, pmsg^);
    end;

  procedure TObjectInspectorContainer.threadedFindClients( const parms : array of const );
    var
      Proxy  : OleVariant;
      res    : string;
      Output : string;
      World  : string;
      Town   : string;
      Name   : string;
      count  : integer;
      Roles  : integer;
      msg    : TLinkSearchMsg;
    begin
      res := '';
      try
        Output := parms[0].VPChar;
        World  := fClientView.getWorldName;
        Town   := parms[1].VPChar;
        Name   := parms[2].VPChar;
        count  := parms[3].VInteger;
        Roles  := parms[4].VInteger;
        try
          msg.Fluid := Output;
          Proxy := GetCacheServerProxy;
          if not VarIsEmpty(Proxy)
            then res := Proxy.FindClients(Output, World, Town, Name, count, fXPos, fYPos, 1, Roles)
            else res := '';
        finally
          msg.Result := res;
          Threads.Join(threadedRenderClients, [@msg]);
        end;
      except
      end;
    end;

  procedure TObjectInspectorContainer.threadedRenderClients( const parms : array of const );
    var
      pmsg : ^TLinkSearchMsg absolute parms[0];
    begin
      fControl.MasterURLHandler.HandleEvent(evnRenderOutputClients, pmsg^);
    end;

  procedure TObjectInspectorContainer.ResetWait;
    begin
      fWaitCounter := 0;
      fControl.Tabs.Enabled := true;
      fControl.Tabs.Cursor  := crDefault;
    end;

  procedure TObjectInspectorContainer.StartWait;
    begin
      inc(fWaitCounter);
      fControl.Tabs.Enabled := false;
      fControl.Tabs.Cursor  := crHourGlass;
    end;

  procedure TObjectInspectorContainer.StopWait;
    begin
      dec(fWaitCounter);
      if fWaitCounter <= 0
        then
          begin
            fWaitCounter := 0;
            fControl.Tabs.Enabled := true;
            fControl.Tabs.Cursor  := crDefault;
          end;
    end;


  // TObjectInspectorHandlerViewer

  procedure TObjectInspectorHandlerViewer.Loaded;
    begin
      inherited;
      fIEControl := TCustomWebBrowser.Create(Self);
      fIEControl.Width  := 0;
      fIEControl.Height := 0;
      IECompPanel.InsertControl(fIEControl);
      fIEControl.HideScrollBars := true;
      fIEControl.HideBorders    := true;
      fIEControl.Visible := false;
      fIEControl.Align := alClient;
    end;

  procedure TObjectInspectorHandlerViewer.ChangeObject(ClientView : IClientView; xPos, yPos, ClassId, ObjectId : integer);
    var
      Container : TObjectInspectorContainer;
    begin
      try
        fMasterURLHandler.HandleURL('?frame_Id=SupplyFinder&frame_Close=YES');
        fMasterURLHandler.HandleURL('?frame_Id=ClientFinder&frame_Close=YES');
        Cursor := crHourGlass;
        if fPropSheetContainer = nil
          then
            begin
              Container := TObjectInspectorContainer.Create(self);
              fPropSheetContInit  := Container;
              fPropSheetContainer := Container;
              fPropSheetContInit.SetClientView(ClientView);
            end;
        fWorldURL := ClientView.getWorldURL;
        fPropSheetContInit.SetObjectData(ClassId, ObjectId);
        //fPropSheetContInit.SetObjectId(ObjectId);
        //fPropSheetContInit.SetClassId(ClassId);
        fPropSheetContInit.SetXPos(xPos);
        fPropSheetContInit.SetYPos(yPos);
        fClassId := ClassId;
        // Update visual stuffs
        AddControlTabs;
        Refresh;
        //Tabs.CurrentTab := 0;
        ShowImage( ClientView.getWorldName, xPos, yPos );
      finally
        Cursor := crDefault;
      end;
    end;

  function TObjectInspectorHandlerViewer.AddSheet(Name, SheetHandler : string) : integer;
    begin
      result := fPropSheetContInit.AddSheet(SheetHandler);
      if result <> noSheet
        then Tabs.TabNames.AddObject(Name, TObject(result));
    end;

  procedure TObjectInspectorHandlerViewer.BeginUpdateTabs;
    begin
      Tabs.TabNames.Clear;
      Tabs.BeginUpdate;
    end;

  procedure TObjectInspectorHandlerViewer.EndUpdateTabs;
    begin
      Tabs.EndUpdate;
    end;

  procedure TObjectInspectorHandlerViewer.ShowImage(WorldName : string; xPos, yPos : integer);
    var
      flags   : olevariant;
      useless : olevariant;
    begin
      flags   := navNoHistory;
      useless := Null;
      fIEControl.Navigate(
        fWorldURL + '/Visual/Voyager/IsoMap/FacilityImage.asp' +
        '?ClassId=' + IntToStr(fClassId) +
        '&WorldName=' + WorldName +
        '&xPos=' + IntToStr(xPos) +
        '&yPos=' + IntToStr(yPos),
        flags, useless, useless, useless );
    end;

  procedure TObjectInspectorHandlerViewer.TabsTabChange(Sender: TObject);
    begin
      if Tabs.CurrentTab <> noTab
        then
          begin
            Pages.PageIndex := integer(Tabs.TabNames.Objects[Tabs.CurrentTab]);
            Pages.ActivePage.Visible := true;
            fPropSheetContainer.FocusSheet(Tabs.CurrentTab);
            fLastSheet := Tabs.TabNames[Tabs.CurrentTab];
          end;
    end;

  function TObjectInspectorHandlerViewer.GetValue(ppSection, ppName : string; aType : TValueType) : TVisualClassItem;
    begin
      System.Initialize(result);
      with result do
        begin
          ClassId     := fClassId;
          ValueName   := ppName;
          Section     := ppSection;
          ValueType   := aType;
        end;
      MasterURLHandler.HandleEvent(evnAnswerVisualClassData, result);
    end;

  procedure TObjectInspectorHandlerViewer.AddControlTabs;
    var
      count    : integer;
      i        : integer;
      name     : string;
      handler  : string;
      TabNames : TStringList;
      TabHdlrs : TStringList;
      equal    : boolean;
      eqCount  : integer;
      tabIdx   : integer;
    begin
      try
        LockWindowUpdate(Handle);
        try
          count    := GetValue(tidObjectInspectorSection, tidTabCount, vtInteger).IntValue;
          equal    := (fLastHandlers <> nil) and (count > 0);
          TabNames := TStringList.Create;
          TabHdlrs := TStringList.Create;
          eqCount  := 0;
          for i := 0 to pred(count) do
            begin
              name    := GetValue(tidObjectInspectorSection, tidTabName + IntToStr(i), vtString).StrValue;
              handler := GetValue(tidObjectInspectorSection, tidTabHandler + IntToStr(i), vtString).StrValue;
              TabNames.Add(tabNamesMLS.TranslateName(name));
              TabHdlrs.Add(uppercase(handler));
              if equal and (fLastHandlers.IndexOf(uppercase(handler)) <> -1)
                then inc(eqCount);
            end;

          fPropSheetContainer.ResetWait;

          equal := equal and (eqCount = count) and (fLastHandlers.Count = count);
          if not equal
            then
              begin
                fSameClass := false;
                BeginUpdateTabs;
                try
                  Tabs.CurrentTab := noTab;
                  fLastHandlers.Free;
                  fLastHandlers := TabHdlrs;
                  fPropSheetContInit.ClearSheets(true);
                  count  := 0;
                  tabIdx := 0;
                  for i := 0 to pred(TabHdlrs.Count) do
                    begin
                      if AddSheet(TabNames[i], TabHdlrs[i]) <> noTab
                        then
                          begin
                            inc(count);
                            if TabNames[i] = fLastSheet
                              then tabIdx := i;
                          end;
                    end;
                  if count > 0
                    then Tabs.CurrentTab := tabIdx
                    else
                      begin
                        Pages.ActivePage := nil;
                        MasterURLHandler.HandleURL( '?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes' );
                      end;
                finally
                  EndUpdateTabs;
                end;
              end
            else
              begin
                fSameClass := true;
                fPropSheetContInit.ClearSheets(false);
                TabHdlrs.Free;
                fPropSheetContainer.FocusSheet(Tabs.CurrentTab);
              end;
          TabNames.Free;
        finally
          LockWindowUpdate(0);
        end;
      except
      end;
    end;

  procedure TObjectInspectorHandlerViewer.Exposed;
    begin
      if not fExposed
        then fExposed := true;
    end;

  procedure TObjectInspectorHandlerViewer.Unexposed;
    begin
      if fExposed
        then
          begin
            fExposed := false;
            fPropSheetContInit.ClearSheets(false);
          end;
    end;

  procedure TObjectInspectorHandlerViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TObjectInspectorHandlerViewer.KeepAliveTimer(Sender: TObject);
    begin
      if fPropSheetContainer <> nil
        then
          try
            fPropSheetContainer.KeepAlive;
          except
          end;
    end;

  procedure TObjectInspectorHandlerViewer.InitSheetContainer(ClientView : IClientView);
    var
      Container : TObjectInspectorContainer;
    begin
      if fPropSheetContainer = nil
        then
          begin
            Container := TObjectInspectorContainer.Create(self);
            fPropSheetContInit  := Container;
            fPropSheetContainer := Container;
            fPropSheetContInit.SetClientView(ClientView);
          end;
    end;

end.
