unit ObjectInspectorInterfaces;

interface

  uses
    Classes, VoyagerInterfaces, VoyagerServerInterfaces, Controls;

  const
    tidSheetHandlerEntryPoint = 'GetPropertySheetHandler';

  type
    IPropertySheetHandler          = interface;
    IPropertySheetContainerHandler = interface;

    TSheetHandlerCreatorFunction = function : IPropertySheetHandler; stdcall;

    IPropertySheetHandler =
      interface
        procedure Lock;
        procedure UnLock;
        procedure SetContainer(aContainer : IPropertySheetContainerHandler);
        procedure UnsetContainer;
        procedure StartSettingProperties;
        procedure StopSettingProperties;
        function  Busy : boolean;
        function  CreateControl(Owner : TControl) : TControl;
        function  GetControl : TControl;
        procedure RenderProperties(Properties : TStringList);
        procedure SetFocus;
        procedure LostFocus;
        procedure Clear;
        procedure Refresh;
        function  HandleURL( URL : TURL ) : TURLHandlingResult;
        function  Exposed : boolean;
        function  Loaded : boolean;
      end;

    IPropertySheetContainerHandlerInit =
      interface
        procedure SetClientView(ClientView : IClientView);
        //procedure SetClassId(ClassId : integer);
        //procedure SetObjectId(ObjectId : integer);
        procedure SetObjectData(ClassId, ObjectId : integer);
        procedure SetXPos(xPos : integer);
        procedure SetYPos(yPos : integer);
        procedure ClearSheets(release : boolean);
        function  AddSheet(Handler : string) : integer;
      end;

    IPropertySheetContainerHandler =
      interface
        procedure Lock;
        procedure UnLock;
        function  GetClientView : IClientView;
        function  GetClassId : integer;
        function  GetObjectId : integer;
        function  GetXPos : integer;
        function  GetYPos : integer;
        function  GetProperties(Names : TStringList) : TStringList;
        function  GetPropertyList (Proxy : OleVariant; Names, Results : TStringList) : boolean;
        function  GetPropertyArray(Proxy : OleVariant; Names : array of string; Results : TStringList) : boolean;
        function  SheetCount : integer;
        procedure FocusSheet(index : integer);
        function  GetFocusedSheet : IPropertySheetHandler;
        function  GetCacheServerProxy : OleVariant;
        function  CreateCacheObjectProxy : OleVariant;
        function  GetCacheObjectProxy : OleVariant;
        function  GetMSProxy : OleVariant;
        function  HandleURL(URL : TURL; incoming : boolean) : TURLHandlingResult;
        procedure KeepAlive;
        function  GetCacheDir : string;
        procedure Refresh;
        procedure ClearConnections;  // NEW!
        procedure ResetWait;
        procedure StartWait;
        procedure StopWait;
      end;

implementation

end.
