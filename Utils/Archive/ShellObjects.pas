unit ShellObjects;

interface

  uses
    Windows, ComObj, SysUtils, {RegistryStr, NetTypes, }ShellGUID, CommCtrl, ShellApi;

//===========================================================================
//
// Object identifiers in the explorer's name space (ItemID and IDList)
//
//  All the items that the user can browse with the explorer (such as files,
// directories, servers, work-groups, etc.) has an identifier which is unique
// among items within the parent folder. Those identifiers are called item
// IDs (SHITEMID). Since all its parent folders have their own item IDs,
// any items can be uniquely identified by a list of item IDs, which is called
// an ID list (ITEMIDLIST).
//
//  ID lists are almost always allocated by the task allocator (see some
// description below as well as OLE 2.0 SDK) and may be passed across
// some of shell interfaces (such as IShellFolder). Each item ID in an ID list
// is only meaningful to its parent folder (which has generated it), and all
// the clients must treat it as an opaque binary data except the first two
// bytes, which indicates the size of the item ID.
//
//  When a shell extension -- which implements the IShellFolder interace --
// generates an item ID, it may put any information in it, not only the data
// with that it needs to identifies the item, but also some additional
// information, which would help implementing some other functions efficiently.
// For example, the shell's IShellFolder implementation of file system items
// stores the primary (long) name of a file or a directory as the item
// identifier, but it also stores its alternative (short) name, size and date
// etc.
//
//  When an ID list is passed to one of shell APIs (such as SHGetPathFromIDList),
// it is always an absolute path -- relative from the root of the name space,
// which is the desktop folder. When an ID list is passed to one of IShellFolder
// member function, it is always a relative path from the folder (unless it
// is explicitly specified).
//
//===========================================================================

//
// SHITEMID -- Item ID
//

  const
    IID_IContextMenu: TGUID = ( D1:$000214E4;D2: $0000;D3:$0000;D4:($C0, $00, $00, $00, $00, $00, $00, $46));

  type
    PShellItemID = ^TShellItemID;
    TShellItemID =
      record
        Count : word;              // Size of the ID (including cb itself)
        Data  : array[0..1] of byte;
      end;
//
// ITEMIDLIST -- List if item IDs (combined with 0-terminator)
//

  type
    PItemIDList = ^TItemIDList;
    TItemIDList =
      record
        ID : TShellItemID;
      end;

//===========================================================================
//
// Task allocator API
//
//  All the shell extensions MUST use the task allocator (see OLE 2.0
// programming guild for its definition) when they allocate or free
// memory objects (mostly ITEMIDLIST) that are returned across any
// shell interfaces. There are two ways to access the task allocator
// from a shell extension depending on whether or not it is linked with
// OLE32.DLL or not (  stdcall;  virtual;  abstract;ly for efficiency).
//
// (1) A shell extension which calls any OLE API (i.e., linked with
//  OLE32.DLL) should call OLE's task allocator (by retrieving
//  the task allocator by calling CoGetMalloc API).
//
// (2) A shell extension which does not call any OLE API (i.e., not linked
//  with OLE32.DLL) should call the shell task allocator API (defined
//  below), so that the shell can quickly loads it when OLE32.DLL is not
//  loaded by any application at that point.
//
// Notes:
//  In next version of Windowso release, SHGetMalloc will be replaced by
// the following macro.
//
//    SHGetMalloc(ppmem)	CoGetMalloc(MEMCTX_TASK, ppmem)
//
//===========================================================================

  function SHGetMalloc( var Malloc : IMAlloc ) : HRESULT; stdcall; external 'Shell32.dll';

//===========================================================================
//
// IContextMenu interface
//
// [OverView]
//
//  The shell uses the IContextMenu interface in following three cases.
//
// case-1: The shell is loading context menu extensions.
//
//   When the user clicks the right mouse button on an item within the shell's
//  name space (i.g., file, directory, server, work-group, etc.), it creates
//  the default context menu for its type, then loads context menu extensions
//  that are registered for that type (and its base type) so that they can
//  add extra menu items. Those context menu extensions are registered at
//  HKCR\{ProgID}\shellex\ContextMenuHandlers.
//
// case-2: The shell is retrieving a context menu of sub-folders in extended
//   name-space.
//
//   When the explorer's name space is extended by name space extensions,
//  the shell calls their IShellFolder::GetUIObjectOf to get the IContextMenu
//  objects when it creates context menus for folders under those extended
//  name spaces.
//
// case-3: The shell is loading non-default drag and drop handler for directories.
//
//   When the user performed a non-default drag and drop onto one of file
//  system folders (i.e., directories), it loads shell extensions that are
//  registered at HKCR\{ProgID}\DragDropHandlers.
//
//
// [Member functions]
//
//
// IContextMenu::QueryContextMenu
//
//   This member function may insert one or more menuitems to the specified
//  menu (hmenu) at the specified location (indexMenu which is never be -1).
//  The IDs of those menuitem must be in the specified range (idCmdFirst and
//  idCmdLast). It returns the maximum menuitem ID offset (ushort) in the
//  'code' field (low word) of the scode.
//
//   The uFlags specify the context. It may have one or more of following
//  flags.
//
//  CMF_DEFAULTONLY: This flag is passed if the user is invoking the default
//   action (typically by double-clicking, case 1 and 2 only). Context menu
//   extensions (case 1) should not add any menu items, and returns NOERROR.
//
//  CMF_VERBSONLY: The explorer passes this flag if it is constructing
//   a context menu for a short-cut object (case 1 and case 2 only). If this
//   flag is passed, it should not add any menu-items that is not appropriate
//   from a short-cut.
//    A good example is the "Delete" menuitem, which confuses the user
//   because it is not clear whether it deletes the link source item or the
//   link itself.
//
//  CMF_EXPLORER: The explorer passes this flag if it has the left-side pane
//   (case 1 and 2 only). Context menu extensions should ignore this flag.
//
//   High word (16-bit) are reserved for context specific communications
//  and the rest of flags (13-bit) are reserved by the system.
//
//
// IContextMenu::InvokeCommand
//
//   This member is called when the user has selected one of menuitems that
//  are inserted by previous QueryContextMenu member. In this case, the
//  LOWORD(lpici->lpVerb) contains the menuitem ID offset (menuitem ID -
//  idCmdFirst).
//
//   This member function may also be called programmatically. In such a case,
//  lpici->lpVerb specifies the canonical name of the command to be invoked,
//  which is typically retrieved by GetCommandString member previously.
//
//  Parameters in lpci:
//    cbSize -- Specifies the size of this structure (sizeof(*lpci))
//    hwnd   -- Specifies the owner window for any message/dialog box.
//    fMask  -- Specifies whether or not dwHotkey/hIcon paramter is valid.
//    lpVerb -- Specifies the command to be invoked.
//    lpParameters -- Parameters (optional)
//    lpDirectory  -- Working directory (optional)
//    nShow -- Specifies the flag to be passed to ShowWindow (SW_*).
//    dwHotKey -- Hot key to be assigned to the app after invoked (optional).
//    hIcon -- Specifies the icon (optional).
//
//
// IContextMenu::GetCommandString
//
//   This member function is called by the explorer either to get the
//  canonical (language independent) command name (uFlags == GCS_VERB) or
//  the help text ((uFlags & GCS_HELPTEXT) != 0) for the specified command.
//  The retrieved canonical string may be passed to its InvokeCommand
//  member function to invoke a command programmatically. The explorer
//  displays the help texts in its status bar; therefore, the length of
//  the help text should be reasonably short (<40 characters).
//
//  Parameters:
//   idCmd -- Specifies menuitem ID offset (from idCmdFirst)
//   uFlags -- Either GCS_VERB or GCS_HELPTEXT
//   pwReserved -- Reserved (must pass NULL when calling, must ignore when called)
//   pszName -- Specifies the string buffer.
//   cchMax -- Specifies the size of the string buffer.
//
//===========================================================================

// QueryContextMenu uFlags
  const
    CMF_NORMAL	 	  = $00000000;
    CMF_DEFAULTONLY = $00000001;
    CMF_VERBSONLY   = $00000002;
    CMF_EXPLORE     = $00000004;
    CMF_RESERVED	  = $ffff0000;	// View specific

// GetCommandString uFlags
    GCS_VERB      = $00000000;     // canonical verb
    GCS_HELPTEXT  = $00000001;     // help text (for status bar)
    GCS_VALIDATE  = $00000002;     // validate command exists

    CMDSTR_NEWFOLDER     = 'NewFolder';
    CMDSTR_VIEWLIST      = 'ViewList';
    CMDSTR_VIEWDETAILS   = 'ViewDetails';

    {
    CMIC_MASK_HOTKEY	 = SEE_MASK_HOTKEY;
    CMIC_MASK_ICON	 = SEE_MASK_ICON;
    CMIC_MASK_FLAG_NO_UI = SEE_MASK_FLAG_NO_UI;
    CMIC_MASK_MODAL      = $80000000;				//  Internal
    CMIC_VALID_SEE_FLAGS = SEE_VALID_CMIC_FLAGS;		// Internal
    }

  type
    PInvokeCommandInfo = ^TInvokeCommandInfo;
    TInvokeCommandInfo =
      record
        Size       : DWORD;	    // must be sizeof(CMINVOKECOMMANDINFO)
        Mask       : DWORD;	    // any combination of CMIC_MASK_*
        hwnd       : THandle;	  // might be NULL (indicating no owner window)
        Verb       : pchar;	    // either a string of MAKEINTRESOURCE(idOffset)
        Parameters : pchar;     // might be NULL (indicating no parameter)
        Directory  : pchar;	    // might be NULL (indicating no specific directory)
        Show       : integer;	  // one of SW_ values for ShowWindow() API

        HotKey     : DWORD;
        hIcon      : THandle;
      end;

  type
    PInvokeCommandInfoEx = ^TInvokeCommandInfoEx;
    TInvokeCommandInfoEx =
      packed record
        Size        : DWORD;     // must be sizeof(CMINVOKECOMMANDINFOEX)
        Mask        : DWORD;     // any combination of CMIC_MASK_*
        wnd         : HWND;      // might be NULL (indicating no owner window)
        Verb        : pchar;     // either a string or MAKEINTRESOURCE(idOffset)
        Parameters  : pchar;     // might be NULL (indicating no parameter)
        Directory   : pchar;     // might be NULL (indicating no specific directory)
        Show        : integer;   // one of SW_ values for ShowWindow() API

        HotKey      : DWORD;
        hIcon       : HICON;
        Title       : pchar;     // For CreateProcess-StartupInfo.lpTitle
        VerbW       : pwidechar; // Unicode verb (for those who can use it)
        ParametersW : pwidechar; // Unicode parameters (for those who can use it)
        DirectoryW  : pwidechar; // Unicode directory (for those who can use it)
        TitleW      : pwidechar; // Unicode title (for those who can use it)
      end;

  type
    IContextMenu =
      class( IUnknown )
        function QueryContextMenu
                   (
                     Menu      : THandle;
                     MenuIndex : UINT;
                     CmdFirst  : UINT;
                     CmdLast   : UINT;
                     Flags     : UINT
                   ) : HRESULT; virtual; stdcall; abstract;
        function InvokeCommand( Info : PInvokeCommandInfo ) : HRESULT ; virtual; stdcall; abstract;
        function GetCommandString
                   (
                     Command      : UINT;
                     uType        : UINT;
                     Reserved     : pointer;
                     Name         : pchar;
                     Max          : UINT
                   ) : HRESULT; virtual; stdcall; abstract;
      end;

//
// IContextMenu2 (IContextMenu with one new member)
//
// IContextMenu2::HandleMenuMsg
//
//  This function is called, if the client of IContextMenu is aware of
// IContextMenu2 interface and receives one of following messages while
// it is calling TrackPopupMenu (in the window proc of hwndOwner):
//      WM_INITPOPUP, WM_DRAWITEM and WM_MEASUREITEM
//  The callee may handle these messages to draw owner draw menuitems.
//

  type
    IContextMenu2 =
      class( IUnknown )
        function QueryContextMenu( Menu : HMENU; indexMenu : UINT; CmdFirst, CmdLast : UINT; Flags : integer ) : HResult; virtual; stdcall; abstract;
        function InvokeCommand( lpici : PInvokeCommandInfo ) : HResult; virtual; stdcall; abstract;
        function GetCommandString( Cmd : UINT; uType : UINT; pwReserved : PInteger; pszName : pchar; cchMax : UINT ) : HResult; virtual; stdcall; abstract;
        function HandleMenuMsg( Msg : UINT; WParameter : WPARAM; lParameter : LPARAM ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// Interface: IShellExtInit
//
//  The IShellExtInit interface is used by the explorer to initialize shell
// extension objects. The explorer (1) calls CoCreateInstance (or equivalent)
// with the registered CLSID and IID_IShellExtInit, (2) calls its Initialize
// member, then (3) calls its QueryInterface to a particular interface (such
// as IContextMenu or IPropSheetExt and (4) performs the rest of operation.
//
//
// [Member functions]
//
// IShellExtInit::Initialize
//
//  This member function is called when the explorer is initializing either
// context menu extension, property sheet extension or non-default drag-drop
// extension.
//
//  Parameters: (context menu or property sheet extension)
//   pidlFolder -- Specifies the parent folder
//   lpdobj -- Spefifies the set of items selected in that folder.
//   hkeyProgID -- Specifies the type of the focused item in the selection.
//
//  Parameters: (non-default drag-and-drop extension)
//   pidlFolder -- Specifies the target (destination) folder
//   lpdobj -- Specifies the items that are dropped (see the description
//    about shell's clipboard below for clipboard formats).
//   hkeyProgID -- Specifies the folder type.
//
//===========================================================================

  type
    IShellExtInit =
      class( IUnknown )
        function Initialize
                   (
                     FolderPIDL : PItemIDList;
		                 lpdobj     : pointer;
                     ProgID     : HKEY
                   ) : HRESULT; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// Interface: IShellPropSheetExt
//
//  The explorer uses the IShellPropSheetExt to allow property sheet
// extensions or control panel extensions to add additional property
// sheet pages.
//
//
// [Member functions]
//
// IShellPropSheetExt::AddPages
//
//  The explorer calls this member function when it finds a registered
// property sheet extension for a particular type of object. For each
// additional page, the extension creates a page object by calling
// CreatePropertySheetPage API and calls lpfnAddPage.
//
//  Parameters:
//   lpfnAddPage -- Specifies the callback function.
//   lParam -- Specifies the opaque handle to be passed to the callback function.
//
//
// IShellPropSheetExt::ReplacePage
//
//  The explorer never calls this member of property sheet extensions. The
// explorer calls this member of control panel extensions, so that they
// can replace some of default control panel pages (such as a page of
// mouse control panel).
//
//  Parameters:
//   uPageID -- Specifies the page to be replaced.
//   lpfnReplace Specifies the callback function.
//   lParam -- Specifies the opaque handle to be passed to the callback function.
//
//===========================================================================

  type
    IShellPropSheetExt =
      class( IUnknown )
        function AddPages( AddPage : TFNAddPropSheetPage; lParameter : LPARAM ) : HResult; virtual; stdcall; abstract;
        function ReplacePage( uPageID : UINT; ReplaceWith : TFNAddPropSheetPage; lParameter : LPARAM ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IPersistFolder Interface
//
//  The IPersistFolder interface is used by the file system implementation of
// IShellFolder::BindToObject when it is initializing a shell folder object.
//
//
// [Member functions]
//
// IPersistFolder::Initialize
//
//  This member function is called when the explorer is initializing a
// shell folder object.
//
//  Parameters:
//   pidl -- Specifies the absolute location of the folder.
//
//===========================================================================

  type
    IPersistFolder =
      class( IPersist )
        function Initialize( pidl : PItemIdList ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IShellIcon Interface
//
// used to get a icon index for a IShellFolder object.
//
// this interface can be implemented by a IShellFolder, as a quick way to
// return the icon for a object in the folder.
//
// a instance of this interface is only created once for the folder, unlike
// IExtractIcon witch is created once for each object.
//
// if a ShellFolder does not implement this interface, the standard
// GetUIObject(....IExtractIcon) method will be used to get a icon
// for all objects.
//
// the following standard imagelist indexs can be returned:
//
//      0   document (blank page) (not associated)
//      1   document (with stuff on the page)
//      2   application (exe, com, bat)
//      3   folder (plain)
//      4   folder (open)
//
// IShellIcon:GetIconOf(pidl, flags, lpIconIndex)
//
//      pidl            object to get icon for.
//      flags           GIL_* input flags (GIL_OPEN, ...)
//      lpIconIndex     place to return icon index.
//
//  returns:
//      NOERROR, if lpIconIndex contains the correct system imagelist index.
//      S_FALSE, if unable to get icon for this object, go through
//               GetUIObject, IExtractIcon, methods.
//
//===========================================================================

  type
    IShellIcon =
      class( IUnknown )
        function GetIconOf( pidl : PItemIdList; flags : UINT; var IconIndex : UINT ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IShellLink Interface
//
//===========================================================================

// IShellLink::Resolve fFlags
  const
    SLR_NO_UI	    = $0001;
    SLR_ANY_MATCH = $0002;
    SLR_UPDATE    = $0004;

// IShellLink::GetPath fFlags
    SLGP_SHORTPATH    = $0001;
    SLGP_UNCPRIORITY  = $0002;

  type
    IShellLink =
      class( IUnknown )
        function GetPath
                   (
                     FileName     : pchar;
                     MaxPath      : integer;
                     var FindData : TWin32FindDataA;
                     Flags        : DWORD
                   ) : HRESULT; virtual; stdcall; abstract;
        function GetIDList( var ppidl : PItemIDList ) : HRESULT; virtual; stdcall; abstract;
        function SetIDList( pidl : PItemIDList ) : HRESULT; virtual; stdcall; abstract;
        function GetDescription( Name : pchar; MaxName : integer ) : HRESULT; virtual; stdcall; abstract;
        function SetDescription( Name : pchar ) : HRESULT; virtual; stdcall; abstract;
        function GetWorkingDirectory( Dir : pchar; MaxPath : integer ) : HRESULT; virtual; stdcall; abstract;
        function SetWorkingDirectory( Dir : pchar) : HRESULT; virtual; stdcall; abstract;
        function GetArguments( Args : pchar; MaxPath : integer ) : HRESULT; virtual; stdcall; abstract;
        function SetArguments( Args : pchar ) : HRESULT; virtual; stdcall; abstract;
        function GetHotkey( var Hotkey : word ) : HRESULT; virtual; stdcall; abstract;
        function SetHotkey( Hotkey : word ) : HRESULT; virtual; stdcall; abstract;
        function GetShowCmd( var ShowCmd : integer ) : HRESULT; virtual; stdcall; abstract;
        function SetShowCmd( ShowCmd : integer ) : HRESULT; virtual; stdcall; abstract;
        function GetIconLocation
                   (
                     IconPath   : pchar;
                     PathLength : integer;
                     var IconIndex : integer
                   ) : HRESULT; virtual; stdcall; abstract;
        function SetIconLocation
                   (
                     IconPath  : pchar;
                     IconIndex : integer
                   ) : HRESULT; virtual; stdcall; abstract;
        function SetRelativePath( PathRel : pchar; Reserved : DWORD ) : HRESULT; virtual; stdcall; abstract;
        function Resolve( hwnd : THandle; Flags : DWORD ) : HRESULT; virtual; stdcall; abstract;
        function SetPath( FileName : pchar ) : HRESULT; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IShellExecuteHook Interface
//
//===========================================================================

  type
    IShellExecuteHookA =
      class( IUnknown )
        function Execute( pei : TShellExecuteInfoA ) : HResult; virtual; stdcall; abstract;
      end;

  type
    IShellExecuteHookW =
      class( IUnknown )
        function Execute( pei : TShellExecuteInfoW ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// INewShortcutHook Interface
//
//===========================================================================

  type
    INewShortcutHookA =
      class( IUnknown )
        function SetReferent( Referent : pchar; wnd : HWND ) : HResult; virtual; stdcall; abstract;
        function GetReferent( Referent : pchar; cchReferent : integer ) : HResult; virtual; stdcall; abstract;
        function SetFolder( Folder : pchar ) : HResult; virtual; stdcall; abstract;
        function GetFolder( Folder : pchar; cchFolder : integer ) : HResult; virtual; stdcall; abstract;
        function GetName( Name : pchar; cchName : integer) : HResult; virtual; stdcall; abstract;
        function GetExtension( Extension : pchar; cchExtension : integer ) : HResult; virtual; stdcall; abstract;
      end;

  type
    INewShortcutHookW =
      class( IUnknown )
        function SetReferent( Referent : pwidechar; wnd : HWND ) : HResult; virtual; stdcall; abstract;
        function GetReferent( Referent : pwidechar; cchReferent : integer ) : HResult; virtual; stdcall; abstract;
        function SetFolder( Folder : pwidechar ) : HResult; virtual; stdcall; abstract;
        function GetFolder( Folder : pwidechar; cchFolder : integer ) : HResult; virtual; stdcall; abstract;
        function GetName( Name : pwidechar; cchName : integer) : HResult; virtual; stdcall; abstract;
        function GetExtension( Extension : pwidechar; cchExtension : integer ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// ICopyHook Interface
//
//
//  The copy hook is called whenever file system directories are
//  copy/moved/deleted/renamed via the shell.  It is also called by the shell
//  on changes of status of printers.
//
//  Clients register their id under STRREG_SHEX_COPYHOOK for file system hooks
//  and STRREG_SHEx_PRNCOPYHOOK for printer hooks.
//  the CopyCallback is called prior to the action, so the hook has the chance
//  to allow, deny or cancel the operation by returning the falues:
//     IDYES  -  means allow the operation
//     IDNO   -  means disallow the operation on this file, but continue with
//              any other operations (eg. batch copy)
//     IDCANCEL - means disallow the current operation and cancel any pending
//              operations
//
//   arguments to the CopyCallback
//      hwnd - window to use for any UI
//      wFunc - what operation is being done
//      wFlags - and flags (FOF_*) set in the initial call to the file operation
//      pszSrcFile - name of the source file
//      dwSrcAttribs - file attributes of the source file
//      pszDestFile - name of the destiation file (for move and renames)
//      dwDestAttribs - file attributes of the destination file
//
//
//===========================================================================

// file operations
  const
    FO_MOVE           = $0001;
    FO_COPY           = $0002;
    FO_DELETE         = $0003;
    FO_RENAME         = $0004;

    FOF_MULTIDESTFILES         = $0001;
    FOF_CONFIRMMOUSE           = $0002;
    FOF_SILENT                 = $0004;  // don't create progress/report
    FOF_RENAMEONCOLLISION      = $0008;
    FOF_NOCONFIRMATION         = $0010;  // Don't prompt the user.
    FOF_WANTMAPPINGHANDLE      = $0020;  // Fill in SHFILEOPSTRUCT.hNameMappings
                                        // Must be freed using SHFreeNameMappings
    FOF_ALLOWUNDO              = $0040;
    FOF_FILESONLY              = $0080;  // on *.*, do only files
    FOF_SIMPLEPROGRESS         = $0100;  // means don't show names of files
    FOF_NOCONFIRMMKDIR         = $0200;  // don't confirm making any needed dirs

  type
    FILEOP_FLAGS = UINT;

// printer operations
  const
    PO_DELETE	  = $0013;  // printer is being deleted
    PO_RENAME	  = $0014;  // printer is being renamed
    PO_PORTCHANGE = $0020;  // port this printer connected to is being changed
                            // if this id is set, the strings received by
			    // the copyhook are a doubly-null terminated
			    // list of strings.  The first is the printer
			    // name and the second is the printer port.
    PO_REN_PORT	  = $0034;  // PO_RENAME and PO_PORTCHANGE at same time.

// no POF_ flags currently defined

  type
    PRINTEROP_FLAGS = UINT;

  type
    ICopyHook =
      class( IUnknown )
        function CopyCallback
                   (
                     hwnd        : THandle;
                     Func        : UINT;
                     Flags       : UINT;
                     SrcFile     : pchar;
                     SrcAttribs  : DWORD;
                     DestFile    : pchar;
                     DestAttribs : DWORD
                   ) : UINT; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IFileViewerSite Interface
//
//===========================================================================


  type
    IFileViewerSite =
      class
        function QueryInterface(const iid: TIID; var obj): HResult; virtual; stdcall; abstract;
        function AddRef : ULONG;  virtual; stdcall; abstract;
        function Release : ULONG; virtual; stdcall; abstract;
        function SetPinnedWindow( hwnd : THandle ) : HResult; virtual; stdcall; abstract;
        function GetPinnedWindow( var hwnd : THandle ) : HResult; virtual; stdcall; abstract;
      end;

//===========================================================================
//
// IFileViewer Interface
//
// Implemented in a FileViewer component object.  Used to tell a
// FileViewer to PrintTo or to view, the latter happening though
// ShowInitialize and Show.  The filename is always given to the
// viewer through IPersistFile.
//
//===========================================================================

  type
    PFileViwerShowInfo = ^TFileViwerShowInfo;
    TFileViwerShowInfo =
      record
        // Stuff passed into viewer (in)
        Size  : DWORD;           // Size of structure for future expansion...
        Owner : THandle;         // who is the owner window.
        Show  : integer;         // The show command

        // Passed in and updated  (in/Out)
        Flags : DWORD;          // flags
        Rect  : TRect;          // Where to create the window may have defaults
        Rel   : IUnknown;       // Relese this interface when window is visible

        // Stuff that might be returned from viewer (out)
        NewFile : POleStr;   // New File to view.
      end;

// Define File View Show Info Flags.
  const
    FVSIF_RECT      = $00000001;      // The rect variable has valid data.
    FVSIF_PINNED    = $00000002;      // We should Initialize pinned

    FVSIF_NEWFAILED = $08000000;      // The new file passed back failed
                                        // to be viewed.

    FVSIF_NEWFILE   = $80000000;      // A new file to view has been returned
    FVSIF_CANVIEWIT = $40000000;      // The viewer can view it.

  type
    IFileViewer =
      class
        function QueryInterface(const iid: TIID; var obj): HResult; virtual; stdcall; abstract;
        function AddRef : ULONG;  virtual; stdcall; abstract;
        function Release : ULONG; virtual; stdcall; abstract;
        function ShowInitialize( FileSite : IFileViewerSite ) : HResult; virtual; stdcall; abstract;
        function Show( Info : PFileViwerShowInfo ) : HResult; virtual; stdcall; abstract;
        function PrintTo( Driver : pchar; SuppressUI : BOOL ) : HResult; virtual; stdcall; abstract;
      end;

//-------------------------------------------------------------------------
//
// struct STRRET
//
// structure for returning strings from IShellFolder member functions
//
//-------------------------------------------------------------------------
  const
    STRRET_WSTR	  = $0000;
    STRRET_OFFSET = $0001;
    STRRET_CSTR	  = $0002;

  type
    PStrRet = ^TStrRet;
    TStrRet =
      record
        case uType : UINT of                                       // One of the STRRET_* values
          0 : (OleStr : POleStr);                                  // OLESTR that will be freed
          1 : (Offset : UINT);                                     // Offset into SHITEMID (ANSI)
          2 : (Str    : array[0..MAX_PATH - 1] of char);           // Buffer to fill in
      end;

(*
typedef struct _STRRET
{
    UINT uType;	// One of the STRRET_* values
    union
    {
        LPWSTR          pOleStr;        // OLESTR that will be freed
        UINT            uOffset;        // Offset into SHITEMID (ANSI)
        char            cStr[MAX_PATH]; // Buffer to fill in
    } DUMMYUNIONNAME;
} STRRET, *LPSTRRET;
*)

//-------------------------------------------------------------------------
//
// SHGetPathFromIDList
//
//  This function assumes the size of the buffer (MAX_PATH). The pidl
// should point to a file system object.
//
//-------------------------------------------------------------------------

  function SHGetPathFromIDList( pidl : PItemIDList; Path : pchar ) : BOOL; stdcall; external 'Shell32.dll'


//-------------------------------------------------------------------------
//
// SHGetSpecialFolderLocation
//
//  Returns a pidl to a predefined shell folder.  The caller must free the
//   pidl.  Use SHGetMalloc to obtain an allocator that can free the pidl.
//
//-------------------------------------------------------------------------
//
// registry entries for special paths are kept in :

  const
    REGSTR_PATH_EXPLORER        =  'Software\\Microsoft\\Windows\\CurrentVersion\\Explorer';
    REGSTR_PATH_SPECIAL_FOLDERS = REGSTR_PATH_EXPLORER + '\\Shell Folders';

    CSIDL_DESKTOP            = $0000;
    CSIDL_PROGRAMS           = $0002;
    CSIDL_CONTROLS           = $0003;
    CSIDL_PRINTERS           = $0004;
    CSIDL_PERSONAL           = $0005;
    CSIDL_FAVORITES          = $0006;
    CSIDL_STARTUP            = $0007;
    CSIDL_RECENT             = $0008;
    CSIDL_SENDTO             = $0009;
    CSIDL_BITBUCKET          = $000a;
    CSIDL_STARTMENU          = $000b;
    CSIDL_DESKTOPDIRECTORY   = $0010;
    CSIDL_DRIVES             = $0011;
    CSIDL_NETWORK            = $0012;
    CSIDL_NETHOOD            = $0013;
    CSIDL_FONTS		           = $0014;
    CSIDL_TEMPLATES          = $0015;

  function SHGetSpecialFolderLocation( Owner : THandle; Folder : integer; var pidl : PItemIDList ) : HResult; stdcall; external 'Shell32.dll';

//-------------------------------------------------------------------------
//
// SHBrowseForFolder API
//
//-------------------------------------------------------------------------

  type
    TBFFCallBack = function( hwnd : THandle; Msg : UINT; lParam : LPARAM; lpData : LPARAM ) : integer;

    //typedef int (CALLBACK* BFFCALLBACK)(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData);

    PBrowseInfo = ^TBrowseInfo;
    TBrowseInfo =
      record
        Owner : THandle;
        Root  : PItemIDList;
        DisplayName : pchar;         // Return display name of item selected.
        Title       : pchar;         // text to go in the banner over the tree.
        Flags       : UINT;          // Flags that control the return stuff
        funct       : TBFFCallBack;
        Param       : LPARAM;        // extra info that's passed back in callbacks

        Image       : integer;       // output var: where to return the Image index.
      end;


// Browsing for directory.
  const
    WM_USER = $0400;

  const
    BIF_RETURNONLYFSDIRS   = $0001;  // For finding a folder to start document searching
    BIF_DONTGOBELOWDOMAIN  = $0002;  // For starting the Find Computer
    BIF_STATUSTEXT         = $0004;
    BIF_RETURNFSANCESTORS  = $0008;

    BIF_BROWSEFORCOMPUTER  = $1000;  // Browsing for Computers.
    BIF_BROWSEFORPRINTER   = $2000;  // Browsing for Printers

// message from browser
    BFFM_INITIALIZED       = 1;
    BFFM_SELCHANGED        = 2;

// messages to browser
    BFFM_SETSTATUSTEXT     = (WM_USER + 100);
    BFFM_ENABLEOK          = (WM_USER + 101);
    BFFM_SETSELECTION      = (WM_USER + 102);

  function SHBrowseForFolder( lpbi : PBrowseInfo ) : PItemIDList; stdcall; external 'Shell32.dll';

//-------------------------------------------------------------------------
//
// SHLoadInProc
//
//   When this function is called, the shell calls CoCreateInstance
//  (or equivalent) with CLSCTX_INPROC_SERVER and the specified CLSID
//  from within the shell's process and release it immediately.
//
//-------------------------------------------------------------------------

  function SHLoadInProc( const rclsid : TCLSID ) : HRESULT; stdcall; external 'Shell32.dll';

//-------------------------------------------------------------------------
//
// IEnumIDList interface
//
//  IShellFolder::EnumObjects member returns an IEnumIDList object.
//
//-------------------------------------------------------------------------

  type
    IEnumIDList =
      class( IUnknown )
        function Next
                   (
                     celt : ULONG;
                     var rgelt   : PItemIDList;
		     var Fetched : ULONG
                   ) : HRESULT; virtual; stdcall; abstract;
        function Skip( celt : ULONG ) : HRESULT; virtual; stdcall; abstract;
        function Reset : HRESULT; virtual; stdcall; abstract;
        function Clone( var Enum : IEnumIDList ) : HRESULT; virtual; stdcall; abstract;
      end;

//-------------------------------------------------------------------------
//
// IShellFolder interface
//
//
// [Member functions]
//
// IShellFolder::BindToObject(pidl, pbc, riid, ppvOut)
//   This function returns an instance of a sub-folder which is specified
//  by the IDList (pidl).
//
// IShellFolder::BindToStorage(pidl, pbc, riid, ppvObj)
//   This function returns a storage instance of a sub-folder which is
//  specified by the IDList (pidl). The shell never calls this member
//  function in the first release of Win95.
//
// IShellFolder::CompareIDs(lParam, pidl1, pidl2)
//   This function compares two IDLists and returns the result. The shell
//  explorer always passes 0 as lParam, which indicates "sort by name".
//  It should return 0 (as CODE of the scode), if two id indicates the
//  same object; negative value if pidl1 should be placed before pidl2;
//  positive value if pidl2 should be placed before pidl1.
//
// IShellFolder::CreateViewObject(hwndOwner, riid, ppvOut)
//   This function creates a view object of the folder itself. The view
//  object is a difference instance from the shell folder object.
//
// IShellFolder::GetAttributesOf(cidl, apidl, prgfInOut)
//   This function returns the attributes of specified objects in that
//  folder. "cidl" and "apidl" specifies objects. "apidl" contains only
//  simple IDLists. The explorer initializes *prgfInOut with a set of
//  flags to be evaluated. The shell folder may optimize the operation
//  by not returning unspecified flags.
//
// IShellFolder::GetUIObjectOf(hwndOwner, cidl, apidl, riid, prgfInOut, ppvOut)
//   This function creates a UI object to be used for specified objects.
//  The shell explorer passes either IID_IDataObject (for transfer operation)
//  or IID_IContextMenu (for context menu operation) as riid.
//
// IShellFolder::GetDisplayNameOf
//   This function returns the display name of the specified object.
//  If the ID contains the display name (in the locale character set),
//  it returns the offset to the name. Otherwise, it returns a pointer
//  to the display name string (UNICODE), which is allocated by the
//  task allocator, or fills in a buffer.
//
// IShellFolder::SetNameOf
//   This function sets the display name of the specified object.
//  If it changes the ID as well, it returns the new ID which is
//  alocated by the task allocator.
//
//-------------------------------------------------------------------------

// IShellFolder::EnumObjects
  const
    SHCONTF_FOLDERS         = 32;	// for shell browser
    SHCONTF_NONFOLDERS      = 64;	// for default view
    SHCONTF_INCLUDEHIDDEN   = 128;	// for hidden/system objects

// IShellFolder::GetDisplayNameOf/SetNameOf uFlags
  const
    SHGDN_NORMAL      = 0;	  // default (display purpose)
    SHGDN_INFOLDER    = 1;        // displayed under a folder (relative)
    SHGDN_FORPARSING  = $8000;   // for ParseDisplayName or path

// IShellFolder::GetAttributesOf flags
  const
    DROPEFFECT_COPY   =	1;          //Size is 4
    DROPEFFECT_MOVE   =	2;          //Size is 4
    DROPEFFECT_LINK   =	4;          //Size is 4
    DROPEFFECT_SCROLL =	$80000000; //Size is 4

  const
    SFGAO_CANCOPY           = DROPEFFECT_COPY; // Objects can be copied
    SFGAO_CANMOVE           = DROPEFFECT_MOVE; // Objects can be moved
    SFGAO_CANLINK           = DROPEFFECT_LINK; // Objects can be linked
    SFGAO_CANRENAME         = $00000010;       // Objects can be renamed
    SFGAO_CANDELETE         = $00000020;       // Objects can be deleted
    SFGAO_HASPROPSHEET      = $00000040;       // Objects have property sheets
    SFGAO_DROPTARGET	      = $00000100;       // Objects are drop target
    SFGAO_CAPABILITYMASK    = $00000177;
    SFGAO_LINK              = $00010000;       // Shortcut (link)
    SFGAO_SHARE             = $00020000;       // shared
    SFGAO_READONLY          = $00040000;       // read-only
    SFGAO_GHOSTED           = $00080000;       // ghosted icon
    SFGAO_DISPLAYATTRMASK   = $000F0000;
    SFGAO_FILESYSANCESTOR   = $10000000;       // It contains file system folder
    SFGAO_FOLDER            = $20000000;       // It's a folder.
    SFGAO_FILESYSTEM        = $40000000;       // is a file system thing (file/folder/root)
    SFGAO_HASSUBFOLDER      = $80000000;       // Expandable in the map pane
    SFGAO_CONTENTSMASK      = $80000000;
    SFGAO_VALIDATE          = $01000000;       // invalidate cached information
    SFGAO_REMOVABLE         = $02000000;       // is this removeable media?
    SFGAO_ALL               = $FFFFFFFF;       // All attributes

  type
    IShellFolder =
      class( IUnknown )
        function ParseDisplayName
                   (
                     Owner           : THandle;
	             Reserved        : pointer;
                     DisplayName     : POleStr;
                     var Eaten       : ULONG;
                     var pidl        : PItemIDList;
                     var Attributes  : ULONG
                   ) : HRESULT; virtual; stdcall; abstract;
        function EnumObjects
                   (
                     Owner          : THandle;
                     Flags          : DWORD;
                     var EnumIDList : IEnumIDList
                   ) : HRESULT; virtual; stdcall; abstract;
        function BindToObject
                   (
                     pidl      : PItemIDList;
                     Reserved  : pointer;
                     const iid : TIID;
                     var Out   : IShellFolder //pointer
                   ) : HRESULT; virtual; stdcall; abstract;
        function BindToStorage
                   (
                     pidl : PItemIDList;
                     Reserved : pointer;
                     const iid : TIID;
                     var Obj : pointer
                   ) : HRESULT; virtual; stdcall; abstract; //Check it out
        function CompareIDs
                   (
                     lParam : LPARAM;
                     pidl1  : PItemIDList;
                     pidl2  : PItemIDList
                   ) : HRESULT; virtual; stdcall; abstract;
        function CreateViewObject
                   (
                     Owner     : THandle;
                     const iid : TIID;
                     var Out   : pointer
                   ) : HRESULT; virtual; stdcall; abstract;
        function GetAttributesOf
                   (
                     cidl      : UINT;
                     var apidl : PItemIDList;
                     var InOut : ULONG
                   ) : HRESULT; virtual; stdcall; abstract;
        function GetUIObjectOf
                   (
                     Owner     : THandle;
                     cidl      : UINT;
                     var apidl : PItemIDList;
                     const iid : TGUID;
                     var InOut : UINT;
                     var Out   : pointer
                   ) : HRESULT; virtual; stdcall; abstract;
        function GetDisplayNameOf
                   (
                     pidl     : PItemIDList;
                     Flags    : DWORD;
                     var Name : TSTRRET
                   ) : HRESULT; virtual; stdcall; abstract;
        function SetNameOf
                   (
                     Owner       : THandle;
                     pidl        : PItemIDList;
                     Name        : POleStr;
                     Flags       : DWORD;
                     var PidlOut : PItemIDList
                   ) : HRESULT; virtual; stdcall; abstract;
      end;


//
//  Helper function which returns a IShellFolder interface to the desktop
// folder. This is equivalent to call CoCreateInstance with CLSID_ShellDesktop.
//
//  CoCreateInstance(CLSID_Desktop, NULL,
//                   CLSCTX_INPROC, IID_IShellFolder, &pshf);
//

  function SHGetDesktopFolder( var ShellFolder : IShellFolder ) : HRESULT; stdcall; external 'Shell32.dll';

//==========================================================================
// Clipboard format which may be supported by IDataObject from system
// defined shell folders (such as directories, network, ...).
//==========================================================================

  const
    CFSTR_SHELLIDLIST       = 'Shell IDList Array';	// CF_IDLIST
    CFSTR_SHELLIDLISTOFFSET = 'Shell Object Offsets';	// CF_OBJECTPOSITIONS
    CFSTR_NETRESOURCES      = 'Net Resource';		// CF_NETRESOURCE
    CFSTR_FILEDESCRIPTOR    = 'FileGroupDescriptor';	// CF_FILEGROUPDESCRIPTOR
    CFSTR_FILECONTENTS 	    = 'FileContents';		// CF_FILECONTENTS
    CFSTR_FILENAME	    = 'FileName';		// CF_FILENAME
    CFSTR_PRINTERGROUP	    = 'PrinterFriendlyName';    // CF_PRINTERS
    CFSTR_FILENAMEMAP	    = 'FileNameMap';		// CF_FILENAMEMAP

//
// CF_OBJECTPOSITIONS
//
//
  const
    DVASPECT_SHORTNAME = 2; // use for CF_HDROP to get short name version

//
// format of CF_NETRESOURCE
//

  type
    PNetResArray = ^TNetResArray;
    TNetResArray =
      record
        Items : UINT;
        nr    : array[0..1] of TNetResource;
      end;

//
// format of CF_IDLIST
//
  PCIDA = ^TCIDA;
  TCIDA =
    record
      cidl   : UINT;		        // number of relative IDList
      offset : array[0..1] of UINT ;	// [0]: folder IDList, [1]-[cidl]: item IDList
    end;

//
// FILEDESCRIPTOR.dwFlags field indicate which fields are to be used
//
  const
    FD_CLSID		= $0001;
    FD_SIZEPOINT	= $0002;
    FD_ATTRIBUTES       = $0004;
    FD_CREATETIME       = $0008;
    FD_ACCESSTIME       = $0010;
    FD_WRITESTIME       = $0020;
    FD_FILESIZE		= $0040;
    FD_LINKUI		= $8000;	// 'link' UI is prefered

  type
    LONG      = integer;
    SIZEL     = LONG;
    FILETIME  =
      record
        LowDateTime  : DWORD;
        HighDateTime : DWORD;
      end;
    POINTL =
      record
        x : LONG;
        y : LONG;
      end;

  type
    PFileDescriptor = ^TFileDescriptor;
    TFileDescriptor =
      record
        Flags : DWORD;

        clsid  : TCLSID;
        size   : SIZEL;
        point  : POINTL;

        FileAttributes : DWORD;
        CreationTime   : FILETIME;
        LastAccessTime : FILETIME;
        LastWriteTime  : FILETIME;
        FileSizeHigh   : DWORD;
        FileSizeLow    : DWORD;
        FileName       : pchar;
      end;

//
// format of CF_FILEGROUPDESCRIPTOR
//
  type
    TFileGroupDescriptor =
      record
        Items : UINT;
        fgd   : array [0..1] of TFileDescriptor;
      end;

//
// format of CF_HDROP and CF_PRINTERS, in the HDROP case the data that follows
// is a double null terinated list of file names, for printers they are printer
// friendly names
//
  type
    PDropFiles = ^TDropFiles;
    TDropFiles =
      record
        Files : DWORD;    // offset of file list
        pt    : TPoint;   // drop point (client coords)
        NC    : BOOL;     // is it on NonClient area
			  // and pt is in screen coords
        Wide  : BOOL;     // WIDE character switch
      end;

//====== File System Notification APIs ===============================
//

//
//  File System Notification flags
//

  const
    SHCNE_RENAMEITEM          = $00000001;
    SHCNE_CREATE	            = $00000002;
    SHCNE_DELETE	            = $00000004;
    SHCNE_MKDIR	              = $00000008;
    SHCNE_RMDIR               = $00000010;
    SHCNE_MEDIAINSERTED       = $00000020;
    SHCNE_MEDIAREMOVED        = $00000040;
    SHCNE_DRIVEREMOVED        = $00000080;
    SHCNE_DRIVEADD            = $00000100;
    SHCNE_NETSHARE            = $00000200;
    SHCNE_NETUNSHARE          = $00000400;
    SHCNE_ATTRIBUTES          = $00000800;
    SHCNE_UPDATEDIR           = $00001000;
    SHCNE_UPDATEITEM          = $00002000;
    SHCNE_SERVERDISCONNECT    = $00004000;
    SHCNE_UPDATEIMAGE         = $00008000;
    SHCNE_DRIVEADDGUI         = $00010000;
    SHCNE_RENAMEFOLDER        = $00020000;
    SHCNE_FREESPACE           = $00040000;

    SHCNE_ASSOCCHANGED        = $08000000;

    SHCNE_DISKEVENTS          = $0002381F;
    SHCNE_GLOBALEVENTS        = $0C0581E0; // Events that dont match pidls first
    SHCNE_ALLEVENTS           = $7FFFFFFF;
    SHCNE_INTERRUPT           = $80000000; // The presence of this flag indicates
                                            // that the event was generated by an
                                            // interrupt.  It is stripped out before
                                            // the clients of SHCNNotify_ see it.

// Flags
// uFlags & SHCNF_TYPE is an ID which indicates what dwItem1 and dwItem2 mean
  const
    SHCNF_IDLIST      = $0000;	// LPITEMIDLIST
    SHCNF_PATH        = $0001;	// path name
    SHCNF_PRINTER     = $0002;	// printer friendly name
    SHCNF_DWORD       = $0003;	// DWORD
    SHCNF_TYPE        = $00FF;
    SHCNF_FLUSH       = $1000;
    SHCNF_FLUSHNOWAIT = $2000;

//
//  APIs
//
  procedure SHChangeNotify( EventId : LONG; Flags : UINT; dwItem1, dwItem2 : pointer ); stdcall; external 'Shell32.dll';

//
// SHAddToRecentDocs
//

  const
    SHARD_PIDL	= $00000001;
    SHARD_PATH  = $00000002;

  procedure SHAddToRecentDocs( Flags : UINT; pv : pointer ); stdcall; external 'Shell32.dll';

  function SHGetInstanceExplorer( var ppunk : IUnknown ): HRESULT; stdcall; external 'Shell32.dll';

//===========================================================================
//
// IExtractIcon interface
//
//  This interface is used in two different places in the shell.
//
// Case-1: Icons of sub-folders for the scope-pane of the explorer.
//
//  It is used by the explorer to get the "icon location" of
// sub-folders from each shell folders. When the user expands a folder
// in the scope pane of the explorer, the explorer does following:
//  (1) binds to the folder (gets IShellFolder),
//  (2) enumerates its sub-folders by calling its EnumObjects member,
//  (3) calls its GetUIObjectOf member to get IExtractIcon interface
//     for each sub-folders.
//  In this case, the explorer uses only IExtractIcon::GetIconLocation
// member to get the location of the appropriate icon. An icon location
// always consists of a file name (typically DLL or EXE) and either an icon
// resource or an icon index.
//
//
// Case-2: Extracting an icon image from a file
//
//  It is used by the shell when it extracts an icon image
// from a file. When the shell is extracting an icon from a file,
// it does following:
//  (1) creates the icon extraction handler object (by getting its CLSID
//     under the {ProgID}\shell\ExtractIconHanler key and calling
//     CoCreateInstance requesting for IExtractIcon interface).
//  (2) Calls IExtractIcon::GetIconLocation.
//  (3) Then, calls IExtractIcon::Extract with the location/index pair.
//  (4) If (3) returns NOERROR, it uses the returned icon.
//  (5) Otherwise, it recursively calls this logic with new location
//     assuming that the location string contains a fully qualified path name.
//
//  From extension programmer's point of view, there are only two cases
// where they provide implementations of IExtractIcon:
//  Case-1) providing explorer extensions (i.e., IShellFolder).
//  Case-2) providing per-instance icons for some types of files.
//
// Because Case-1 is described above, we'll explain only Case-2 here.
//
// When the shell is about display an icon for a file, it does following:
//  (1) Finds its ProgID and ClassID.
//  (2) If the file has a ClassID, it gets the icon location string from the
//    "DefaultIcon" key under it. The string indicates either per-class
//    icon (e.g., "FOOBAR.DLL,2") or per-instance icon (e.g., "%1,1").
//  (3) If a per-instance icon is specified, the shell creates an icon
//    extraction handler object for it, and extracts the icon from it
//    (which is described above).
//
//  It is important to note that the shell calls IExtractIcon::GetIconLocation
// first, then calls IExtractIcon::Extract. Most application programs
// that support per-instance icons will probably store an icon location
// (DLL/EXE name and index/id) rather than an icon image in each file.
// In those cases, a programmer needs to implement only the GetIconLocation
// member and it Extract member simply returns S_FALSE. They need to
// implement Extract member only if they decided to store the icon images
// within files themselved or some other database (which is very rare).
//
//
//
// [Member functions]
//
//
// IExtractIcon::GetIconLocation
//
//  This function returns an icon location.
//
//  Parameters:
//   uFlags     [in]  -- Specifies if it is opened or not (GIL_OPENICON or 0)
//   szIconFile [out] -- Specifies the string buffer buffer for a location name.
//   cchMax     [in]  -- Specifies the size of szIconFile (almost always MAX_PATH)
//   piIndex    [out] -- Sepcifies the address of UINT for the index.
//   pwFlags    [out] -- Returns GIL_* flags
//  Returns:
//   NOERROR, if it returns a valid location; S_FALSE, if the shell use a
//   default icon.
//
//  Notes: The location may or may not be a path to a file. The caller can
//   not assume anything unless the subsequent Extract member call returns
//   S_FALSE.
//
//   if the returned location is not a path to a file, GIL_NOTFILENAME should
//   be set in the returned flags.
//
// IExtractIcon::Extract
//
//  This function extracts an icon image from a specified file.
//
//  Parameters:
//   pszFile [in] -- Specifies the icon location (typically a path to a file).
//   nIconIndex [in] -- Specifies the icon index.
//   phiconLarge [out] -- Specifies the HICON variable for large icon.
//   phiconSmall [out] -- Specifies the HICON variable for small icon.
//   nIconSize [in] -- Specifies the size icon required (size of large icon)
//                     LOWORD is the requested large icon size
//                     HIWORD is the requested small icon size
//  Returns:
//   NOERROR, if it extracted the from the file.
//   S_FALSE, if the caller should extract from the file specified in the
//           location.
//
//===========================================================================

// GetIconLocation() input flags

const
  GIL_OPENICON     = $0001;      // allows containers to specify an "open" look
  GIL_FORSHELL     = $0002;      // icon is to be displayed in a ShellFolder


// GetIconLocation() return flags

const
  GIL_SIMULATEDOC  = $0001;      // simulate this document icon for this
  GIL_PERINSTANCE  = $0002;      // icons from this class are per instance (each file has its own)
  GIL_PERCLASS     = $0004;      // icons from this class per class (shared for all files of this type)
  GIL_NOTFILENAME  = $0008;      // location is not a filename, must call ::Extract
  GIL_DONTCACHE    = $0010;      // this icon should not be cached


type
  IExtractIcon =
    class(IUnknown)
      function GetIconLocation
                 (
                   Flags       : UINT;
                   IconFile    : pchar;
                   cchMax      : UINT;
                   var index   : integer;
                   var pwFlags : UINT
                 ) : HRESULT;   virtual; stdcall; abstract;
      function Extract
                 (
                   FileName      : pchar;
			             Index         : UINT;
			             var LargeIcon : HIcon;
                   var SmallIcon : HIcon;
                   IconSize      : UINT
                 ) : HRESULT;   virtual; stdcall; abstract;
    end;


//==========================================================================
//
// IShellBrowser/IShellView/IShellFolder interface
//
//  These three interfaces are used when the shell communicates with
// name space extensions. The shell (explorer) provides IShellBrowser
// interface, and extensions implements IShellFolder and IShellView
// interfaces.
//
//==========================================================================


//--------------------------------------------------------------------------
//
// Command/menuitem IDs
//
//  The explorer dispatches WM_COMMAND messages based on the range of
// command/menuitem IDs. All the IDs of menuitems that the view (right
// pane) inserts must be in FCIDM_SHVIEWFIRST/LAST (otherwise, the explorer
// won't dispatch them). The view should not deal with any menuitems
// in FCIDM_BROWSERFIRST/LAST (otherwise; it won't work with the future
// version of the shell).
//
//  FCIDM_SHVIEWFIRST/LAST      for the right pane (IShellView)
//  FCIDM_BROWSERFIRST/LAST     for the explorer frame (IShellBrowser)
//  FCIDM_GLOBAL/LAST           for the explorer's submenu IDs
//
//--------------------------------------------------------------------------

  const
    FCIDM_SHVIEWFIRST  = $0000;
    FCIDM_SHVIEWLAST   = $7fff;
    FCIDM_BROWSERFIRST = $a000;
    FCIDM_BROWSERLAST  = $bf00;
    FCIDM_GLOBALFIRST  = $8000;
    FCIDM_GLOBALLAST   = $9fff;

//
// Global submenu IDs and separator IDs
//

  const
    FCIDM_MENU_FILE             = (FCIDM_GLOBALFIRST + $0000);
    FCIDM_MENU_EDIT             = (FCIDM_GLOBALFIRST + $0040);
    FCIDM_MENU_VIEW             = (FCIDM_GLOBALFIRST + $0080);
    FCIDM_MENU_VIEW_SEP_OPTIONS = (FCIDM_GLOBALFIRST + $0081);
    FCIDM_MENU_TOOLS            = (FCIDM_GLOBALFIRST + $00c0);
    FCIDM_MENU_TOOLS_SEP_GOTO   = (FCIDM_GLOBALFIRST + $00c1);
    FCIDM_MENU_HELP             = (FCIDM_GLOBALFIRST + $0100);
    FCIDM_MENU_FIND             = (FCIDM_GLOBALFIRST + $0140);
    FCIDM_MENU_EXPLORE          = (FCIDM_GLOBALFIRST + $0150);
    FCIDM_MENU_FAVORITES        = (FCIDM_GLOBALFIRST + $0170);

//--------------------------------------------------------------------------
// control IDs known to the view
//--------------------------------------------------------------------------

    FCIDM_TOOLBAR = (FCIDM_BROWSERFIRST + 0);
    FCIDM_STATUS  = (FCIDM_BROWSERFIRST + 1);


//--------------------------------------------------------------------------
//
// FOLDERSETTINGS
//
//  FOLDERSETTINGS is a data structure that explorer passes from one folder
// view to another; when the user is browsing. It calls ISV::GetCurrentInfo
// member to get the current settings and pass it to ISV::CreateViewWindow
// to allow the next folder view "inherit" it. These settings assumes a
// particular UI (which the shell's folder view has); and shell extensions
// may or may not use those settings.
//
//--------------------------------------------------------------------------

  type
    PViewSettings = PByte;

// NB Bitfields.
// FWF_DESKTOP implies FWF_TRANSPARENT/NOCLIENTEDGE/NOSCROLL

  const
    FWF_AUTOARRANGE         = $0001;
    FWF_ABBREVIATEDNAMES    = $0002;
    FWF_SNAPTOGRID          = $0004;
    FWF_OWNERDATA           = $0008;
    FWF_BESTFITWINDOW       = $0010;
    FWF_DESKTOP             = $0020;
    FWF_SINGLESEL           = $0040;
    FWF_NOSUBFOLDERS        = $0080;
    FWF_TRANSPARENT         = $0100;
    FWF_NOCLIENTEDGE        = $0200;
    FWF_NOSCROLL            = $0400;
    FWF_ALIGNLEFT           = $0800;
    FWF_SINGLECLICKACTIVATE = $8000;  // TEMPORARY -- NO UI FOR THIS

  const
    FVM_ICON      = 1;
    FVM_SMALLICON = 2;
    FVM_LIST      = 3;
    FVM_DETAILS   = 4;

  type
    PFolderSettings = ^TFolderSettings;
    TFolderSettings =
      packed record
        ViewMode : UINT; // View mode (FOLDERVIEWMODE values)
        Flags    : UINT; // View options (FOLDERFLAGS bits)
      end;

//--------------------------------------------------------------------------
//
// Interface:   IShellBrowser
//
//  IShellBrowser interface is the interface that is provided by the shell
// explorer/folder frame window. When it creates the "contents pane" of
// a shell folder (which provides IShellFolder interface); it calls its
// CreateViewObject member function to create an IShellView object. Then;
// it calls its CreateViewWindow member to create the "contents pane"
// window. The pointer to the IShellBrowser interface is passed to
// the IShellView object as a parameter to this CreateViewWindow member
// function call.
//
//    +--------------------------+  <-- Explorer window
//    | [] Explorer              |
//    |--------------------------+       IShellBrowser
//    | File Edit View ..        |
//    |--------------------------|
//    |        |                 |
//    |        |              <-------- Content pane
//    |        |                 |
//    |        |                 |       IShellView
//    |        |                 |
//    |        |                 |
//    +--------------------------+
//
//
//
// [Member functions]
//
//
// IShellBrowser::GetWindow(phwnd)
//
//   Inherited from IOleWindow::GetWindow.
//
//
// IShellBrowser::ContextSensitiveHelp(fEnterMode)
//
//   Inherited from IOleWindow::ContextSensitiveHelp.
//
//
// IShellBrowser::InsertMenusSB(hmenuShared; lpMenuWidths)
//
//   Similar to the IOleInPlaceFrame::InsertMenus. The explorer will put
//  "File" and "Edit" pulldown in the File menu group; "View" and "Tools"
//  in the Container menu group and "Help" in the Window menu group. Each
//  pulldown menu will have a uniqu ID; FCIDM_MENU_FILE/EDIT/VIEW/TOOLS/HELP.
//  The view is allowed to insert menuitems into those sub-menus by those
//  IDs must be between FCIDM_SHVIEWFIRST and FCIDM_SHVIEWLAST.
//
//
// IShellBrowser::SetMenuSB(hmenuShared; holemenu; hwndActiveObject)
//
//   Similar to the IOleInPlaceFrame::SetMenu. The explorer ignores the
//  holemenu parameter (reserved for future enhancement)  and performs
//  menu-dispatch based on the menuitem IDs (see the description above).
//  It is important to note that the explorer will add different
//  set of menuitems depending on whether the view has a focus or not.
//  Therefore; it is very important to call ISB::OnViewWindowActivate
//  whenever the view window (or its children) gets the focus.
//
//
// IShellBrowser::RemoveMenusSB(hmenuShared)
//
//   Same as the IOleInPlaceFrame::RemoveMenus.
//
//
// IShellBrowser::SetStatusTextSB(lpszStatusText)
//
//   Same as the IOleInPlaceFrame::SetStatusText. It is also possible to
//  send messages directly to the status window via SendControlMsg.
//
//
// IShellBrowser::EnableModelessSB(fEnable)
//
//   Same as the IOleInPlaceFrame::EnableModeless.
//
//
// IShellBrowser::TranslateAcceleratorSB(lpmsg; wID)
//
//   Same as the IOleInPlaceFrame::TranslateAccelerator; but will be
//  never called because we don't support EXEs (i.e.; the explorer has
//  the message loop). This member function is defined here for possible
//  future enhancement.
//
//
// IShellBrowser::BrowseObject(pidl; wFlags)
//
//   The view calls this member to let shell explorer browse to another
//  folder. The pidl and wFlags specifies the folder to be browsed.
//
//  Following three flags specifies whether it creates another window or not.
//   SBSP_SAMEBROWSER  -- Browse to another folder with the same window.
//   SBSP_NEWBROWSER   -- Creates another window for the specified folder.
//   SBSP_DEFBROWSER   -- Default behavior (respects the view option).
//
//  Following three flags specifies open; explore; or default mode. These   .
//  are ignored if SBSP_SAMEBROWSER or (SBSP_DEFBROWSER && (single window   .
//  browser || explorer)).                                                  .
//   SBSP_OPENMODE     -- Use a normal folder window
//   SBSP_EXPLOREMODE  -- Use an explorer window
//   SBSP_DEFMODE      -- Use the same as the current window
//
//  Following three flags specifies the pidl.
//   SBSP_ABSOLUTE -- pidl is an absolute pidl (relative from desktop)
//   SBSP_RELATIVE -- pidl is relative from the current folder.
//   SBSP_PARENT   -- Browse the parent folder (ignores the pidl)
//
//
// IShellBrowser::GetViewStateStream(grfMode; ppstm)
//
//   The browser returns an IStream interface as the storage for view
//  specific state information.
//
//   grfMode -- Specifies the read/write access (STGM_READ/WRITE/READWRITE)
//   ppstm   -- Specifies the LPSTREAM variable to be filled.
//
//
// IShellBrowser::GetControlWindow(id; phwnd)
//
//   The shell view may call this member function to get the window handle
//  of Explorer controls (toolbar or status winodw -- FCW_TOOLBAR or
//  FCW_STATUS).
//
//
// IShellBrowser::SendControlMsg(id; uMsg; wParam; lParam; pret)
//
//   The shell view calls this member function to send control messages to
//  one of Explorer controls (toolbar or status window -- FCW_TOOLBAR or
//  FCW_STATUS).
//
//
// IShellBrowser::QueryActiveShellView(IShellView * ppshv)
//
//   This member returns currently activated (displayed) shellview object.
//  A shellview never need to call this member function.
//
//
// IShellBrowser::OnViewWindowActive(pshv)
//
//   The shell view window calls this member function when the view window
//  (or one of its children) got the focus. It MUST call this member before
//  calling IShellBrowser::InsertMenus; because it will insert different
//  set of menu items depending on whether the view has the focus or not.
//
//
// IShellBrowser::SetToolbarItems(lpButtons; nButtons; uFlags)
//
//   The view calls this function to add toolbar items to the exporer's
//  toolbar. "lpButtons" and "nButtons" specifies the array of toolbar
//  items. "uFlags" must be one of FCT_MERGE; FCT_CONFIGABLE; FCT_ADDTOEND.
//
//-------------------------------------------------------------------------

//
// Values for wFlags parameter of ISB::BrowseObject() member.
//

  const
    SBSP_DEFBROWSER            = $0000;
    SBSP_SAMEBROWSER           = $0001;
    SBSP_NEWBROWSER            = $0002;

    SBSP_DEFMODE               = $0000;
    SBSP_OPENMODE              = $0010;
    SBSP_EXPLOREMODE           = $0020;

    SBSP_ABSOLUTE              = $0000;
    SBSP_RELATIVE              = $1000;
    SBSP_PARENT                = $2000;

    SBSP_INITIATEDBYHLINKFRAME = $80000000;
    SBSP_REDIRECT              = $40000000;

//
// Values for id parameter of ISB::GetWindow/SendControlMsg members.
//
// WARNING:
//  Any shell extensions which sends messages to those control windows
// might not work in the future version of windows. If you really need
// to send messages to them; (1) don't assume that those control window
// always exist (i.e. GetControlWindow may fail) and (2) verify the window
// class of the window before sending any messages.
//

  const
    FCW_STATUS  = $0001;
    FCW_TOOLBAR = $0002;
    FCW_TREE    = $0003;

//
// Values for uFlags paremeter of ISB::SetToolbarItems member.
//

  const
    FCT_MERGE      = $0001;
    FCT_CONFIGABLE = $0002;
    FCT_ADDTOEND   = $0004;

  const
    SBSC_HIDE   = 0;
    SBSC_SHOW   = 1;
    SBSC_TOGGLE = 2;
    SBSC_QUERY  = 3;

// CommandTarget ids.

  const
    SBCMDID_ENABLESHOWTREE   = 0;
    SBCMDID_SHOWCONTROL      = 1; // variant vt_i4 = loword = FCW_* hiword = SBSC_*
    SBCMDID_CANCELNAVIGATION = 2; // cancel last navigation
    SBCMDID_MAYSAVECHANGES   = 3; // about to close and may save changes
    SBCMDID_SETHLINKFRAME    = 4; // variant vt_i4 = phlinkframe
    SBCMDID_ENABLESTOP       = 5; // variant vt_bool = fEnable
    SBCMDID_OPTIONS          = 6; // the view.options page

  const
    CDBOSC_SETFOCUS  = $00000000;
    CDBOSC_KILLFOCUS = $00000001;
    CDBOSC_SELCHANGE = $00000002;
    CDBOSC_RENAME    = $00000003;

//
// shellview select item flags
//
  const
    SVSI_DESELECT       = $0000;
    SVSI_SELECT         = $0001;
    SVSI_EDIT           = $0003;  // includes select
    SVSI_DESELECTOTHERS = $0004;
    SVSI_ENSUREVISIBLE  = $0008;
    SVSI_FOCUSED        = $0010;

//
// shellview get item object flags
//

  const
    SVGIO_BACKGROUND = $00000000;
    SVGIO_SELECTION  = $00000001;
    SVGIO_ALLVIEW    = $00000002;

//
// uState values for IShellView::UIActivate
//

  const
    SVUIA_DEACTIVATE       = 0;
    SVUIA_ACTIVATE_NOFOCUS = 1;
    SVUIA_ACTIVATE_FOCUS   = 2;
    SVUIA_INPLACEACTIVATE  = 3;          // new flag for IShellView2

  type
    IShellView = class;
    IShellBrowser =
      class( IOleWindow )
        function InsertMenusSB( hmenuShared : HMENU; MenuWidths : POleMenuGroupWidths ) : HResult; virtual; stdcall; abstract;
        function SetMenuSB( hmenuShared : HMENU; holemenuReserved : HMENU; hwndActiveObject : HWND ) : HResult; virtual; stdcall; abstract;
        function RemoveMenusSB( hmenuShared : HMENU ) : HResult; virtual; stdcall; abstract;
        function SetStatusTextSB( lpszStatusText : POleStr ) : HResult; virtual; stdcall; abstract;
        function EnableModelessSB( fEnable : BOOL ) : HResult; virtual; stdcall; abstract;
        function TranslateAcceleratorSB( var msg : TMsg; wID : WORD ) : HResult; virtual; stdcall; abstract;
        function BrowseObject( pidl : PitemIdList; wFlags : UINT ) : HResult; virtual; stdcall; abstract;
        function GetViewStateStream( grfMode : DWORD; var ppStrm : IStream ) : HResult; virtual; stdcall; abstract;
        function GetControlWindow( id : UINT; var wnd : HWND ) : HResult; virtual; stdcall; abstract;
        function SendControlMsg( id : UINT; uMsg : UINT; wParameter : WPARAM; lParameter : LPARAM; var ret : LRESULT ) : HResult; virtual; stdcall; abstract;
        function QueryActiveShellView( var shv : IShellView ) : HResult; virtual; stdcall; abstract;
        function OnViewWindowActive( shv : IShellView ) : HResult; virtual; stdcall; abstract;
        function SetToolbarItems( Buttons : PTBBUTTON; nButtons : UINT; Flags : UINT ) : HResult; virtual; stdcall; abstract;
      end;

//-------------------------------------------------------------------------
// ICommDlgBrowser interface
//
//  ICommDlgBrowser interface is the interface that is provided by the new
// common dialog window to hook and modify the behavior of IShellView.  When
// a default view is created; it queries its parent IShellBrowser for the
// ICommDlgBrowser interface.  If supported; it calls out to that interface
// in several cases that need to behave differently in a dialog.
//
// Member functions:
//
//  ICommDlgBrowser::OnDefaultCommand()
//    Called when the user double-clicks in the view or presses Enter.  The
//   browser should return S_OK if it processed the action itself; S_FALSE
//   to let the view perform the default action.
//
//  ICommDlgBrowser::OnStateChange(ULONG uChange)
//    Called when some states in the view change.  'uChange' is one of the
//   CDBOSC_* values.  This call is made after the state (selection; focus;
//   etc) has changed.  There is no return value.
//
//  ICommDlgBrowser::IncludeObject(LPCITEMIDLIST pidl)
//    Called when the view is enumerating objects.  'pidl' is a relative
//   IDLIST.  The browser should return S_OK to include the object in the
//   view; S_FALSE to hide it
//
//-------------------------------------------------------------------------

  ICommDlgBrowser =
    class( IUnknown )
      function OnDefaultCommand( shv : IShellView ) : HResult; virtual; stdcall; abstract;
      function OnStateChange( shv : IShellView; uChange : ULONG) : HResult; virtual; stdcall; abstract;
      function IncludeObject( shv : IShellView; pidl : PItemIdList ) : HResult; virtual; stdcall; abstract;
    end;

//==========================================================================
//
// Interface:   IShellView
//
// IShellView::GetWindow(phwnd)
//
//   Inherited from IOleWindow::GetWindow.
//
//
// IShellView::ContextSensitiveHelp(fEnterMode)
//
//   Inherited from IOleWindow::ContextSensitiveHelp.
//
//
// IShellView::TranslateAccelerator(lpmsg)
//
//   Similar to IOleInPlaceActiveObject::TranlateAccelerator. The explorer
//  calls this function BEFORE any other translation. Returning S_OK
//  indicates that the message was translated (eaten) and should not be
//  translated or dispatched by the explorer.
//
//
// IShellView::EnableModeless(fEnable)
//   Similar to IOleInPlaceActiveObject::EnableModeless.
//
//
// IShellView::UIActivate(uState)
//
//   The explorer calls this member function whenever the activation
//  state of the view window is changed by a certain event that is
//  NOT caused by the shell view itself.
//
//   SVUIA_DEACTIVATE will be passed when the explorer is about to
//  destroy the shell view window; the shell view is supposed to remove
//  all the extended UIs (typically merged menu and modeless popup windows).
//
//   SVUIA_ACTIVATE_NOFOCUS will be passsed when the shell view is losing
//  the input focus or the shell view has been just created without the
//  input focus; the shell view is supposed to set menuitems appropriate
//  for non-focused state (no selection specific items should be added).
//
//   SVUIA_ACTIVATE_FOCUS will be passed when the explorer has just
//  created the view window with the input focus; the shell view is
//  supposed to set menuitems appropriate for focused state.
//
//   SVUIA_INPLACEACTIVATE(new) will be passed when the shell view is opened
//  within an ActiveX control; which is not a UI active. In this case;
//  the shell view should not merge menus or put toolbas. To be compatible
//  with Win95 client; we don't pass this value unless the view supports
//  IShellView2.
//
//   The shell view should not change focus within this member function.
//  The shell view should not hook the WM_KILLFOCUS message to remerge
//  menuitems. However; the shell view typically hook the WM_SETFOCUS
//  message; and re-merge the menu after calling IShellBrowser::
//  OnViewWindowActivated.
//
//
// IShellView::Refresh()
//
//   The explorer calls this member when the view needs to refresh its
//  contents (such as when the user hits F5 key).
//
//
// IShellView::CreateViewWindow
//
//   This member creates the view window (right-pane of the explorer or the
//  client window of the folder window).
//
//
// IShellView::DestroyViewWindow
//
//   This member destroys the view window.
//
//
// IShellView::GetCurrentInfo
//
//   This member returns the folder settings.
//
//
// IShellView::AddPropertySHeetPages
//
//   The explorer calls this member when it is opening the option property
//  sheet. This allows the view to add additional pages to it.
//
//
// IShellView::SaveViewState()
//
//   The explorer calls this member when the shell view is supposed to
//  store its view settings. The shell view is supposed to get a view
//  stream by calling IShellBrowser::GetViewStateStream and store the
//  current view state into that stream.
//
//
// IShellView::SelectItem(pidlItem; uFlags)
//
//   The explorer calls this member to change the selection state of
//  item(s) within the shell view window.  If pidlItem is NULL and uFlags
//  is SVSI_DESELECTOTHERS; all items should be deselected.
//
//-------------------------------------------------------------------------

  IShellView =
    class( IOleWindow )
      function TranslateAccelerator( var msg : TMsg ) : HResult; virtual; stdcall; abstract;
      function EnableModeless( fEnable : BOOL ) : HResult; virtual; stdcall; abstract;
      function UIActivate( uState : UINT ) : HResult; virtual; stdcall; abstract;
      function Refresh : HResult; virtual; stdcall; abstract;

      function CreateViewWindow( PrevView : IShellView; var fs : TFolderSettings; psb : IShellBrowser; var prcView : TRect; var Wnd : HWnd ) : HResult; virtual; stdcall; abstract;
      function DestroyViewWindow : HResult; virtual; stdcall; abstract;
      function GetCurrentInfo( var fs : TFolderSettings ) : HResult; virtual; stdcall; abstract;
      function AddPropertySheetPages( dwReserved : DWORD; lpfn : TFNAddPropSheetPage; lparameter : LPARAM ) : HResult; virtual; stdcall; abstract;
      function SaveViewState : HResult; virtual; stdcall; abstract;
      function SelectItem( pidlItem : PItemIdList; uFlags : UINT ) : HResult; virtual; stdcall; abstract;
      function GetItemObject( uItem : UINT; riid : TIID; var ppv : pointer ) : HResult; virtual; stdcall; abstract;
    end;

  PSV2CVW2_PARAMS = ^TSV2CVW2_PARAMS;
  TSV2CVW2_PARAMS =
    packed record
      cbSize   : DWORD;
      Prev     : IShellView;
      pfs      : PFolderSettings;
      Owner    : IShellBrowser;
      rView    : PRect;
      pvid     : TIID;
      hwndView : HWND;
    end;

  IShellView2 =
    class( IShellView )
      function GetView( pvid : PIID; uView : ULONG ) : HResult; virtual; stdcall; abstract;
      function CreateViewWindow2( lpParams : PSV2CVW2_PARAMS ) : HResult; virtual; stdcall; abstract;
    end;

implementation

end.
