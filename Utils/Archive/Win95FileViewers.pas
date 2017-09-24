unit Win95FileViewers;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, ShellGUID;

  const  // FileViewerShowInfo Flags
    FVSIF_RECT      = $00000001;
    FVSIF_PINNED    = $00000002;
    FVSIF_NEWFAILED = $08000000;
    FVSIF_NEWFILE   = $80000000;
    FVSIF_CANVIEWIT = $40000000;

  type
    PFileViewerShowInfo = ^TFileViewerShowInfo;
    TFileViewerShowInfo =
      record
        Size    : dword;
        Owner   : HWND;
        Show    : integer;
        Flags   : dword;
        Rect    : TRect;
        UnkRel  : IUnknown;
        NewFile : array[0..MAX_PATH] of char;//TOleChar;
      end;

  type
    IFileViewerSite =
      class( IUnknown )
        public
          constructor Create( aWinHandle : HWND );

        public
          function QueryInterface( const iid : TIID; var obj) : HRESULT;   override; stdcall;
          function AddRef : longint;                                       override; stdcall;
          function Release : longint;                                      override; stdcall;

        public
          function SetPinnedWindow( aWinHandle : HWND ) : HRESULT;         virtual; stdcall;
          function GetPinnedWindow( var aWinHandle : HWND ) : HRESULT;     virtual; stdcall;

        public
          WinHandle : HWND;

        private
          RefCount  : integer;
      end;

  type
    IFileViewer =
      class( IUnknown)
        public
          function ShowInitialize( ViewerSite : IFileViewerSite ) : HRESULT; virtual; stdcall; abstract;
          function Show( ShowInfo : PFileViewerShowInfo) : HRESULT;          virtual; stdcall; abstract;
          function PrintTo( Driver : pchar; SuppressUI : BOOL) : HRESULT;    virtual; stdcall; abstract;
      end;

  function GetFileViewer( const aFileName : string; aSite : IFileViewerSite ) : IFileViewer;

  function AssociatedViewer( const ext : string; var ClassId : TCLSID ) : boolean;
  function ViewerLoader( ClassId : TCLSID) : IPersistFile;
  function CreateFileViewer( ClassId : TCLSID;
                             const aFileName : string; aSite : IFileViewerSite ) : IFileViewer;

  const
     qvTryQuickViewOnly     = 0;
     qvTryQuickViewFirst    = 1;
     qvTryQuickViewPlusOnly = 2;

  var
    QuickViewPreference    : integer = qvTryQuickViewOnly;
    QuickViewPlusInstalled : boolean;

implementation

  uses
    SysUtils, RegUtils, OLEUtils;

  function AssociatedViewer( const ext : string; var ClassId : TCLSID ) : boolean;
    var
      CLSID : TRegistryKey;
      hk    : HKEY;
      i     : integer;

    procedure TryViewer;
      begin
        i := 0;
        while RegEnumKey( hk, i, CLSID, sizeof(CLSID)) = ERROR_SUCCESS do
          inc( i );
        if i > 0
          then
            begin
              try
                ClassId := StrToCLSID( CLSID );
                Result  := true;
              except
                on EBadCLSID do
                  Result := false;
              end;
            end;
        RegCloseKey( hk );
      end;

    begin
      Result := false;
      if (QuickViewPreference < qvTryQuickViewPlusOnly) and
         (RegOpenKey( HKEY_CLASSES_ROOT, pchar('QuickView.Original\' + ext), hk) = ERROR_SUCCESS)
        then TryViewer;
      if (not Result)
         and ((QuickViewPreference <> qvTryQuickViewOnly) or (not QuickViewPlusInstalled))
         and (RegOpenKey( HKEY_CLASSES_ROOT, pchar('QuickView\' + ext), hk) = ERROR_SUCCESS)
        then TryViewer;
    end;

  function CreateSuitableViewer( const aFileName : string; aSite : IFileViewerSite ) : IFileViewer;
    var
      ClassId : TCLSID;
      CLSID   : TRegistryKey;
      hk      : HKEY;
      i       : integer;

    procedure TryViewers;
      begin
        i := 0;
        while (RegEnumKey( hk, i, CLSID, sizeof(CLSID)) = ERROR_SUCCESS) and (Result = nil) do
          begin
            inc( i );
            try
              ClassId := StrToCLSID( CLSID );
              Result := CreateFileViewer( ClassId, aFileName, aSite );
            except
              on EBadCLSID do
                Result := nil;
            end;
          end;
        RegCloseKey( hk );
      end;

    begin
      Result := nil;
      if (QuickViewPreference < qvTryQuickViewPlusOnly) and
         (RegOpenKey( HKEY_CLASSES_ROOT, 'QuickView.Original\*', hk) = ERROR_SUCCESS)
        then TryViewers;
      if (Result = nil)
         and ((QuickViewPreference <> qvTryQuickViewOnly) or (not QuickViewPlusInstalled))
         and (RegOpenKey( HKEY_CLASSES_ROOT, 'QuickView\*', hk) = ERROR_SUCCESS)
        then TryViewers;
    end;

  function ViewerLoader( ClassId : TCLSID ) : IPersistFile;
    begin
      if CoCreateInstance( ClassId, nil, CLSCTX_INPROC_SERVER, IID_IPersistFile, Result ) <> NO_ERROR
        then Result := nil;
    end;

  function CreateFileViewer( ClassId : TCLSID;
                             const aFileName : string; aSite : IFileViewerSite ) : IFileViewer;
    var
      PersistFile : IPersistFile;
      WideStr     : POleStr;
    begin
      PersistFile := ViewerLoader( ClassId );
      if PersistFile <> nil
        then
          begin
            GetMem( WideStr, (length( aFileName ) + 1) * sizeof( TOleChar ) );
            StringToWideChar( aFileName, WideStr, length( aFileName ) + 1 );
            if (PersistFile.Load( WideStr, STGM_READ or STGM_SHARE_DENY_NONE ) = NO_ERROR) and
               (PersistFile.QueryInterface( IID_IFileViewer, Result ) = NO_ERROR)
              then
                begin
                  if Result.ShowInitialize( aSite ) <> NO_ERROR
                    then
                      begin
                        Result.Release;
                        Result := nil;
                      end;
                end
              else Result := nil;
            PersistFile.Release;
            FreeMem( WideStr );
          end
        else Result := nil;
    end;

  function GetFileViewer( const aFileName : string; aSite : IFileViewerSite ) : IFileViewer;
    var
      Ext         : string;
      ClassId     : TCLSID;
    begin
      Ext := ExtractFileExt( aFileName );
      if AssociatedViewer( Ext, ClassId )
        then Result := CreateFileViewer( ClassId, aFileName, aSite)
        else Result := CreateSuitableViewer( aFileName, aSite );
    end;

  // IFileViewerSite

  constructor IFileViewerSite.Create( aWinHandle : HWND );
    begin
      WinHandle := aWinHandle;
    end;

  function IFileViewerSite.QueryInterface( const iid : TIID; var obj) : HRESULT;
    begin
      Result := E_NOTIMPL;
    end;

  function IFileViewerSite.AddRef : longint;
    begin
      inc( RefCount );
      Result := RefCount;
    end;

  function IFileViewerSite.Release : longint;
    begin
      dec( RefCount );
      Result := RefCount;
      if RefCount = 0
        then Free;
    end;

  function IFileViewerSite.SetPinnedWindow( aWinHandle : HWND ) : HRESULT;
    begin
      WinHandle := aWinHandle;
      Result    := NO_ERROR;
    end;

  function IFileViewerSite.GetPinnedWindow( var aWinHandle : HWND ) : HRESULT;
    begin
      aWinHandle := WinHandle;
      Result     := NO_ERROR;
    end;

var
  hk     : HKEY;
  QvpDll : string;

initialization
  if RegOpenKey( HKEY_LOCAL_MACHINE, 'Software\Inso\Quick View Plus', hk ) = ERROR_SUCCESS 
    then
      begin
        QvpDll := GetRegValue( hk, 'APIDLL' );
        QuickViewPlusInstalled := (QvpDll <> '') and FileExists( QvpDll );
        RegCloseKey( hk );
      end
    else QuickViewPlusInstalled := false;


end.

