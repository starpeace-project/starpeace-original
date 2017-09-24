unit URLUtils;

interface

  uses
    Windows;

  const
    InvalidTime : TFileTime = ( dwLowDateTime : 0; dwHighDateTime : 0 );

  type
    TDownloadNotifyProc = function ( Progress: Longint; ProgressMax : Longint; StatusCode: Longint; StatusText: PWideChar ) : HRESULT of object;

  function GetURLLastModified( const URL : string ) : TFileTime;
  function DownloadURLToFile( const URL, Dest : string; NotifyProc : TDownloadNotifyProc ) : boolean;
  function DownloadURLToCacheFile( const URL : string; NotifyProc : TDownloadNotifyProc ) : string;

implementation

  uses
    ActiveX, UrlMon, SysUtils;

  type
    TBindStatusCallback =
      class( TInterfacedObject, IBindStatusCallback)
        public
          constructor Create( NotifyProc : TDownloadNotifyProc );

        private
          fNotifyProc : TDownloadNotifyProc;

        private
          function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
          function GetPriority(out nPriority): HResult; stdcall;
          function OnLowResource(reserved: DWORD): HResult; stdcall;
          function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult; stdcall;
          function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
          function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
          function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc; stgmed: PStgMedium): HResult; stdcall;
          function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;
      end;

  constructor TBindStatusCallback.Create( NotifyProc : TDownloadNotifyProc );
    begin
      inherited Create;
      fNotifyProc := NotifyProc;
    end;

  function TBindStatusCallback.OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; 
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.GetPriority(out nPriority): HResult; 
    begin
      integer(nPriority) := THREAD_PRIORITY_NORMAL;
      Result := S_OK;
    end;

  function TBindStatusCallback.OnLowResource(reserved: DWORD): HResult; 
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult; 
    begin
      if Assigned( fNotifyProc )
        then Result := fNotifyProc( ulProgress, ulProgressMax, ulStatusCode, szStatusText )
        else Result := S_OK;
    end;

  function TBindStatusCallback.OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; 
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; 
    begin
      Result := E_INVALIDARG;
    end;

  function TBindStatusCallback.OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc; stgmed: PStgMedium): HResult; 
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; 
    begin
      Result := S_OK;
    end;

  function GetURLLastModified( const URL : string ) : TFileTime;
    var
      Moniker : IMoniker;
      WideURL : widestring;
      BindCtx : IBindCtx;
      hRes    : HRESULT;
    begin
      WideURL := URL;
      if CreateURLMoniker( nil, pwidechar(WideURL), Moniker ) = S_OK
        then
          begin
            if CreateBindCtx( 0, BindCtx ) = S_OK
              then
                begin
                  hRes := Moniker.GetTimeOfLastChange( BindCtx, nil, Result );
                  if hRes <> S_OK
                    then Result := InvalidTime;
                end
              else Result := InvalidTime;
          end
        else Result := InvalidTime;
    end;

  function DownloadURLToFile( const URL, Dest : string; NotifyProc : TDownloadNotifyProc ) : boolean;
    var
     StatusCallback : TBindStatusCallback;
    begin
      StatusCallback := TBindStatusCallback.Create( NotifyProc );
      Result := URLDownloadToFile( nil, pchar(URL), pchar(Dest), 0, StatusCallback ) = S_OK;
    end;

  function DownloadURLToCacheFile( const URL : string; NotifyProc : TDownloadNotifyProc ) : string;
    var
     StatusCallback : TBindStatusCallback;
     Storage        : array[0..pred(MAX_PATH)] of char;
    begin
      StatusCallback := TBindStatusCallback.Create( NotifyProc );
      if URLDownloadToCacheFile( nil, pchar(URL), pchar(@Storage), MAX_PATH, 0, StatusCallback ) = S_OK
        then Result := Storage
        else Result := '';
    end;

end.
