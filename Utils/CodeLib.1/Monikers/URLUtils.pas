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
          function OnStartBinding(dwReserved: Longint; pib: IBinding): HResult; stdcall;
          function GetPriority(out pnPriority: Longint): HResult; stdcall;
          function OnLowResource(reserved: Longint): HResult; stdcall;
          function OnProgress(ulProgress: Longint; ulProgressMax: Longint; ulStatusCode: Longint; szStatusText: PWideChar): HResult; stdcall;
          function OnStopBinding( hRes: HResult; szError: PWideChar ): HResult; stdcall;
          function GetBindInfo(out grfBINDF: Longint; var pbindinfo: TBindInfo): HResult; stdcall;
          function OnDataAvailable(grfBSCF: Longint; dwSize: Longint; var pformatetc: TFormatEtc; var pstgmed: TSTGMEDIUM): HResult; stdcall;
          function OnObjectAvailable(const iid: TGUID; const punk: IUnknown): HResult; stdcall;
      end;

  constructor TBindStatusCallback.Create( NotifyProc : TDownloadNotifyProc );
    begin
      inherited Create;
      fNotifyProc := NotifyProc;
    end;

  function TBindStatusCallback.OnStartBinding(dwReserved: Longint; pib: IBinding): HResult;
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.GetPriority(out pnPriority: Longint): HResult;
    begin
      pnPriority := THREAD_PRIORITY_NORMAL;
      Result := S_OK;
    end;

  function TBindStatusCallback.OnLowResource(reserved: Longint): HResult;
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.OnProgress(ulProgress: Longint; ulProgressMax: Longint; ulStatusCode: Longint; szStatusText: PWideChar): HResult;
    begin
      if Assigned( fNotifyProc )
        then Result := fNotifyProc( ulProgress, ulProgressMax, ulStatusCode, szStatusText )
        else Result := S_OK;
    end;

  function TBindStatusCallback.OnStopBinding( hRes: HResult; szError: PWideChar ): HResult;
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.GetBindInfo(out grfBINDF: Longint; var pbindinfo: TBindInfo): HResult;
    begin
      Result := E_INVALIDARG;
    end;

  function TBindStatusCallback.OnDataAvailable(grfBSCF: Longint; dwSize: Longint; var pformatetc: TFormatEtc; var pstgmed: TSTGMEDIUM): HResult;
    begin
      Result := S_OK;
    end;

  function TBindStatusCallback.OnObjectAvailable(const iid: TGUID; const punk: IUnknown): HResult;
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
