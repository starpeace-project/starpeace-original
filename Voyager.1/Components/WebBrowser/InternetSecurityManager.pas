unit InternetSecurityManager;

interface

  uses
    Windows, ActiveX, URLMon2;

  type
    TInternetSecurityManager =
      class(TInterfacedObject, IInternetSecurityManager)
        private
          function SetSecuritySite(pSite : IInternetSecurityMgrSite) : HRESULT; stdcall;
          function GetSecuritySite(out ppSite : IInternetSecurityMgrSite) : HRESULT; stdcall;
          function MapUrlToZone(pwszUrl : PWideChar; out pdwZone : DWORD; dwFlags : DWORD) : HRESULT; stdcall;
          function GetSecurityId(pwszUrl : PWideChar; pbSecurityId : PByte; var pcbSecurityId : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
          function ProcessUrlAction(pwszUrl : PWideChar; dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwFlags : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
          function QueryCustomPolicy(pwszUrl : PWideChar; guidKey : PGUID; out ppPolicy : PByte; out pcbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
          function SetZoneMapping(dwZone : DWORD; lpszPattern : PWideChar; dwFlags : DWORD) : HRESULT; stdcall;
          function GetZoneMappings(dwZone : DWORD; out ppenumString : IEnumString; dwFlags : DWORD) : HRESULT; stdcall;
      end;

implementation

  function TInternetSecurityManager.SetSecuritySite(pSite : IInternetSecurityMgrSite) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.GetSecuritySite(out ppSite : IInternetSecurityMgrSite) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.MapUrlToZone(pwszUrl : PWideChar; out pdwZone : DWORD; dwFlags : DWORD) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.GetSecurityId(pwszUrl : PWideChar; pbSecurityId : PByte; var pcbSecurityId : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.ProcessUrlAction(pwszUrl : PWideChar; dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwFlags : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
    begin
      if cbPolicy >= sizeof(DWORD)
        then
          begin
            PDWORD(pPolicy)^ := URLPOLICY_ALLOW;
            Result := S_OK;
          end
        else Result := E_OUTOFMEMORY;
    end;

  function TInternetSecurityManager.QueryCustomPolicy(pwszUrl : PWideChar; guidKey : PGUID; out ppPolicy : PByte; out pcbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwReserved : DWORD) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.SetZoneMapping(dwZone : DWORD; lpszPattern : PWideChar; dwFlags : DWORD) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

  function TInternetSecurityManager.GetZoneMappings(dwZone : DWORD; out ppenumString : IEnumString; dwFlags : DWORD) : HRESULT; stdcall;
    begin
      Result := INET_E_DEFAULT_ACTION;
    end;

end.
