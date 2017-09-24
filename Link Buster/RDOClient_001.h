#include <comdef.h>

struct __declspec(uuid("58c4cb00-873a-11d1-af26-008029e5ca8c"))
/* LIBID */ __RDOClient;
struct __declspec(uuid("58c4cb01-873a-11d1-af26-008029e5ca8c"))
/* dual interface */ IRDOConnectionInit;
struct /* coclass */ WinSockRDOConnection;
struct __declspec(uuid("dba984a0-8c05-11d1-af26-008029e5ca8c"))
/* dual interface */ IRDOObjectProxy;
struct /* coclass */ RDOObjectProxy;

//
// Smart pointer typedef declarations
//

_COM_SMARTPTR_TYPEDEF(IRDOConnectionInit, __uuidof(IRDOConnectionInit));
_COM_SMARTPTR_TYPEDEF(IRDOObjectProxy, __uuidof(IRDOObjectProxy));

//
// Type library items
//

struct __declspec(uuid("58c4cb01-873a-11d1-af26-008029e5ca8c"))
IRDOConnectionInit : IDispatch
{
    //
    // Property data
    //

    __declspec(property(get=GetServer,put=PutServer))
    _bstr_t Server;
    __declspec(property(get=GetPort,put=PutPort))
    long Port;

    //
    // Wrapper methods for error-handling
    //

    _bstr_t GetServer ( );
    void PutServer (
        _bstr_t Value );
    long GetPort ( );
    void PutPort (
        long Value );
    VARIANT_BOOL Connect (
        long TimeOut );
    HRESULT Disconnect ( );

    //
    // Raw methods provided by interface
    //

      virtual HRESULT __stdcall get_Server (
        /*[out,retval]*/ BSTR * Value ) = 0;
      virtual HRESULT __stdcall put_Server (
        /*[in]*/ BSTR Value ) = 0;
      virtual HRESULT __stdcall get_Port (
        /*[out,retval]*/ long * Value ) = 0;
      virtual HRESULT __stdcall put_Port (
        /*[in]*/ long Value ) = 0;
      virtual HRESULT __stdcall raw_Connect (
        /*[in]*/ long TimeOut,
        /*[out,retval]*/ VARIANT_BOOL * TimeOut1 ) = 0;
      virtual HRESULT __stdcall raw_Disconnect ( ) = 0;
};

struct __declspec(uuid("58c4cb02-873a-11d1-af26-008029e5ca8c"))
WinSockRDOConnection;
    // [ default ] interface IRDOConnectionInit

struct __declspec(uuid("dba984a0-8c05-11d1-af26-008029e5ca8c"))
IRDOObjectProxy : IDispatch
{};

struct __declspec(uuid("dba984a1-8c05-11d1-af26-008029e5ca8c"))
RDOObjectProxy;
    // [ default ] interface IRDOObjectProxy

//
// Wrapper method implementations

#pragma once

//
// interface IRDOConnectionInit wrapper method implementations
//

inline _bstr_t IRDOConnectionInit::GetServer ( ) {
    BSTR _result = 0;
    HRESULT _hr = get_Server(&_result);
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
    return _bstr_t(_result, false);
}

inline void IRDOConnectionInit::PutServer ( _bstr_t Value ) {
    HRESULT _hr = put_Server(Value);
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
}

inline long IRDOConnectionInit::GetPort ( ) {
    long _result = 0;
    HRESULT _hr = get_Port(&_result);
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
    return _result;
}

inline void IRDOConnectionInit::PutPort ( long Value ) {
    HRESULT _hr = put_Port(Value);
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
}

inline VARIANT_BOOL IRDOConnectionInit::Connect ( long TimeOut ) {
    VARIANT_BOOL _result = 0;
    HRESULT _hr = raw_Connect(TimeOut, &_result);
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
    return _result;
}

inline HRESULT IRDOConnectionInit::Disconnect ( ) {
    HRESULT _hr = raw_Disconnect();
    if (FAILED(_hr)) _com_issue_errorex(_hr, this, __uuidof(this));
    return _hr;
}
