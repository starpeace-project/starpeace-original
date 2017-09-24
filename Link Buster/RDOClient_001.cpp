// Created by Microsoft (R) C/C++ Compiler Version 13.00.9254 (cfa68b0f).
//
// w:\newwork\iroel\spservice\srvsp\debug\RDOClient.tli
//
// Wrapper implementations for Win32 type library C:\IcqUser\Iroel\RDOClient\RDOClient.dll
// compiler-generated file created 12/22/01 at 00:13:27 - DO NOT EDIT!

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
