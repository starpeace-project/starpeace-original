<%@ WebService Class="FiveWS" %>

Imports System
Imports System.Web.Services

Public Class FiveWS : Inherits WebService

    Private Const IP = "216.191.227.164"
    Private Const Port = 2222
    Private Const TimeOut = 2000

    <WebMethod()> Public Function SPcheckName(ByVal name As String) As Boolean
        Try
            Dim WSDSCnx = Server.CreateObject("RDOClient.WinSockRDOConnection")
            WSDSCnx.Server = IP
            WSDSCnx.Port = Port

            Dim ISProxy = Server.CreateObject("RDOClient.RDOObjectProxy")

            If WSDSCnx.Connect(TimeOut) Then
                ISProxy.SetConnection(WSDSCnx)
                ISProxy.BindTo("DirectoryServer")
                ISProxy.TimeOut = TimeOut

                Dim session = ISProxy.RDOOpenSession
                If (session <> 0) And ISProxy.BindTo(session) Then
                    Try
                        Dim fullpath = ISProxy.RDOGetUserPath(name)
                        SPcheckName = ISProxy.RDOFullPathKeyExists(fullpath)
                    Finally
                        ISProxy.RDOEndSession()
                    End Try
                Else
                    SPcheckName = False
                End If
            Else
                SPcheckName = False
            End If
        Catch
            SPcheckName = False
        End Try
    End Function

    <WebMethod()> Public Function SPauthorize(ByVal name As String, ByVal password As String) As Integer
        Try
            Dim WSDSCnx = Server.CreateObject("RDOClient.WinSockRDOConnection")
            WSDSCnx.Server = IP
            WSDSCnx.Port = Port

            Dim ISProxy = Server.CreateObject("RDOClient.RDOObjectProxy")

            If WSDSCnx.Connect(TimeOut) Then
                ISProxy.SetConnection(WSDSCnx)
                ISProxy.BindTo("DirectoryServer")
                ISProxy.TimeOut = TimeOut

                Dim session = ISProxy.RDOOpenSession
                If (session <> 0) And ISProxy.BindTo(session) Then
                    Try
                        If ISProxy.RDOLogonUser(name, password) = 0 Then
                            SPauthorize = 1
                        Else
                            SPauthorize = 0
                        End If
                    Finally
                        ISProxy.RDOEndSession()
                    End Try
                Else
                    SPauthorize = -1
                End If
            Else
                SPauthorize = -1
            End If
        Catch
            SPauthorize = -1
        End Try
    End Function

End Class
