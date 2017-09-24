Imports System.ComponentModel
Imports System.Drawing
Imports System.WinForms


Public Class Form1
    Inherits System.WinForms.Form

    Public Sub New()
        MyBase.New

        Form1 = Me

        'This call is required by the Win Form Designer.
        InitializeComponent

        'TODO: Add any initialization after the InitializeComponent() call
    End Sub

    'Form overrides dispose to clean up the component list.
    Overrides Public Sub Dispose()
        MyBase.Dispose
        components.Dispose
    End Sub 

#Region " Windows Form Designer generated code "

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.Container
    Private WithEvents ListBox1 As System.WinForms.ListBox
    
    Private WithEvents btnUnsubscribe As System.WinForms.Button
    Private WithEvents btnRefresh As System.WinForms.Button
    Private WithEvents tbAlias As System.WinForms.TextBox
    
    Dim WithEvents Form1 As System.WinForms.Form

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.btnUnsubscribe = New System.WinForms.Button()
        Me.tbAlias = New System.WinForms.TextBox()
        Me.ListBox1 = New System.WinForms.ListBox()
        Me.btnRefresh = New System.WinForms.Button()
        
        '@design Me.TrayHeight = 0
        '@design Me.TrayLargeIcon = False
        '@design Me.TrayAutoArrange = True
        btnUnsubscribe.Location = New System.Drawing.Point(272, 56)
        btnUnsubscribe.Size = New System.Drawing.Size(96, 24)
        btnUnsubscribe.TabIndex = 2
        btnUnsubscribe.Text = "Unsubscribe"
        
        tbAlias.Location = New System.Drawing.Point(8, 16)
        tbAlias.Text = "SP-Alias"
        tbAlias.TabIndex = 0
        tbAlias.Size = New System.Drawing.Size(248, 20)
        
        ListBox1.Location = New System.Drawing.Point(8, 48)
        ListBox1.Size = New System.Drawing.Size(248, 199)
        ListBox1.TabIndex = 3
        
        btnRefresh.Location = New System.Drawing.Point(272, 16)
        btnRefresh.Size = New System.Drawing.Size(96, 24)
        btnRefresh.TabIndex = 1
        btnRefresh.Text = "Refresh"
        Me.Text = "Subscription Master"

    End Sub

#End Region
    
    Private Function getProxy(ByVal DSAddr As String, ByVal port As Integer) As Object
        ' Code here    
        Dim Cnx As Object
        
        DSCnx = CreateObject("RDOClient.WinSockRDOConnection")
        DSCnx.Server = "dir.starpeace.net"
        DSCnx.Port = 2222
        
        If DSCnx.Connect(10000) Then
            DSProxy = Server.CreateObject("RDOClient.RDOObjectProxy")
            DSProxy.SetConnection(DSCnx)
            If DSProxy.BindTo("DirectoryServer") Then
                ss = DSProxy.RDOOpenSession
            End If
        End If

    End Function

    Protected Sub btnRefresh_Click(ByVal sender As Object, ByVal e As System.EventArgs)
        Dim Proxy As Object
        Proxy = getProxy("dir.starpeace.net", 2222)
    End Sub

End Class
