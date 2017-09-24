Module Module1

    Dim MsgBody As String

    Sub LoadMessage(ByVal path As String)
        Dim f As System.IO.StreamReader = System.IO.File.OpenText(path)
        MsgBody = f.ReadToEnd()
    End Sub

    Sub RunPromotion(ByVal promId As Integer)
        Dim RDOConn
        Dim Obj
        Dim key, status, expiry, email, username, ntfy
        Dim count = 0
        RDOConn = CreateObject("RDOClient.WinSockRDOConnection")
        RDOConn.Server = "dir.starpeace.net"
        RDOConn.Port = 2222
        If RDOConn.Connect(120000) Then
            Obj = CreateObject("RDOClient.RDOObjectProxy")
            Call Obj.SetConnection(RDOConn)
            If Obj.BindTo("DirectoryServer") Then
                Obj.WaitForAnswer = True
                Obj.TimeOut = 360000
                Dim Session
                Session = Obj.RDOOpenSession
                If Session <> 0 Then
                    Try
                        Obj.BindTo(Session)
                        Dim c As Int16
                        For c = AscW("a") To AscW("z")
                            key = "root/users/" & ChrW(c)
                            Obj.RDOCurrentKey = key
                            Dim keys As String = Obj.RDOGetKeyNames
                            Dim users() As String = Split(keys, vbCrLf)
                            Dim i As Integer
                            For i = 0 To users.Length - 1
                                key = Obj.RDOGetUserPath(users(i))
                                Obj.RDOCurrentKey = key
                                username = CStr(Obj.RDOReadString("Alias"))
                                status = CInt(Obj.RDOReadInteger("AccountStatus"))
                                expiry = CDbl(Obj.RDOReadDate("TrialExpires"))
                                email = CStr(Obj.RDOReadString("Email"))

                                Dim D As DateTime
                                D = #9/1/2002#
                                
                                If ((status = 0) Or (expiry < D.ToOADate)) And (email <> "") Then
                                    ntfy = CBool(Obj.RDOReadBoolean("notify"))
                                    If ntfy Then
                                        Console.WriteLine(CStr(count) & vbTab & users(i) & vbTab & vbTab & email)
                                        count = count + 1
                                        If False Then
                                            Try
                                                Dim myMail
                                                myMail = CreateObject("Persits.MailSender")
                                                myMail.Host = "mail.starpeace.net"
                                                myMail.Port = 25
                                                myMail.From = "promotion@starpeace.net"
                                                myMail.FromName = "Star Peace Promotion"
                                                myMail.AddAddress(email)
                                                myMail.Subject = "Special Star Peace Promotion"
                                                Dim CustomizedBody As String = MsgBody
                                                CustomizedBody = CustomizedBody.Replace("%username%", username)
                                                CustomizedBody = CustomizedBody.Replace("%serial%", serial)
                                                myMail.Body = CustomizedBody
                                                myMail.IsHTML = True
                                                myMail.Send()
                                                myMail = Nothing
                                            Catch
                                                Console.WriteLine(">> Error sending message!")
                                            End Try
                                        End If
                                    End If
                                Else
                                    'Console.WriteLine(users(i) & ": not eligible for this promotion.")
                                End If
                                
                            Next
                        Next
                        Console.WriteLine("Success!")
                        Console.WriteLine(CStr(count) & " users processed.")
                        Try
                            Call Obj.RDOEndSession()
                        Catch
                        End Try
                    Catch
                        Console.WriteLine("Error!")
                        Console.WriteLine(CStr(count) & " users processed.")
                    End Try
                End If
            End If
        End If
    End Sub


    Sub Main()

        Console.WriteLine("STAR PEACE NewsLetter Daemon 1.0 Copyright (C) 2002 Oceanus Communications.")
        If Environment.GetCommandLineArgs.Length = 2 Then
            Console.WriteLine("Sending NewsLetter: " & Environment.GetCommandLineArgs(1))
            LoadMessage(Environment.GetCommandLineArgs(1))
            RunPromotion(0) 'CInt(Environment.GetCommandLineArgs(1))
        Else
            Console.WriteLine("Usage: NewsLetter.exe <NewsLetterHtmlFile>")
        End If
        Console.WriteLine("Press ENTER to close this application.")
        Console.ReadLine()

    End Sub

End Module
