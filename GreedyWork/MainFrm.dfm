object MainForm: TMainForm
  Left = 132
  Top = 160
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GreedWorks'
  ClientHeight = 444
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    7000000700000070022020200700070222000000007000220078888870007020
    78BFFC44C7070207FBFBC4004C70007FBFBF400004300208FBFB40F004800207
    8FBFC4004C30022007FBFC44C3800222207FBFB8387070222200788777070022
    222200000000070222220202007000700220202007000000700000070000F00F
    0000C00300008001000080010000000000000000000000000000000000000000
    00000000000000000000000000008001000080010000C0030000F00F0000}
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pctrlMain: TPageControl
    Left = 0
    Top = 0
    Width = 745
    Height = 444
    ActivePage = tabMain
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabOrder = 0
    object tabMain: TTabSheet
      Caption = 'Credit Cards'
      object Label1: TLabel
        Left = 5
        Top = 0
        Width = 64
        Height = 13
        Caption = 'Target Users:'
      end
      object Label8: TLabel
        Left = 682
        Top = 343
        Width = 41
        Height = 11
        Caption = 'Chargable'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'MS Serif'
        Font.Style = []
        ParentFont = False
      end
      object Image1: TImage
        Left = 661
        Top = 340
        Width = 17
        Height = 17
        Picture.Data = {
          055449636F6E0000010001001010100000000000280100001600000028000000
          10000000200000000100040000000000C0000000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000002000000000000000220000000000002222200
          000000000AAAA220000000000AAAA20000000000000A200000000000000A0000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000FFD70000FFED0000FF570000DF010000CF020000C703000003010000
          01020000000300000100000003030000C7000000CF010000DFFF0000FFFF0000
          FFFF0000}
      end
      object Label9: TLabel
        Left = 682
        Top = 375
        Width = 51
        Height = 11
        Caption = 'Unchargable'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'MS Serif'
        Font.Style = []
        ParentFont = False
      end
      object Image2: TImage
        Left = 661
        Top = 373
        Width = 16
        Height = 16
        Picture.Data = {
          055449636F6E0000010001001010100000000000280100001600000028000000
          10000000200000000100040000000000C0000000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000000000000110000000000001111100
          0000000009999110000000000999910000000000000910000000000000090000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000FFD70000FFED0000FF570000DF010000CF020000C703000003010000
          01020000000300000100000003030000C7000000CF010000DFFF0000FFFF0000
          FFFF0000}
      end
      object lvUsers: TListView
        Left = 5
        Top = 16
        Width = 644
        Height = 376
        Columns = <
          item
            Caption = 'ALIAS'
            Width = 120
          end
          item
            Alignment = taRightJustify
            Caption = 'SUBID'
          end
          item
            Alignment = taRightJustify
            Caption = 'CHG DATE'
            Width = 80
          end
          item
            Alignment = taCenter
            Caption = 'CARD'
            Width = 90
          end
          item
            Alignment = taCenter
            Caption = 'EXP'
          end
          item
            Caption = 'ADDRESS'
            Width = 95
          end
          item
            Alignment = taCenter
            Caption = 'ZIP'
          end
          item
            Caption = 'RESPONSE'
            Width = 100
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = SubsMenu
        SmallImages = ImageList1
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRefresh: TButton
        Left = 661
        Top = 16
        Width = 68
        Height = 25
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = btnRefreshClick
      end
      object btnCollect: TButton
        Left = 661
        Top = 59
        Width = 68
        Height = 25
        Caption = 'Collect'
        Enabled = False
        TabOrder = 2
        OnClick = btnCollectClick
      end
      object pbCCRefresh: TProgressBar
        Left = 5
        Top = 397
        Width = 644
        Height = 7
        Min = 0
        Max = 100
        TabOrder = 3
      end
      object eAccStatus: TButton
        Left = 661
        Top = 102
        Width = 68
        Height = 25
        Caption = 'Fix A.Status'
        TabOrder = 4
        OnClick = eAccStatusClick
      end
      object btnFindCC: TButton
        Left = 661
        Top = 145
        Width = 68
        Height = 25
        Caption = 'Find CC'
        TabOrder = 5
        OnClick = btnFindCCClick
      end
    end
    object tabTransfers: TTabSheet
      Caption = 'Money Transfers'
      ImageIndex = 4
      object Label15: TLabel
        Left = 5
        Top = 0
        Width = 64
        Height = 13
        Caption = 'Target Users:'
      end
      object lvTransfSubs: TListView
        Left = 5
        Top = 16
        Width = 644
        Height = 376
        Columns = <
          item
            Caption = 'alias'
            Width = 120
          end
          item
            Alignment = taRightJustify
            Caption = 'subId'
          end
          item
            Alignment = taRightJustify
            Caption = 'chg-date'
            Width = 80
          end
          item
            Alignment = taCenter
            Caption = 'type'
            Width = 40
          end
          item
            Alignment = taCenter
            Caption = 'info'
            Width = 100
          end
          item
            Alignment = taRightJustify
            Caption = 'period'
          end
          item
            Alignment = taCenter
            Caption = 'notified'
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = SubsMenu
        SmallImages = ImageList1
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnRefreshTransfers: TButton
        Left = 661
        Top = 16
        Width = 68
        Height = 25
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = btnRefreshTransfersClick
      end
      object btnCheckTransf: TButton
        Left = 661
        Top = 59
        Width = 68
        Height = 25
        Caption = 'Check'
        Enabled = False
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      object Label3: TLabel
        Left = 8
        Top = 13
        Width = 34
        Height = 13
        Caption = 'Server:'
      end
      object Label4: TLabel
        Left = 8
        Top = 58
        Width = 22
        Height = 13
        Caption = 'Port:'
      end
      object Label5: TLabel
        Left = 8
        Top = 108
        Width = 86
        Height = 13
        Caption = 'Subscriptors Path:'
      end
      object Label7: TLabel
        Left = 125
        Top = 108
        Width = 51
        Height = 13
        Caption = 'Billing Info:'
      end
      object Label6: TLabel
        Left = 10
        Top = 164
        Width = 77
        Height = 13
        Caption = 'Amount: ($USD)'
      end
      object Label2: TLabel
        Left = 248
        Top = 13
        Width = 61
        Height = 13
        Caption = 'Charge Only:'
      end
      object Label10: TLabel
        Left = 10
        Top = 224
        Width = 84
        Height = 13
        Caption = 'Configuration File:'
      end
      object Label11: TLabel
        Left = 10
        Top = 272
        Width = 53
        Height = 13
        Caption = 'ProgramID:'
      end
      object Label13: TLabel
        Left = 10
        Top = 321
        Width = 49
        Height = 13
        Caption = 'Password:'
      end
      object Label14: TLabel
        Left = 361
        Top = 320
        Width = 53
        Height = 13
        Caption = 'Certificade:'
      end
      object Label16: TLabel
        Left = 160
        Top = 321
        Width = 46
        Height = 13
        Caption = 'Key Path:'
      end
      object Label17: TLabel
        Left = 11
        Top = 366
        Width = 62
        Height = 13
        Caption = 'Mailer Name:'
      end
      object Label18: TLabel
        Left = 160
        Top = 366
        Width = 72
        Height = 13
        Caption = 'Mailer Address:'
      end
      object Label19: TLabel
        Left = 360
        Top = 366
        Width = 56
        Height = 13
        Caption = 'Mailer Host:'
      end
      object Label20: TLabel
        Left = 477
        Top = 367
        Width = 73
        Height = 13
        Caption = 'Message Body:'
      end
      object Label21: TLabel
        Left = 113
        Top = 166
        Width = 63
        Height = 13
        Caption = 'Charge Date:'
      end
      object eServer: TEdit
        Left = 8
        Top = 28
        Width = 218
        Height = 21
        TabOrder = 0
        Text = 'dir.starpeace.net'
      end
      object ePort: TEdit
        Left = 8
        Top = 74
        Width = 52
        Height = 21
        TabOrder = 1
        Text = '2222'
      end
      object eSearchPath: TEdit
        Left = 8
        Top = 124
        Width = 104
        Height = 21
        TabOrder = 2
        Text = 'root/paying'
      end
      object eBillInfo: TEdit
        Left = 125
        Top = 124
        Width = 101
        Height = 21
        TabOrder = 3
        Text = '/subscription'
      end
      object eAmount: TEdit
        Left = 10
        Top = 180
        Width = 85
        Height = 21
        TabOrder = 4
        Text = '9.95'
      end
      object mCustUsers: TMemo
        Left = 246
        Top = 28
        Width = 227
        Height = 172
        ScrollBars = ssVertical
        TabOrder = 5
        WordWrap = False
      end
      object btnClear: TButton
        Left = 484
        Top = 28
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 6
        OnClick = btnClearClick
      end
      object eConfFilePath: TEdit
        Left = 10
        Top = 240
        Width = 215
        Height = 21
        TabOrder = 7
        Text = 'pki\payflow-cfg.xml'
      end
      object cbProgId: TComboBox
        Left = 10
        Top = 288
        Width = 217
        Height = 21
        ItemHeight = 13
        TabOrder = 8
        Text = 'PSS.BS.PayFlow'
        Items.Strings = (
          'PSS.BS.PayFlow'
          'PSS.BS.PayFlow.Remote')
      end
      object ePassword: TEdit
        Left = 10
        Top = 336
        Width = 143
        Height = 21
        PasswordChar = '*'
        TabOrder = 9
        Text = 'itzamna'
      end
      object eCertificade: TEdit
        Left = 361
        Top = 336
        Width = 247
        Height = 21
        TabOrder = 10
        Text = '6D5F F873 C8AA C982 434E 14D3 48A5 BFCC'
      end
      object btnReloadKeys: TButton
        Left = 248
        Top = 216
        Width = 105
        Height = 25
        Caption = 'Reload Keys'
        TabOrder = 11
        OnClick = btnReloadKeysClick
      end
      object eCertPath: TEdit
        Left = 160
        Top = 336
        Width = 193
        Height = 21
        TabOrder = 12
        Text = 'pki\cert-pss.pfx'
      end
      object eMailerName: TEdit
        Left = 10
        Top = 381
        Width = 143
        Height = 21
        TabOrder = 13
        Text = 'Oceanus Customer Service'
      end
      object eMailer: TEdit
        Left = 160
        Top = 381
        Width = 193
        Height = 21
        TabOrder = 14
        Text = 'customer-service@oceanus.ca'
      end
      object eMailerHost: TEdit
        Left = 361
        Top = 381
        Width = 104
        Height = 21
        TabOrder = 15
        Text = 'mail.starpeace.net'
      end
      object eMessageFile: TEdit
        Left = 476
        Top = 381
        Width = 130
        Height = 21
        TabOrder = 16
        Text = 'greednot.msg'
      end
      object cbDebugMode: TCheckBox
        Left = 631
        Top = 386
        Width = 97
        Height = 17
        Caption = 'Debug Mode'
        TabOrder = 17
      end
      object eChargeDate: TEdit
        Left = 112
        Top = 180
        Width = 121
        Height = 21
        TabOrder = 18
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Financies'
      object Label12: TLabel
        Left = 8
        Top = 18
        Width = 47
        Height = 13
        Caption = 'Collected:'
      end
      object eTotalCol: TEdit
        Left = 8
        Top = 34
        Width = 89
        Height = 21
        TabOrder = 0
        Text = '$0.0'
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Log'
      ImageIndex = 2
      object reLog: TRichEdit
        Left = 0
        Top = 0
        Width = 737
        Height = 416
        Align = alClient
        PopupMenu = LogMenu
        TabOrder = 0
      end
    end
  end
  object ImageList1: TImageList
    Left = 676
    Top = 224
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000800000008000000080
      0000008000000080000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      8000000080000000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF00000080000000800000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000800000008000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF00000080000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFD7FFD700000000FFEDFFED00000000
      FF57FF5700000000DF01DF0100000000CF02CF0200000000C703C70300000000
      0301030100000000010201020000000000030003000000000100010000000000
      0303030300000000C700C70000000000CF01CF0100000000DFFFDFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object LogMenu: TPopupMenu
    Left = 604
    Top = 168
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
    object Savetofile1: TMenuItem
      Caption = '&Save to file'
      OnClick = Savetofile1Click
    end
  end
  object LogSaveDialog: TSaveDialog
    DefaultExt = 'log'
    Filter = 'Log Files|*.log|Any File|*.*'
    Left = 492
    Top = 168
  end
  object SubsMenu: TPopupMenu
    Left = 556
    Top = 168
    object SelectExceptions1: TMenuItem
      Caption = '&Select Exceptions'
      OnClick = SelectExceptions1Click
    end
    object UnsubscribeSelected1: TMenuItem
      Caption = '&Unsubscribe Selected'
      OnClick = UnsubscribeSelected1Click
    end
  end
end
