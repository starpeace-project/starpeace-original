object FilenameEditor: TFilenameEditor
  Left = 343
  Top = 479
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 87
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 648
    Top = 0
    Width = 21
    Height = 87
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object Button1: TButton
      Left = 3
      Top = 3
      Width = 21
      Height = 21
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 648
    Height = 87
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object edValue: TEdit
      Left = 160
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = edValueChange
    end
  end
  object OpenDialog1: TOpenDialog
    InitialDir = 'c:\work'
    Left = 352
    Top = 48
  end
end
