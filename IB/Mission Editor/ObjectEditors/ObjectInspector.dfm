object ObjectInspectorEditor: TObjectInspectorEditor
  Left = 451
  Top = 217
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'ObjectInspectorEditor'
  ClientHeight = 448
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcContainer: TPageControl
    Left = 0
    Top = 21
    Width = 299
    Height = 427
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Properties'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 291
        Height = 399
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvNone
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 121
          Top = 1
          Width = 3
          Height = 397
          Cursor = crHSplit
        end
        object pnLeft: TPanel
          Left = 1
          Top = 1
          Width = 120
          Height = 397
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnRight: TPanel
          Left = 124
          Top = 1
          Width = 166
          Height = 397
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Events'
      Enabled = False
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 291
        Height = 399
        Align = alClient
        BevelOuter = bvLowered
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 113
          Top = 1
          Width = 3
          Height = 397
          Cursor = crHSplit
        end
        object pnEvRight: TPanel
          Left = 116
          Top = 1
          Width = 174
          Height = 397
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnEvLeft: TPanel
          Left = 1
          Top = 1
          Width = 112
          Height = 397
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
  end
  object pnActions: TPanel
    Left = 0
    Top = 0
    Width = 299
    Height = 21
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 3
      Top = 3
      Width = 20
      Height = 16
      Caption = '+'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 26
      Top = 3
      Width = 20
      Height = 16
      Caption = '-'
      TabOrder = 1
    end
  end
end
