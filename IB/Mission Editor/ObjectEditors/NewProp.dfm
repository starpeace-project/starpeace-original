object NewPropForm: TNewPropForm
  Left = 562
  Top = 310
  Width = 343
  Height = 208
  Caption = 'Add Property'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 335
    Height = 152
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'New Property'
      object Label4: TLabel
        Left = 8
        Top = 9
        Width = 70
        Height = 13
        Caption = 'Property Name'
      end
      object Label5: TLabel
        Left = 8
        Top = 36
        Width = 67
        Height = 13
        Caption = 'Property Class'
      end
      object lbTemplate: TLabel
        Left = 8
        Top = 90
        Width = 44
        Height = 13
        Caption = 'Template'
        Visible = False
      end
      object Label7: TLabel
        Left = 11
        Top = 63
        Width = 27
        Height = 13
        Caption = 'Value'
      end
      object edPropName: TEdit
        Left = 82
        Top = 5
        Width = 214
        Height = 21
        TabOrder = 0
      end
      object cbPropClass: TComboBox
        Left = 82
        Top = 32
        Width = 214
        Height = 21
        ItemHeight = 0
        TabOrder = 1
        OnChange = cbPropClassChange
      end
      object cbTemplates: TComboBox
        Left = 82
        Top = 85
        Width = 214
        Height = 21
        ItemHeight = 0
        TabOrder = 2
        Visible = False
        OnChange = cbTemplatesChange
      end
      object cbValue: TComboBox
        Left = 82
        Top = 58
        Width = 214
        Height = 21
        ItemHeight = 0
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'New From Template'
      ImageIndex = 1
      object pnContainer: TPanel
        Left = 0
        Top = 33
        Width = 327
        Height = 91
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 327
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 7
          Top = 9
          Width = 83
          Height = 13
          Caption = 'Choose Template'
        end
        object cbPropTemplates: TComboBox
          Left = 95
          Top = 4
          Width = 223
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbPropTemplatesChange
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 152
    Width = 335
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 150
      Top = 0
      Width = 185
      Height = 29
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button2: TButton
        Left = 30
        Top = 3
        Width = 75
        Height = 25
        Caption = 'OK'
        TabOrder = 0
        OnClick = Button2Click
      end
      object Button1: TButton
        Left = 108
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
  end
end
