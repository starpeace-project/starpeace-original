object NewStepClassForm: TNewStepClassForm
  Left = 270
  Top = 107
  Width = 331
  Height = 507
  Caption = 'Setup New Step Class'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Notebook1: TNotebook
    Left = 0
    Top = 0
    Width = 323
    Height = 480
    Align = alClient
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Name'
      object Panel3: TPanel
        Left = 0
        Top = 439
        Width = 323
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Panel4: TPanel
          Left = 56
          Top = 0
          Width = 267
          Height = 41
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object Button3: TButton
            Left = 112
            Top = 16
            Width = 75
            Height = 25
            Caption = 'Next >'
            TabOrder = 0
            OnClick = Button3Click
          end
          object Button4: TButton
            Left = 189
            Top = 16
            Width = 75
            Height = 25
            Caption = 'Cancel'
            TabOrder = 1
            OnClick = Button1Click
          end
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 323
        Height = 439
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 32
          Width = 88
          Height = 13
          Caption = 'Name of the class '
        end
        object edName: TEdit
          Left = 110
          Top = 29
          Width = 209
          Height = 21
          TabOrder = 0
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Properties'
      object pnContainer: TPanel
        Left = 0
        Top = 0
        Width = 323
        Height = 442
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 442
        Width = 323
        Height = 38
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Panel2: TPanel
          Left = 72
          Top = 0
          Width = 251
          Height = 38
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object Button1: TButton
            Left = 170
            Top = 8
            Width = 75
            Height = 25
            Caption = 'Cancel'
            TabOrder = 0
            OnClick = Button1Click
          end
          object Button2: TButton
            Left = 92
            Top = 8
            Width = 75
            Height = 25
            Caption = 'Finish'
            TabOrder = 1
            OnClick = Button2Click
          end
          object Button6: TButton
            Left = 12
            Top = 8
            Width = 75
            Height = 25
            Caption = '< Back'
            TabOrder = 2
            OnClick = Button6Click
          end
        end
      end
    end
  end
end
