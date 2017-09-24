object NewStepForm: TNewStepForm
  Left = 441
  Top = 223
  BorderIcons = [biHelp]
  BorderStyle = bsDialog
  Caption = 'Select Step Type'
  ClientHeight = 337
  ClientWidth = 494
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
    Width = 494
    Height = 337
    Align = alClient
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'SelectStepType'
      object Button1: TButton
        Left = 417
        Top = 310
        Width = 75
        Height = 25
        Caption = 'Cancel'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 339
        Top = 311
        Width = 75
        Height = 25
        Caption = 'Next >'
        TabOrder = 1
        OnClick = Button2Click
      end
      object pnContainer: TPanel
        Left = 0
        Top = 0
        Width = 494
        Height = 304
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'EnterName'
      object Label1: TLabel
        Left = 32
        Top = 40
        Width = 81
        Height = 13
        Caption = 'Enter Step Name'
      end
      object Button3: TButton
        Left = 339
        Top = 311
        Width = 75
        Height = 25
        Caption = 'Finish'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 417
        Top = 310
        Width = 75
        Height = 25
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button5: TButton
        Left = 260
        Top = 312
        Width = 75
        Height = 25
        Caption = '< Back'
        TabOrder = 2
        OnClick = Button5Click
      end
      object edName: TEdit
        Left = 122
        Top = 36
        Width = 231
        Height = 21
        TabOrder = 3
      end
    end
  end
end
