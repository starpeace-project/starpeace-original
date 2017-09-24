object EditorMainForm: TEditorMainForm
  Left = 436
  Top = 309
  Width = 634
  Height = 498
  Caption = 'Mission Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 379
    Top = 0
    Width = 3
    Height = 452
    Cursor = crHSplit
    Align = alRight
  end
  object pnLeft: TPanel
    Left = 0
    Top = 0
    Width = 379
    Height = 452
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnRightContainer: TPanel
    Left = 382
    Top = 0
    Width = 244
    Height = 452
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 244
      Height = 17
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = ' Object Inspector'
      Color = clHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object pnRight: TPanel
      Left = 0
      Top = 17
      Width = 244
      Height = 435
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 449
    Top = 120
    object File1: TMenuItem
      Caption = '&File'
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object Load1: TMenuItem
        Caption = '&Load'
        OnClick = Load1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Mission Files|*.mission'
    Left = 160
    Top = 104
  end
  object OpenDialog1: TOpenDialog
    Left = 280
    Top = 96
  end
end
