object Form1: TForm1
  Left = 190
  Top = 135
  Width = 870
  Height = 640
  Caption = 'Memory Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 385
    Top = 29
    Width = 3
    Height = 559
    Cursor = crHSplit
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 29
    Width = 385
    Height = 559
    Align = alLeft
    Indent = 19
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 862
    Height = 29
    ButtonWidth = 25
    Caption = 'ToolBar1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Hint = 'Crear el arbol de la persistencia.'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton3: TToolButton
      Left = 25
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 33
      Top = 2
      Hint = 'Pedir memoria con Getmem en la persistencia'
      Caption = 'ToolButton4'
      ImageIndex = 2
      OnClick = ToolButton4Click
    end
    object ToolButton2: TToolButton
      Left = 58
      Top = 2
      Hint = 'Hacer un objeto en la persistencia'
      Caption = 'ToolButton2'
      ImageIndex = 1
      OnClick = ToolButton2Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 588
    Width = 862
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object RichEdit1: TRichEdit
    Left = 388
    Top = 29
    Width = 474
    Height = 559
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object ImageList1: TImageList
    Left = 16
    Top = 48
  end
end
