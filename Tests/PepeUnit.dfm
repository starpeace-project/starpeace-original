object Form1: TForm1
  Left = 201
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Algor Filter'
  ClientHeight = 105
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 74
    Height = 13
    Caption = 'Node Numbers:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 98
    Height = 13
    Caption = 'Temperature Values:'
  end
  object btnLoad: TButton
    Left = 412
    Top = 20
    Width = 65
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object eNodes: TEdit
    Left = 8
    Top = 22
    Width = 385
    Height = 21
    TabOrder = 1
    OnKeyPress = eNodesKeyPress
  end
  object eValues: TEdit
    Left = 8
    Top = 62
    Width = 385
    Height = 21
    TabOrder = 2
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 90
    Width = 385
    Height = 10
    Min = 0
    Max = 100
    TabOrder = 3
    Visible = False
  end
  object btnSave: TButton
    Left = 412
    Top = 56
    Width = 65
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'L Files|*.l'
    FilterIndex = 0
    InitialDir = 'c:\temp'
    Left = 304
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Algor Result File|*.txt|All Files|*.*'
    Left = 264
    Top = 8
  end
end
