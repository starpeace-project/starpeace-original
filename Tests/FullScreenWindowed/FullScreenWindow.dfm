object FullScreenForm: TFullScreenForm
  Left = 132
  Top = 103
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Full Screen Window'
  ClientHeight = 352
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object OpenDialog: TOpenDialog
    FileName = '*.gif'
    InitialDir = 'C:\Work\Five\Release\Client\Cache\BuildingImages'
    Left = 240
    Top = 8
  end
end
