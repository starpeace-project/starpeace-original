object PlainTextEditor: TPlainTextEditor
  Left = 565
  Top = 545
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 71
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object edValue: TEdit
    Left = 0
    Top = 0
    Width = 503
    Height = 19
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    OnChange = edValueChange
  end
end
