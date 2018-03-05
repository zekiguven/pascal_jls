object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 443
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object scrlbx1: TScrollBox
    Left = 105
    Top = 0
    Width = 466
    Height = 443
    Align = alClient
    TabOrder = 0
    object img1: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 105
    Height = 443
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object btnOpen: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnSave: TButton
      Left = 16
      Top = 47
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnOptions: TButton
      Left = 16
      Top = 78
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 2
      OnClick = btnOptionsClick
    end
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = 'jls'
    Filter = '*.jls|*.jls'
    Left = 16
    Top = 176
  end
  object dlgSave1: TSaveDialog
    DefaultExt = 'jls'
    Filter = '*.jls|*.jls'
    Left = 72
    Top = 176
  end
end
