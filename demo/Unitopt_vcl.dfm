object frmOptions: TfrmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 252
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblT1: TLabel
    Left = 230
    Top = 103
    Width = 12
    Height = 13
    Caption = 'T1'
  end
  object lblT2: TLabel
    Left = 230
    Top = 128
    Width = 12
    Height = 13
    Caption = 'T2'
  end
  object lblT3: TLabel
    Left = 232
    Top = 155
    Width = 12
    Height = 13
    Caption = 'T3'
  end
  object lblReset: TLabel
    Left = 8
    Top = 154
    Width = 28
    Height = 13
    Caption = 'Reset'
  end
  object lbl2: TLabel
    Left = 8
    Top = 99
    Width = 53
    Height = 13
    Caption = 'Error Value'
  end
  object lbl3: TLabel
    Left = 8
    Top = 8
    Width = 174
    Height = 13
    Caption = 'Interleaved Mode for Colour Images'
    WordWrap = True
  end
  object edtT3: TEdit
    Left = 291
    Top = 151
    Width = 61
    Height = 21
    TabOrder = 0
    Text = 'edtT3'
  end
  object edtT2: TEdit
    Left = 291
    Top = 124
    Width = 61
    Height = 21
    TabOrder = 1
    Text = 'edtT2'
  end
  object edtT1: TEdit
    Left = 291
    Top = 99
    Width = 61
    Height = 21
    TabOrder = 2
    Text = 'edtT1'
  end
  object chkDefaultT: TCheckBox
    Left = 232
    Top = 74
    Width = 169
    Height = 24
    Caption = 'Use defaut thresold values'
    TabOrder = 3
    WordWrap = True
    OnClick = chkDefaultTClick
  end
  object edtRESET: TEdit
    Left = 77
    Top = 151
    Width = 80
    Height = 21
    TabOrder = 4
    Text = 'edtRESET'
  end
  object chkR: TCheckBox
    Left = 8
    Top = 129
    Width = 149
    Height = 17
    Caption = 'Use default RESET value'
    TabOrder = 5
    WordWrap = True
    OnClick = chkRClick
  end
  object edtComp: TEdit
    Left = 77
    Top = 96
    Width = 80
    Height = 21
    TabOrder = 6
    Text = 'edtComp'
  end
  object chkComp: TCheckBox
    Left = 8
    Top = 77
    Width = 149
    Height = 17
    Caption = 'Lossless Compression'
    TabOrder = 7
    OnClick = chkCompClick
  end
  object cbb1: TComboBox
    Left = 8
    Top = 31
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 8
    Text = 'Plane Interleaved Mode'
    Items.Strings = (
      'Plane Interleaved Mode'
      'Line Interleaved Mode'
      'Sample Interleaved Mode')
  end
  object bClose: TButton
    Left = 316
    Top = 209
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 9
  end
end
