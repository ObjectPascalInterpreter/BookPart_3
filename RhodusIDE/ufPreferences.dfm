object frmPreferences: TfrmPreferences
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 370
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 21
    Top = 23
    Width = 32
    Height = 15
    Caption = 'Fonts:'
  end
  object Label2: TLabel
    Left = 21
    Top = 75
    Width = 95
    Height = 15
    Caption = 'Available Themes:'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 329
    Width = 411
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 324
    ExplicitWidth = 403
    object btnOk: TButton
      Left = 240
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 321
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object cboFontlist: TComboBox
    Left = 21
    Top = 44
    Width = 266
    Height = 25
    DropDownCount = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'cboFontlist'
    OnClick = cboFontlistClick
  end
  object spFontSize: TSpinEdit
    Left = 302
    Top = 44
    Width = 57
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    MaxValue = 0
    MinValue = 0
    ParentFont = False
    TabOrder = 2
    Value = 0
    OnChange = spFontSizeChange
  end
  object lbThemes: TListBox
    Left = 21
    Top = 96
    Width = 266
    Height = 153
    ItemHeight = 15
    TabOrder = 3
    OnClick = lbThemesClick
  end
end
