object frmPreferences: TfrmPreferences
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 555
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 25
  object Label1: TLabel
    Left = 32
    Top = 32
    Width = 48
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Fonts:'
  end
  object Label2: TLabel
    Left = 32
    Top = 128
    Width = 141
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Available Themes:'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 494
    Width = 608
    Height = 61
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 487
    ExplicitWidth = 594
    object btnOk: TButton
      Left = 360
      Top = 12
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 482
      Top = 12
      Width = 112
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object cboFontlist: TComboBox
    Left = 32
    Top = 66
    Width = 398
    Height = 36
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    DropDownCount = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'cboFontlist'
    OnClick = cboFontlistClick
  end
  object spFontSize: TSpinEdit
    Left = 453
    Top = 66
    Width = 85
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
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
    Left = 32
    Top = 176
    Width = 398
    Height = 145
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ItemHeight = 25
    TabOrder = 3
    OnClick = lbThemesClick
  end
end
