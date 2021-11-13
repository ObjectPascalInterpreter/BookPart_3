object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = ' About'
  ClientHeight = 154
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 144
  TextHeight = 25
  object lblMessage: TLabel
    Left = 96
    Top = 25
    Width = 76
    Height = 27
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Message'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object btnClose: TButton
    Left = 161
    Top = 97
    Width = 112
    Height = 37
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnCloseClick
  end
end
