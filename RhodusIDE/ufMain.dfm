object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 539
  ClientWidth = 813
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 813
    Height = 57
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 777
    object btnRun: TButton
      Left = 152
      Top = 12
      Width = 113
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnLoad: TButton
      Left = 8
      Top = 12
      Width = 113
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 57
    Width = 86
    Height = 482
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 435
  end
  object Panel3: TPanel
    Left = 86
    Top = 57
    Width = 727
    Height = 482
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 691
    ExplicitHeight = 435
    object Splitter1: TSplitter
      Left = 1
      Top = 309
      Width = 725
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 291
      ExplicitWidth = 689
    end
    object moutput: TMemo
      Left = 1
      Top = 312
      Width = 725
      Height = 169
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 725
      Height = 308
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      Lines.Strings = (
        'a = 3; b = 6; println (a + b)')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      ExplicitHeight = 304
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.rh'
    FileName = 'akt.rh'
    Filter = 'Any files|*.*|Rhodus Scripts|*.rh'
    Title = 'Load Rhodus Script'
    Left = 534
    Top = 25
  end
end
