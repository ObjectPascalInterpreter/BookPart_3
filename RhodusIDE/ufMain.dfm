object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 801
  ClientWidth = 1241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 25
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1241
    Height = 86
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 0
    DesignSize = (
      1241
      86)
    object lblVersion: TLabel
      Left = 1037
      Top = 18
      Width = 77
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
    end
    object btnRun: TButton
      Left = 228
      Top = 18
      Width = 170
      Height = 56
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnLoad: TButton
      Left = 12
      Top = 18
      Width = 170
      Height = 56
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 86
    Width = 129
    Height = 715
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 129
    Top = 86
    Width = 1112
    Height = 715
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 1
      Top = 456
      Width = 1110
      Height = 5
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 2
      ExplicitWidth = 1108
    end
    object moutput: TMemo
      Left = 1
      Top = 461
      Width = 1110
      Height = 253
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitLeft = 2
      ExplicitWidth = 1108
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 1110
      Height = 455
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = []
      Lines.Strings = (
        'a = 3; b = 6; println ("sum = ", a + b)'
        'for x = 1 to 10 do'
        '    print (x * x, ", ")'
        'end')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      ExplicitLeft = 2
      ExplicitTop = 2
      ExplicitWidth = 1108
      ExplicitHeight = 454
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.rh'
    FileName = 'akt.rh'
    Filter = 'Any files|*.*|Rhodus Scripts|*.rh'
    Title = 'Load Rhodus Script'
    Left = 694
    Top = 425
  end
end
