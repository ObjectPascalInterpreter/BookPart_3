object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 534
  ClientWidth = 827
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
    Width = 827
    Height = 57
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 819
    DesignSize = (
      827
      57)
    object lblVersion: TLabel
      Left = 683
      Top = 12
      Width = 51
      Height = 15
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
      ExplicitLeft = 691
    end
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
    Height = 477
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 472
  end
  object Panel3: TPanel
    Left = 86
    Top = 57
    Width = 741
    Height = 477
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 733
    ExplicitHeight = 472
    object Splitter1: TSplitter
      Left = 1
      Top = 304
      Width = 739
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitWidth = 740
    end
    object moutput: TMemo
      Left = 1
      Top = 307
      Width = 739
      Height = 169
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitTop = 302
      ExplicitWidth = 731
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 739
      Height = 303
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
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
      ExplicitWidth = 731
      ExplicitHeight = 298
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
