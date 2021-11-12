object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 534
  ClientWidth = 828
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
    Width = 828
    Height = 57
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 1
    ExplicitTop = -5
    DesignSize = (
      828
      57)
    object lblVersion: TLabel
      Left = 684
      Top = 12
      Width = 51
      Height = 15
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
      ExplicitLeft = 691
    end
    object Label1: TLabel
      Left = 230
      Top = 8
      Width = 80
      Height = 15
      Caption = 'Sample Scripts:'
    end
    object btnRun: TButton
      Left = 83
      Top = 10
      Width = 65
      Height = 36
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
      Top = 10
      Width = 65
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnClear: TButton
      Left = 157
      Top = 10
      Width = 65
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnClearClick
    end
    object cboExamples: TComboBox
      Left = 229
      Top = 26
      Width = 145
      Height = 23
      DropDownCount = 16
      TabOrder = 3
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 57
    Width = 185
    Height = 477
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 472
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 232
      Align = alTop
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 233
      Width = 183
      Height = 243
      Align = alClient
      ItemHeight = 15
      TabOrder = 1
      OnClick = FileListBox1Click
      ExplicitHeight = 238
    end
  end
  object Panel3: TPanel
    Left = 185
    Top = 57
    Width = 643
    Height = 477
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 634
    ExplicitHeight = 472
    object Splitter1: TSplitter
      Left = 1
      Top = 304
      Width = 641
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitWidth = 740
    end
    object moutput: TMemo
      Left = 1
      Top = 307
      Width = 641
      Height = 169
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -15
      Font.Name = 'Fira Code'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      ExplicitTop = 302
      ExplicitWidth = 632
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 641
      Height = 303
      Align = alClient
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -16
      Font.Name = 'Fira Code Medium'
      Font.Style = []
      Lines.Strings = (
        'a = 3; b = 6; println ("sum = ", a + b)'
        'for x = 1 to 10 do'
        '    print (x * x, ", ")'
        'end')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      StyleElements = [seClient, seBorder]
      ExplicitWidth = 632
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
