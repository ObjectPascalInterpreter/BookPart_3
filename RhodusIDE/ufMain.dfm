object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 779
  ClientWidth = 1242
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
    Width = 1242
    Height = 86
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 0
    DesignSize = (
      1242
      86)
    object lblVersion: TLabel
      Left = 1026
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
    object Label1: TLabel
      Left = 345
      Top = 12
      Width = 121
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sample Scripts:'
    end
    object btnRun: TButton
      Left = 125
      Top = 15
      Width = 97
      Height = 54
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
      Top = 15
      Width = 98
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnClear: TButton
      Left = 236
      Top = 15
      Width = 97
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnClearClick
    end
    object cboExamples: TComboBox
      Left = 344
      Top = 39
      Width = 217
      Height = 23
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      DropDownCount = 16
      TabOrder = 3
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 86
    Width = 278
    Height = 693
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 1
      Width = 276
      Height = 348
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      ItemHeight = 25
      TabOrder = 0
      OnChange = DirectoryListBox1Change
      ExplicitLeft = 2
      ExplicitTop = 2
      ExplicitWidth = 274
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 349
      Width = 276
      Height = 343
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      ItemHeight = 25
      TabOrder = 1
      OnClick = FileListBox1Click
      ExplicitLeft = 2
      ExplicitTop = 350
      ExplicitWidth = 274
      ExplicitHeight = 342
    end
  end
  object Panel3: TPanel
    Left = 278
    Top = 86
    Width = 964
    Height = 693
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 1
      Top = 434
      Width = 962
      Height = 4
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 2
      ExplicitWidth = 961
    end
    object moutput: TMemo
      Left = 1
      Top = 438
      Width = 962
      Height = 254
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -23
      Font.Name = 'Fira Code'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      ExplicitLeft = 2
      ExplicitWidth = 961
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 962
      Height = 433
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -24
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
