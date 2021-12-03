object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 1086
  ClientWidth = 1749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 1211
    Top = 86
    Width = 6
    Height = 1000
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
  end
  object Splitter4: TSplitter
    Left = 321
    Top = 86
    Width = 5
    Height = 1000
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1749
    Height = 86
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 1735
    DesignSize = (
      1749
      86)
    object lblVersion: TLabel
      Left = 1644
      Top = 24
      Width = 86
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 579
      Top = 5
      Width = 133
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Sample Scripts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnRun: TButton
      Left = 351
      Top = 15
      Width = 98
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Run'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnLoad: TButton
      Left = 122
      Top = 15
      Width = 99
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Load'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object cboExamples: TComboBox
      Left = 578
      Top = 39
      Width = 217
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
      TabOrder = 2
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
    object btnNew: TButton
      Left = 8
      Top = 15
      Width = 97
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'New'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = mnuNewClick
    end
    object btnSave: TButton
      Left = 236
      Top = 15
      Width = 99
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnSaveClick
    end
  end
  object pnlLeftPanel: TPanel
    Left = 0
    Top = 86
    Width = 321
    Height = 1000
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 993
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 40
      Width = 319
      Height = 348
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 28
      ParentFont = False
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 388
      Width = 319
      Height = 611
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 28
      ParentFont = False
      TabOrder = 1
      OnClick = FileListBox1Click
      ExplicitHeight = 604
    end
    object pnlSideControls: TPanel
      Left = 1
      Top = 1
      Width = 319
      Height = 39
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      TabOrder = 2
      object btnCloseSidePanel: TSpeedButton
        Left = 14
        Top = 5
        Width = 37
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Glyph.Data = {
          DE000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777077777700077777007777770007777060777777000777066000007
          7000770666666607700070666666660770007706666666077000777066000007
          7000777706077777700077777007777770007777770777777000777777777777
          7000}
        OnClick = btnCloseSidePanelClick
      end
    end
  end
  object pnlEditor: TPanel
    Left = 326
    Top = 86
    Width = 885
    Height = 1000
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 871
    ExplicitHeight = 993
    object pblBottomBase: TPanel
      Left = 1
      Top = 669
      Width = 883
      Height = 330
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 662
      ExplicitWidth = 869
      object Splitter1: TSplitter
        Left = 1
        Top = 1
        Width = 881
        Height = 3
        Cursor = crVSplit
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 879
      end
      object memoOutput: TMemo
        Left = 1
        Top = 4
        Width = 881
        Height = 262
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ExplicitWidth = 867
      end
      object pnlMemoButtons: TPanel
        Left = 1
        Top = 266
        Width = 881
        Height = 63
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alBottom
        TabOrder = 1
        ExplicitWidth = 867
        object btnClear: TButton
          Left = 8
          Top = 5
          Width = 99
          Height = 54
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Caption = 'Clear'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = btnClearClick
        end
      end
    end
    object SynEditor: TSynEdit
      Left = 1
      Top = 1
      Width = 883
      Height = 668
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -30
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      BookMarkOptions.LeftMargin = 3
      BookMarkOptions.Xoffset = 18
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -24
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.LeftOffset = 24
      Gutter.RightOffset = 3
      Gutter.RightMargin = 3
      Gutter.ShowLineNumbers = True
      Gutter.Width = 45
      Highlighter = SynGeneralSyn1
      Lines.Strings = (
        'import graphics'
        'import math'
        ''
        'p = graphics.size()'
        'x = p[0]/2-50; y = p[1]/2-50;'
        'heading = 45'
        ''
        'function forward (distance)'
        '  global x, y'
        ''
        '  graphics.moveto(x,y)'
        '  radians = heading*math.pi/180'
        '  dx = math.cos (radians) * distance'
        '  dy = math.sin (radians) * distance'
        '  x = x + dx'
        '  y = y + dy'
        '  graphics.lineto(x,y)'
        'end'
        ''
        'graphics.clear()'
        'graphics.moveto(x,y)'
        'graphics.lineto(x,y)'
        'for i = 1 to 12 do'
        '    forward (200)'
        '    heading = heading + 150 '
        'end')
      ExplicitLeft = 468
      ExplicitTop = 336
      ExplicitWidth = 300
      ExplicitHeight = 225
    end
  end
  object pnlRight: TPanel
    Left = 1217
    Top = 86
    Width = 532
    Height = 1000
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    ExplicitLeft = 1203
    ExplicitHeight = 993
    object Splitter3: TSplitter
      Left = 1
      Top = 540
      Width = 530
      Height = 3
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 2
      ExplicitWidth = 529
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 543
      Width = 530
      Height = 456
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 536
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 530
      Height = 539
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      ExplicitHeight = 532
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 528
        Height = 537
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 526
        ExplicitHeight = 535
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.rh'
    FileName = 'akt.rh'
    Filter = 'Any files|*.*|Rhodus Scripts|*.rh'
    Title = 'Load Rhodus Script'
    Left = 1057
    Top = 150
  end
  object MainMenu: TMainMenu
    Left = 1296
    Top = 28
    object File1: TMenuItem
      Caption = 'File'
      object mnuNew: TMenuItem
        Caption = 'New'
        OnClick = mnuNewClick
      end
      object Quit1: TMenuItem
        Caption = 'Open'
      end
      object mnuSave: TMenuItem
        Caption = 'Save'
        OnClick = mnuSaveClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object N2: TMenuItem
        Caption = 'Quit'
        OnClick = N2Click
      end
    end
    object File2: TMenuItem
      Caption = 'Edit'
    end
    object mnuSettings: TMenuItem
      Caption = 'Settings'
      object mnuPreferences: TMenuItem
        Caption = 'Preferences'
        OnClick = mnuPreferencesClick
      end
    end
    object Run1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object SynGeneralSyn1: TSynGeneralSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    DetectPreprocessor = False
    KeyAttri.Foreground = clMenuHighlight
    KeyWords.Strings = (
      'CASE'
      'DO'
      'DOWNTO'
      'ELSE'
      'END'
      'FOR'
      'FUNCTION'
      'GLOBAL'
      'IF'
      'IMPORT'
      'REPEAT'
      'SWITCH'
      'THEN'
      'TO'
      'WHILE')
    Left = 869
    Top = 152
  end
  object SynEditSearch1: TSynEditSearch
    Left = 1282
    Top = 351
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.rh'
    Filter = 'Any files|*.*|Rhodus Scripts|*.rh'
    Title = 'Save Rhodus Script'
    Left = 1044
    Top = 226
  end
end
