object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 2309
  ClientWidth = 3844
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 216
  TextHeight = 25
  object Splitter2: TSplitter
    Left = 2764
    Top = 128
    Width = 9
    Height = 2181
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alRight
    ExplicitLeft = 3596
    ExplicitHeight = 2833
  end
  object Splitter4: TSplitter
    Left = 482
    Top = 128
    Width = 6
    Height = 2181
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    ExplicitHeight = 2833
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 3844
    Height = 128
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 893
      Top = 7
      Width = 192
      Height = 40
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Caption = 'Sample Scripts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblVersion: TLabel
      Left = 3719
      Top = 1
      Width = 124
      Height = 126
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alRight
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'lblVersion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 4545
      ExplicitTop = 2
      ExplicitHeight = 40
    end
    object btnRun: TButton
      Left = 684
      Top = 23
      Width = 149
      Height = 81
      Margins.Left = 11
      Margins.Top = 11
      Margins.Right = 11
      Margins.Bottom = 11
      Caption = 'Run'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnLoad: TButton
      Left = 182
      Top = 23
      Width = 149
      Height = 81
      Margins.Left = 11
      Margins.Top = 11
      Margins.Right = 11
      Margins.Bottom = 11
      Caption = 'Load'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object cboExamples: TComboBox
      Left = 891
      Top = 59
      Width = 338
      Height = 48
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      DropDownCount = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
    object btnNew: TButton
      Left = 11
      Top = 23
      Width = 147
      Height = 81
      Margins.Left = 11
      Margins.Top = 11
      Margins.Right = 11
      Margins.Bottom = 11
      Caption = 'New'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = mnuNewClick
    end
    object btnSave: TButton
      Left = 353
      Top = 23
      Width = 149
      Height = 81
      Margins.Left = 11
      Margins.Top = 11
      Margins.Right = 11
      Margins.Bottom = 11
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnSaveClick
    end
    object btnSaveAs: TButton
      Left = 520
      Top = 23
      Width = 148
      Height = 81
      Margins.Left = 11
      Margins.Top = 11
      Margins.Right = 11
      Margins.Bottom = 11
      Caption = 'Save As'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnSaveAsClick
    end
  end
  object pnlLeftPanel: TPanel
    Left = 0
    Top = 128
    Width = 482
    Height = 2181
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alLeft
    TabOrder = 1
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 60
      Width = 480
      Height = 522
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 40
      ParentFont = False
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 582
      Width = 480
      Height = 1598
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 40
      ParentFont = False
      TabOrder = 1
      OnClick = FileListBox1Click
    end
    object pnlSideControls: TPanel
      Left = 1
      Top = 1
      Width = 480
      Height = 59
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alTop
      TabOrder = 2
      object btnCloseSidePanel: TSpeedButton
        Left = 20
        Top = 7
        Width = 57
        Height = 45
        Margins.Left = 7
        Margins.Top = 7
        Margins.Right = 7
        Margins.Bottom = 7
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
    Left = 488
    Top = 128
    Width = 2276
    Height = 2181
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alClient
    TabOrder = 2
    object pblBottomBase: TPanel
      Left = 1
      Top = 1685
      Width = 2274
      Height = 495
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alBottom
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 1
        Top = 1
        Width = 2272
        Height = 5
        Cursor = crVSplit
        Margins.Left = 7
        Margins.Top = 7
        Margins.Right = 7
        Margins.Bottom = 7
        Align = alTop
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 3099
      end
      object memoOutput: TMemo
        Left = 1
        Top = 6
        Width = 2272
        Height = 393
        Margins.Left = 7
        Margins.Top = 7
        Margins.Right = 7
        Margins.Bottom = 7
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -29
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlMemoButtons: TPanel
        Left = 1
        Top = 399
        Width = 2272
        Height = 95
        Margins.Left = 7
        Margins.Top = 7
        Margins.Right = 7
        Margins.Bottom = 7
        Align = alBottom
        TabOrder = 1
        object btnClear: TButton
          Left = 11
          Top = 7
          Width = 149
          Height = 81
          Margins.Left = 11
          Margins.Top = 11
          Margins.Right = 11
          Margins.Bottom = 11
          Caption = 'Clear'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -29
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
      Width = 2274
      Height = 1684
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -45
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      BookMarkOptions.LeftMargin = 5
      BookMarkOptions.Xoffset = 27
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -36
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.ShowLineNumbers = True
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
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
      SelectedColor.Alpha = 0.400000005960464500
    end
  end
  object pnlRight: TPanel
    Left = 2773
    Top = 128
    Width = 1071
    Height = 2181
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    object Splitter3: TSplitter
      Left = 1
      Top = 1491
      Width = 1069
      Height = 5
      Cursor = crVSplit
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alBottom
      ExplicitLeft = 2
      ExplicitTop = 2142
      ExplicitWidth = 1067
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 1496
      Width = 1069
      Height = 684
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alBottom
      TabOrder = 0
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 1069
      Height = 1490
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 1067
        Height = 1488
        Margins.Left = 7
        Margins.Top = 7
        Margins.Right = 7
        Margins.Bottom = 7
        Align = alClient
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 1062
        ExplicitHeight = 2136
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'rh'
    FileName = 'akt.rh'
    Filter = 'Rhodus scripts|*.rh|Any file|*.*'
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
        OnClick = Quit1Click
      end
      object mnuSave: TMenuItem
        Caption = 'Save'
        OnClick = mnuSaveClick
      end
      object mnuSaveAs: TMenuItem
        Caption = 'Save As...'
        OnClick = mnuSaveAsClick
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
    Filter = 'Rhodus Scripts|*.rh|Any file|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Rhodus Script'
    Left = 1044
    Top = 226
  end
end
