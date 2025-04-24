object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 987
  ClientWidth = 1756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 1036
    Top = 86
    Width = 6
    Height = 901
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    ExplicitLeft = 981
    ExplicitHeight = 808
  end
  object Splitter4: TSplitter
    Left = 321
    Top = 86
    Width = 5
    Height = 901
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ExplicitHeight = 808
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1756
    Height = 86
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 1701
    object Label1: TLabel
      Left = 596
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
    object lblVersion: TLabel
      Left = 1669
      Top = 1
      Width = 86
      Height = 84
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'lblVersion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 1614
      ExplicitHeight = 28
    end
    object btnRun: TButton
      Left = 456
      Top = 15
      Width = 99
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
      Left = 594
      Top = 39
      Width = 225
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
    object btnSaveAs: TButton
      Left = 347
      Top = 15
      Width = 99
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Save As'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnSaveAsClick
    end
  end
  object pnlLeftPanel: TPanel
    Left = 0
    Top = 86
    Width = 321
    Height = 901
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 808
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
      Height = 512
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
      ExplicitHeight = 419
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
    Width = 710
    Height = 901
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 655
    ExplicitHeight = 808
    object pblBottomBase: TPanel
      Left = 1
      Top = 570
      Width = 708
      Height = 330
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 477
      ExplicitWidth = 653
      object Splitter1: TSplitter
        Left = 1
        Top = 1
        Width = 706
        Height = 3
        Cursor = crVSplit
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alTop
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 649
      end
      object memoOutput: TMemo
        Left = 1
        Top = 4
        Width = 706
        Height = 260
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
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitWidth = 651
      end
      object pnlMemoButtons: TPanel
        Left = 1
        Top = 264
        Width = 706
        Height = 65
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alBottom
        TabOrder = 1
        ExplicitWidth = 651
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
      Width = 708
      Height = 569
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
        'x = p[0]/2-50; y = p[1]/2-80;'
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
      ExplicitWidth = 653
      ExplicitHeight = 476
    end
  end
  object pnlRight: TPanel
    Left = 1042
    Top = 86
    Width = 714
    Height = 901
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    ExplicitLeft = 987
    ExplicitHeight = 808
    object Splitter3: TSplitter
      Left = 1
      Top = 441
      Width = 712
      Height = 3
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 2
      ExplicitTop = 348
      ExplicitWidth = 711
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 444
      Width = 712
      Height = 456
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ParentBackground = False
      TabOrder = 0
      ExplicitTop = 351
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 712
      Height = 440
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      ExplicitHeight = 347
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 710
        Height = 438
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 708
        ExplicitHeight = 343
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
      'PRINT'
      'PRINTLN'
      'REPEAT'
      'SWITCH'
      'THEN'
      'TO'
      'UNTIL'
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
