object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 741
  ClientWidth = 1166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -8
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 11
  object Splitter2: TSplitter
    Left = 807
    Top = 57
    Width = 4
    Height = 684
    Align = alRight
  end
  object Splitter4: TSplitter
    Left = 214
    Top = 57
    Height = 684
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1166
    Height = 57
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 1158
    object Label1: TLabel
      Left = 397
      Top = 3
      Width = 94
      Height = 17
      Caption = 'Sample Scripts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblVersion: TLabel
      Left = 1108
      Top = 1
      Width = 57
      Height = 55
      Align = alRight
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'lblVersion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ExplicitLeft = 1096
      ExplicitTop = 18
      ExplicitHeight = 17
    end
    object btnRun: TButton
      Left = 304
      Top = 10
      Width = 66
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Run'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnRunClick
    end
    object btnLoad: TButton
      Left = 81
      Top = 10
      Width = 66
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object cboExamples: TComboBox
      Left = 396
      Top = 26
      Width = 150
      Height = 25
      DropDownCount = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
    object btnNew: TButton
      Left = 5
      Top = 10
      Width = 65
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'New'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = mnuNewClick
    end
    object btnSave: TButton
      Left = 157
      Top = 10
      Width = 66
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnSaveClick
    end
    object btnSaveAs: TButton
      Left = 231
      Top = 10
      Width = 66
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Save As'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = btnSaveAsClick
    end
  end
  object pnlLeftPanel: TPanel
    Left = 0
    Top = 57
    Width = 214
    Height = 684
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 679
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 27
      Width = 212
      Height = 232
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 17
      ParentFont = False
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 259
      Width = 212
      Height = 424
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 17
      ParentFont = False
      TabOrder = 1
      OnClick = FileListBox1Click
      ExplicitHeight = 419
    end
    object pnlSideControls: TPanel
      Left = 1
      Top = 1
      Width = 212
      Height = 26
      Align = alTop
      TabOrder = 2
      object btnCloseSidePanel: TSpeedButton
        Left = 9
        Top = 3
        Width = 25
        Height = 20
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
    Left = 217
    Top = 57
    Width = 590
    Height = 684
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 582
    ExplicitHeight = 679
    object pblBottomBase: TPanel
      Left = 1
      Top = 463
      Width = 588
      Height = 220
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 458
      ExplicitWidth = 580
      object Splitter1: TSplitter
        Left = 1
        Top = 1
        Width = 586
        Height = 2
        Cursor = crVSplit
        Align = alTop
        ExplicitWidth = 587
      end
      object memoOutput: TMemo
        Left = 1
        Top = 3
        Width = 586
        Height = 174
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitWidth = 578
      end
      object pnlMemoButtons: TPanel
        Left = 1
        Top = 177
        Width = 586
        Height = 42
        Align = alBottom
        TabOrder = 1
        ExplicitWidth = 578
        object btnClear: TButton
          Left = 5
          Top = 3
          Width = 66
          Height = 36
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Clear'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
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
      Width = 588
      Height = 462
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -16
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
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
      ExplicitWidth = 580
      ExplicitHeight = 457
    end
  end
  object pnlRight: TPanel
    Left = 811
    Top = 57
    Width = 355
    Height = 684
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    ExplicitLeft = 803
    ExplicitHeight = 679
    object Splitter3: TSplitter
      Left = 1
      Top = 377
      Width = 353
      Height = 2
      Cursor = crVSplit
      Align = alBottom
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 379
      Width = 353
      Height = 304
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 374
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 353
      Height = 376
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      ExplicitHeight = 371
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 351
        Height = 374
        Align = alClient
        ExplicitWidth = 352
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
      end
      object mnuSave: TMenuItem
        Caption = 'Save'
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
    Filter = 'Rhodus Scripts|*.rh|Any file|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Rhodus Script'
    Left = 1044
    Top = 226
  end
end
