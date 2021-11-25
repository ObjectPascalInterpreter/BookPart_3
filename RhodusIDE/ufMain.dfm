object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 701
  ClientWidth = 1166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -8
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 11
  object Splitter2: TSplitter
    Left = 807
    Top = 57
    Width = 4
    Height = 644
    Align = alRight
    ExplicitHeight = 693
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1166
    Height = 57
    Align = alTop
    TabOrder = 0
    DesignSize = (
      1166
      57)
    object lblVersion: TLabel
      Left = 1013
      Top = 12
      Width = 57
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 303
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Sample Scripts:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnRun: TButton
      Left = 156
      Top = 10
      Width = 65
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
      Width = 65
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
    object btnClear: TButton
      Left = 230
      Top = 10
      Width = 65
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
      TabOrder = 2
      OnClick = btnClearClick
    end
    object cboExamples: TComboBox
      Left = 302
      Top = 26
      Width = 145
      Height = 25
      DropDownCount = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
    object btnNew: TButton
      Left = 5
      Top = 12
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
      TabOrder = 4
      OnClick = mnuNewClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 57
    Width = 185
    Height = 644
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 693
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 1
      Width = 183
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
      ExplicitWidth = 184
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 233
      Width = 183
      Height = 410
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
      ExplicitWidth = 184
      ExplicitHeight = 459
    end
  end
  object Panel3: TPanel
    Left = 185
    Top = 57
    Width = 622
    Height = 644
    Align = alClient
    TabOrder = 2
    ExplicitHeight = 693
    object Splitter1: TSplitter
      Left = 1
      Top = 400
      Width = 620
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 449
    end
    object moutput: TMemo
      Left = 1
      Top = 403
      Width = 620
      Height = 240
      Align = alBottom
      Color = clHighlight
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -15
      Font.Name = 'Fira Code'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      ExplicitTop = 452
    end
    object editor: TSynEdit
      Left = 1
      Top = 1
      Width = 620
      Height = 399
      Align = alClient
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      Gutter.Color = clGradientActiveCaption
      Gutter.DigitCount = 3
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -16
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.LeftOffset = 8
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
        'end'
        '')
      RightEdge = 200
      SearchEngine = SynEditSearch1
      ExplicitHeight = 448
    end
  end
  object pnlRight: TPanel
    Left = 811
    Top = 57
    Width = 355
    Height = 644
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    ExplicitHeight = 693
    object Splitter3: TSplitter
      Left = 1
      Top = 337
      Width = 353
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 386
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 339
      Width = 353
      Height = 304
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 388
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 353
      Height = 336
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      ExplicitHeight = 385
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 351
        Height = 334
        Align = alClient
        ExplicitWidth = 352
        ExplicitHeight = 384
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
        OnClick = Quit1Click
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
    KeyAttri.Foreground = clFuchsia
    KeyWords.Strings = (
      'CASE'
      'DO'
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
end
