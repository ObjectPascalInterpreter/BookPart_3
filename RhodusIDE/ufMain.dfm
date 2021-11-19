object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Rhodus IDE'
  ClientHeight = 1109
  ClientWidth = 1749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 25
  object Splitter2: TSplitter
    Left = 1211
    Top = 86
    Width = 5
    Height = 1023
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    ExplicitLeft = 1179
    ExplicitHeight = 871
  end
  object Panel1: TPanel
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
    ExplicitWidth = 1561
    DesignSize = (
      1749
      86)
    object lblVersion: TLabel
      Left = 1533
      Top = 18
      Width = 77
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'lblVersion'
      ExplicitLeft = 1359
    end
    object Label1: TLabel
      Left = 454
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
      Left = 234
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
      Left = 121
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
      Left = 345
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
      Left = 453
      Top = 39
      Width = 217
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      DropDownCount = 16
      TabOrder = 3
      Text = 'Examples'
      OnChange = cboExamplesChange
    end
    object btnNew: TButton
      Left = 8
      Top = 18
      Width = 97
      Height = 54
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'New'
      TabOrder = 4
      OnClick = mnuNewClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 86
    Width = 278
    Height = 1023
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 864
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
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 349
      Width = 276
      Height = 673
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      ItemHeight = 25
      TabOrder = 1
      OnClick = FileListBox1Click
      ExplicitHeight = 514
    end
  end
  object Panel3: TPanel
    Left = 278
    Top = 86
    Width = 933
    Height = 1023
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 901
    ExplicitHeight = 871
    object Splitter1: TSplitter
      Left = 1
      Top = 658
      Width = 931
      Height = 4
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 2
      ExplicitTop = 506
      ExplicitWidth = 898
    end
    object moutput: TMemo
      Left = 1
      Top = 662
      Width = 931
      Height = 360
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
      ExplicitTop = 503
      ExplicitWidth = 885
    end
    object editor: TMemo
      Left = 1
      Top = 1
      Width = 931
      Height = 657
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCream
      Font.Height = -16
      Font.Name = 'Fira Code Medium'
      Font.Style = []
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
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      StyleElements = [seClient, seBorder]
      ExplicitWidth = 896
    end
  end
  object pnlRight: TPanel
    Left = 1216
    Top = 86
    Width = 533
    Height = 1023
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    TabOrder = 3
    OnResize = pnlRightResize
    object Splitter3: TSplitter
      Left = 1
      Top = 563
      Width = 531
      Height = 3
      Cursor = crVSplit
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ExplicitLeft = 4
      ExplicitTop = 411
      ExplicitWidth = 389
    end
    object pnlInfo: TPanel
      Left = 1
      Top = 566
      Width = 531
      Height = 456
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      TabOrder = 0
      ExplicitLeft = -15
      ExplicitTop = 139
      ExplicitWidth = 389
    end
    object pnlImage: TPanel
      Left = 1
      Top = 1
      Width = 531
      Height = 562
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'pnlImage'
      TabOrder = 1
      OnResize = pnlRightResize
      ExplicitLeft = 108
      ExplicitTop = 156
      ExplicitWidth = 278
      ExplicitHeight = 62
      object pnlDrawing: TImage
        Left = 1
        Top = 1
        Width = 529
        Height = 560
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        ExplicitLeft = 144
        ExplicitTop = 168
        ExplicitWidth = 158
        ExplicitHeight = 158
      end
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
  object MainMenu: TMainMenu
    Left = 848
    Top = 24
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
end
