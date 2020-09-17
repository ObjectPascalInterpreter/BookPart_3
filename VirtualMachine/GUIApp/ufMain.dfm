object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'VM GUI Application'
  ClientHeight = 555
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 768
    Height = 41
    Align = alTop
    Caption = 'Virtual Machine Runner'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
  end
  object plkBottom: TPanel
    Left = 0
    Top = 506
    Width = 768
    Height = 49
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      768
      49)
    object btnClose: TButton
      Left = 667
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object pnkMain: TPanel
    Left = 0
    Top = 41
    Width = 768
    Height = 465
    Align = alClient
    TabOrder = 2
    DesignSize = (
      768
      465)
    object Label1: TLabel
      Left = 378
      Top = 396
      Width = 232
      Height = 14
      Caption = 'Average time taken to execute (millisecs):'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblTimeTaken: TLabel
      Left = 616
      Top = 396
      Width = 7
      Height = 14
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 376
      Top = 39
      Width = 40
      Height = 14
      Caption = 'Output'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 360
      Height = 463
      Align = alLeft
      TabOrder = 0
      DesignSize = (
        360
        463)
      object lblFileName: TLabel
        Left = 97
        Top = 37
        Width = 46
        Height = 13
        Caption = 'File Name'
      end
      object Label2: TLabel
        Left = 144
        Top = 403
        Width = 114
        Height = 13
        Caption = 'How many times to run:'
      end
      object mnoCode: TMemo
        Left = 16
        Top = 56
        Width = 328
        Height = 337
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier'
        Font.Style = []
        Lines.Strings = (
          'pushi 2'
          'pushi 7'
          'add'
          'halt')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btnAssemble: TButton
        Left = 16
        Top = 399
        Width = 113
        Height = 25
        Caption = 'Assemble and Run'
        TabOrder = 1
        OnClick = btnAssembleClick
      end
      object btnLoad: TButton
        Left = 16
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Load Script'
        TabOrder = 2
        OnClick = btnLoadClick
      end
      object edtTimesToRun: TEdit
        Left = 264
        Top = 399
        Width = 49
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '1'
      end
      object btnSaveData: TButton
        Left = 16
        Top = 430
        Width = 113
        Height = 25
        Caption = 'Save Data'
        TabOrder = 4
        OnClick = btnSaveDataClick
      end
    end
    object mnoOutput: TMemo
      Left = 376
      Top = 55
      Width = 375
      Height = 335
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'asm'
    Filter = 'Any Files|*.*|Asm Files|*.asm'
    Left = 592
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'csv'
    Left = 520
    Top = 9
  end
end
