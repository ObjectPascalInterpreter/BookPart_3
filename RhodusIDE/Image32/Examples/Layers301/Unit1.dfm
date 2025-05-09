object MainForm: TMainForm
  Left = 594
  Top = 418
  Caption = 'Layers301'
  ClientHeight = 411
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar1: TStatusBar
    Left = 0
    Top = 385
    Width = 455
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 
      'Selecting a layer before adding a new one will create a nested c' +
      'hild layer'
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = mnuExitClick
      end
    end
    object Action1: TMenuItem
      Caption = '&Action'
      OnClick = PopupMenu1Popup
      object mnuHideHatching: TMenuItem
        AutoCheck = True
        Caption = 'Hide &Hatching'
        ShortCut = 16456
        OnClick = mnuHideHatchingClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuAddRectangle: TMenuItem
        Caption = 'Add &Rectangle'
        ShortCut = 16466
        OnClick = mnuAddRectangleClick
      end
      object mnuAddEllipse: TMenuItem
        Caption = 'Add &Ellipse'
        ShortCut = 16453
        OnClick = mnuAddEllipseClick
      end
      object AddStar1: TMenuItem
        Caption = 'Add &Star'
        ShortCut = 16467
        OnClick = AddStar1Click
      end
      object AddImage1: TMenuItem
        Caption = 'Add &Image ...'
        ShortCut = 16457
        OnClick = AddImage1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuSendBack: TMenuItem
        Caption = 'Send &Back'
        ShortCut = 16450
        OnClick = mnuSendBackClick
      end
      object mnuBringForward: TMenuItem
        Caption = 'Bring &Forward'
        ShortCut = 16454
        OnClick = mnuBringForwardClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuDeleteLayer2: TMenuItem
        Caption = '&Delete Layer'
        ShortCut = 46
        OnClick = mnuDeleteLayerClick
      end
      object ChangeColor1: TMenuItem
        Caption = '&Change Color ...'
        ShortCut = 16451
        OnClick = ChangeColor1Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 176
    Top = 80
    object AddRectangle1: TMenuItem
      Caption = 'Add &Rectangle'
      ShortCut = 16466
      OnClick = mnuAddRectangleClick
    end
    object AddEllipse1: TMenuItem
      Caption = 'Add &Ellipse'
      ShortCut = 16453
      OnClick = mnuAddEllipseClick
    end
    object AddStar2: TMenuItem
      Caption = 'Add &Star'
      ShortCut = 16467
      OnClick = AddStar1Click
    end
    object AddImage2: TMenuItem
      Caption = 'Add &Image ...'
      ShortCut = 16457
      OnClick = AddImage1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SendBack1: TMenuItem
      Caption = 'Send &Back'
      ShortCut = 16450
      OnClick = mnuSendBackClick
    end
    object BringForward1: TMenuItem
      Caption = 'Bring &Forward'
      ShortCut = 16454
      OnClick = mnuBringForwardClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuDeleteLayer: TMenuItem
      Caption = '&Delete Layer'
      ShortCut = 46
      OnClick = mnuDeleteLayerClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image Files|*.bmp;*.png;*.jpg;*.svg'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 232
    Top = 80
  end
  object ColorDialog1: TColorDialog
    Left = 296
    Top = 80
  end
end
