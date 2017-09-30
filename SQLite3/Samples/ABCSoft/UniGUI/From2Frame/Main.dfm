object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 639
  ClientWidth = 1291
  Caption = 'MainForm'
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  OnBeforeShow = UniFormBeforeShow
  PixelsPerInch = 96
  TextHeight = 13
  object UniPanel1: TUniPanel
    Left = 0
    Top = 0
    Width = 1291
    Height = 65
    Hint = ''
    Align = alTop
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Caption = ''
    object UniButton1: TUniButton
      Left = 24
      Top = 16
      Width = 75
      Height = 25
      Hint = ''
      Caption = 'UniButton1'
      TabOrder = 1
      OnClick = UniButton1Click
    end
    object UniButton2: TUniButton
      Left = 105
      Top = 16
      Width = 75
      Height = 25
      Hint = ''
      Caption = 'UniButton2'
      TabOrder = 2
      OnClick = UniButton2Click
    end
  end
  object NavPage: TUniPageControl
    Left = 0
    Top = 392
    Width = 1291
    Height = 247
    Hint = ''
    ActivePage = UniTabSheet1
    Align = alCustom
    TabOrder = 1
    object UniTabSheet1: TUniTabSheet
      Hint = ''
      Caption = 'UniTabSheet1'
    end
  end
  object pnBody: TUniPanel
    Left = 8
    Top = 88
    Width = 1089
    Height = 289
    Hint = ''
    TabOrder = 2
    Caption = 'pnBody'
    AlignmentControl = uniAlignmentClient
    ParentAlignmentControl = False
  end
end
