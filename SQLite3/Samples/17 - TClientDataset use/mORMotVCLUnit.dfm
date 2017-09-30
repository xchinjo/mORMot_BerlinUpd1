object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'Form1'
  ClientHeight = 41
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = chkFromSQLClick
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrdData: TDBGrid
    Left = 0
    Top = 41
    Width = 741
    Height = 0
    Align = alClient
    DataSource = ds1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 741
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitTop = -6
    object lblTiming: TLabel
      Left = 456
      Top = 13
      Width = 241
      Height = 13
      AutoSize = False
    end
    object lblFrom: TLabel
      Left = 40
      Top = 12
      Width = 28
      Height = 13
      Caption = 'From:'
      Visible = False
    end
    object chkViaTClientDataSet: TCheckBox
      Left = 216
      Top = 12
      Width = 137
      Height = 17
      Caption = 'Via TClientDataSet'
      TabOrder = 0
      Visible = False
      OnClick = chkFromSQLClick
    end
    object cbbDataSource: TComboBox
      Left = 74
      Top = 10
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 10
      ItemIndex = 0
      TabOrder = 1
      Text = 'JSON direct'
      Visible = False
      OnChange = chkFromSQLClick
      Items.Strings = (
        'JSON direct'
        'JSON TDocVariant'
        'SQLite3 direct'
        'SQLite3 proxy direct'
        'SQLite3 proxy compressed'
        'SQLite3 HTTP WinHTTP'
        'SQLite3 HTTP WinINet'
        'SQLite3 HTTP Sockets'
        'SQLite3 SQL TDataSet')
    end
    object btnRefresh: TButton
      Left = 359
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      Visible = False
      OnClick = chkFromSQLClick
    end
    object btnApply: TButton
      Left = 648
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Apply Updates'
      TabOrder = 3
      Visible = False
      OnClick = btnApplyClick
    end
  end
  object ds1: TDataSource
    Left = 96
    Top = 72
  end
end
