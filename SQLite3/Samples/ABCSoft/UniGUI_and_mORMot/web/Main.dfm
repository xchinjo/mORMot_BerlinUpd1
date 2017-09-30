object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 390
  ClientWidth = 877
  Caption = 'MainForm'
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  OnCreate = UniFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object UniButton1: TUniButton
    Left = 8
    Top = 33
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'get'
    TabOrder = 0
    OnClick = UniButton1Click
  end
  object UniDBGrid1: TUniDBGrid
    Left = 8
    Top = 64
    Width = 861
    Height = 318
    Hint = ''
    DataSource = ds
    LoadMask.Message = 'Loading data...'
    TabOrder = 1
  end
  object btnSave: TUniButton
    Left = 89
    Top = 33
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Save'
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnExecute: TUniButton
    Left = 248
    Top = 33
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Execute'
    TabOrder = 3
    OnClick = btnExecuteClick
  end
  object btnExecuteUpdate: TUniButton
    Left = 352
    Top = 33
    Width = 121
    Height = 25
    Hint = ''
    Caption = 'Execute Update'
    TabOrder = 4
    OnClick = btnExecuteUpdateClick
  end
  object cds: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 120
    Top = 16
  end
  object ds: TDataSource
    DataSet = cds
    Left = 184
    Top = 16
  end
end
