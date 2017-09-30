object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 340
  ClientWidth = 847
  Caption = 'MainForm'
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  PixelsPerInch = 96
  TextHeight = 13
  object btnUpload: TUniButton
    Left = 32
    Top = 17
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Upload'
    TabOrder = 0
    OnClick = btnUploadClick
  end
  object lbUploadInfo: TUniLabel
    Left = 176
    Top = 20
    Width = 4
    Height = 13
    Hint = ''
    Caption = ':'
    TabOrder = 1
  end
  object UniDBGrid1: TUniDBGrid
    Left = 32
    Top = 48
    Width = 793
    Height = 241
    Hint = ''
    DataSource = DataSource1
    LoadMask.Message = 'Loading data...'
    TabOrder = 2
  end
  object UniFileUpload1: TUniFileUpload
    OnCompleted = UniFileUpload1Completed
    Title = 'Upload'
    Messages.Uploading = 'Uploading...'
    Messages.PleaseWait = 'Please Wait'
    Messages.Cancel = 'Cancel'
    Messages.Processing = 'Processing...'
    Messages.UploadError = 'Upload Error'
    Messages.Upload = 'Upload'
    Messages.NoFileError = 'Please Select a File'
    Messages.BrowseText = 'Browse...'
    Left = 432
    Top = 96
  end
  object cds: TClientDataSet
    PersistDataPacket.Data = {
      E90000009619E0BD010000001800000009000000000003000000E90006524543
      53455104000100000000000654524F52554E04000100000000000654524E5255
      4E04000100000000000654524E43494401004900000001000557494454480200
      0200C8000654524E464E41010049000000010005574944544802000200C80006
      54524E4C4E41010049000000010005574944544802000200C800064E41544E41
      54010049000000010005574944544802000200C800065241434E415401004900
      0000010005574944544802000200C8000654524E545854010049000000010005
      574944544802000200C8000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 416
    Top = 176
    object cdsRECSEQ: TIntegerField
      FieldName = 'RECSEQ'
    end
    object cdsTRORUN: TIntegerField
      FieldName = 'TRORUN'
    end
    object cdsTRNRUN: TIntegerField
      FieldName = 'TRNRUN'
    end
    object cdsTRNCID: TStringField
      FieldName = 'TRNCID'
      Size = 200
    end
    object cdsTRNFNA: TStringField
      FieldName = 'TRNFNA'
      Size = 200
    end
    object cdsTRNLNA: TStringField
      FieldName = 'TRNLNA'
      Size = 200
    end
    object cdsNATNAT: TStringField
      FieldName = 'NATNAT'
      Size = 200
    end
    object cdsRACNAT: TStringField
      FieldName = 'RACNAT'
      Size = 200
    end
    object cdsTRNTXT: TStringField
      FieldName = 'TRNTXT'
      Size = 200
    end
  end
  object DataSource1: TDataSource
    DataSet = cds
    Left = 544
    Top = 160
  end
end
