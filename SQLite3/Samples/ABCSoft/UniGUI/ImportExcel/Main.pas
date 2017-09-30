unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniGUIBaseClasses,
  uniButton, uniFileUpload, uniLabel, Data.DB, Datasnap.DBClient,
  uniBasicGrid, uniDBGrid;

type
  TMainForm = class(TUniForm)
    UniFileUpload1: TUniFileUpload;
    btnUpload: TUniButton;
    lbUploadInfo: TUniLabel;
    cds: TClientDataSet;
    DataSource1: TDataSource;
    cdsRECSEQ: TIntegerField;
    cdsTRORUN: TIntegerField;
    cdsTRNRUN: TIntegerField;
    cdsTRNCID: TStringField;
    cdsTRNFNA: TStringField;
    cdsTRNLNA: TStringField;
    cdsNATNAT: TStringField;
    cdsRACNAT: TStringField;
    cdsTRNTXT: TStringField;
    UniDBGrid1: TUniDBGrid;
    procedure btnUploadClick(Sender: TObject);
    procedure UniFileUpload1Completed(Sender: TObject;
      AStream: TFileStream);
  private
    { Private declarations }
    procedure ImportTXT(fileName:string);
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication, ServerModule;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

procedure TMainForm.btnUploadClick(Sender: TObject);
begin
  UniFileUpload1.Execute;
end;

function LoadStringFromFile(const filename: string; encoding: TEncoding = nil): string;
var
  FPreambleLength: Integer;
begin
  with TBytesStream.Create do
  try
    LoadFromFile(filename);
    FPreambleLength := TEncoding.GetBufferEncoding(Bytes, encoding);
    Result := encoding.GetString(Bytes, FPreambleLength, Size - FPreambleLength);
  finally
    Free;
  end;
end;


procedure TMainForm.ImportTXT(fileName: string);
var
 F: TextFile;
 S: string;
 i: integer;
 strList,tmpLine:TStringList;
begin
  strList:=TStringList.Create;
  tmpLine:=TStringList.Create;
  try
    tmpLine.Delimiter := #9;
    strList.Text:=LoadStringFromFile(fileName,TEncoding.Unicode);
    //  UniMemo1.Lines.Text:=strList.Text;

    for i:=0 to strList.Count - 1 do
    begin
    tmpLine.DelimitedText := strList[i];

    //UniMemo1.Lines.Add(tmpLine[0]+','+tmpLine[1]+','+tmpLine[2]+','+tmpLine[3]+','+tmpLine[4]+','+tmpLine[5]) ;


        if cds.State in [dsInsert,dsEdit] then
          cds.Post;



       cds.Append;

        cds.FieldByName('RECSEQ').AsInteger:=cds.RecordCount+1;
        cds.FieldByName('TRORUN').AsString:='001';
        cds.FieldByName('TRNRUN').AsInteger:=cds.RecordCount+1;


       //cdsTRAN_getTranOderNameList.FieldByName('RECSEQ').AsString:=inttostr(i+1);
       cds.FieldByName('TRNCID').AsString:=tmpLine[0];
       cds.FieldByName('TRNFNA').AsString:=tmpLine[1];
       cds.FieldByName('TRNLNA').AsString:=tmpLine[2];
       cds.FieldByName('NATNAT').AsString:=tmpLine[3];
       cds.FieldByName('RACNAT').AsString:=tmpLine[4];
       cds.FieldByName('TRNTXT').AsString:=strList[i];

       cds.post;








    end;

  finally
    strList.Free;
    tmpLine.Free;
  end;


 end;

procedure TMainForm.UniFileUpload1Completed(Sender: TObject;
  AStream: TFileStream);
var
  DestName : string;
  DestFolder,DestPath : string;
  fn,ext,AgentDocumentRunno,AgentDocumentRun:string;


begin
  //if IsImage then
  //begin
    //UniImage1.Picture.LoadFromFile(AStream.FileName);
   // UniLabel3.Caption:='File Name: '+UniFileUpload1.FileName;
  //end
  //else
  //begin


    ext:=ExtractFileExt(UniFileUpload1.FileName);
    fn:= 'DOC-' +FormatDateTime('hhmmss-zzz', Now())+ext;


    if (UpperCase(ext)='.JPG') or (UpperCase(ext)='.BMP') or (UpperCase(ext)='.GIF') or (UpperCase(ext)='.PNG') then
    begin
      DestPath:='Images';
    end
    else if (UpperCase(ext)='.DOC') or (UpperCase(ext)='.PDF') then
    begin
      DestPath:='Documents';
    end
    else
    begin
      DestPath:='Other';


    end;

      DestFolder:=UniServerModule.StartPath+'Upload\'+DestPath+'\';
      if not DirectoryExists(DestFolder) then
        ForceDirectories(DestFolder);


    DestName:=DestFolder+fn;//ExtractFileName(UniFileUpload1.FileName);
    lbUploadInfo.Caption:='File Name: '+UniFileUpload1.FileName;
    CopyFile(PChar(AStream.FileName), PChar(DestName), False); //DestName


    // process import
    ImportTXT(DestName);


end;


initialization
  RegisterAppFormClass(TMainForm);

end.
