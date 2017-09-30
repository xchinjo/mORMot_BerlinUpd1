unit Project16ClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons,mORMotMidasVCL, mORMot, mORMotHttpClient, mORMotUI, mORMotUILogin,
  Project16Interface, ExtCtrls, Grids, Data.DB, Datasnap.DBClient,
  Vcl.DBGrids;

type
  TProjectSettings = class(TPersistent)
  private
    fDatabaseName: RawUTF8;
    fPassword: RawUTF8;
    fUserID: RawUTF8;
    fServerName: RawUTF8;
    fEngine: TRemoteSQLEngine;
  published
    property Engine: TRemoteSQLEngine read fEngine;
    property ServerName: RawUTF8 read fServerName;
    property DatabaseName: RawUTF8 read fDatabaseName;
    property UserID: RawUTF8 read fUserID;
    property PassWord: RawUTF8 read fPassword;
  end;

  TMainForm = class(TForm)
    mmoQuery: TMemo;
    spl1: TSplitter;
    pnlLogin: TPanel;
    drwgrdData: TDrawGrid;
    lbledtServer: TLabeledEdit;
    lbledtDatabase: TLabeledEdit;
    lbledtUser: TLabeledEdit;
    lbledtPassword: TLabeledEdit;
    cbbEngine: TComboBox;
    btnOpen: TButton;
    pnlCommand: TPanel;
    btnExecute: TButton;
    lbl1: TLabel;
    cbbTableNames: TComboBox;
    lblSelectTable: TLabel;
    lbedRemoteSErver: TLabeledEdit;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure cbbTableNamesChange(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  protected
    fSettings: TProjectSettings;
    fSettingsFileName: TFileName;
    fModel: TSQLModel;
    fClient: TSQLRestClientURI;
    fTableJSON: RawUTF8;
    fService: IRemoteSQL;
  public
    function Execute(FormatSQLWhere: PUTF8Char; const BoundsSQLWhere: array of const): TSQLTableJSON; overload;
    procedure ExecuteSQL(FormatSQLWhere: PUTF8Char; const BoundsSQLWhere: array of const);
    function getDataset(strSQL:RawUTF8):TClientDataSet;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{$R Vista.res}

function TMainForm.Execute(FormatSQLWhere: PUTF8Char;
  const BoundsSQLWhere: array of const): TSQLTableJSON;
var SQL: RawUTF8;
begin
  SQL := FormatUTF8(FormatSQLWhere,[],BoundsSQLWhere);
  result := TSQLTableJSON.Create(SQL,fService.Execute(SQL,True,False));
end;

procedure TMainForm.ExecuteSQL(FormatSQLWhere: PUTF8Char;
  const BoundsSQLWhere: array of const);
begin
  fService.Execute(FormatUTF8(FormatSQLWhere,[],BoundsSQLWhere),False,False);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fService := nil;
  fClient.Free;
  fModel.Free;
  fSettings.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PTypeInfo(TypeInfo(TRemoteSQLEngine))^.EnumBaseType^.AddCaptionStrings(cbbEngine.Items);
  fSettings := TProjectSettings.Create;
  fSettingsFileName := ChangeFileExt(ExeVersion.ProgramFileName,'.settings');
  JSONFileToObject(fSettingsFileName,fSettings);
  cbbEngine.ItemIndex := ord(fSettings.fEngine);
  lbledtServer.Text := UTF8ToString(fSettings.fServerName);
  lbledtDatabase.Text := UTF8ToString(fSettings.fDatabaseName);
  lbledtUser.Text := UTF8ToString(fSettings.fUserID);
  lbledtPassword.Text := UTF8ToString(fSettings.fPassword);
  fModel := TSQLModel.Create([],ROOT_NAME);
  fClient := TSQLHttpClient.Create(lbedRemoteSErver.Text,PORT_NAME,fModel); //'localhost'
  if not fClient.ServerTimeStampSynchronize then begin
    ShowLastClientError(fClient,'Please run Project16ServerHttp.exe');
    Close;
    exit;
  end;
  if (not fClient.SetUser('User','synopse'))  or
     (not fClient.ServiceRegisterClientDriven(TypeInfo(IRemoteSQL),fService)) then begin
    ShowLastClientError(fClient,'Remote service not available on server');
    Close;
    exit;
  end;
end;


procedure TMainForm.btnOpenClick(Sender: TObject);
var TableNames: TRawUTF8DynArray;
begin
  if cbbEngine.ItemIndex>=0 then
  try
    fSettings.fEngine := TRemoteSQLEngine(cbbEngine.ItemIndex);
    fSettings.fServerName := StringToUTF8(lbledtServer.Text);
    fSettings.fDatabaseName := StringToUTF8(lbledtDatabase.Text);
    fSettings.fUserID := StringToUTF8(lbledtUser.Text);
    fSettings.fPassword := StringToUTF8(lbledtPassword.Text);
    ObjectToJSONFile(fSettings,fSettingsFileName);
    with fSettings do
      fService.Connect(Engine,ServerName,DatabaseName,UserID,PassWord);
    pnlLogin.Hide;
    mmoQuery.Show;
    pnlCommand.Show;
    drwgrdData.Show;
    with CreateTempForm('Please wait') do begin
      TableNames := fService.GetTableNames;
      Free;
    end;
    cbbTableNames.Items.Text := UTF8ToString(RawUTF8ArrayToCSV(TableNames,#13#10));
  except
    on E: Exception do
      ShowException(E);
  end;
end;

procedure TMainForm.cbbTableNamesChange(Sender: TObject);
begin
  mmoQuery.Text := 'select * from '+cbbTableNames.Text;
end;


function TMainForm.getDataset(strSQL:RawUTF8): TClientDataSet;
begin
  result := JSONToClientDataSet(self,fService.Execute(strSQL,True,False));
end;

procedure TMainForm.btnExecuteClick(Sender: TObject);
var SQL: RawUTF8;
    cds:TClientDataSet;
begin
  SQL := trim(StringToUTF8(mmoQuery.Text));
  Screen.Cursor := crHourGlass;
  cds := TClientDataSet.Create(nil);
  try
    try

      if isSelect(pointer(SQL)) then begin
        fTableJSON := fService.Execute(SQL,True,False);
        TSQLTableToGrid.Create(drwgrdData,
          TSQLTableJSON.Create(SQL,pointer(fTableJSON),Length(fTableJSON)),fClient);
      end; //else

      // ##1
       DataSource1.DataSet
       :=JSONToClientDataSet(self,fService.Execute(SQL,True,False));// fService.Execute(SQL,False,False);

      //## 2
       ClientDataSet1:=getDataset(SQL);
       DataSource1.DataSet:=ClientDataSet1;




      //JSONToClientDataSet(cds,fService.Execute(SQL,True,False));// fService.Execute(SQL,False,False);
      //DataSource1.DataSet:=cds;


      // ShowMessage(inttostr(cds.RecordCount));







    except
      on E: Exception do
        ShowException(E);
    end;
  finally
    Screen.Cursor := crDefault;
    cds.Free;
  end;
end;

end.
