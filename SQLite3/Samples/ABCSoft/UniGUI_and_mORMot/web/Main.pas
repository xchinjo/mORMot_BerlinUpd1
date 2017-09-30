unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm,data.db,

  SynRestMidasVCL, DBClient,
  SynCommons, mORMot, mORMotHttpClient, uniBasicGrid, uniDBGrid,
  uniGUIBaseClasses, uniButton,Project16Interface,mORMotMidasVCL;

type
  TMainForm = class(TUniForm)
    UniButton1: TUniButton;
    UniDBGrid1: TUniDBGrid;
    cds: TClientDataSet;
    ds: TDataSource;
    btnSave: TUniButton;
    btnExecute: TUniButton;
    btnExecuteUpdate: TUniButton;
    procedure UniFormCreate(Sender: TObject);
    procedure UniButton1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnExecuteUpdateClick(Sender: TObject);
  private
    { Private declarations }

    fServer, fPort, fRoot: RawUTF8;
    fSQLModel: TSQLModel;
    fRestClient: TSQLRestClientURI;


    fModel: TSQLModel;
    fClient: TSQLRestClientURI;
    fTableJSON: RawUTF8;
    fService: IRemoteSQL;

    function GetJsonDataset(StrSQL:RawUTF8):OleVariant;
    function GetCliRestDataset(const SQL: string): OleVariant;
    function UpdateDataset(cdsUpd:TclientDataset;RecValue:TSQLRecord;IDFieldName:RawUTF8):boolean;

  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication, RESTData;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

{ TMainForm }

procedure TMainForm.btnExecuteClick(Sender: TObject);
var
  cds:TClientDataSet;
begin
  cds:=TClientDataSet.Create(nil);
  try
    cds.Data:=GetJsonDataset('select * from INVM_PRICLIST');

    ShowMessage(inttostr(cds.RecordCount));

  finally
    cds.Free;
  end;


end;

procedure TMainForm.btnExecuteUpdateClick(Sender: TObject);
begin
 // GetJsonDataset('update INVM_PRICLIST set PRGCDE=''''')
 //fService.Execute('update INVM_PRICLIST set ITMSEQ=1',True,False);
 fService.Execute('update INVM_PRICLIST set PGRCDE=''xxxx''',True,False);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  pricelist:TSQLINVM_PRICLIST;
begin
  pricelist:=TSQLINVM_PRICLIST.Create;
  try
    UpdateDataset(cds,pricelist,'ID');
    ShowMessage('Save Successful.');
  finally
    pricelist.Free;
  end;


end;

function TMainForm.GetCliRestDataset(const SQL: string): OleVariant;
var
  restCds:TSynRestDataSet;
begin
  restCds:=TSynRestDataSet.Create(nil);
  try
    restCds.RestClient:=fRestClient;
    restCds.CommandText:=SQL;
    restCds.Open;

    //result := restCds.Data;

    Result := restCds.data;

  finally
    restCds.Free;
  end;

end;

function TMainForm.GetJsonDataset(StrSQL: RawUTF8): OleVariant;
var
  Cds:TClientDataSet;
  i:integer;
begin
  Cds:=TClientDataSet.Create(nil);
  try
    JSONToClientDataSet(cds,fService.Execute(StrSQL,True,False),nil,cdsNew,false);
    Result := cds.data;

  finally
    Cds.Free;
  end;

end;

procedure TMainForm.UniButton1Click(Sender: TObject);
begin
  cds.Data:=GetCliRestDataset('select * from INVM_PRICLIST');
end;

procedure TMainForm.UniFormCreate(Sender: TObject);
begin


  fServer := '127.0.0.1';
  fPort := '8880';
  fRoot := 'root';
  fSQLModel := DataModel;

  //fSQLModel.Props[TSQLSALT_BOOKMOB].ExternalDB.MapField('ID','BOORUN');


  fRestClient := TSQLHttpClient.Create(fServer, fPort, fSQLModel);
  fRestClient.SetUser('User','synopse');





  // execute
  fModel := TSQLModel.Create([],ROOT_NAME);
  fClient := TSQLHttpClient.Create('127.0.0.1','888',fModel);
  if not fClient.ServerTimeStampSynchronize then begin
    Close;
    exit;
  end;

  if //(not fClient.SetUser('User','synopse'))  or
     (not fClient.ServiceRegisterClientDriven(TypeInfo(IRemoteSQL),fService)) then begin
    Close;
    exit;
  end;



   fService.Connect(rseODBC,'','','','');


end;

function TMainForm.UpdateDataset(cdsUpd: TclientDataset;
  RecValue: TSQLRecord; IDFieldName: RawUTF8): boolean;
var    aColCount ,iColumn,fcount,i,k: integer;
    icount,ivar,fKeyCount,fIx:integer;

    fUpdDateTime:TDateTime;
    ss:string;

    procedure restupdatedata(IsNew:boolean=true);
    var k:integer;
    begin
               RecValue.IDValue:=cdsUpd.FieldByName(IDFieldName).Value;
               for k := 0 to  cdsUpd.FieldCount-1 do
                 RecValue.SetFieldVariant(cdsUpd.Fields[k].FieldName,cdsUpd.FieldByName(cdsUpd.Fields[k].FieldName).Value);
    end;

begin


  if not Assigned(RecValue) then
  begin
    result  := false;
    exit;
  end;



   cdsUpd.DisableControls;
   cdsUpd.StatusFilter := [usInserted,usModified];
   cdsUpd.First;
   while not cdsUpd.Eof do
   begin
      // record loop
      if fRestClient.Retrieve(cdsUpd.FieldByName(IDFieldName).Value,RecValue)  then
      begin
       restupdatedata(false);

       fRestClient.Update(RecValue,[]);
      end
      else
      begin
         restupdatedata;
         fRestClient.Add(RecValue,true,true);
      end;

      cdsUpd.Next;
   end;

   cdsUpd.StatusFilter := [usDeleted];
   cdsUpd.First;
   while not cdsUpd.Eof do
   begin
      if fRestClient.Retrieve(cdsUpd.FieldByName(IDFieldName).Value,RecValue)  then
      begin
       RecValue.SetFieldVariant('RECACT','I');
       fRestClient.Update(RecValue,[]);
      end;

      cdsUpd.Next;
   end;


   cdsUpd.StatusFilter:=[];
   cdsUpd.EnableControls;


end;



initialization
  RegisterAppFormClass(TMainForm);

end.
