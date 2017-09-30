unit Ffactwin;

{ This application shows how to display TSynRestDataset style memo and graphic
 fields in a form.

 - This application use TImage for display the image from Project19Server.db3.
 - Originally Implemented by EMartin
 - Modified by HOUDW2006 2016-05-09
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, DBCtrls, DBGrids, DB, Buttons, ExtCtrls,
  SynRestMidasVCL, DBClient,
  SynCommons, mORMot, mORMotHttpClient,
  OleCtrls, Dialogs, ExtDlgs,
  SynGdiPlus, Grids, SampleData, ImageLoader;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBMemo1: TDBMemo;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    dbnvgr1: TDBNavigator;
    btnUpload: TButton;
    dlgOpenPic1: TOpenPictureDialog;
    img: TImage;
    DBLabel1: TDBText;
    pnl1: TPanel;
    lbl1: TLabel;
    dbtxtCommon_Name: TDBText;
    cds: TClientDataSet;
    ds1: TDataSource;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
    procedure btnUploadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fImageLoader: TImageLoader;
    fServer, fPort, fRoot: RawUTF8;
    fSQLModel: TSQLModel;
    fRestClient: TSQLRestClientURI;
    SynRestDataset: TSynRestDataset;

    procedure DoOnAfterScroll(Dataset: TDataset);

    function GetCliRestDataset(const SQL: string): OleVariant;
    function UpdateDataset(cdsUpd:TclientDataset;RecValue:TSQLRecord;IDFieldName:RawUTF8):boolean;


  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  fImageLoader := TImageLoader.Create;	// used to load image from blob fields

  //http://1.4.157.59:8880/root/INVM_PROD
  fServer := '127.0.0.1';
  fPort := '8880';
  fRoot := 'root';
  fSQLModel := CreateSampleModel(fRoot);
  //fSQLModel.Props[TSQLSALT_BOOKMOB].ExternalDB.MapField('ID','BOORUN');


  fRestClient := TSQLHttpClient.Create(fServer, fPort, fSQLModel);
  fRestClient.SetUser('User','synopse');

  cds.Data:=GetCliRestDataset('SELECT * FROM INVM_PRICLIST');

  (*
  SynRestDataset := TSynRestDataset.Create(Nil);
  SynRestDataset.RestClient := fRestClient;
  SynRestDataset.CommandText := 'SELECT * FROM INVM_PRICLIST ';

  // WHERE and/or ORDER BY clauses, and Parameters can be used as well.
  //SynRestDataset.CommandText := 'SELECT * FROM BioLife '
  //    + 'WHERE Species_No < :Species_No '
  //    + 'ORDER BY Species_No ';
  //SynRestDataset.Params.ParamByName('SPecies_No').Value := 100;
  //SynRestDataset.AfterScroll := DoOnAfterScroll;
  SynRestDataset.Open;




  DataSource1.DataSet := SynRestDataset;
  // show the first record image
  DoOnAfterScroll(Nil);
  // hide blob and ID fields in the grid

  for I := 0 to DBGrid1.Columns.Count-1 do
  begin
    if (DBGrid1.Columns[I].Field.DataType = DB.ftBlob) then
      DBGrid1.Columns[I].Visible := False
    else if (DBGrid1.Columns[I].Field.FieldName = 'ID') then  // Hide the ID column
      DBGrid1.Columns[I].Visible := False;
  end;


  *)

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fImageLoader);
  FreeAndNil(SynRestDataset);
  FreeAndNil(fRestClient);
  FreeAndNil(fSQLModel);
end;

function TForm1.GetCliRestDataset(const SQL: string): OleVariant;
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

function TForm1.UpdateDataset(cdsUpd: TclientDataset; RecValue: TSQLRecord;
  IDFieldName: RawUTF8): boolean;
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


procedure TForm1.Button1Click(Sender: TObject);
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

procedure TForm1.dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
begin
{  case Button of
    nbDelete, nbPost:
      SynRestDataset.ApplyUpdates(0);
  end;
  }
end;

procedure TForm1.btnUploadClick(Sender: TObject);
var
  fID: Integer;
  fStream: TMemoryStream;
begin
  fStream := TMemoryStream.Create;
  try
    if dlgOpenPic1.Execute then
    begin
      fStream.LoadFromFile(dlgOpenPic1.FileName);
      fStream.Position := 0;
      fID := SynRestDataset.FieldByName('ID').AsInteger;
      SynRestDataset.RestClient.UpdateBlob(TSQLBiolife, fID, 'Graphic', fStream);

      fImageLoader.LoadImage(img.Picture, fStream);
    end;
  finally
    fStream.Free;
  end;
end;

procedure TForm1.DoOnAfterScroll(Dataset: TDataset);
var
  fID: TID;
  fBlobData: TSQLRawBlob;
begin
  {fID := SynRestDataset.FieldByName('ID').AsInteger;
  if (SynRestDataset.RestClient.RetrieveBlob(TSQLBiolife, fID, 'Graphic', fBlobData)) then
  begin
    fImageLoader.LoadImage(img.Picture, fBlobData);
  end;
  }
end;

end.