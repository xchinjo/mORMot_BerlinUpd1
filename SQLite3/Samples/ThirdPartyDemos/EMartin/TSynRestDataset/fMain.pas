unit fMain;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$endif}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, mORMot, mORMotSQLite3, SynSQLite3,SynSQLite3Static,
  mORMotHttpServer, SampleData,

  SynDBFireDAC,

  FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.DB2

  ,FireDAC.Phys.FBDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB;



type
  TfrmMain = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Model: TSQLModel;
    DB: TSQLRestServerDB;
    Server: TSQLHttpServer;
  end;

var
  frmMain: TfrmMain;

  aProps : TSQLDBFireDACConnectionProperties;
  FDPhysFBDriverLink: TFDPhysFBDriverLink;

implementation

uses
  mORMotDB;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FDPhysFBDriverLink := TFDPhysFBDriverLink.Create(Nil);
  FDPhysFBDriverLink.VendorLib:='fbclient.dll';
  //aProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server='+_SERVERIP,_DBNAME,_DBUSRNAME,_DBPASSWD);
  aProps := TSQLDBFireDACConnectionProperties.Create('IB?Server=rootcode.info;Port=3050','/fbdb/sam.fdb','SYSDBA','masterkey');


  Model := CreateSampleModel;
  VirtualTableExternalRegisterAll(Model,aProps,[regMapAutoKeywordFields]); //[regMapAutoKeywordFields]


  DB := TSQLRestServerDB.Create(Model,SQLITE_MEMORY_DATABASE_NAME,false); //TSQLRestServerDB.Create(Model, 'Project19Server.db3', False);
  DB.CreateMissingTables;
  Server := TSQLHttpServer.Create('8080',[DB],'+',HTTP_DEFAULT_MODE);
  Server.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Server.Free;
  DB.Free;
  Model.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Label1.Caption := Caption;
end;

end.
