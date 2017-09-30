/// this server will use TSQLRestServerFullMemory over HTTP
program Project16ServerHttp;

{.$APPTYPE CONSOLE} // is done below by calling AllocConsole API 

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Windows,Winapi.Messages,
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDB,
  mORMotHttpServer,
  SynDB,
  SynDBOracle,
  SynDBSQLite3,SynSQLite3Static,
  SynOleDB,
  SynDBODBC,
  Project16Interface,
  SynDBFireDAC,
  FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.DB2

  ,FireDAC.Phys.FBDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB

  ;

type
  TServiceRemoteSQL = class(TInterfacedObject,IRemoteSQL)
  protected
    //fProps: TSQLDBConnectionProperties;
    fProps: TSQLDBFireDACConnectionProperties;
  public
    destructor Destroy; override;
  public // implements IRemoteSQL methods
    procedure Connect(aEngine: TRemoteSQLEngine; const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUTF8);
    function GetTableNames: TRawUTF8DynArray;
    function hello:RawJSON;
    function Execute(const aSQL: RawUTF8; aExpectResults, aExpanded: Boolean): RawJSON;
  end;


{ TServiceRemoteSQL }
procedure TServiceRemoteSQL.Connect(aEngine: TRemoteSQLEngine;
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
const // rseOleDB, rseODBC, rseOracle, rseSQlite3, rseJet, rseMSSQL
  TYPES: array[TRemoteSQLEngine] of TSQLDBConnectionPropertiesClass = (
     TOleDBConnectionProperties, TODBCConnectionProperties,
     TSQLDBOracleConnectionProperties, TSQLDBSQLite3ConnectionProperties,
     {$ifdef WIN64}nil{$else}TOleDBJetConnectionProperties{$endif},
     TOleDBMSSQL2008ConnectionProperties,TSQLDBFireDACConnectionProperties);

var
    //GFFireDACConn: TSQLDBFireDACConnection;
    FDPhysMySQLDriverLink:TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;


begin
  if fProps<>nil then
    raise Exception.Create('Connect called more than once');
  if TYPES[aEngine]=nil then
    raise Exception.CreateFmt('aEngine=%s is not supported',
      [GetEnumName(TypeInfo(TRemoteSQLEngine),ord(aEngine))^]);

  //fProps := TYPES[aEngine].Create(aServerName,aDatabaseName,aUserID,aPassWord);
 {
  FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
  fProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server=rootcode.info;Port=3306','ErpDB','joni','P@ssw0rd123');

  }

  FDPhysFBDriverLink := TFDPhysFBDriverLink.Create(Nil);
  FDPhysFBDriverLink.VendorLib:='fbclient.dll';
  fProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server=192.168.1.111','delsnet','root','delsnet');
  //fProps := TSQLDBFireDACConnectionProperties.Create('IB?Server=rootcode.info;Port=3050','/fbdb/sam.fdb','SYSDBA','masterkey');

  //fProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server=192.168.1.9','Shadow','insysc','insysc1234567890*');
  //GFFireDACConn := TSQLDBFireDACConnection.Create(fProps);
  //GFFireDACConn.Connect;

end;

function TServiceRemoteSQL.Execute(const aSQL: RawUTF8; aExpectResults, aExpanded: Boolean): RawJSON;
var res: ISQLDBRows;
begin
  if fProps=nil then
    raise Exception.Create('Connect call required before Execute');

  if isSelect(Pointer(aSQL)) then
  begin
    res := fProps.ExecuteInlined(aSQL,aExpectResults);
    if res=nil then
      result := ''
    else
      result := res.FetchAllAsJSON(aExpanded);
  end else
  begin
    fProps.ExecuteNoResult(aSQL,[]) ;
    result :='';
  end;


end;

function TServiceRemoteSQL.GetTableNames: TRawUTF8DynArray;
begin
  if fProps=nil then
    raise Exception.Create('Connect call required before GetTableNames');
  fProps.GetTableNames(result);
end;

function TServiceRemoteSQL.hello: RawJSON;
const // rseOleDB, rseODBC, rseOracle, rseSQlite3, rseJet, rseMSSQL
  TYPES: array[TRemoteSQLEngine] of TSQLDBConnectionPropertiesClass = (
     TOleDBConnectionProperties, TODBCConnectionProperties,
     TSQLDBOracleConnectionProperties, TSQLDBSQLite3ConnectionProperties,
     {$ifdef WIN64}nil{$else}TOleDBJetConnectionProperties{$endif},
     TOleDBMSSQL2008ConnectionProperties,TSQLDBFireDACConnectionProperties);

var
   // GFFireDACConn: TSQLDBFireDACConnection;
    FDPhysMySQLDriverLink:TFDPhysMySQLDriverLink;
    res: ISQLDBRows;
begin
  //FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
 // fProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server=192.168.1.8','Shadow','joni','123456');
  //fProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server=192.168.1.9','Shadow','insysc','insysc1234567890*');
  //GFFireDACConn := TSQLDBFireDACConnection.Create(fProps);
 // GFFireDACConn.Connect;

 try
  if fProps=nil then
    raise Exception.Create('Connect call required before Execute');
  res := fProps.ExecuteInlined('select * from SALT_BOOK',true);
  if res=nil then
    result := '' else
    result := res.FetchAllAsJSON(true);
 finally
   res:=nil;
 end;
end;

destructor TServiceRemoteSQL.Destroy;
begin
  FreeAndNil(fProps);
  inherited;
end;


var
  aModel: TSQLModel;
  aServer: TSQLRestServer;
  aHTTPServer: TSQLHttpServer;
begin
  // define the log level

  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE; // log all events to the console
    PerThreadLog := ptIdentifiedInOnFile; 
  end;
  // manual switch to console mode
  AllocConsole;
  TextColor(ccLightGray); // needed to notify previous AllocConsole
  // create a Data Model
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    // initialize a TObjectList-based database engine
    aServer := TSQLRestServerFullMemory.Create(aModel,'users.json',false,true);
    try
      // register our IRemoteSQL service on the server side
      aServer.ServiceRegister(TServiceRemoteSQL,[TypeInfo(IRemoteSQL)],sicClientDriven).
        // fProps should better be executed/released in the one main thread
        SetOptions([],[optExecInMainThread,optFreeInMainThread]);
      // launch the HTTP server
      aHTTPServer := TSQLHttpServer.Create(PORT_NAME,[aServer],'+',useHttpApiRegisteringURI);
      try
        aHTTPServer.AccessControlAllowOrigin := '*'; // for AJAX requests to work
        Sleep(200); // allow all HTTP threads to be launched and logged
        writeln(#10'Background server is running.'#10);
        writeln('Press [Enter] to close the server.'#10);
        ConsoleWaitForEnterKey;
      finally
        aHTTPServer.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
