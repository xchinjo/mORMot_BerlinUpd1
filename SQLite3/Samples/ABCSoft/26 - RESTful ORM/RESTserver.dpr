/// RESTful ORM server
program RESTserver;

{$APPTYPE CONSOLE}

// first line after uses clause should be  {$I SynDprUses.inc}  for FastMM4
uses
  {$I SynDprUses.inc}
   Windows,
  Classes,
  SysUtils,
  SynCommons,
  SynLog,
  mORMot,SynDB,mORMotDB,mORMotSQLite3, SynSQLite3, SynSQLite3Static,
  SynCrtSock,
  mORMotHTTPServer,
  RESTData,
  RESTServerClass,

  SynDBFireDAC,

  FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.DB2

  ,FireDAC.Phys.FBDef, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB;

var
    aRestServer: TSQLRestServerDB;
    HTTPServer: TSQLHttpServer;
    aModel: TSQLModel;
    aHttpServer: TSQLHttpServer;

    aProps : TSQLDBFireDACConnectionProperties;
    FDPhysMySQLDriverLink:TFDPhysMySQLDriverLink;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;

begin
    with TSQLLog.Family do begin

        Level := LOG_VERBOSE;
        EchoToConsole := LOG_VERBOSE; // log all events to the console
        PerThreadLog := ptIdentifiedInOnFile;
        NoFile:=true;
    end;
      // manual switch to console mode
     AllocConsole;
     TextColor(ccLightGray); // needed to noti   ccLightGray


  FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
  FDPhysFBDriverLink := TFDPhysFBDriverLink.Create(Nil);
  FDPhysFBDriverLink.VendorLib:='fbclient.dll';
  //aProps := TSQLDBFireDACConnectionProperties.Create('MySQL?Server='+_SERVERIP,_DBNAME,_DBUSRNAME,_DBPASSWD);
  aProps := TSQLDBFireDACConnectionProperties.Create('IB?Server=rootcode.info;Port=3050','/fbdb/sam.fdb','SYSDBA','masterkey');


  try

    with TSQLLog.Family do begin
        Level := LOG_VERBOSE;
        EchoToConsole := LOG_VERBOSE; // log all events to the console
        PerThreadLog := ptIdentifiedInOnFile;
        NoFile:=true;
    end;
    // manual switch to console mode
    //AllocConsole;
    TextColor(ccLightGray); // needed to noti

    // get the shared data model
    aModel := DataModel;
    VirtualTableExternalRegisterAll(aModel,aProps,[regMapAutoKeywordFields]); //[regMapAutoKeywordFields]
    try
      aRestServer := TSQLRestServerDB.Create(aModel,SQLITE_MEMORY_DATABASE_NAME,true); //TSQLRestServerDB
      try
        aRestServer.CreateMissingTables;

        // serve aRestServer data over HTTP
         aHttpServer := TSQLHttpServer.Create('8880',[aRestServer]); //useHttpApiRegisteringURI
        try
          aHttpServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
          writeln('Background server is running.'#10);
          write('Press [Enter] to close the server.');
          readln;
        finally
          aHttpServer.Free;
        end;
      finally
        aRestServer.Free;
      end;
    finally
      aModel.Free;
    end;
  finally
    aProps.Free;
  end;

end.
