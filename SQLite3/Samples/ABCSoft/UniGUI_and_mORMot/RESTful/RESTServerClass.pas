unit RESTServerClass;

interface

uses
  SysUtils,dbxjson,JSON,
  Classes,SynCrtSock,
  SynCommons,
  SynLog,
  mORMot,mORMotDB,
  RESTData,SynDB,

    SynDBFireDAC,

    FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc, FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL,
    FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG, FireDAC.Phys.DB2 ;

type
  ENoteServer = class(EORMException);
  
  TNoteServer = class(TSQLRestServerFullMemory)
  protected
    fRootFolder: TFileName;
    fBlobFolder: TFileName;
  public
    constructor Create(const aRootFolder: TFileName; const aRootURI: RawUTF8); reintroduce;
    destructor Destroy; override;
    property RootFolder: TFileName read fRootFolder;
  published
    procedure Blob(Ctxt: TSQLRestServerURIContext);
    procedure helloworld();
    procedure Sum(Ctxt: TSQLRestServerURIContext);
    procedure getProductList(Ctxt: TSQLRestServerURIContext);

    function sam:RawJSON;
  end;

var

    GFireDACConnProp : TSQLDBFireDACConnectionProperties;
    GFFireDACConn: TSQLDBFireDACConnection;

    FDPhysMySQLDriverLink:TFDPhysMySQLDriverLink;
const
  {
  _SERVERIP='192.168.1.8';
  _DBUSRNAME='joni';
  _DBPASSWD='123456';
  _DBNAME='Shadow';

  }


  _SERVERIP='124.109.2.164;Port=3307';
  _DBUSRNAME='insysc';
  _DBPASSWD='insysc1234567890*';
  _DBNAME='Shadow';




implementation


{ TNoteServer }

constructor TNoteServer.Create(const aRootFolder: TFileName;
  const aRootURI: RawUTF8);
begin
{
  fRootFolder := EnsureDirectoryExists(ExpandFileName(aRootFolder),true);
  fBlobFolder := EnsureDirectoryExists(fRootFolder+'blob\',true);
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE; // LOG_STACKTRACE;
    DestinationPath := fRootFolder+'..\log\';
    if not FileExists(DestinationPath) then
      CreateDir(DestinationPath);
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  // prepare the server in-memory storage
  inherited Create(DataModel(aRootURI),fRootFolder+'data.json',false,false);
  UpdateToFile;
  }
end;

destructor TNoteServer.Destroy;
begin
  inherited;
  fModel.Free;
end;

procedure TNoteServer.getProductList(Ctxt: TSQLRestServerURIContext);
var
  PtyRun: Extended;
  res: ISQLDBRows;  //TSQLRecord
  Rows: ISQLDBRows;


begin
try




  //if Ctxt.Input['ptyrun']<>'' then
  //begin
      FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
      GFireDACConnProp := TSQLDBFireDACConnectionProperties.Create('MySQL?Server='+_SERVERIP,_DBNAME,_DBUSRNAME,_DBPASSWD);
     // GFFireDACConn := TSQLDBFireDACConnection.Create(GFireDACConnProp);
     // GFFireDACConn.Connect;

      //res := GFireDACConnProp.ExecuteInlined('select PRDCDE,PRDRUN,PRDNAT,PRDNAE from INVM_PROD  limit 100 ',true);
      res := GFireDACConnProp.Execute(
     // 'select PRDCDE,PRDRUN,PRDNAT,PRDNAE from INVM_PROD  limit 1000 '
     ' select  '+
     '        	c.*,a.PRIVAL as PRIADU,b.PRIVAL as PRICHI '+
     ' from INVM_PROD c , INVM_PRODUNIT u '+
     ' , INVM_PRICLIST a  ,INVM_PRICLIST b '+
     ' where  '+
     ' c.RECACT=''A''  '+
     ' and c.PTYRUN=3 '+
     ' and c.PRDRUN=u.PRDRUN		   '+
     ' and c.PRDRUN=a.PRDRUN and a.PGRCDE=''02'' and a.PCACDE=''01'' and a.SPGCDE=''01'' and a.UNIRUN=u.UNIRUN '+
     ' and c.PRDRUN=b.PRDRUN and b.PGRCDE=''02'' and b.PCACDE=''01'' and b.SPGCDE=''02'' and b.UNIRUN=u.UNIRUN '+
     ' limit 1000 '

      ,[]);

      //Writeln(res.FetchAllAsJSON(true,nil,true));



      with Ctxt do
      begin
        // Ctxt.Server.Add('AAA');


        //Ctxt.Results([res.FetchAllAsJSON(true,nil,true)]);

        //Ctxt.Returns(Int64ToUtf8(ServerTimeStamp),HTML_SUCCESS,TEXT_CONTENT_TYPE_HEADER);
        //Ctxt.Returns(res.FetchAllAsJSON(false,nil,true),HTML_SUCCESS,JSON_CONTENT_TYPE);
        Ctxt.Returns(res.FetchAllAsJSON(true,nil,true),HTTP_SUCCESS,JSON_CONTENT_TYPE);

//      Ctxt.Server.Add(res,true,true);

      //result := 200;
      end;
        //Results([res.FetchAllAsJSON(true)]);     //JSONDecode

  //end else
  //  Ctxt.Error('Missing Parameter');

finally
  FDPhysMySQLDriverLink.free;
  GFireDACConnProp.Free;
  GFFireDACConn.free;
end;

end;

procedure TNoteServer.helloworld;
begin
  //
end;

function TNoteServer.sam: RawJSON;
var res: ISQLDBRows;
begin


  FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
  GFireDACConnProp := TSQLDBFireDACConnectionProperties.Create('MySQL?Server='+_SERVERIP,_DBNAME,_DBUSRNAME,_DBPASSWD);
  GFFireDACConn := TSQLDBFireDACConnection.Create(GFireDACConnProp);
  GFFireDACConn.Connect;


  //  GFFireDACConn.Connect;
  //res:= GFireDACConnProp.Execute('select * from SALT_BOOK', []);


  //  if GFireDACConnProp=nil then
  //  raise Exception.Create('Connect call required before Execute');
  res := GFireDACConnProp.ExecuteInlined('select * from SALT_BOOK limit 100',true);


  //if res=nil then
  //  result := '' else
   // result := res.FetchAllAsJSON(true);

end;

procedure TNoteServer.Sum(Ctxt: TSQLRestServerURIContext);
var
  str:TStringList;
  res: ISQLDBRows;
  res2:TSQLRecord;

    jsobj, jso : TJsonObject;
    jsa : TJsonArray;
    jsp : TJsonPair;
    JSONObj : TJSONObject;
begin


  FDPhysMySQLDriverLink := TFDPhysMySQLDriverLink.Create(Nil);
  GFireDACConnProp := TSQLDBFireDACConnectionProperties.Create('MySQL?Server='+_SERVERIP,_DBNAME,_DBUSRNAME,_DBPASSWD);
  GFFireDACConn := TSQLDBFireDACConnection.Create(GFireDACConnProp);
  GFFireDACConn.Connect;


    GFFireDACConn.Connect;
  //res:= GFireDACConnProp.Execute('select * from SALT_BOOK', []);


  //  if GFireDACConnProp=nil then
  //  raise Exception.Create('Connect call required before Execute');
  res := GFireDACConnProp.ExecuteInlined('select * from INVM_PROD limit 10',true);



{

  str  := TStringList.Create;
  str.Add('a');
  str.Add('b');
  str.Add('c');
  str.Add('d');
  }

  {
  JSONObj := TJSONObject.Create;
  JSONObj.AddPair(TJSONPair.Create('FirtName','aaa'));
  JSONObj.AddPair(TJSONPair.Create('SurName','й║ье'));
  }




 with Ctxt do
    Results([res.FetchAllAsJSON(true)]); //res.FetchAllAsJSON(true)
    //Results([InputDouble['a']+InputDouble['b']]);



end;

procedure TNoteServer.Blob(Ctxt: TSQLRestServerURIContext);
var FileName: TFileName;
begin
  if (Ctxt.Table=TSQLNoteFile) and (Ctxt.TableID<>0) then begin
    FileName := fBlobFolder+UTF8ToString(
      OneFieldValue(TSQLNoteFile,'FileName',Ctxt.TableID));
    case Ctxt.Method of
    mGET:
      Ctxt.ReturnFile(FileName);
    mPOST,mPUT: begin
      FileFromString(Ctxt.Call.InBody,FileName);
      Ctxt.Success;
    end;
    mDELETE:
      if DeleteFile(FileName) then
        Ctxt.Success else
        Ctxt.Error('',HTTP_NOTFOUND);
    end;
  end;
end;

end.
