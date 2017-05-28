/// minimal REST client for a list of Persons from RESTserver.exe
// ********************* ABCSoft Co.,Ltd. (http://www.abcsoft.co.th) ****************************//
//  Sample Work on Original mORMot CRUD                                                          //
//***********************************************************************************************//
program RESTclient;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SynCommons,          // framework core
  mORMot,              // RESTful server & ORM
  mORMotHttpClient,    // HTTP client to a mORMot RESTful server
  RESTModel;           // data model unit, shared between server and client


var aModel: TSQLModel;
    aClient: TSQLHttpClientWebsockets; // Change from TSQLHttpClient ---to---> TSQLHttpClientWebsockets;
    aPerson: TPerson;
    aID: integer;
    newID:integer;
begin
  aModel := DataModel;
  try
    //aClient := TSQLHttpClientWinHTTP.Create('localhost',SERVER_PORT,aModel);
    aClient := TSQLHttpClientWebsockets.Create('localhost',SERVER_PORT,aModel);  // Change from TSQLHttpClient ---to---> TSQLHttpClientWebsockets;

      if not aClient.ServerTimeStampSynchronize then
      begin
       // ShowLastClientError(fClient,'Please run Project16ServerHttp.exe');
       // Close;
        exit;
      end;


    try

      //aClient.Delete()
      //aClient.Update()
      //aClient.Retrieve()
      //aClient.Refresh()


      {**************** delte *****************}
      aClient.Delete(TPerson,4);

      {**************** retrieve *****************}
      aClient.Retrieve(100,aPerson,true);
      if aPerson.GetID>0 then
      begin
        Writeln('Retrive Person Name:'+aPerson.Name);
        Writeln('update name to--> : abcsoft');
        aPerson.Name:='abcsoft';

        aClient.Update(aPerson,[]);
      end;

      //writeln(' id=',aPerson.GetID);

      {**************** add new *****************}
      writeln('Add a new TPerson');
      aPerson := TPerson.Create;
      try
        Randomize;
        newid:=Random(10000);
        aPerson.IDValue:=newid;
        aPerson.Name := 'Name'+Int32ToUtf8(newid);
        aID := aClient.Add(aPerson,true,true);

      finally
        aPerson.Free;
      end;
      writeln('Added TPerson.ID=',aID);
      aPerson := TPerson.Create(aClient,aID);
      try
        writeln('Name read for ID=',aPerson.ID,' from DB = "',aPerson.Name,'"');
      finally
        aPerson.Free;
      end;


    finally
      aClient.Free;
    end;
    write(#10'Press [Enter] to quit');
    readln;
  finally
    aModel.Free;
  end;
end.
