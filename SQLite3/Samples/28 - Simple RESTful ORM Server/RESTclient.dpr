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
  RESTModel,           // data model unit, shared between server and client

  mORMotMidasVCL,

  sysutils,DateUtils,Datasnap.DBClient;

var aModel: TSQLModel;
    aClient: TSQLHttpClientWebsockets; // Change from TSQLHttpClient ---to---> TSQLHttpClientWebsockets;
    aPerson: TPerson;
    aID,i: integer;
    newID,m:integer;
    stdate:TDateTime;
   // cds:TClientDataSet;

   job:TSynBackgroundThreadProcess;


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

    //cds:=TClientDataSet.Create(nil);
    try

      //aClient.Delete()
      //aClient.Update()
      //aClient.Retrieve()
      //aClient.Refresh()


      {**************** delte *****************}
      //aClient.Delete(TPerson,4);
      stdate:=now;

{
            JSONToClientDataSet(cds,aClient.RetrieveListJSON(TPerson,''));


            if cds<>nil then
            Writeln(' record count='+Int32ToUtf8(cds.RecordCount));
 }


      {**************** retrieve *****************}
      for i := 1 to 500000 do
        begin

            //aPerson.IDValue:=i;
           // aClient.Retrieve(i,aPerson,true);
            aPerson:=TPerson.Create;




            if aClient.Retrieve(i,aPerson) then
            begin
             m := SecondsBetween(now(),stdate);


              Writeln('Retrive Person Name:'+aPerson.Name+'  time : '+Format('%2.2d:%2.2d',[m div 60,m mod 60]));
              Writeln('update name to--> : abcsoft update ',i);
              aPerson.Name:='abcsoft  update '+Int32ToUtf8(i);

              aClient.Update(aPerson,[]);
            end else
            begin
                {**************** add new *****************}
                //m := MinutesBetween(now(),stdate);
                m := SecondsBetween(now(),stdate);


                writeln('Add a new TPerson'+Int32ToUtf8(newid)+'  time : '+Format('%2.2d:%2.2d',[m div 60,m mod 60]));
                //aPerson := TPerson.Create;
                try
                  Randomize;
                  newid:=i;//Random(10000);
                  aPerson.IDValue:=newid;
                  aPerson.Name := 'Name'+Int32ToUtf8(newid);
                  aID := aClient.Add(aPerson,true,true);

                finally
                 // aPerson.Free;
                end;

            end;




        end;



      {
      aClient.Retrieve(7,aPerson,true);
      if aPerson.ID>0 then
      begin
        Writeln('Retrive Person Name:'+aPerson.Name);
        Writeln('update name to--> : abcsoft2');
        aPerson.Name:='abcsoft2';

        aClient.Update(aPerson,[]);
      end;

      aPerson.Free;
      }



      //writeln(' id=',aPerson.GetID);



    finally
      aClient.Free;
    end;
    write(#10'Press [Enter] to quit');
    readln;
  finally
    //cds.Free;
    aModel.Free;
  end;
end.
