unit fmxMain;

interface

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,



  SynCommons,          // framework core
  mORMot,              // RESTful server & ORM
  mORMotHttpClient,    // HTTP client to a mORMot RESTful server
  RESTModel, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;           // data model unit, shared between server and client


var aModel: TSQLModel;
    aClient: TSQLHttpClientWebsockets; // Change from TSQLHttpClient ---to---> TSQLHttpClientWebsockets;
    aPerson: TPerson;
    aID,i: integer;
    newID:integer;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnTestClick(Sender: TObject);
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
      //aClient.Delete(TPerson,4);



      {**************** retrieve *****************}
      for i := 1 to 1000 do
        begin

            //aPerson.IDValue:=i;
           // aClient.Retrieve(i,aPerson,true);
            aPerson:=TPerson.Create;

            if aClient.Retrieve(i,aPerson) then
            begin
              memo1.Lines.Add('Retrive Person Name:'+aPerson.Name);
              memo1.Lines.Add('update name to--> : abcsoft update ');
              aPerson.Name:='abcsoft  update '+Int32ToUtf8(i);

              aClient.Update(aPerson,[]);
            end else
            begin
                {**************** add new *****************}
                memo1.Lines.Add('Add a new TPerson');
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

            Application.ProcessMessages;



        end;


    finally
      aClient.Free;
    end;
  finally
    aModel.Free;
  end;
end;

end.
