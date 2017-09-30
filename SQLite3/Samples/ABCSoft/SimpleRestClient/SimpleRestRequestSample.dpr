program SimpleRestRequestSample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  RestRequest in 'RestRequest.pas';

var RestReq: TRestRequest;
    RestResp: THttpResponse;
    putParams:TStringList;
begin
  try
    RestReq := nil;
    try
      //root/person
      putParams:=TStringList.Create();
      //putParams.Add('ID=50000');
      putParams.Add('Name:50000');
      //putParams.Add('{"Name":"aaa-1"}'); // insert new


      //RestReq := TRestRequest.Create().Domain('127.0.0.1:888').Path('root/person').Path('9999');
      //RestReq := TRestRequest.Create().Domain('127.0.0.1:888').Path('root');
      // RestReq := TRestRequest.Create().Domain('127.0.0.1:888').Path('root').Path('person');  // insert new
      RestReq := TRestRequest.Create().Domain('127.0.0.1:888').Path('root').Path('person').Path('50000');
      //RestReq := TRestRequest.Create().Domain('127.0.0.1:888').Path('root').Path('person').WithCredentials('user', 'pass');
      //RestReq := TRestRequest.Create().Domain('jsonplaceholder.typicode.com').Path('todos').Path('1');

      RestResp := RestReq.put(putParams);
      if RestResp.ResponseCode = 200 then WriteLn('Your todo was added!') else WriteLn('Failed to add your todo.');
      WriteLn(RestResp.ResponseStr);
      readln;


      RestResp := RestReq.Get();
      if RestResp.ResponseCode = 200 then WriteLn('Your todo was added!') else WriteLn('Failed to add your todo.');
      WriteLn(RestResp.ResponseStr);
      readln;
    finally
      RestReq.Free;
    end;
  except
   // on E: Exception do
    //  Writeln(E.ClassName, ': ', E.Message);
  end;
end.
