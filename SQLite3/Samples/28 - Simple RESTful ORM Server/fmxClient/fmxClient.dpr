// ********************* ABCSoft Co.,Ltd. (http://www.abcsoft.co.th) ****************************//
//  Sample Work on Original mORMot CRUD                                                          //
//***********************************************************************************************//
program fmxClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmxMain in 'fmxMain.pas' {Form1},
  RESTModel in '..\RESTModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
