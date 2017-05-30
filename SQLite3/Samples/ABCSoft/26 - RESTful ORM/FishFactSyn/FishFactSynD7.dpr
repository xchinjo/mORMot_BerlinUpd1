program FishFactSynD7;

uses
  FastMM4,Forms,
  Ffactwin in 'Ffactwin.pas' {Form1},
  SampleData in 'SampleData.pas',
  SynRestMidasVCL in 'SynRestMidasVCL.pas',
  SynRestVCL in 'SynRestVCL.pas';



begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
