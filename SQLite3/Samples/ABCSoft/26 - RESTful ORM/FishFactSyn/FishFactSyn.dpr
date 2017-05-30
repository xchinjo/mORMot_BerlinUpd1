program FishFactSyn;

uses
  Forms,
  Ffactwin in 'Ffactwin.pas' {Form1},
  SampleData in 'SampleData.pas',
  SynRestMidasVCL in 'SynRestMidasVCL.pas',
  SynRestVCL in 'SynRestVCL.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
