program Project16fmxClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufmxClient in 'ufmxClient.pas' {fmxClient},
  Project16Interface in 'Project16Interface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmxClient, fmxClient);
  Application.Run;
end.
