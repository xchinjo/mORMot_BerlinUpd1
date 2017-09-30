unit uSample;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIForm;

type
  TfrmSample = class(TUniForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function frmSample: TfrmSample;

implementation

{$R *.dfm}

uses
  MainModule, uniGUIApplication;

function frmSample: TfrmSample;
begin
  Result := TfrmSample(UniMainModule.GetFormInstance(TfrmSample));
end;

end.
