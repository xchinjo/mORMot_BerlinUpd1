unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniHTMLFrame, uniImage,
  uniGUIBaseClasses, uniPanel,Soap.EncdDecd,IdCoderMIME;

type
  TMainForm = class(TUniForm)
    UniPanel1: TUniPanel;
    UniPanel2: TUniPanel;
    uniImageSnapshot: TUniImage;
    UniHTMLFrame1: TUniHTMLFrame;
    procedure uniImageSnapshotAjaxEvent(Sender: TComponent;
      EventName: string; Params: TUniStrings);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

function After(InpStr, s: string): string;
var
    i: integer;
begin
    i := Pos(InpStr, s);
    if i <> 0 then
        Result := Copy(s, i + Length(InpStr), Length(S));
end;


procedure TMainForm.uniImageSnapshotAjaxEvent(Sender: TComponent;
  EventName: string; Params: TUniStrings);
var
  MS: TMemoryStream;
begin
   if  ((EventName='UpdatePIC') or (EventName = '_post' )) then
    begin

     ShowMessage(Params.Values['PICDATA']);


      MS := TMemoryStream.Create;
      try
        TIdDecoderMIME.DecodeStream(After(',',Params.Values['PICDATA']), MS);
        MS.Position := 0;
        uniImageSnapshot.LoadFromStream(ms);
      finally
        MS.Free;
      end;
  end;
end;

initialization
  RegisterAppFormClass(TMainForm);

end.
