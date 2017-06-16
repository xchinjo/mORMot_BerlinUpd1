unit fmxMain;

interface

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  IdHttp,
  IdAuthentication,
  IdMultipartFormData,
  IdURI,
  IdSSL,
  IdSSLOpenSSL, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

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
  var    jsonToSend:TStringList;
         http:TIDHttp;
  begin
  http := TIDHttp.Create(nil);
  http.HandleRedirects := true;
  http.ReadTimeout := 5000;
  jsonToSend:=TStringList.create;
  jsonToSend.Add('Name="TESt"');
  Memo1.Lines.Text:=http.Post('http://127.0.0.1:888/root/person', jsonToSend);
  jsonToSend.Destroy;
  http.Destroy;
  end;

end.
