unit ufmxClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  SynCommons,mORMotMidasVCL, mORMot, mORMotHttpClient,
  Project16Interface, Data.DB, Datasnap.DBClient,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, Data.Bind.Controls, Data.Bind.Components,
  Data.Bind.DBScope, FMX.Layouts, Fmx.Bind.Navigator, FMX.Edit, FMX.Memo;


type

  TfmxClient = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fModel: TSQLModel;
    fClient: TSQLRestClientURI;
    fTableJSON: RawUTF8;
    fService: IRemoteSQL;

  public
    { Public declarations }
  end;

var
  fmxClient: TfmxClient;

implementation

{$R *.fmx}

procedure TfmxClient.Button1Click(Sender: TObject);
var SQL: RawUTF8;
    cds:TClientDataSet;
begin
  fService.Connect(rseOleDB,'rootcode.info','sam.fdb','SYSDBA','MASTERKEY');

  SQL := trim(StringToUTF8('select * from PERSON'));
  //Screen.Cursor := crHourGlass;
  cds := TClientDataSet.Create(nil);
  try


      if isSelect(pointer(SQL)) then begin
           cds:=JSONToClientDataSet(self,fService.Execute(SQL,True,False));// fService.Execute(SQL,False,False);
           if cds.RecordCount>0 then
           begin
            cds.First;
            while not cds.Eof do
            begin
              Memo1.Lines.Add(cds.FieldByName('ID').AsString+' '+cds.FieldByName('NAME').AsString) ;

              Application.ProcessMessages;
              cds.Next;
            end;

           end;
      end; //else




  finally
   // Screen.Cursor := crDefault;
   cds.Free;
  end;
end;

procedure TfmxClient.FormShow(Sender: TObject);
begin
  fModel := TSQLModel.Create([],ROOT_NAME);
  fClient := TSQLHttpClient.Create('localhost','777',fModel); //'localhost'
  if not fClient.ServerTimeStampSynchronize then begin

    Close;
    exit;
  end;
  if (not fClient.SetUser('User','synopse'))  or
     (not fClient.ServiceRegisterClientDriven(TypeInfo(IRemoteSQL),fService)) then begin
    Close;
    exit;
  end;

end;

end.
