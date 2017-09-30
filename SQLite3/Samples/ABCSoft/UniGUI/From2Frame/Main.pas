unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes,uniGUIFrame,uniGUIForm, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses,  uniPageControl,
  uniGUIBaseClasses, uniPanel, uniButton;

type
  TMainForm = class(TUniForm)
    UniPanel1: TUniPanel;
    NavPage: TUniPageControl;
    UniButton1: TUniButton;
    pnBody: TUniPanel;
    UniButton2: TUniButton;
    UniTabSheet1: TUniTabSheet;
    procedure UniButton1Click(Sender: TObject);
    procedure UniButton2Click(Sender: TObject);
    procedure UniFormBeforeShow(Sender: TObject);
  private
    { Private declarations }
    FActiveForm:TUniForm;


    procedure InsertFrameTab(Name, Titulo: string;allowDup:boolean;param,PrgRUN:string;allowclose:boolean=true);
    procedure CreateForm(AUniFormClass: TComponentClass; pn: TUniPanel);
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication, uSample;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

{ TMainForm }

procedure TMainForm.CreateForm(AUniFormClass: TComponentClass;
  pn: TUniPanel);
var
  pnlBase: TComponent;
begin
  if Assigned(FActiveForm) then
  begin
    if FActiveForm.ClassType = AUniFormClass then
      exit
    else
    begin
      FActiveForm.DisposeOf;
      FActiveForm := nil;

    end;
  end;

  FActiveForm := TUniForm(UniMainModule.GetFormInstance(AUniFormClass));

  FActiveForm.Parent  := pn;
  FActiveForm.Top     := 0;
  FActiveForm.Left    := 0;
  FActiveForm.Width   := pn.Width;
  FActiveForm.Height  := pn.Height;

  pn.Color := clWhite;

  FActiveForm.ShowModal();
  FActiveForm.Refresh;
end;

procedure TMainForm.InsertFrameTab(Name, Titulo: string; allowDup: boolean;
  param, PrgRUN: string; allowclose: boolean);
var FCurrentFrameTab: TUniFrame; TabSheetNew: TUniTabSheet; I: Integer;
    PanelTabSheetNew: TUniPanel;
   // VClass : TUniFrameClass;
   strsql:string;

   FActiveForm:TUniForm;
begin

  //UniApplication.UniSession.AddJS('uniVars._mask = new Ext.LoadMask(Ext.getBody(), {msg:"Loading..."}); uniVars._mask.show();');
  try

    NavPage.Visible:= True;
    {Check if exists, active tab if exist and exit}

    //if not allowDup then

    for I := 0 to NavPage.PageCount - 1 do
       if NavPage.Pages[I].Name = Name  then
      //if NavPage.Pages[I].Caption = Titulo  then
        begin
            NavPage.ActivePageIndex := I;
            Exit;
        end;


    //if not found, then create it...
    TabSheetNew              := TUniTabSheet.Create(NavPage);
    TabSheetNew.PageControl  := NavPage;
    TabSheetNew.Caption      := Titulo;
    TabSheetNew.Name         := Name;
    TabSheetNew.Closable     := allowclose;
   // TabSheetNew.OnClose      := CheckTab;

   // PanelTabSheetNew := TUniPanel.Create(TabSheetNew);
   // PanelTabSheetNew.Align := alClient;
   // PanelTabSheetNew.Parent := TabSheetNew;
    (*
    VClass := TUniFrameClass(FindClass(Name));
    if Assigned(VClass) then
    begin
      FCurrentFrameTab := VClass.Create(self) as TCustomFrame;
      FCurrentFrameTab.Parent := PanelTabSheetNew;
      FCurrentFrameTab.Align := alClient;
      {
      with TComponentClass(VClass).Create(self) as TCustomFrame do
      begin
        Parent := PanelTabSheetNew;
        Align := alClient;
      end;
      }
    end
    else
    begin
      FCurrentFrameTab := TUniFrameClass(FindClass(Name)).Create(self);
      FCurrentFrameTab.Parent := PanelTabSheetNew;
      FCurrentFrameTab.Align := alClient;
    end;
    *)




   // cdsUserRole.data := FDmMain.CliGetDataset('select * from CENS_USER where USRUID='''+FDmMain.mUSRUID+'''');

   /// FActiveForm := TUniFrameClass(UniMainModule.GetFormInstance(TUniFormClass(FindClass(Name)).Create(self)));
    //FActiveForm := TUniFormClass(FindClass(Name)).Create(TabSheetNew);
    FActiveForm := TUniForm(UniMainModule.GetFormInstance(TUniFormClass(FindClass(Name))));

    FActiveForm.Parent:=TabSheetNew;
    FActiveForm.Align:=alClient;
    FActiveForm.Show;

    //FCurrentFrameTab := TUniFrameClass(FindClass(Name)).Create(self);
   // FCurrentFrameTab.Parent := PanelTabSheetNew;
   // FCurrentFrameTab.Align := alClient;



    //goto last page...
    NavPage.ActivePageIndex := NavPage.PageCount - 1;
  finally

    //UniApplication.UniSession.AddJS('uniVars._mask.hide();');
    //UniApplication.UniSession.AddJS('setTimeout(function() {uniVars._mask.hide();}, 1000);');
  end;
end;


procedure TMainForm.UniButton1Click(Sender: TObject);
begin
  InsertFrameTab('TfrmSample','TfrmSample',true,'','');
end;

procedure TMainForm.UniButton2Click(Sender: TObject);
begin
// CreateForm(TfrmSample,pnBody);
 TfrmSample(UniMainModule.GetFormInstance(TfrmSample)).Parent:=pnBody;
end;

procedure TMainForm.UniFormBeforeShow(Sender: TObject);
begin
  //UniForm1.Parent := UniPanel1;
  //UniForm1.Show();

 // TfrmSample(UniMainModule.GetFormInstance(TfrmSample)).Parent:=UniTabSheet1;
 // frmSample.Parent:=pnBody;

end;

initialization
  RegisterAppFormClass(TMainForm);

 //RegisterClasses([TfrmSample]);

end.
