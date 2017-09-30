unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniGUIBaseClasses, uniPanel,
  uniHTMLFrame, uniPageControl, uniEdit, uniLabel, uniDBEdit, uniMultiItem,
  uniComboBox, uniDateTimePicker, uniBasicGrid, uniDBGrid, uniImageList,
  uniButton, uniImage, Vcl.Menus, uniMainMenu, uniBitBtn, uniMenuButton;

type
  TMainForm = class(TUniForm)
    pnNavigation: TUniPanel;
    UniPanel1: TUniPanel;
    UniPageControl1: TUniPageControl;
    UniTabSheet1: TUniTabSheet;
    UniTabSheet2: TUniTabSheet;
    UniTabSheet3: TUniTabSheet;
    UniHTMLFrame1: TUniHTMLFrame;
    UniPanel4: TUniPanel;
    dtQuotationDocDate: TUniDateTimePicker;
    UniLabel1: TUniLabel;
    UniLabel6: TUniLabel;
    UniLabel8: TUniLabel;
    cmbAgentCompany: TUniComboBox;
    UniLabel12: TUniLabel;
    UniLabel2: TUniLabel;
    UniLabel14: TUniLabel;
    edCustomerName: TUniDBEdit;
    UniLabel27: TUniLabel;
    UniLabel30: TUniLabel;
    UniLabel15: TUniLabel;
    UniLabel11: TUniLabel;
    UniLabel13: TUniLabel;
    edQuotationNo: TUniDBEdit;
    UniLabel9: TUniLabel;
    cmbStatus: TUniComboBox;
    edContactPerson: TUniDBEdit;
    edCusTelephone: TUniDBEdit;
    edCusEmail: TUniDBEdit;
    UniLabel3: TUniLabel;
    UniLabel5: TUniLabel;
    UniLabel29: TUniLabel;
    UniLabel39: TUniLabel;
    UniLabel40: TUniLabel;
    UniDBEdit1: TUniDBEdit;
    UniLabel45: TUniLabel;
    UniLabel46: TUniLabel;
    UniLabel55: TUniLabel;
    UniLabel56: TUniLabel;
    cmbPriceGroup: TUniComboBox;
    cmbPriceCategory: TUniComboBox;
    UniLabel47: TUniLabel;
    UniLabel48: TUniLabel;
    cmbNationality: TUniComboBox;
    UniLabel43: TUniLabel;
    UniLabel44: TUniLabel;
    cmbStepDoc: TUniComboBox;
    UniLabel49: TUniLabel;
    dtTravelDate: TUniDateTimePicker;
    UniLabel51: TUniLabel;
    UniLabel52: TUniLabel;
    UniLabel53: TUniLabel;
    UniDBEdit3: TUniDBEdit;
    UniDBEdit4: TUniDBEdit;
    UniLabel54: TUniLabel;
    UniLabel57: TUniLabel;
    UniDBEdit5: TUniDBEdit;
    UniLabel58: TUniLabel;
    UniLabel59: TUniLabel;
    dtEndDate1: TUniDateTimePicker;
    cmbHotelList: TUniComboBox;
    UniDBGrid1: TUniDBGrid;
    pnBaseAllClientButton: TUniPanel;
    btnNew: TUniButton;
    btnEdit: TUniButton;
    btnDelete: TUniButton;
    btnPrint: TUniButton;
    btnSave: TUniButton;
    btnCancel: TUniButton;
    btnApprove: TUniButton;
    UniEdit1: TUniEdit;
    UniTabSheet4: TUniTabSheet;
    pnTItle: TUniPanel;
    ImgHeadRight: TUniImage;
    lbTiltle: TUniLabel;
    imgLogo: TUniImage;
    UniLbInfo: TUniLabel;
    LbUserLogIn: TUniLabel;
    LbBranch: TUniLabel;
    LbDateTime: TUniLabel;
    lbVersion: TUniLabel;
    lbLang: TUniLabel;
    lbCompanyName: TUniLabel;
    pnTopSpace: TUniPanel;
    UniTabSheet5: TUniTabSheet;
    btnShowMenu: TUniMenuButton;
    UniPopupMenu1: TUniPopupMenu;
    Sales1: TUniMenuItem;
    Purchase1: TUniMenuItem;
    ourProductInfomation1: TUniMenuItem;
    Accounting1: TUniMenuItem;
    ARAccounting1: TUniMenuItem;
    HRManagement1: TUniMenuItem;
    MasterData1: TUniMenuItem;
    N1: TUniMenuItem;
    Report1: TUniMenuItem;
    ourProductInfomation2: TUniMenuItem;
    AgentProductManager1: TUniMenuItem;
    Price1: TUniMenuItem;
    ProductPriceList1: TUniMenuItem;
    menuImgList: TUniImageList;
    procedure btnApproveClick(Sender: TObject);
    procedure UniFormShow(Sender: TObject);
    procedure btnShowMenuClick(Sender: TObject);
  private
    { Private declarations }
    procedure initTitlePanel();
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

procedure TMainForm.btnApproveClick(Sender: TObject);
begin
  pnNavigation.Visible:=not (pnNavigation.Visible);
  initTitlePanel;



end;

procedure TMainForm.initTitlePanel;
begin
  if pnNavigation.Visible then
  begin
    pnTItle.Visible:=false;
    pnTopSpace.Visible:=true;
    btnShowMenu.ImageIndex:=222;
  end else
  begin
    pnTItle.Visible:=true;
    pnTopSpace.Visible:=true;
    btnShowMenu.ImageIndex:=224;
  end;

end;

procedure TMainForm.UniFormShow(Sender: TObject);
begin
  initTitlePanel;
end;

procedure TMainForm.btnShowMenuClick(Sender: TObject);
begin
  pnNavigation.Visible:=not (pnNavigation.Visible);
  initTitlePanel;

end;

initialization
  RegisterAppFormClass(TMainForm);

end.
