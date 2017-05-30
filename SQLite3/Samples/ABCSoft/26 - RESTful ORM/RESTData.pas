unit RESTData;

interface

uses
  SynCommons,
  mORMot;

type
  TSQLRecordWithModTimeAndMetaData = class(TSQLRecord)
  protected
    fCreated: TCreateTime;
    fModified: TModTime;
    fMetaData: variant;
  published
    property Modified: TModTime read fModified write fModified;
    property Created: TCreateTime read fCreated write fCreated;
    property MetaData: variant read fMetaData write fMetaData;
  end;

  TSQLNoteKind = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fName: RawUTF8;
  public
    class procedure InitializeTable(Server: TSQLRestServer;
      const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
  published
    property Name: RawUTF8 read fName write fName stored AS_UNIQUE;
  end;

  TSQLNote = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fIdent: RawUTF8;
    fKind: TSQLNoteKind;
    fParent: TSQLNote;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
    property Kind: TSQLNoteKind read fKind write fKind;
    property Parent: TSQLNote read fParent write fParent;
  end;

  TSQLNoteFile = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fFileName: RawUTF8;
    fNote: TSQLNote;
  published
    property FileName: RawUTF8 read fFileName write fFileName;
    property Note: TSQLNote read fNote write fNote;
  end;

  TSQLUser = class(TSQLAuthUser)
  protected
    fMetaData: variant;
  published
    property MetaData: variant read fMetaData write fMetaData;
  end;

TSQLINVM_PROD = class(TSQLRecord)
  protected
   // fPRDRUN: Int64;
    fPRDCDE: RawUTF8;
    fPRDBAR: RawUTF8;
    fPRDNAT: RawUTF8;
    fPRDNAE: RawUTF8;
    fPRDNSE: RawUTF8;
    fMANRUN: Int64;
    fPRDREG: TDateTime;
    fPTYRUN: Int64;
    fUNIRUN: Int64;
    fPRDCOA: Currency;
    fPRDCOU: Currency;
    fROURUN: Int64;
    fVENRUN: Int64;
    fROUCDE: RawUTF8;
    fPSTRUN: RawUTF8;
    fCMPCDE: RawUTF8;
    fCMPBRN: RawUTF8;
    fCMPDEP: RawUTF8;
    fCMPSEC: RawUTF8;
    fENTUSR: RawUTF8;
    fENTDTE: TDateTime;
    fENTTIM: TDateTime;
    fENTWKS: RawUTF8;
    fMODUSR: RawUTF8;
    fMODDTE: TDateTime;
    fMODTIM: TDateTime;
    fMODWKS: RawUTF8;
    fRECACT: RawUTF8;
    fRECSTS: RawUTF8;
    fRECSEQ: Int64;
    fRECFL1: RawUTF8;
    fRECFL2: RawUTF8;
    fRECFL3: RawUTF8;
    fRECFL4: RawUTF8;
    fRECFL5: RawUTF8;
    fENTUID: Int64;
    fMODUID: Int64;
  published
    /// match INVM_PROD.PRDRUN [int 0 19 0] *
//    property PRDRUN: Int64 read fPRDRUN write fPRDRUN;
    /// match INVM_PROD.PRDCDE [nvarchar 60 0 0]
    property PRDCDE: RawUTF8 index 60 read fPRDCDE write fPRDCDE;
    /// match INVM_PROD.PRDBAR [nvarchar 60 0 0]
    property PRDBAR: RawUTF8 index 60 read fPRDBAR write fPRDBAR;
    /// match INVM_PROD.PRDNAT [nvarchar 200 0 0]
    property PRDNAT: RawUTF8 index 200 read fPRDNAT write fPRDNAT;
    /// match INVM_PROD.PRDNAE [nvarchar 200 0 0]
    property PRDNAE: RawUTF8 index 200 read fPRDNAE write fPRDNAE;
    /// match INVM_PROD.PRDNSE [nvarchar 200 0 0]
    property PRDNSE: RawUTF8 index 200 read fPRDNSE write fPRDNSE;
    /// match INVM_PROD.MANRUN [int 0 19 0]
    property MANRUN: Int64 read fMANRUN write fMANRUN;
    /// match INVM_PROD.PRDREG [date]
    property PRDREG: TDateTime read fPRDREG write fPRDREG;
    /// match INVM_PROD.PTYRUN [int 0 19 0]
    property PTYRUN: Int64 read fPTYRUN write fPTYRUN;
    /// match INVM_PROD.UNIRUN [int 0 19 0]
    property UNIRUN: Int64 read fUNIRUN write fUNIRUN;
    /// match INVM_PROD.PRDCOA [currency 0 15 4]
    property PRDCOA: Currency read fPRDCOA write fPRDCOA;
    /// match INVM_PROD.PRDCOU [currency 0 15 4]
    property PRDCOU: Currency read fPRDCOU write fPRDCOU;
    /// match INVM_PROD.ROURUN [int 0 19 0]
    property ROURUN: Int64 read fROURUN write fROURUN;
    /// match INVM_PROD.VENRUN [int 0 19 0]
    property VENRUN: Int64 read fVENRUN write fVENRUN;
    /// match INVM_PROD.ROUCDE [nvarchar 60 0 0]
    property ROUCDE: RawUTF8 index 60 read fROUCDE write fROUCDE;
    /// match INVM_PROD.PSTRUN [nvarchar 60 0 0]
    property PSTRUN: RawUTF8 index 60 read fPSTRUN write fPSTRUN;
    /// match INVM_PROD.CMPCDE [nvarchar 5 0 0]
    property CMPCDE: RawUTF8 index 5 read fCMPCDE write fCMPCDE;
    /// match INVM_PROD.CMPBRN [nvarchar 5 0 0]
    property CMPBRN: RawUTF8 index 5 read fCMPBRN write fCMPBRN;
    /// match INVM_PROD.CMPDEP [nvarchar 5 0 0]
    property CMPDEP: RawUTF8 index 5 read fCMPDEP write fCMPDEP;
    /// match INVM_PROD.CMPSEC [nvarchar 5 0 0]
    property CMPSEC: RawUTF8 index 5 read fCMPSEC write fCMPSEC;
    /// match INVM_PROD.ENTUSR [nvarchar 60 0 0]
    property ENTUSR: RawUTF8 index 60 read fENTUSR write fENTUSR;
    /// match INVM_PROD.ENTDTE [date]
    property ENTDTE: TDateTime read fENTDTE write fENTDTE;
    /// match INVM_PROD.ENTTIM [date]
    property ENTTIM: TDateTime read fENTTIM write fENTTIM;
    /// match INVM_PROD.ENTWKS [nvarchar 100 0 0]
    property ENTWKS: RawUTF8 index 100 read fENTWKS write fENTWKS;
    /// match INVM_PROD.MODUSR [nvarchar 60 0 0]
    property MODUSR: RawUTF8 index 60 read fMODUSR write fMODUSR;
    /// match INVM_PROD.MODDTE [date]
    property MODDTE: TDateTime read fMODDTE write fMODDTE;
    /// match INVM_PROD.MODTIM [date]
    property MODTIM: TDateTime read fMODTIM write fMODTIM;
    /// match INVM_PROD.MODWKS [nvarchar 100 0 0]
    property MODWKS: RawUTF8 index 100 read fMODWKS write fMODWKS;
    /// match INVM_PROD.RECACT [nvarchar 10 0 0]
    property RECACT: RawUTF8 index 10 read fRECACT write fRECACT;
    /// match INVM_PROD.RECSTS [nvarchar 10 0 0]
    property RECSTS: RawUTF8 index 10 read fRECSTS write fRECSTS;
    /// match INVM_PROD.RECSEQ [int 0 10 0]
    property RECSEQ: Int64 read fRECSEQ write fRECSEQ;
    /// match INVM_PROD.RECFL1 [nvarchar 3 0 0]
    property RECFL1: RawUTF8 index 3 read fRECFL1 write fRECFL1;
    /// match INVM_PROD.RECFL2 [nvarchar 3 0 0]
    property RECFL2: RawUTF8 index 3 read fRECFL2 write fRECFL2;
    /// match INVM_PROD.RECFL3 [nvarchar 3 0 0]
    property RECFL3: RawUTF8 index 3 read fRECFL3 write fRECFL3;
    /// match INVM_PROD.RECFL4 [nvarchar 3 0 0]
    property RECFL4: RawUTF8 index 3 read fRECFL4 write fRECFL4;
    /// match INVM_PROD.RECFL5 [nvarchar 3 0 0]
    property RECFL5: RawUTF8 index 3 read fRECFL5 write fRECFL5;
    /// match INVM_PROD.ENTUID [int 0 19 0]
    property ENTUID: Int64 read fENTUID write fENTUID;
    /// match INVM_PROD.MODUID [int 0 19 0]
    property MODUID: Int64 read fMODUID write fMODUID;
  end;


  TSQLINVM_PRICLIST = class(TSQLRecord)
  protected
    //fPLIRUN: Int64;
    fPRDRUN: Int64;
    fUNIRUN: Int64;
    fPGRCDE: RawUTF8;
    fSPGCDE: RawUTF8;
    fITMSEQ: Int64;
    fPCACDE: RawUTF8;
    fPRIRUN: Int64;
    fPRDCDE: RawUTF8;
    fPRTRUN: Int64;
    fPACRUN: Int64;
    fWH0RUN: Int64;
    fPRIVAL: Currency;
    fCMPCDE: RawUTF8;
    fCMPBRN: RawUTF8;
    fCMPDEP: RawUTF8;
    fCMPSEC: RawUTF8;
    fENTUSR: RawUTF8;
    fENTDTE: TDateTime;
    fENTTIM: TDateTime;
    fENTWKS: RawUTF8;
    fMODUSR: RawUTF8;
    fMODDTE: TDateTime;
    fMODTIM: TDateTime;
    fMODWKS: RawUTF8;
    fRECACT: RawUTF8;
    fRECSTS: RawUTF8;
    fRECSEQ: Int64;
    fRECFL1: RawUTF8;
    fRECFL2: RawUTF8;
    fRECFL3: RawUTF8;
    fRECFL4: RawUTF8;
    fRECFL5: RawUTF8;
    fENTUID: Int64;
    fMODUID: Int64;
    fRECREV: RawUTF8;
    fRECRVI: Int64;
  published
    /// match INVM_PRICLIST.PLIRUN [int 0 19 0] *
    //property PLIRUN: Int64 read fPLIRUN write fPLIRUN;
    /// match INVM_PRICLIST.PRDRUN [int 0 19 0] *
    property PRDRUN: Int64 read fPRDRUN write fPRDRUN;
    /// match INVM_PRICLIST.UNIRUN [int 0 19 0] *
    property UNIRUN: Int64 read fUNIRUN write fUNIRUN;
    /// match INVM_PRICLIST.PGRCDE [nvarchar 60 0 0] *
    property PGRCDE: RawUTF8 index 60 read fPGRCDE write fPGRCDE;
    /// match INVM_PRICLIST.SPGCDE [nvarchar 60 0 0] *
    property SPGCDE: RawUTF8 index 60 read fSPGCDE write fSPGCDE;
    /// match INVM_PRICLIST.ITMSEQ [int 0 10 0] *
    property ITMSEQ: Int64 read fITMSEQ write fITMSEQ;
    /// match INVM_PRICLIST.PCACDE [nvarchar 60 0 0] *
    property PCACDE: RawUTF8 index 60 read fPCACDE write fPCACDE;
    /// match INVM_PRICLIST.PRIRUN [int 0 19 0] *
    property PRIRUN: Int64 read fPRIRUN write fPRIRUN;
    /// match INVM_PRICLIST.PRDCDE [nvarchar 60 0 0] *
    property PRDCDE: RawUTF8 index 60 read fPRDCDE write fPRDCDE;
    /// match INVM_PRICLIST.PRTRUN [int 0 19 0]
    property PRTRUN: Int64 read fPRTRUN write fPRTRUN;
    /// match INVM_PRICLIST.PACRUN [int 0 19 0]
    property PACRUN: Int64 read fPACRUN write fPACRUN;
    /// match INVM_PRICLIST.WH0RUN [int 0 19 0]
    property WH0RUN: Int64 read fWH0RUN write fWH0RUN;
    /// match INVM_PRICLIST.PRIVAL [currency 0 18 4]
    property PRIVAL: Currency read fPRIVAL write fPRIVAL;
    /// match INVM_PRICLIST.CMPCDE [nvarchar 5 0 0]
    property CMPCDE: RawUTF8 index 5 read fCMPCDE write fCMPCDE;
    /// match INVM_PRICLIST.CMPBRN [nvarchar 5 0 0]
    property CMPBRN: RawUTF8 index 5 read fCMPBRN write fCMPBRN;
    /// match INVM_PRICLIST.CMPDEP [nvarchar 5 0 0]
    property CMPDEP: RawUTF8 index 5 read fCMPDEP write fCMPDEP;
    /// match INVM_PRICLIST.CMPSEC [nvarchar 5 0 0]
    property CMPSEC: RawUTF8 index 5 read fCMPSEC write fCMPSEC;
    /// match INVM_PRICLIST.ENTUSR [nvarchar 60 0 0]
    property ENTUSR: RawUTF8 index 60 read fENTUSR write fENTUSR;
    /// match INVM_PRICLIST.ENTDTE [date]
    property ENTDTE: TDateTime read fENTDTE write fENTDTE;
    /// match INVM_PRICLIST.ENTTIM [date]
    property ENTTIM: TDateTime read fENTTIM write fENTTIM;
    /// match INVM_PRICLIST.ENTWKS [nvarchar 100 0 0]
    property ENTWKS: RawUTF8 index 100 read fENTWKS write fENTWKS;
    /// match INVM_PRICLIST.MODUSR [nvarchar 60 0 0]
    property MODUSR: RawUTF8 index 60 read fMODUSR write fMODUSR;
    /// match INVM_PRICLIST.MODDTE [date]
    property MODDTE: TDateTime read fMODDTE write fMODDTE;
    /// match INVM_PRICLIST.MODTIM [date]
    property MODTIM: TDateTime read fMODTIM write fMODTIM;
    /// match INVM_PRICLIST.MODWKS [nvarchar 100 0 0]
    property MODWKS: RawUTF8 index 100 read fMODWKS write fMODWKS;
    /// match INVM_PRICLIST.RECACT [nvarchar 10 0 0]
    property RECACT: RawUTF8 index 10 read fRECACT write fRECACT;
    /// match INVM_PRICLIST.RECSTS [nvarchar 10 0 0]
    property RECSTS: RawUTF8 index 10 read fRECSTS write fRECSTS;
    /// match INVM_PRICLIST.RECSEQ [int 0 10 0]
    property RECSEQ: Int64 read fRECSEQ write fRECSEQ;
    /// match INVM_PRICLIST.RECFL1 [nvarchar 3 0 0]
    property RECFL1: RawUTF8 index 3 read fRECFL1 write fRECFL1;
    /// match INVM_PRICLIST.RECFL2 [nvarchar 3 0 0]
    property RECFL2: RawUTF8 index 3 read fRECFL2 write fRECFL2;
    /// match INVM_PRICLIST.RECFL3 [nvarchar 3 0 0]
    property RECFL3: RawUTF8 index 3 read fRECFL3 write fRECFL3;
    /// match INVM_PRICLIST.RECFL4 [nvarchar 3 0 0]
    property RECFL4: RawUTF8 index 3 read fRECFL4 write fRECFL4;
    /// match INVM_PRICLIST.RECFL5 [nvarchar 3 0 0]
    property RECFL5: RawUTF8 index 3 read fRECFL5 write fRECFL5;
    /// match INVM_PRICLIST.ENTUID [int 0 19 0]
    property ENTUID: Int64 read fENTUID write fENTUID;
    /// match INVM_PRICLIST.MODUID [int 0 19 0]
    property MODUID: Int64 read fMODUID write fMODUID;
    /// match INVM_PRICLIST.RECREV [nvarchar 3 0 0]
    property RECREV: RawUTF8 index 3 read fRECREV write fRECREV;
    /// match INVM_PRICLIST.RECRVI [int 0 10 0]
    property RECRVI: Int64 read fRECRVI write fRECRVI;
  end;




function DataModel:TSQLModel;

const
  SERVER_ROOT = 'root';
  HTTP_PORT = '888';
  
implementation

function DataModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLINVM_PROD,TSQLINVM_PRICLIST],SERVER_ROOT);
end;

{ TSQLNoteKind }

class procedure TSQLNoteKind.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
var Kind: TSQLNoteKind;
begin
  inherited;
  Kind := TSQLNoteKind.Create;
  Kind.Name := 'PostIt';
  Server.Add(Kind,true);
  Kind.Name := 'Todo';
  Server.Add(Kind,true);
  Kind.Free;
end;

end.