/// fill a VCL TClientDataset from SynVirtualDataset data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynRestVCL;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2016 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2016
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Esteban Martin (EMartin)
  - houdw2006

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18,
    which is an extraction from former SynDBVCL.pas unit.
  - Added that blob field updates they are made with AddJSONEscapeString.
  - bug fix when updating accentuated string fields.
  - bug fix with datetime fields

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef KYLIX3}
  Types,
  LibC,
  {$endif}
{$endif}
{$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
{$ifndef DELPHI5OROLDER}
  Variants,
{$endif}
  mORMot,
  SynCrtSock, // remover una vez implementado TSQLHttpClient
  SynCommons,
  SynDB, SynDBVCL,
  DB,
  {$ifdef FPC}
  BufDataset
  {$else}
  Contnrs,
  DBClient,
  Provider,
  SqlConst
  {$endif};


type
  /// generic Exception type
  ESQLRestException = class(ESynException);

  /// URI signature event
  TOnGetURISignature = procedure(Sender: TObject; var aURI: string) of object;

  /// a TDataSet which allows to apply updates on a Restful connection
  TSynRestSQLDataSet = class(TSynBinaryDataSet)
  private
    fRestClient: TSQLRest;

    function Compute(const aJSON: SockString; const aOccasion: TSQLOccasion): SockString;
    function ExtractFields(const aSQL, aAfterStr, aBeforeStr: string): string;
    function SQLFieldsToJSON(const aSQLOccasion: TSQLOccasion; var aFieldNames: RawUTF8;
      const aSQL, aAfterStr, aBeforeStr: string; aParams: TParams): SockString;
    function GetSQLOccasion(const aSQL: string): TSQLOccasion;
  protected
    fInsertedID, fDeletedID: TID;
    fCommandText: string;
    fDataSet: TSynBinaryDataSet;

    fOnGetURISignature: TOnGetURISignature;
    fParams: TParams;
    fProvider: TDataSetProvider;
    fTableName: RawUTF8;
    function BindParams(const aStatement: RawUTF8): RawUTF8;
    function GetSQLRecordClass: TSQLRecordClass;
    function GetTableName: string;
    // get the data
    procedure InternalInitFieldDefs; override;
    function InternalFrom(const aStatement: RawUTF8): RawByteString;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure ParseCommandText;
    // IProvider implementation
    procedure PSSetCommandText(const ACommandText: string); override;
    function PSGetTableName: string; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    {$ifdef ISDELPHIXE3}
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer; overload; override;
    {$else}
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer=nil): Integer; overload; override;
    {$endif}
    procedure SetCommandText(const Value: string);
  public
    property DeletedID: TID read fDeletedID write fDeletedID;
    property InsertedID: TID read fInsertedID;

    /// the associated RestClient must be set, or else access violation is raised.
    property RestClient: TSQLRest read fRestClient write fRestClient;
  published
    // - Statement is the nature SQL statement
    // examples:
    //   SELECT * FROM tableName
    //   SELECT * FROM tableName WHERE whereCondition ORDER BY orderByColumns
    //   SELECT * FROM tableName WHERE whereParam = :param ORDER BY orderByColumns
    // if :param is used then before open assign the value: ds.Params.ParamByName('param').value := XXX
    property CommandText: string read fCommandText write fCommandText;
    /// the associated SynDB TDataSet, used to retrieve and update data
    property DataSet: TSynBinaryDataSet read fDataSet;
    /// event to get URI signature
    property OnGetURISignature: TOnGetURISignature write fOnGetURISignature;
  end;

// JSON columns to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  const Null: TSQLDBProxyStatementColumns;
  const ColTypes: TSQLDBFieldTypeDynArray);
// convert to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
                      const DefaultDataType: TSQLDBFieldType = SynCommons.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;

implementation

uses
  DBCommon,
  SynVirtualDataset;

const
  FETCHALLTOBINARY_MAGIC = 1;

  SQLFIELDTYPETODBFIELDTYPE: array[TSQLFieldType] of TSQLDBFieldType =
    (SynCommons.ftUnknown,   // sftUnknown
     SynCommons.ftUTF8,      // sftAnsiText
     SynCommons.ftUTF8,      // sftUTF8Text
     SynCommons.ftInt64,     // sftEnumerate
     SynCommons.ftInt64,     // sftSet
     SynCommons.ftInt64,     // sftInteger
     SynCommons.ftInt64,     // sftID = TSQLRecord(aID)
     SynCommons.ftInt64,     // sftRecord = TRecordReference
     SynCommons.ftInt64,     // sftBoolean
     SynCommons.ftDouble,    // sftFloat
     SynCommons.ftDate,      // sftDateTime
     SynCommons.ftInt64,     // sftTimeLog
     SynCommons.ftCurrency,  // sftCurrency
     SynCommons.ftUTF8,      // sftObject
{$ifndef NOVARIANTS}
     SynCommons.ftUTF8,      // sftVariant
     SynCommons.ftUTF8,      // sftNullable
{$endif}
     SynCommons.ftBlob,      // sftBlob
     SynCommons.ftBlob,      // sftBlobDynArray
     SynCommons.ftBlob,      // sftBlobCustom
     SynCommons.ftUTF8,      // sftUTF8Custom
     SynCommons.ftUnknown,   // sftMany
     SynCommons.ftInt64,     // sftModTime
     SynCommons.ftInt64,     // sftCreateTime
     SynCommons.ftInt64,     // sftTID
     SynCommons.ftInt64,
     SynCommons.ftInt64
     );    // sftRecordVersion = TRecordVersion

  SQLFieldTypeToVCLDB: array[TSQLFieldType] of TFieldType =
    (DB.ftUnknown,           // sftUnknown
     DB.ftString,            // sftAnsiText
     DB.ftString,            // sftUTF8Text
     DB.ftLargeInt,          // sftEnumerate
     DB.ftLargeInt,          // sftSet
     DB.ftLargeInt,          // sftInteger
     DB.ftLargeInt,          // sftID = TSQLRecord(aID)
     DB.ftLargeInt,          // sftRecord = TRecordReference
     DB.ftLargeInt,          // sftBoolean
     DB.ftFloat,             // sftFloat
     DB.ftDateTime,          // sftDateTime
     DB.ftLargeInt,          // sftTimeLog
     DB.ftCurrency,          // sftCurrency
     DB.ftString,            // sftObject
{$ifndef NOVARIANTS}
     DB.ftString,            // sftVariant
     DB.ftString,            // sftNullable
{$endif}
     DB.ftBlob,              // sftBlob
     DB.ftBlob,              // sftBlobDynArray
     DB.ftBlob,              // sftBlobCustom
     DB.ftString,            // sftUTF8Custom
     DB.ftUnknown,           // sftMany
     DB.ftLargeInt,          // sftModTime
     DB.ftLargeInt,          // sftCreateTime
     DB.ftLargeInt,          // sftTID
     DB.ftLargeInt,DB.ftLargeInt);         // sftRecordVersion = TRecordVersion

  VCLDBFieldTypeSQLDB: array[0..23] of TSQLFieldType =
    (sftUnknown,        // ftUnknown
     sftAnsiText,       //  ftString
     sftUTF8Text,       // ftString
     sftEnumerate,      // ftInteger
     sftSet,            // ftInteger
     sftInteger,        // ftInteger
     sftID,             // ftLargeInt = TSQLRecord(aID)
     sftRecord,         // ftLargeInt
     sftBoolean,        // ftBoolean
     sftFloat,          // ftFloat
     sftDateTime,       // ftDate
     sftTimeLog,        // ftLargeInt
     sftCurrency,       // ftCurrency
     sftObject,         // ftString
{$ifndef NOVARIANTS}
     sftVariant,        // ftString
{$endif}
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftUTF8Custom,     // ftString
     sftMany,           // ftUnknown
     sftModTime,        // ftLargeInt
     sftCreateTime,     // ftLargeInt
     sftID,             // ftLargeInt
     sftRecordVersion); // ftLargeInt = TRecordVersion

{$ifndef FPC}


procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  const Null: TSQLDBProxyStatementColumns; const ColTypes: TSQLDBFieldTypeDynArray);
var F: integer;
    VDouble: double;
    VCurrency: currency absolute VDouble;
    VDateTime: TDateTime absolute VDouble;
    colType: TSQLDBFieldType;
begin
  for F := 0 to length(ColTypes)-1 do
    if not (F in Null) then begin
      colType := ColTypes[F];
      if colType<ftInt64 then begin // ftUnknown,ftNull
        colType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)]; // per-row column type (SQLite3 only)
        W.Write1(ord(colType));
      end;
      case colType of
      ftInt64:
      begin
        W.WriteVarInt64(aTable.FieldAsInteger(F));
      end;
      ftDouble: begin
        VDouble := aTable.FieldAsFloat(F);
        W.Write(@VDouble,sizeof(VDouble));
      end;
      SynCommons.ftCurrency: begin
        VCurrency := aTable.Field(F);
        W.Write(@VCurrency,sizeof(VCurrency));
      end;
      SynCommons.ftDate: begin
        VDateTime := aTable.Field(F);
        W.Write(@VDateTime,sizeof(VDateTime));
      end;
      SynCommons.ftUTF8:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      SynCommons.ftBlob:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      else
      raise ESQLDBException.CreateUTF8('JSONColumnsToBinary: Invalid ColumnType(%)=%',
        [aTable.Get(0, F),ord(colType)]);
    end;
  end;
end;

function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
                      const DefaultDataType: TSQLDBFieldType = SynCommons.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;
var F, FMax, FieldSize, NullRowSize: integer;
    StartPos: cardinal;
    Null: TSQLDBProxyStatementColumns;
    W: TFileBufferWriter;
    ColTypes: TSQLDBFieldTypeDynArray;
    FieldType: TSQLDBFieldType;
begin
  FillChar(Null,sizeof(Null),0);
  result := 0;
  W := TFileBufferWriter.Create(Dest);
  try
    W.WriteVarUInt32(FETCHALLTOBINARY_MAGIC);
    FMax := aTable.FieldCount;
    W.WriteVarUInt32(FMax);
    if FMax>0 then begin
      // write column description
      SetLength(ColTypes,FMax);
      dec(FMax);
      for F := 0 to FMax do begin
        W.Write(aTable.Get(0, F));
        FieldType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)];
        if (FieldType = SynCommons.ftUnknown) and (DefaultDataType <> SynCommons.ftUnknown) then
          FieldType := DefaultDataType;
        ColTypes[F] := FieldType;
        FieldSize := aTable.FieldLengthMax(F);
        if (FieldSize = 0) and (FieldType = DefaultDataType) and (DefaultFieldSize <> 0) then
          FieldSize := DefaultFieldSize;
        W.Write1(ord(ColTypes[F]));
        W.WriteVarUInt32(FieldSize);
      end;
      // initialize null handling
      NullRowSize := (FMax shr 3)+1;
      if NullRowSize>sizeof(Null) then
        raise ESQLDBException.CreateUTF8(
          'JSONToBinary: too many columns', []);
      // save all data rows
      StartPos := W.TotalWritten;
      if aTable.Step or (aTable.RowCount=1) then // Need step first or error is raised in Table.Field function.
      repeat
        // save row position in DataRowPosition[] (if any)
        if DataRowPosition<>nil then begin
          if Length(DataRowPosition^)<=integer(result) then
            SetLength(DataRowPosition^,result+result shr 3+256);
          DataRowPosition^[result] := W.TotalWritten-StartPos;
        end;
        // first write null columns flags
        if NullRowSize>0 then begin
          FillChar(Null,NullRowSize,0);
          NullRowSize := 0;
        end;
        for F := 0 to FMax do
        begin
          if VarIsNull(aTable.Field(F)) then begin
            include(Null,F);
            NullRowSize := (F shr 3)+1;
          end;
        end;
        W.WriteVarUInt32(NullRowSize);
        if NullRowSize>0 then
          W.Write(@Null,NullRowSize);
        // then write data values
        JSONColumnsToBinary(aTable, W,Null,ColTypes);
        inc(result);
        if (MaxRowCount>0) and (result>=MaxRowCount) then
          break;
      until not aTable.Step;
    end;
    W.Write(@result,SizeOf(result)); // fixed size at the end for row count
    W.Flush;
  finally
    W.Free;
  end;
end;

{ TSynRestSQLDataSet }

function TSynRestSQLDataSet.Compute(const aJSON: SockString; const aOccasion: TSQLOccasion): SockString;
var
  lRec: TSQLRecord;
  lRecBak: TSQLRecord; // backup for get modifications
  lJSON: TDocVariantData;
  I: Integer;
  lCount: Integer;
  lOccasion: TSQLEvent;
  lVarValue: Variant;
  lVarValueBak: Variant;
begin
  lRec := GetSQLRecordClass.Create;
  lRecBak := GetSQLRecordClass.Create;
  try
    lJSON.InitJSON(aJSON);
    lCount := lJSON.Count;
    // update record fields
    for I := 0 to lCount-1 do
      lRec.SetFieldVariant(lJSON.Names[I], lJSON.Values[I]);
    lOccasion := seUpdate;
    if (aOccasion = soInsert) then
      lOccasion := seAdd;
    lRec.ComputeFieldsBeforeWrite(Nil, lOccasion);
    // get modified fields
    for I := 0 to lRec.RecordProps.Fields.Count-1 do
    begin
      lRec.RecordProps.Fields.Items[I].GetVariant(lRec, lVarValue);
      lRecBak.RecordProps.Fields.Items[I].GetVariant(lRecBak, lVarValueBak);
      if (lVarValue <> lVarValueBak) then
        lJSON.AddOrUpdateValue(lRec.RecordProps.Fields.Items[I].Name, lVarValue);
    end;
    Result := lJSON.ToJSON;
  finally
    lRec.Free;
    lRecBak.Free;
  end;
end;

function TSynRestSQLDataSet.ExtractFields(const aSQL, aAfterStr, aBeforeStr: string): string;
var
  lPosStart: Integer;
  lPosEnd: Integer;
  lSQL: string;
begin
  lSQL := StringReplace(aSQL, sLineBreak, ' ', [rfReplaceAll]);
  lPosStart := Pos(aAfterStr, lSQL)+Length(aAfterStr);
  lPosEnd   := Pos(aBeforeStr, lSQL);
  Result := Trim(Copy(lSQL, lPosStart, lPosEnd-lPosStart));
end;

function TSynRestSQLDataSet.SQLFieldsToJSON(const aSQLOccasion: TSQLOccasion;
  var aFieldNames: RawUTF8;
  const aSQL, aAfterStr, aBeforeStr: string; aParams: TParams): SockString;
var
  I: Integer;
  lLastPos: Integer;
  lFieldValues: TStrings;
  lBlob: TSQLRawBlob;
  aFieldNameWriter: TTextWriter;
begin
  aFieldNames := '';
  lFieldValues := TStringList.Create;
  aFieldNameWriter := TTextWriter.CreateOwnedStream;
  try
    ExtractStrings([','], [], PChar(ExtractFields(aSQL, aAfterStr, aBeforeStr)), lFieldValues);
    lLastPos := 0;
    with TTextWriter.CreateOwnedStream do
    begin
      Add('{');
      for I := 0 to lFieldValues.Count-1 do
      begin
        if (Pos('=', lFieldValues[I]) = 0) then
          lFieldValues[I] := lFieldValues[I] + '=';
        AddFieldName(Trim(lFieldValues.Names[I]));

        if (aParams[I].DataType <> ftBlob) then
        begin
          aFieldNameWriter.AddString(Trim(lFieldValues.Names[I]));
          aFieldNameWriter.Add(',');

          if (TVarData(aParams[I].Value).VType = varString) then
            AddVariant(StringToUTF8(aParams[I].Value))
          else
            AddVariant(aParams[I].Value);
        end
        else
        begin
          Add('"');
          lBlob :=  BlobToTSQLRawBlob(PUTF8Char(aParams[I].AsBlob));
          AddJSONEscapeString(lBlob);
          Add('"');
        end;
        Add(',');
        lLastPos := I;
      end;
      CancelLastComma;
      Add('}');
      Result := Text;
      Free;
    end;
    aFieldNameWriter.CancelLastComma;
    aFieldNames := aFieldNameWriter.Text;
    lFieldValues.Clear;
    // the first field after the where clause is the ID
    if (aSQLOccasion <> soInsert) then
      aParams[lLastPos+1].Name := 'ID';
  finally
    aFieldNameWriter.Free;
    lFieldValues.Free;
  end;
end;

function TSynRestSQLDataSet.GetSQLOccasion(const aSQL: string): TSQLOccasion;
begin
  if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'DELETE') then
    Result := soDelete
  else if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'INSERT') then
    Result := soInsert
  else
    Result := soUpdate;
end;

function TSynRestSQLDataSet.BindParams(const aStatement: RawUTF8): RawUTF8;
var
  I: Integer;
  lParamName: string;
begin
  Result := aStatement;
  if (PosEx(':', aStatement) = 0) and (fParams.Count = 0) then
    Exit;
  if ((PosEx(':', aStatement) = 0) and (fParams.Count > 0)) or ((PosEx(':', aStatement) > 0) and (fParams.Count = 0)) then
    raise ESQLRestException.CreateUTF8('Statement parameters (%) not match with Params (Count=%) property',
      [aStatement, fParams.Count]);
  for I := 0 to fParams.Count-1 do
  begin
    lParamName := ':' + fParams[I].Name;
    Result := StringReplace(Result, lParamName, fParams[I].AsString, [rfIgnoreCase]);
  end;
end;

function TSynRestSQLDataSet.GetSQLRecordClass: TSQLRecordClass;
begin
  Result := fRestClient.Model.Table[GetTableName];
  if not Assigned(Result) then
    raise ESQLRestException.CreateUTF8('Table % not registered in SQL Model', [GetTableName]);
end;

function TSynRestSQLDataSet.GetTableName: string;
begin
  Result := PSGetTableName
end;

procedure TSynRestSQLDataSet.InternalClose;
begin
  inherited InternalClose;
  FreeAndNil(fDataAccess);
  fData := '';
end;

function TSynRestSQLDataSet.InternalFrom(const aStatement: RawUTF8): RawByteString;

  procedure UpdateFields(aSQLTableJSON: TSQLTableJSON);
  var
    I, J: Integer;
    lFields: TSQLPropInfoList;
  begin
    lFields := GetSQLRecordClass.RecordProps.Fields;
    for I := 0 to aSQLTableJSON.FieldCount-1 do
    begin
      J := lFields.IndexByName(aSQLTableJSON.Get(0, I));
      if (J > -1) then
        aSQLTableJSON.SetFieldType(I, lFields.Items[J].SQLFieldType, Nil, lFields.Items[J].FieldWidth);
    end;
  end;

var
  lData: TRawByteStringStream;
  lSQLTableJSON: TSQLTableJSON;
  lStatement: RawUTF8;
  lResp: TDocVariantData;
  lErrMsg: RawUTF8;
  lURI: RawUTF8;
begin
  lURI := GetTableName;
  lStatement := BindParams(aStatement);
  Result := fRestClient.ExecuteJson([GetSQLRecordClass], lStatement);
  if (Result = '') then
    raise ESynException.CreateUTF8('Cannot get response (timeout?) from %', [lURI]);
  if (Result <> '') then
  begin
    lResp.InitJSON(Result);
    if (lResp.Kind = dvUndefined) then
      raise ESynException.CreateUTF8('Invalid JSON response' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                     [Result, lURI]);
    if (lResp.Kind = dvObject) then
      if (lResp.GetValueIndex('errorCode') > -1) then
        if (lResp.GetValueIndex('errorText') > -1) then
        begin
          lErrMsg := AnyAnsiToUTF8(lResp.Value['errorText']);
          raise ESynException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                         [lResp.Value['errorText'], lURI]);
        end
        else if (lResp.GetValueIndex('error') > -1) then
        begin
          lErrMsg := AnyAnsiToUTF8(lResp.Value['error']);
          raise ESynException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%', [lErrMsg, lURI]);
        end;

    lSQLTableJSON := TSQLTableJSON.CreateFromTables([GetSQLRecordClass], '', Result);
    // update info fields for avoid error conversion in JSONToBinary
    UpdateFields(lSQLTableJSON);
    lData := TRawByteStringStream.Create('');
    try
      JSONToBinary(lSQLTableJSON, lData);
      Result := lData.DataString
    finally
      FreeAndNil(lData);
      FreeAndNil(lSQLTableJSON);
    end;
  end;
end;

procedure TSynRestSQLDataSet.InternalInitFieldDefs;
var
  F, aFieldWidth: integer;
  aFieldType: TSQLFieldType;
  lFields: TSQLPropInfoList;
  lFieldDef: TFieldDef;
begin
  inherited;
  if (GetTableName = '') then // JSON conversion to dataset ?
    Exit;
  // update field definitions from associated TSQLRecordClass of the table
  lFields := GetSQLRecordClass.RecordProps.Fields;
  for F := 0 to lFields.Count-1 do
  begin
    lFieldDef := TFieldDef(TDefCollection(FieldDefs).Find(lFields.Items[F].Name));
    if Assigned(lFieldDef) then
    begin
      aFieldWidth := lFields.Items[F].FieldWidth;
      aFieldType := lFields.Items[F].SQLFieldType;

      if (lFieldDef.DataType <> SQLFieldTypeToVCLDB[aFieldType]) then
        lFieldDef.DataType := SQLFieldTypeToVCLDB[aFieldType];
      if (aFieldWidth > 0) and (lFieldDef.Size <> aFieldWidth) then
        lFieldDef.Size := aFieldWidth;
    end;
  end;
end;

procedure TSynRestSQLDataSet.InternalOpen;
var
  lData: RawByteString;
begin
  if (fCommandText='') then begin
    if fData<>'' then // called e.g. after From() method
      inherited InternalOpen;
    exit;
  end;
  lData := InternalFrom(fCommandText);
  if (lData <> '') then
  begin
    From(lData);
    inherited InternalOpen;
  end;
end;

procedure TSynRestSQLDataSet.ParseCommandText;
var
  temp: RawUTF8;
begin
  // it is assumed that fCommandText is in the nature SQL Statement form. eg. SELECT * FROM tableName [WHERE ...]
  Split(UpperCase(fCommandText), 'FROM', temp, fTableName);
  fTableName := Trim(fTableName);
  Split(fTableName, ' ', fTableName, temp);
end;

{$ifdef ISDELPHIXE3}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): Integer;
var DS: TDataSet;
begin
  DS := nil;
  result := PSExecuteStatement(ASQL,AParams,DS);
  DS.Free;
end;

function TSynRestSQLDataSet.PSExecuteStatement(const ASQL:
    string; AParams: TParams; var ResultSet: TDataSet): Integer;
{$else}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string;
    AParams: TParams; ResultSet: Pointer): Integer;
{$endif}
var
  lJSON: SockString;
  lOccasion: TSQLOccasion;

  fID: TID;
  fFieldNames: RawUTF8;
  fRec: TSQLRecord;
begin // only execute writes in current implementation
  Result := -1;
  lOccasion := GetSQLOccasion(aSQL);
  case lOccasion of
  soDelete:
    begin
      fID := fDeletedID;  // fDeletedID is set by instance of TSynRestDataSet
      if fRestClient.Delete(GetSQLRecordClass, fID) then
        Result := 1;
    end;
  soInsert:
    begin
      lJSON := SQLFieldsToJSON(soInsert, fFieldNames, aSQL, '(', ') ', aParams);
      fRec := GetSQLRecordClass.CreateFrom(lJSON);
      try
        fInsertedID := fRestClient.Add(fRec, fRec.RecordProps.FieldBitsFromCSV(fFieldNames), True);
        if fInsertedID > 0 then
        begin
          Result := 1;
          AfterInsert(Self);  // Update the ID field in the instance of TSynRestDataSet
        end;
      finally
        fRec.Free;
      end;
    end;
  soUpdate:
    begin
      lJSON := SQLFieldsToJSON(soUpdate, fFieldNames, aSQL, 'set ', 'where ', aParams);
      fRec := GetSQLRecordClass.CreateFrom(lJSON);
      try
        fID := aParams.ParamByName('ID').Value;
        fRec.IDValue := fID;  // fRec.ID is readonly, fRec.IDValue is writable
        if fRestClient.Update(fRec, fRec.RecordProps.FieldBitsFromCSV(fFieldNames)) then
          Result := 1;
      finally
        fRec.Free;
      end;
    end
  end;
end;

function TSynRestSQLDataSet.PSGetTableName: string;
begin
  Result := fTableName;
end;

function TSynRestSQLDataSet.PSIsSQLBased: Boolean;
begin
  result := true;
end;

function TSynRestSQLDataSet.PSIsSQLSupported: Boolean;
begin
  result := true;
end;

procedure TSynRestSQLDataSet.PSSetCommandText(const ACommandText: string);
begin
  if (fCommandText <> ACommandText) then
    SetCommandText(ACommandText);
end;

function TSynRestSQLDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  result := false;
end;

procedure TSynRestSQLDataSet.SetCommandText(const Value: string);
begin
  if (Value <> fCommandtext) then
  begin
    fCommandText := Value;
    ParseCommandText;
  end;
end;

{$endif FPC}

end.

