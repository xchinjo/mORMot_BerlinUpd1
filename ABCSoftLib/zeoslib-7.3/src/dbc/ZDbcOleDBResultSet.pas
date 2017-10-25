{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcOleDBResultSet;

interface

{$I ZDbc.inc}

{$IFDEF ENABLE_OLEDB}

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons,
  {$ENDIF}
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  {$IFDEF OLD_FPC}ZClasses, {$ENDIF}ZSysUtils, ZDbcIntfs, ZDbcGenericResolver,
  ZOleDB, ZDbcOleDBUtils,
  ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet, ZDbcResultsetMetadata, ZCompatibility;

type
  {** Implements Ado ResultSet. }
  TZOleDBResultSet = class(TZAbstractResultSet)
  private
    FEnhancedColInfo: Boolean;
    FInMemoryDataLobs: Boolean;
    FChunkSize: Integer;
    FRowSet: IRowSet;
    FZBufferSize: Integer;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FRowSize: NativeInt;
    FAccessor:HACCESSOR;
    FLobAccessors: array of HACCESSOR;
    FRowCount: DBROWCOUNT;
    FCurrentBufRowNo: DBROWOFFSET;
    FRowsObtained: DBCOUNTITEM;
    FHROWS: PHROWS_Array;
    FColBuffer: TByteDynArray;
    FRowStates: TDBROWSTATUSDynArray;
    FLobColsIndex: TIntegerDynArray;
    fpcColumns: DBORDINAL;
    fTempBlob: IZBlob;
  private
    FData: Pointer;
    FLength: DBLENGTH;
    procedure ReleaseFetchedRows;
    procedure CreateAccessors;
  protected
    procedure Open; override;
  public
    constructor Create(Statement: IZStatement; const SQL: string; RowSet: IRowSet;
      ZBufferSize, ChunkSize: Integer; InMemoryDataLobs: Boolean; const EnhancedColInfo: Boolean = True);
    procedure ResetCursor; override;
    function Next: Boolean; override;
    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetRawByteString(ColumnIndex: Integer): RawByteString; override;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; EndJSONObject: Boolean = True;
      With_DATETIME_MAGIC: Boolean = False; SkipNullFields: Boolean = False); override;
    {$ENDIF USE_SYNCOMMONS}
  end;

  {** Implements a cached resolver with MSSQL specific functionality. }
  TZOleDBMSSQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
    FResultSet: IZResultSet;
    fStmt: IZPreparedStatement;
  public
    constructor Create(Statement: IZStatement; Metadata: IZResultSetMetadata);
    destructor Destroy; override;

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

  TZOleDBCLOB = class(TZAbstractClob)
  public
    constructor Create(RowSet: IRowSet; Accessor: HACCESSOR; wType: DBTYPE;
      CurrentRow: HROW; ChunkSize: Integer; const ConSettings: PZConSettings);
  end;

  TZOleDBBLOB = class(TZAbstractBlob)
  public
    constructor Create(RowSet: IRowSet; Accessor: HACCESSOR;
      CurrentRow: HROW; ChunkSize: Integer);
  end;

  TZOleDBCachedResultSet = class(TZCachedResultSet)
  private
    FResultSet: TZOleDBResultSet;
  protected
    function Fetch: Boolean; override;
  public
    constructor Create(ResultSet: TZOleDBResultSet; const SQL: string;
      Resolver: IZCachedResolver; ConSettings: PZConSettings);
  end;

function GetCurrentResultSet(RowSet: IRowSet; Statement: IZStatement;
  Const SQL: String; ConSettings: PZConSettings; BuffSize, ChunkSize: Integer;
  EnhancedColInfo, InMemoryDataLobs: Boolean; var PCurrRS: Pointer): IZResultSet;

implementation

uses
  Variants, Math, ComObj,
  ZDbcOleDB, ZDbcOleDBStatement, ZMessages, ZEncoding, ZFastCode;

var
  LobReadObj: TDBObject;
  LobDBBinding: TDBBinding;


{$IFDEF USE_SYNCOMMONS}
procedure TZOleDBResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  EndJSONObject: Boolean; With_DATETIME_MAGIC: Boolean; SkipNullFields: Boolean);
var I, C, L, H: Integer;
    P: PAnsiChar;
    Len: NativeUInt;
    blob: IZBlob;
begin
  //init
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if IsNull(C+FirstDbcIndex) then
      if JSONWriter.Expand then begin
        if (not SkipNullFields) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[i]);
      case FDBBindingArray[C].wType of
        DBTYPE_EMPTY,
        DBTYPE_IDISPATCH,
        DBTYPE_IUNKNOWN:  JSONWriter.AddShort('""');
        DBTYPE_NULL:      JSONWriter.AddShort('null');
        DBTYPE_I2:        JSONWriter.Add(PSmallInt(FData)^);
        DBTYPE_I4,
        DBTYPE_ERROR:     JSONWriter.Add(PLongInt(FData)^);
        DBTYPE_R4:        JSONWriter.AddSingle(PSingle(FData)^);
        DBTYPE_R8:        JSONWriter.AddDouble(PDouble(FData)^);
        DBTYPE_CY:        JSONWriter.AddCurr64(PCurrency(FData)^);
        DBTYPE_DATE:      JSONWriter.AddDateTime(PDateTime(FData), 'T', '"');
        DBTYPE_BOOL:      JSONWriter.AddShort(JSONBool[PWordBool(FData)^]);
        DBTYPE_VARIANT: begin
            JSONWriter.Add('"');
            FUniTemp := POleVariant(FData)^;
            JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            JSONWriter.Add('"');
          end;
        //DBTYPE_DECIMAL = 14;
        DBTYPE_UI1:       JSONWriter.AddU(PByte(FData)^);
        DBTYPE_I1:        JSONWriter.Add(PShortInt(FData)^);
        DBTYPE_UI2:       JSONWriter.AddU(PWord(FData)^);
        DBTYPE_UI4:       JSONWriter.AddU(PLongWord(FData)^);
        DBTYPE_I8:        JSONWriter.Add(PInt64(FData)^);
        DBTYPE_UI8:       JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PUInt64(FData)^));
        DBTYPE_GUID:      begin
                            JSONWriter.Add('"');
                            JSONWriter.Add(PGUID(FData)^);
                            JSONWriter.Add('"');
                          end;
        DBTYPE_GUID or DBTYPE_BYREF:
                          begin
                            JSONWriter.Add('"');
                            JSONWriter.Add(PGUID(PPointer(FData)^)^);
                            JSONWriter.Add('"');
                          end;
        DBTYPE_BYTES:
          if FDBBindingArray[C].cbMaxLen = 0 then begin //streamed
            blob := TZOleDBBLOB.Create(FRowSet, FLobAccessors[FDBBindingArray[C].obLength], FHROWS^[FCurrentBufRowNo], FChunkSize);
            JSONWriter.WrBase64(Blob.GetBuffer,Blob.Length,true); // withMagic=true
          end else
            JSONWriter.WrBase64(FData,FLength, True);
        DBTYPE_BYTES or DBTYPE_BYREF:
          JSONWriter.WrBase64(PPointer(FData)^,FLength,True);
        DBTYPE_STR: begin
            JSONWriter.Add('"');
            if FDBBindingArray[C].cbMaxLen = 0 then begin
              blob := TZOleDBCLOB.Create(FRowSet, FLobAccessors[FDBBindingArray[C].obLength],
                DBTYPE_STR, FHROWS^[FCurrentBufRowNo], FChunkSize, ConSettings);
              P := Pointer(blob.GetPWideChar);
              JSONWriter.AddJSONEscapeW(Pointer(P), blob.Length shr 1);
            end else begin
              if FDBBindingArray[c].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
                FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, ConSettings^.ClientCodePage^.CP)
              else begin
                L := FLength;
                while (PAnsiChar(FData)+L-1)^ = ' ' do Dec(L);
                  FUniTemp := PRawToUnicode(PAnsiChar(FData), L, ConSettings^.ClientCodePage^.CP);
              end;
              JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_STR or DBTYPE_BYREF: begin
            JSONWriter.Add('"');
            FUniTemp := PRawToUnicode(PPAnsiChar(FData)^, FLength, ConSettings^.ClientCodePage^.CP);
            JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            JSONWriter.Add('"');
          end;
        DBTYPE_BSTR,
        DBTYPE_WSTR: begin
            JSONWriter.Add('"');
            if FDBBindingArray[c].cbMaxLen = 0 then begin
              blob := TZOleDBCLOB.Create(FRowSet,
                FLobAccessors[FDBBindingArray[C].obLength],
                DBTYPE_WSTR, FHROWS^[FCurrentBufRowNo], FChunkSize, ConSettings);
              P := Pointer(blob.GetPWideChar);
              JSONWriter.AddJSONEscapeW(Pointer(P), blob.Length shr 1);
            end else if FDBBindingArray[c].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
              JSONWriter.AddJSONEscapeW(FData, FLength shr 1)
            else begin //Fixed width
              L := FLength shr 1;
              while (PWideChar(FData)+L-1)^ = ' ' do Dec(L);
              JSONWriter.AddJSONEscapeW(FData, L);
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_BSTR or DBTYPE_BYREF,
        DBTYPE_WSTR or DBTYPE_BYREF: begin
            JSONWriter.Add('"');
            JSONWriter.AddJSONEscapeW(PPointer(FData)^, FLength shr 1);
            JSONWriter.Add('"');
          end;
        //DBTYPE_NUMERIC	= 131;
        //DBTYPE_UDT	= 132;
        DBTYPE_DBDATE:    begin
                            JSONWriter.Add('"');
                            if With_DATETIME_MAGIC then
                              JSONWriter.AddNoJSONEscapeUTF8(SynCommons.DateTimeToSQL(EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month, PDBDate(FData)^.day)))
                            else
                              JSONWriter.AddDateTime(EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month, PDBDate(FData)^.day));
                              JSONWriter.Add('"');
                            end;
        DBTYPE_DBTIME:    begin
                            JSONWriter.Add('"');
                            if With_DATETIME_MAGIC then
                              JSONWriter.AddNoJSONEscapeUTF8( SynCommons.DateTimeToSQL(EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute, PDBTime(FData)^.second, 0)))
                            else
                              JSONWriter.AddDateTime(EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute, PDBTime(FData)^.second, 0));
                            JSONWriter.Add('"');
                          end;
        DBTYPE_DBTIMESTAMP: begin
                              JSONWriter.Add('"');
                              if With_DATETIME_MAGIC then
                                JSONWriter.AddNoJSONEscapeUTF8(SynCommons.DateTimeToSQL(EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day)+
                                  EncodeTime(PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second, 0)))
                                else
                                  JSONWriter.AddDateTime((EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day)+
                                    EncodeTime(PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second, 0)));
                              JSONWriter.Add('"');
                            end;
        DBTYPE_HCHAPTER:  JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PCHAPTER(FData)^));
        //DBTYPE_FILETIME	= 64;
        //DBTYPE_PROPVARIANT	= 138;
        //DBTYPE_VARNUMERIC	= 139;
      end;
      JSONWriter.Add(',');
    end;
  end;
  if EndJSONObject then
  begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZOleDBResultSet.Create(Statement: IZStatement; const SQL: string;
  RowSet: IRowSet; ZBufferSize, ChunkSize: Integer; InMemoryDataLobs: Boolean;
  const EnhancedColInfo: Boolean = True);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);
  FRowSet := RowSet;
  FZBufferSize := ZBufferSize;
  FEnhancedColInfo := EnhancedColInfo;
  FAccessor := 0;
  FCurrentBufRowNo := 0;
  FRowsObtained := 0;
  FHROWS := nil;
  FInMemoryDataLobs := InMemoryDataLobs;
  FChunkSize := ChunkSize;
  Open;
end;

procedure TZOleDBResultSet.CreateAccessors;
var I: Integer;
begin
  OleDBCheck((FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA{ or DBACCESSOR_OPTIMIZED, 8Byte alignments do NOT work with fixed width fields},
    fpcColumns, Pointer(FDBBindingArray), FRowSize, @FAccessor,
    Pointer(FDBBINDSTATUSArray)), FDBBINDSTATUSArray);

  SetLength(FLobAccessors, Length(FLobColsIndex));
  for i := 0 to high(FLobColsIndex) do
  begin
    LobDBBinding.iOrdinal := FDBBindingArray[FLobColsIndex[i]].iOrdinal;
    OleDBCheck((FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA, 1,
      @LobDBBinding, 0, @FLobAccessors[i], nil), nil);
  end;
end;

procedure TZOleDBResultSet.ReleaseFetchedRows;
var I,j: Integer;
begin
  if (FRowsObtained > 0) then
  begin
    if FInMemoryDataLobs and (Length(FLobColsIndex) > 0) then
      for i := 0 to high(FLobColsIndex) do
        for J := 0 to FRowsObtained -1 do
          (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(Pointer(@FColBuffer[FDBBindingArray[FLobColsIndex[i]].obValue+NativeUInt(FRowSize*J)]));
    OleDBCheck(fRowSet.ReleaseRows(FRowsObtained,FHROWS,nil,nil,Pointer(FRowStates)), FRowStates);
    (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(FHROWS);
    FHROWS := nil;
    FRowsObtained := 0;
  end;
end;
{**
  Opens this recordset and initializes the Column information.
}
procedure TZOleDBResultSet.Open;
var
  OleDBColumnsInfo: IColumnsInfo;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
begin
  if not Assigned(FRowSet) or
     Failed(FRowSet.QueryInterface(IID_IColumnsInfo, OleDBColumnsInfo)) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  OleDBColumnsInfo.GetColumnInfo(fpcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo; //save pointer for Malloc.Free
  try
    { Fills the column info }
    ColumnsInfo.Clear;
    if Assigned(prgInfo) then
      if prgInfo.iOrdinal = 0 then // skip possible bookmark column
      begin
        Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));
        Dec(fpcColumns);
      end;
    SetLength(FDBBINDSTATUSArray, fpcColumns);
    FRowSize := PrepareOleColumnDBBindings(fpcColumns, FInMemoryDataLobs,
      FDBBindingArray, prgInfo, FLobColsIndex);
    FRowCount := Max(1, FZBufferSize div NativeInt(FRowSize));
    if (MaxRows > 0) and (FRowCount > MaxRows) then
      FRowCount := MaxRows; //fetch only wanted count of rows

    for I := prgInfo.iOrdinal-1 to fpcColumns-1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      if (prgInfo^.pwszName=nil) or (prgInfo^.pwszName^=#0) then
        ColumnInfo.ColumnLabel := 'col_'+ZFastCode.IntToStr(i)
      else
        ColumnInfo.ColumnLabel := String(prgInfo^.pwszName);
      ColumnInfo.ColumnName := ColumnInfo.ColumnLabel;
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(prgInfo^.wType,
        prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG <> 0, ConSettings.CPType);

      if prgInfo^.ulColumnSize > Cardinal(MaxInt) then
        FieldSize := 0
      else
        FieldSize := prgInfo^.ulColumnSize;
      if ColumnInfo.ColumnType = stGUID then
        ColumnInfo.ColumnDisplaySize := 38
      else
        ColumnInfo.ColumnDisplaySize := FieldSize;
      ColumnInfo.Precision := FieldSize;
      ColumnInfo.Currency := ColumnInfo.ColumnType = stCurrency;
      ColumnInfo.AutoIncrement := prgInfo.dwFlags and DBCOLUMNFLAGS_ISROWID = DBCOLUMNFLAGS_ISROWID;
      ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stDouble, stBigDecimal];
      ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0);
      ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0);
      ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
      ColumnsInfo.Add(ColumnInfo);
      Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
    end;
  finally
    if Assigned(ppStringsBuffer) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(ppStringsBuffer);
    if Assigned(OriginalprgInfo) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(OriginalprgInfo);
  end;
  inherited Open;
end;

procedure TZOleDBResultSet.ResetCursor;
var
  FAccessorRefCount: DBREFCOUNT;
  i: Integer;
begin
  if not Closed then
    try
      ReleaseFetchedRows;
      {first release Accessor rows}
      for i := Length(FLobAccessors)-1 downto 0 do
        OleDBCheck((fRowSet As IAccessor).ReleaseAccessor(FLobAccessors[i], @FAccessorRefCount));
      SetLength(FLobAccessors, 0);
      if FAccessor > 0 then
        OleDBCheck((fRowSet As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount));
    finally
      FRowSet := nil;
      FAccessor := 0;
      RowNo := 0;
      FCurrentBufRowNo := 0;
      FRowsObtained := 0;
    end;
  FRowSet := nil;//handle 'Object is in use Exception'
  inherited ResetCursor;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZOleDBResultSet.Next: Boolean;
var
  I: NativeInt;
  stmt: IZOleDBPreparedStatement;
label Success, NoSuccess;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or
    Closed or ((not Closed) and (FRowSet = nil) and  (not (Supports(Statement, IZOleDBPreparedStatement, Stmt) and Stmt.GetNewRowSet(FRowSet)))) then
    goto NoSuccess;

  if (RowNo = 0) then //fetch Iteration count of rows
  begin
    CreateAccessors;
    OleDBCheck(fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS));
    if FRowsObtained > 0 then
    begin
      if DBROWCOUNT(FRowsObtained) < FRowCount then
      begin //reserve required mem only
        SetLength(FColBuffer, NativeInt(FRowsObtained) * FRowSize);
        MaxRows := FRowsObtained;
      end
      else //reserve full allowed mem
        SetLength(FColBuffer, (FRowCount * FRowSize));
      SetLength(FRowStates, FRowsObtained);
      {fetch data into the buffer}
      for i := 0 to FRowsObtained -1 do
        OleDBCheck(fRowSet.GetData(FHROWS^[i], FAccessor, @FColBuffer[I*FRowSize]));
      goto success;
    end
    else //we do NOT need a buffer here!
      goto NoSuccess;
  end
  else
    if FCurrentBufRowNo < DBROWCOUNT(FRowsObtained)-1 then
    begin
      Inc(FCurrentBufRowNo);
      goto Success;
    end
    else
    begin
      {release old rows}
      ReleaseFetchedRows;
      OleDBCheck(fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS));
      if DBROWCOUNT(FRowsObtained) < FCurrentBufRowNo then
        MaxRows := RowNo+Integer(FRowsObtained);  //this makes Exit out in first check on next fetch
      FCurrentBufRowNo := 0; //reset Buffer offsett
      if FRowsObtained > 0 then
      begin
        {fetch data into the buffer}
        for i := 0 to FRowsObtained -1 do
          OleDBCheck(fRowSet.GetData(FHROWS[i], FAccessor, @FColBuffer[I*FRowSize]));
        goto Success;
      end else goto NoSuccess;
    end;

Success:
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
    Exit;
NoSuccess:
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZOleDBResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := PDBSTATUS(@FColBuffer[FDBBindingArray[ColumnIndex].obStatus+NativeUInt(FRowSize*FCurrentBufRowNo)])^ = DBSTATUS_S_ISNULL;
  LastWasNull := Result;
  if Result then
  begin
    FData := nil;
    FLength := 0;
  end
  else
  begin
    //note FData is valid only if no Lobs DBPART_VALUE was set on binding!!!
    FData := @FColBuffer[FDBBindingArray[ColumnIndex].obValue+NativeUInt(FRowSize*FCurrentBufRowNo)];
    //note FLength is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
    FLength := PDBLENGTH(@FColBuffer[FDBBindingArray[ColumnIndex].obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetString(ColumnIndex: Integer): String;
var
  I: Integer;
begin
  if (IsNull(ColumnIndex)) then //Sets LastWasNull, FData, FLength!!
    Result := ''
  else
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_EMPTY:     Result := '';
      DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToStr(PSmallInt(FData)^);
      DBTYPE_I4:        Result := ZFastCode.IntToStr(PLongInt(FData)^);
      DBTYPE_R4:        Result := FloatToSQLStr(PSingle(FData)^);
      DBTYPE_R8:        Result := FloatToSQLStr(PDouble(FData)^);
      DBTYPE_CY:        Result := CurrToStr(PCurrency(FData)^);
      DBTYPE_DATE:
        Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(
          PDateTime(FData)^, ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
          {$IFDEF UNICODE}
          System.SetString(Result, PWideChar(FData),FLength shr 1)
          {$ELSE}
          Result := PUnicodeToRaw(PWideChar(FData),FLength shr 1, ConSettings^.Ctrl_CP)
          {$ENDIF}
        else
        begin //Fixed width
          I := FLength shr 1;
          while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
          {$IFDEF UNICODE}
          System.SetString(Result, PWideChar(FData), I)
          {$ELSE}
          Result := PUnicodeToRaw(PWideChar(FData), I, ConSettings^.Ctrl_CP)
          {$ENDIF}
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(FData)^, FLength shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToStr(PLongInt(FData)^);
      DBTYPE_BOOL:      Result := {$IFDEF UNICODE}BoolToUnicodeEx{$ELSE}BoolToRawEx{$ENDIF}(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToStr(PByte(FData)^);
      DBTYPE_I1:        Result := ZFastCode.IntToStr(PShortInt(FData)^);
      DBTYPE_UI2:       Result := ZFastCode.IntToStr(PWord(FData)^);
      DBTYPE_UI4:       Result := ZFastCode.IntToStr(PLongWord(FData)^);
      DBTYPE_I8:        Result := ZFastCode.IntToStr(PInt64(FData)^);
      DBTYPE_UI8:       Result := ZFastCode.IntToStr(PUInt64(FData)^);
      DBTYPE_GUID:      Result := {$IFDEF UNICODE}GuidToUnicode{$ELSE}GuidToRaw{$ENDIF}(PGUID(FData)^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          {$IFDEF UNICODE}
          Result := GetBlob(ColumnIndex).GetUniCodeString
          {$ELSE}
          if ConSettings^.AutoEncode then
          begin
            FUniTemp := GetBlob(ColumnIndex).GetUniCodeString;
            Result := ConSettings^.ConvFuncs.ZUnicodeToRaw(FUniTemp, ConSettings^.CTRL_CP)
          end
          else
            Result := GetBlob(ColumnIndex).GetRawByteString
          {$ENDIF}
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            {$IFDEF UNICODE}
              Result := PRawToUnicode(PAnsiChar(FData), FLength, ConSettings^.ClientCodePage^.CP)
            {$ELSE}
              System.SetString(Result, PAnsiChar(FData), FLength)
            {$ENDIF}
          else
          begin
            I := FLength;
            while (PAnsiChar(FData)+I-1)^ = ' ' do Dec(I);
            {$IFDEF UNICODE}
              Result := PRawToUnicode(PAnsiChar(FData), I, ConSettings^.ClientCodePage^.CP);
            {$ELSE}
              System.SetString(Result, PAnsiChar(FData), I);
            {$ENDIF}
          end;
      DBTYPE_STR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
          Result := PRawToUnicode(PPAnsiChar(FData)^, FLength, ConSettings^.ClientCodePage^.CP);
        {$ELSE}
          System.SetString(Result, PPAnsiChar(FData)^, FLength);
        {$ENDIF}
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          {$IFDEF UNICODE}
          Result := GetBlob(ColumnIndex).GetUniCodeString
          {$ELSE}
          begin
            FUniTemp := GetBlob(ColumnIndex).GetUniCodeString;
            Result := ConSettings^.ConvFuncs.ZUnicodeToRaw(FUniTemp, ConSettings^.CTRL_CP)
          end
          {$ENDIF}
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            {$IFDEF UNICODE}
            System.SetString(Result, PWideChar(FData), FLength shr 1)
            {$ELSE}
            Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, ConSettings^.Ctrl_CP)
            {$ENDIF}
          else
          begin //Fixed width
            I := FLength shr 1;
            while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
            {$IFDEF UNICODE}
            System.SetString(Result, PWideChar(FData), I)
            {$ELSE}
            Result := PUnicodeToRaw(PWideChar(FData), I, ConSettings^.Ctrl_CP)
            {$ENDIF}
          end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        {$IFDEF UNICODE}
        System.SetString(Result, ZPPWideChar(FData)^, FLength shr 1);
        {$ELSE}
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.Ctrl_CP);
        {$ENDIF}
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLDate{$ELSE}DateTimeToRawSQLDate{$ENDIF}(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLTime{$ELSE}DateTimeToRawSQLTime{$ENDIF}(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        Result := {$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToStr(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var I: Integer;
begin
  if (IsNull(ColumnIndex)) then //Sets LastWasNull, FData, FLength!!
    Result := ''
  else
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(FData)^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(FData)^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(FData)^);
      DBTYPE_CY:        Result := AnsiString(CurrToStr(PCurrency(FData)^));
      DBTYPE_DATE:      Result := DateTimeToRawSQLTimeStamp(PDateTime(FData)^,
                          ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
          Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, GetACP)
        else
        begin //Fixed width
          I := FLength shr 1;
          while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
          Result := PUnicodeToRaw(PWideChar(FData), I, GetACP)
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, GetACP);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := AnsiString(POleVariant(FData)^);
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(FData)^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(FData)^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(FData)^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(FData)^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(FData)^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(FData)^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(FData)^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetAnsiString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            ZSetString(PAnsiChar(FData), FLength, Result)
          else
          begin
            I := FLength shr 1;
            while (PAnsiChar(FData)+I-1)^ = ' ' do Dec(I);
            ZSetString(PAnsiChar(FData), I, Result);
          end;
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(FData)^, FLength);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetAnsiString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, GetACP)
          else
          begin //Fixed width
            I := FLength shr 1;
            while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
            Result := PUnicodeToRaw(PWideChar(FData), I, GetACP)
          end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, GetACP);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        Result := DateTimeToRawSQLDate(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        Result := DateTimeToRawSQLTime(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        Result := DateTimeToRawSQLTimeStamp(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var I: Integer;
begin
  if (IsNull(ColumnIndex)) then //Sets LastWasNull, FData, FLength!!
    Result := ''
  else
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(FData)^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(FData)^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(FData)^);
      DBTYPE_CY:        Result := UTF8String(CurrToStr(PCurrency(FData)^));
      DBTYPE_DATE:      Result := DateTimeToRawSQLTimeStamp(PDateTime(FData)^,
                          ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
          Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, zCP_UTF8)
        else
        begin //Fixed width
          I := FLength shr 1;
          while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
          Result := PUnicodeToRaw(PWideChar(FData), I, zCP_UTF8)
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(PPWideChar(FData)^, FLength shr 1, zCP_UTF8);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := UTF8String(ZWideString(POleVariant(FData)^));
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(FData)^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(FData)^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(FData)^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(FData)^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(FData)^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(FData)^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(FData)^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetUTF8String
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            ZSetString(PAnsiChar(FData), FLength, Result)
          else
          begin
            I := FLength shr 1;
            while (PAnsiChar(FData)+I-1)^ = ' ' do Dec(I);
            ZSetString(PAnsiChar(FData), I, Result);
          end;
      DBTYPE_STR or DBTYPE_BYREF:
        ZSetString(PPAnsiChar(FData)^, FLength, Result);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetUTF8String
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, zCP_UTF8)
          else
          begin //Fixed width
            I := FLength shr 1;
            while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
            Result := PUnicodeToRaw(PWideChar(FData), I, zCP_UTF8)
          end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, zCP_UTF8);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        Result := DateTimeToRawSQLDate(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        Result := DateTimeToRawSQLTime(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        Result := DateTimeToRawSQLTimeStamp(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
var I: Integer;
begin
  if (IsNull(ColumnIndex)) then //Sets LastWasNull, FData, FLength!!
    Result := ''
  else
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      //DBTYPE_EMPTY:     Result := '';
      //DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToRaw(PSmallInt(FData)^);
      DBTYPE_I4:        Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_R4:        Result := FloatToSQLRaw(PSingle(FData)^);
      DBTYPE_R8:        Result := FloatToSQLRaw(PDouble(FData)^);
      DBTYPE_CY:        Result := RawByteString(CurrToStr(PCurrency(FData)^));
      DBTYPE_DATE:      Result := DateTimeToRawSQLTimeStamp(PDateTime(FData)^,
                          ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
          Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, ConSettings^.ClientCodePage^.CP)
        else
        begin //Fixed width
          I := FLength shr 1;
          while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
          Result := PUnicodeToRaw(PWideChar(FData), I, ConSettings^.ClientCodePage^.CP)
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToRaw(PLongInt(FData)^);
      DBTYPE_BOOL:      Result := BoolToRawEx(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := RawByteString(ZWideString(POleVariant(FData)^));
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToRaw(PByte(FData)^);
      DBTYPE_I1:        Result := ZFastCode.IntToRaw(PShortInt(FData)^);
      DBTYPE_UI2:       Result := ZFastCode.IntToRaw(PWord(FData)^);
      DBTYPE_UI4:       Result := ZFastCode.IntToRaw(PLongWord(FData)^);
      DBTYPE_I8:        Result := ZFastCode.IntToRaw(PInt64(FData)^);
      DBTYPE_UI8:       Result := ZFastCode.IntToRaw(PUInt64(FData)^);
      DBTYPE_GUID:      Result := GUIDToRaw(PGUID(FData)^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetRawByteString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            ZSetString(PAnsiChar(FData), FLength, Result)
          else
          begin //Fixed width
            I := FLength shr 1;
            while (PAnsiChar(FData)+I-1)^ = ' ' do Dec(I);
            ZSetString(PAnsiChar(FData), I, Result);
          end;
      DBTYPE_STR or DBTYPE_BYREF:
        ZSetString(PPAnsiChar(FData)^, FLength,Result);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetRawByteString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            Result := PUnicodeToRaw(PWideChar(FData), FLength shr 1, ConSettings^.ClientCodePage^.CP)
          else
          begin //Fixed width
            I := FLength shr 1;
            while (PWideChar(FData)+I-1)^ = ' ' do Dec(I);
            Result := PUnicodeToRaw(PWideChar(FData), I, ConSettings^.ClientCodePage^.CP)
          end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.ClientCodePage^.CP);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        Result := DateTimeToRawSQLDate(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        Result := DateTimeToRawSQLTime(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        Result := DateTimeToRawSQLTimeStamp(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToRaw(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
  Len := 0;
  if IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    Result := nil
  else
  begin
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        FUniTemp := ZFastCode.IntToUnicode(PSmallInt(FData)^);
      DBTYPE_I4:        FUniTemp := ZFastCode.IntToUnicode(PLongInt(FData)^);
      DBTYPE_R4:        FUniTemp := FloatToSQLUnicode(PSingle(FData)^);
      DBTYPE_R8:        FUniTemp := FloatToSQLUnicode(PDouble(FData)^);
      DBTYPE_CY:        FUniTemp := {%H-}CurrToStr(PCurrency(FData)^);
      DBTYPE_DATE:      FUniTemp := DateTimeToUnicodeSQLTimeStamp(PDateTime(FData)^,
                          ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        begin
          Result := PWideChar(FData);
          Len := FLength shr 1;
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
            while (Result+Len-1)^ = ' ' do Dec(Len);
          Exit;
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        begin
          Result := ZPPWideChar(FData)^;
          Len := FLength shr 1;
          Exit;
        end;
      DBTYPE_ERROR:     FUniTemp := ZFastCode.IntToUnicode(PLongInt(FData)^);
      DBTYPE_BOOL:      FUniTemp := BoolToUnicodeEx(PWordBool(FData)^);
      DBTYPE_VARIANT:   FUniTemp := POleVariant(FData)^;
      DBTYPE_UI1:       FUniTemp := ZFastCode.IntToUnicode(PByte(FData)^);
      DBTYPE_I1:        FUniTemp := ZFastCode.IntToUnicode(PShortInt(FData)^);
      DBTYPE_UI2:       FUniTemp := ZFastCode.IntToUnicode(PWord(FData)^);
      DBTYPE_UI4:       FUniTemp := ZFastCode.IntToUnicode(PLongWord(FData)^);
      DBTYPE_I8:        FUniTemp := ZFastCode.IntToUnicode(PInt64(FData)^);
      DBTYPE_UI8:       FUniTemp := ZFastCode.IntToUnicode(PUInt64(FData)^);
      DBTYPE_GUID:      FUniTemp := GuidToUnicode(PGUID(FData)^);
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          FUniTemp := GetBlob(ColumnIndex).GetUnicodeString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, ConSettings^.ClientCodePage^.CP)
          else
          begin
            Len := FLength;
            while (PAnsiChar(FData)+Len-1)^ = ' ' do Dec(Len);
            FUniTemp := PRawToUnicode(PAnsiChar(FData), Len, ConSettings^.ClientCodePage^.CP);
          end;
      DBTYPE_STR or DBTYPE_BYREF:
          FUniTemp := PRawToUnicode(PPAnsiChar(FData)^, FLength, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
        begin
          fTempBlob := GetBlob(ColumnIndex);
          Result := fTempBlob.GetPWideChar;
          Len := fTempBlob.Length shr 1;
          Exit;
        end
        else
        begin
          Result := PWideChar(FData);
          Len := FLength shr 1;
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
            while (PWideChar(FData)+Len-1)^ = ' ' do Dec(Len);
          Exit;
        end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        begin
          Result := ZPPWideChar(FData)^;
          Len := FLength shr 1;
          Exit;
        end;
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        FUniTemp := DateTimeToUnicodeSQLDate(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        FUniTemp := DateTimeToUnicodeSQLTime(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        FUniTemp := DateTimeToUnicodeSQLTimeStamp(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  FUniTemp := ZFastCode.IntToUnicode(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else FUniTemp := '';
    end;
    Len := Length(FUniTemp);
    if Len = 0 then
      Result := PEmptyUnicodeString
    else
      Result := Pointer(FUniTemp);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
begin
  if IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    Result := ''
  else
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_EMPTY:     Result := '';
      DBTYPE_NULL:      Result := '';
      DBTYPE_I2:        Result := ZFastCode.IntToUnicode(PSmallInt(FData)^);
      DBTYPE_I4:        Result := ZFastCode.IntToUnicode(PLongInt(FData)^);
      DBTYPE_R4:        Result := FloatToSQLUnicode(PSingle(FData)^);
      DBTYPE_R8:        Result := FloatToSQLUnicode(PDouble(FData)^);
      DBTYPE_CY:        Result := {%H-}CurrToStr(PCurrency(FData)^);
      DBTYPE_DATE:      Result := DateTimeToUnicodeSQLTimeStamp(PDateTime(FData)^,
                          ConSettings.ReadFormatSettings, False);
      DBTYPE_BSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
          System.SetString(Result, ZPPWideChar(FData)^, FLength shr 1)
        else
        begin //Fixed width
          FLength := FLength shr 1;
          while (PWideChar(FData)+FLength-1)^ = ' ' do Dec(FLength);
          System.SetString(Result, ZPPWideChar(FData)^, FLength);
        end;
      DBTYPE_BSTR or DBTYPE_BYREF:
        System.SetString(Result, ZPPWideChar(FData)^, FLength shr 1);
      DBTYPE_IDISPATCH: Result := '';
      DBTYPE_ERROR:     Result := ZFastCode.IntToUnicode(PLongInt(FData)^);
      DBTYPE_BOOL:      Result := BoolToUnicodeEx(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      DBTYPE_IUNKNOWN:  Result := '';
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := ZFastCode.IntToUnicode(PByte(FData)^);
      DBTYPE_I1:        Result := ZFastCode.IntToUnicode(PShortInt(FData)^);
      DBTYPE_UI2:       Result := ZFastCode.IntToUnicode(PWord(FData)^);
      DBTYPE_UI4:       Result := ZFastCode.IntToUnicode(PLongWord(FData)^);
      DBTYPE_I8:        Result := ZFastCode.IntToUnicode(PInt64(FData)^);
      DBTYPE_UI8:       Result := ZFastCode.IntToUnicode(PUInt64(FData)^);
      DBTYPE_GUID:      Result := GuidToUnicode(PGUID(FData)^);
      DBTYPE_BYTES, DBTYPE_BYTES or DBTYPE_BYREF:   Result := '';
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetUnicodeString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            Result := PRawToUnicode(PAnsiChar(FData), FLength, ConSettings^.ClientCodePage^.CP)
          else
          begin //Fixed width
            while (PAnsiChar(FData)+FLength-1)^ = ' ' do Dec(FLength);
            Result := PRawToUnicode(PAnsiChar(FData),
              FLength, ConSettings^.ClientCodePage^.CP);
          end;
      DBTYPE_STR or DBTYPE_BYREF:
        Result := PRawToUnicode(PPAnsiChar(FData)^, FLength, ConSettings^.ClientCodePage^.CP);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := GetBlob(ColumnIndex).GetUnicodeString
        else
          if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
            System.SetString(Result, PWideChar(FData), FLength shr 1)
          else
          begin //Fixed width
            FLength := FLength shr 1;
            while (PWideChar(FData)+FLength-1)^ = ' ' do Dec(FLength);
            Result := PRawToUnicode(PAnsiChar(FData),
              FLength, ConSettings^.ClientCodePage^.CP);
          end;
      DBTYPE_WSTR or DBTYPE_BYREF:
        System.SetString(Result, ZPPWideChar(FData)^, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      DBTYPE_DBDATE:
        Result := DateTimeToUnicodeSQLDate(
          EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIME:
        Result := DateTimeToUnicodeSQLTime(
          EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0), ConSettings.ReadFormatSettings, False);
      DBTYPE_DBTIMESTAMP:
        Result := DateTimeToUnicodeSQLTimeStamp(
          EncodeDate(Abs(PDBTimeStamp(FData)^.year), PDBTimeStamp(FData)^.month,
          PDBTimeStamp(FData)^.day)+ EncodeTime(PDBTimeStamp(FData)^.hour,
          PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
          PDBTimeStamp(FData)^.fraction div 1000000), ConSettings.ReadFormatSettings, False);
      DBTYPE_HCHAPTER:  Result := ZFastCode.IntToUnicode(PCHAPTER(FData)^);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else Result := '';
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZOleDBResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  Result := False;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^ <> 0;
      DBTYPE_I4:        Result := PLongInt(FData)^ <> 0;
      DBTYPE_R4:        Result := PSingle(FData)^  <> 0;
      DBTYPE_R8:        Result := PDouble(FData)^ <> 0;
      DBTYPE_CY:        Result := PCurrency(FData)^ <> 0;
      DBTYPE_DATE:      Result := PDateTime(FData)^ <> 0;
      DBTYPE_BSTR:
        Result := StrToBoolEx(PWideChar(FData),
          True, FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := StrToBoolEx(ZPPWideChar(FData)^);
      DBTYPE_ERROR:     Result := PLongInt(FData)^ <> 0;
      DBTYPE_BOOL:      Result := PWordBool(FData)^;
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^ <> 0;
      DBTYPE_I1:        Result := PShortInt(FData)^ <> 0;
      DBTYPE_UI2:       Result := PWord(FData)^ <> 0;
      DBTYPE_UI4:       Result := PLongWord(FData)^ <> 0;
      DBTYPE_I8:        Result := PInt64(FData)^ <> 0;
      DBTYPE_UI8:       Result := PUInt64(FData)^ <> 0;
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := StrToBoolEx(GetBlob(ColumnIndex).GetRawByteString)
        else
          Result := StrToBoolEx(PAnsiChar(FData),
            True, FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := StrToBoolEx(PPAnsiChar(FData)^);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := StrToBoolEx(GetBlob(ColumnIndex).GetUnicodeString)
        else
          Result := StrToBoolEx(PWideChar(FData),
            True, FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := StrToBoolEx(ZPPWideChar(FData)^);
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^ <> 0;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BSTR:      Result := UnicodeToIntDef(PWideChar(FData), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        Result := UnicodeToIntDef(ZPPWideChar(FData)^,0);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := RawToIntDef(GetBlob(ColumnIndex).GetBuffer, 0)
        else
          Result := RawToIntDef(PAnsiChar(FData),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(FData)^, 0);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := UnicodeToIntDef(GetBlob(ColumnIndex).GetPWideChar, 0)
        else
          Result := UnicodeToIntDef(PWideChar(FData),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(FData)^,0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BSTR:      Result := UnicodeToIntDef(PWideChar(FData), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        Result := UnicodeToIntDef(ZPPWideChar(FData)^,0);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := RawToIntDef(GetBlob(ColumnIndex).GetBuffer, 0)
        else
          Result := RawToIntDef(PAnsiChar(FData),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(FData)^,0);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := UnicodeToIntDef(GetBlob(ColumnIndex).GetPWideChar, 0)
        else
          Result := UnicodeToIntDef(PWideChar(FData),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(FData)^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BSTR:      Result := UnicodeToIntDef(PWideChar(FData), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(FData)^,0);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := RawToIntDef(GetBlob(ColumnIndex).GetBuffer, 0)
        else
          Result := RawToIntDef(PAnsiChar(FData),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToIntDef(PPAnsiChar(FData)^,0);
      DBTYPE_WSTR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := UnicodeToIntDef(GetBlob(ColumnIndex).GetPWideChar, 0)
        else
          Result := UnicodeToIntDef(PWideChar(FData),0);
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeToIntDef(ZPPWideChar(FData)^,0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BSTR:      Result := UnicodeToInt64Def(PWideChar(FData), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        Result := UnicodeToInt64Def(ZPPWideChar(FData)^, 0);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
                          Result := RawToInt64Def(GetBlob(ColumnIndex).GetBuffer, 0)
                        else
                          Result := RawToInt64Def(PAnsiChar(FData),0);
      DBTYPE_STR or DBTYPE_BYREF:
                        Result := RawToInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:      if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
                          Result := UnicodeToInt64Def(GetBlob(ColumnIndex).GetPWideChar, 0)
                        else
                          Result := UnicodeToInt64Def(PWideChar(FData), 0);
      DBTYPE_WSTR or DBTYPE_BYREF:
                        Result := UnicodeToInt64Def(ZPPWideChar(FData)^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BSTR:      Result := UnicodeToUInt64Def(PWideChar(FData), 0);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        Result := UnicodeToUInt64Def(ZPPWideChar(FData)^, 0);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
                          Result := RawToUInt64Def(GetBlob(ColumnIndex).GetBuffer, 0)
                        else
                          Result := RawToUInt64Def(PAnsiChar(FData),0);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := RawToUInt64Def(PPAnsiChar(FData)^, 0);
      DBTYPE_WSTR:      if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
                          Result := UnicodeToInt64Def(GetBlob(ColumnIndex).GetPWideChar, 0)
                        else
                          Result := UnicodeToUInt64Def(PWideChar(FData), 0);
      DBTYPE_WSTR or DBTYPE_BYREF:
                          Result := UnicodeToUInt64Def(ZPPWideChar(FData)^, 0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_BSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_STR or DBTYPE_BYREF:
                        SQLStrToFloatDef(PPAnsiChar(FData)^, 0, Result, FLength);
      DBTYPE_WSTR:
                        SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_WSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOleDBResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_BSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_STR or DBTYPE_BYREF:
                        SQLStrToFloatDef(PPAnsiChar(FData)^, 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_WSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PLongInt(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_BSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_BSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      DBTYPE_ERROR:     Result := PLongInt(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWordBool(FData)^);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PLongWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_STR or DBTYPE_BYREF:
                        SQLStrToFloatDef(PPAnsiChar(FData)^, 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_WSTR or DBTYPE_BYREF:
                        SQLStrToFloatDef(ZPPWideChar(FData)^, 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  Result := nil;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_GUID:
        begin
          SetLength(Result, 16);
          System.Move(Pointer(FData)^, Pointer(Result)^, 16);
        end;
      DBTYPE_GUID or DBTYPE_BYREF:
        begin
          SetLength(Result, 16);
          System.Move(PPointer(FData)^^, Pointer(Result)^, 16);
        end;
      DBTYPE_BYTES:
        begin
          SetLength(Result, FLength);
          System.Move(Pointer(FData)^,
            Pointer(Result)^, FLength);
        end;
      DBTYPE_BYTES or DBTYPE_BYREF:
        begin
          SetLength(Result, FLength);
          System.Move(PPointer(FData)^^,
            Pointer(Result)^, FLength);
        end;
      else LastWasNull := True;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_DATE: Result := Trunc(PDateTime(FData)^);
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(FData),
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^,
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(FData),
          FLength);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(FData)^,
          FLength);*)
      DBTYPE_WSTR:
        Result := UnicodeSQLDateToDateTime(PWideChar(FData), FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeSQLDateToDateTime(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_DBDATE:
        Result := EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day);
      DBTYPE_DBTIME: Result := 0;
      DBTYPE_DBTIMESTAMP:
        Result := EncodeDate(Abs(PDBTimeStamp(FData)^.year),
          PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day);
      else LastWasNull := True;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOleDBResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_DATE:
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stTime: Result := PDateTime(FData)^;
          stDate: Result := 0;
          else
            Result := Frac(PDateTime(FData)^);
        end;
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(FData),
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^,
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(FData),
          FLength);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(FData)^,
          FLength);*)
      DBTYPE_WSTR:
        Result := UnicodeSQLTimeToDateTime(PWideChar(FData), FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeSQLTimeToDateTime(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_DBDATE: Result := 0;
      DBTYPE_DBTIME:
        Result := EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0);
      DBTYPE_DBTIME2:
        Result := EncodeTime(PDBTime2(FData)^.hour, PDBTime2(FData)^.minute,
            PDBTime2(FData)^.second,PDBTime2(FData)^.fraction div 1000000);
      DBTYPE_DBTIMESTAMP:
        Result := EncodeDate(Abs(PDBTimeStamp(FData)^.year),
                    PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day)+
                  EncodeTime(PDBTimeStamp(FData)^.hour,
                    PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
                    PDBTimeStamp(FData)^.fraction div 1000000);
      else LastWasNull := True;
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZOleDBResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_DATE:
        Result := PDateTime(FData)^;
(*      DBTYPE_BSTR:
        Result := PUnicodeToRaw(PWideChar(FData),
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_BSTR or DBTYPE_BYREF:
        Result := PUnicodeToRaw(ZPPWideChar(FData)^,
          FLength shr 1, ConSettings^.ClientCodePage^.CP);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      DBTYPE_STR:
        System.SetString(Result, PAnsiChar(FData),
          FLength);
      DBTYPE_STR or DBTYPE_BYREF:
        System.SetString(Result, PPAnsiChar(FData)^,
          FLength);*)
      DBTYPE_WSTR:
        Result := UnicodeSQLTimeStampToDateTime(PWideChar(FData), FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_WSTR or DBTYPE_BYREF:
        Result := UnicodeSQLTimeStampToDateTime(ZPPWideChar(FData)^, FLength shr 1, ConSettings^.ReadFormatSettings, Failed{%H-});
      DBTYPE_DBDATE:
        Result := EncodeDate(Abs(PDBDate(FData)^.year), PDBDate(FData)^.month,
          PDBDate(FData)^.day);
      DBTYPE_DBTIME:
        Result := EncodeTime(PDBTime(FData)^.hour, PDBTime(FData)^.minute,
            PDBTime(FData)^.second,0);
      DBTYPE_DBTIME2:
        Result := EncodeTime(PDBTime2(FData)^.hour, PDBTime2(FData)^.minute,
            PDBTime2(FData)^.second,PDBTime2(FData)^.fraction div 1000000);
      DBTYPE_DBTIMESTAMP:
        Result := EncodeDate(Abs(PDBTimeStamp(FData)^.year),
                    PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day)+
                  EncodeTime(PDBTimeStamp(FData)^.hour,
                    PDBTimeStamp(FData)^.minute, PDBTimeStamp(FData)^.second,
                    PDBTimeStamp(FData)^.fraction div 1000000);
      else LastWasNull := True;
    end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZOleDBResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
  Result := nil;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
      DBTYPE_GUID:
        Result := TZAbstractBlob.CreateWithData(Pointer(FData), 16);
      DBTYPE_GUID or DBTYPE_BYREF:
        Result := TZAbstractBlob.CreateWithData(PPointer(FData)^, 16);
      DBTYPE_BYTES:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := TZOleDBBLOB.Create(FRowSet,
            FLobAccessors[FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].obLength],
            FHROWS^[FCurrentBufRowNo], FChunkSize)
        else
         Result := TZAbstractBlob.CreateWithData(Pointer(FData), FLength);
      DBTYPE_BYTES or DBTYPE_BYREF:
        Result := TZAbstractBlob.CreateWithData(PPointer(FData)^,
          FLength);
      DBTYPE_STR:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSet,
            FLobAccessors[FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].obLength],
            DBTYPE_STR, FHROWS^[FCurrentBufRowNo], FChunkSize, ConSettings)
        else
          Result := TZAbstractClob.CreateWithData(PAnsiChar(FData),
            FLength, ConSettings^.ClientCodePage^.CP, ConSettings);
      DBTYPE_STR or DBTYPE_BYREF:
        Result := TZAbstractClob.CreateWithData(PPAnsiChar(FData)^,
          FLength, ConSettings^.ClientCodePage^.CP, ConSettings);
      DBTYPE_BSTR, DBTYPE_WSTR, DBTYPE_XML:
        if FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSet,
            FLobAccessors[FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].obLength],
            DBTYPE_WSTR, FHROWS^[FCurrentBufRowNo], FChunkSize, ConSettings)
        else
          Result := TZAbstractClob.CreateWithData(PWideChar(FData), FLength shr 1, ConSettings);
      DBTYPE_BSTR or DBTYPE_BYREF,
      DBTYPE_WSTR or DBTYPE_BYREF,
      DBTYPE_XML or DBTYPE_BYREF:
        Result := TZAbstractClob.CreateWithData(ZPPWideChar(FData)^,
          FLength shr 1, ConSettings);
      else LastWasNull := True;
    end;
end;


{ TZOleDBMSSQLCachedResolver }

{**
  Creates a OleDB specific cached resolver object.
}
constructor TZOleDBMSSQLCachedResolver.Create(Statement: IZStatement;
  Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  fStmt := TZOleDBPreparedStatement.Create(Statement.GetConnection, 'SELECT SCOPE_IDENTITY()', nil);
end;

destructor TZOleDBMSSQLCachedResolver.Destroy;
begin
  if fStmt <> nil then
    fStmt.Close;
  inherited Destroy;
end;
{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZOleDBMSSQLCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    FResultSet := fStmt.ExecuteQueryPrepared;
    if Assigned(FResultSet) and FResultSet.Next then
      NewRowAccessor.SetLong(FAutoColumnIndex, FResultSet.GetLong(FAutoColumnIndex));
  end;
end;

{ TZOleDBCLOB }
constructor TZOleDBCLOB.Create(RowSet: IRowSet; Accessor: HACCESSOR; wType: DBTYPE;
  CurrentRow: HROW; ChunkSize: Integer; const ConSettings: PZConSettings);
var
  IStream: ISequentialStream;
  pcbRead: LongInt;
begin
  inherited Create;
  FConSettings := ConSettings;

  if wType = DBTYPE_STR then
    FCurrentCodePage := ConSettings^.ClientCodePage^.CP
  else
    FCurrentCodePage := zCP_UTF16;
  OleDBCheck(RowSet.GetData(CurrentRow, Accessor, @IStream));
  try
    GetMem(FBlobData, ChunkSize);
    FBlobSize := ChunkSize;
    repeat
      IStream.Read(PAnsiChar(FBlobData)+FBlobSize-ChunkSize, ChunkSize, @pcbRead); //move chunks to buffer
      if pcbRead < ChunkSize then
        FBlobSize := FBlobSize - ChunkSize + pcbRead + 1+Ord(wType <> DBTYPE_STR) //reserve space for trailing #0
      else
        Inc(FBlobSize, ChunkSize);
      ReallocMem(FBlobData, FBlobSize);
    until pcbRead < ChunkSize;
    if wType = DBTYPE_STR then
      (PAnsiChar(FBlobData)+FBlobSize-1)^ := #0
    else
      (PWideChar(FBlobData)+(FBlobSize shr 1)-1)^ := #0
  finally
    IStream := nil;
    RowSet := nil;
  end;
end;

{ TZOleDBBLOB }
constructor TZOleDBBLOB.Create(RowSet: IRowSet; Accessor: HACCESSOR;
  CurrentRow: HROW; ChunkSize: Integer);
var
  IStream: ISequentialStream;
  pcbRead: LongInt;
begin
  inherited Create;
  OleDBCheck(RowSet.GetData(CurrentRow, Accessor, @IStream));
  try
    GetMem(FBlobData, ChunkSize);
    FBlobSize := ChunkSize;
    repeat
      IStream.Read(PAnsiChar(FBlobData)+FBlobSize-ChunkSize, ChunkSize, @pcbRead); //move chunks to buffer
      if pcbRead < ChunkSize then
        FBlobSize := FBlobSize - ChunkSize + pcbRead
      else
        Inc(FBlobSize, ChunkSize);
      ReallocMem(FBlobData, FBlobSize);
    until pcbRead < ChunkSize;
  finally
    IStream := nil;
    RowSet := nil;
  end;
end;

function GetCurrentResultSet(RowSet: IRowSet; Statement: IZStatement;
  Const SQL: String; ConSettings: PZConSettings; BuffSize, ChunkSize: Integer;
  EnhancedColInfo, InMemoryDataLobs: Boolean; var PCurrRS: Pointer): IZResultSet;
var
  CachedResolver: IZCachedResolver;
  NativeResultSet: TZOleDBResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Result := nil;
  if Assigned(RowSet) then
  begin
    NativeResultSet := TZOleDBResultSet.Create(Statement, SQL, RowSet,
      BuffSize, ChunkSize, InMemoryDataLobs, EnhancedColInfo);
    if (Statement.GetResultSetConcurrency = rcUpdatable) or
       (Statement.GetResultSetType <> rtForwardOnly) then
    begin
      if Statement.GetConnection.GetServerProvider = spMSSQL then
        CachedResolver := TZOleDBMSSQLCachedResolver.Create(Statement, NativeResultSet.GetMetaData)
      else
        CachedResolver := TZGenericCachedResolver.Create(Statement, NativeResultSet.GetMetaData);
      CachedResultSet := TZOleDBCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(Statement.GetResultSetConcurrency);
      Result := CachedResultSet;
    end
    else
      Result := NativeResultSet;
  end;
  PCurrRS := Pointer(Result);
end;

{ TZOleDBCachedResultSet }

constructor TZOleDBCachedResultSet.Create(ResultSet: TZOleDBResultSet;
  const SQL: string; Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet, SQL, Resolver, ConSettings);
  FResultSet := ResultSet;
end;

function TZOleDBCachedResultSet.Fetch: Boolean;
var
  I: Integer;
  TempRow: PZRowBuffer;
  DBBINDING: PDBBINDING;
  FData: PPointer;
  FLength: PDBLENGTH;
  Len: NativeUInt;
begin
  if Assigned(FResultSet) then
    Result := FResultSet.Next
  else
    Result := False;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
    Exit;

  TempRow := RowAccessor.RowBuffer;
  FData := @FResultSet.FData;
  FLength := @FResultSet.FLength;
  RowAccessor.Alloc;
  RowAccessor.RowBuffer.Index := GetNextRowIndex;
  RowAccessor.RowBuffer.UpdateType := utUnmodified;
  try
    for I := FirstDbcIndex to {$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FResultSet.FDBBindingArray) do
      if FResultSet.IsNull(I) then
        RowAccessor.SetNull(I)
      else begin
        DBBINDING := @FResultSet.FDBBindingArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
        case DBBINDING.wType of
          DBTYPE_EMPTY,
          DBTYPE_NULL                 : RowAccessor.SetNull(I);
          DBTYPE_I2                   : RowAccessor.SetSmall(I, PSmallInt(FData^)^);
          DBTYPE_I4,
          DBTYPE_ERROR,
          DBTYPE_HCHAPTER             : RowAccessor.SetInt(I, PInteger(FData^)^);
          DBTYPE_R4	                  : RowAccessor.SetFloat(I, PSingle(FData^)^);
          DBTYPE_R8                   : RowAccessor.SetDouble(I, PDouble(FData^)^);
          DBTYPE_CY                   : RowAccessor.SetCurrency(I, PCurrency(FData^)^);
          DBTYPE_DATE                 : RowAccessor.SetTimeStamp(I, PDateTime(FData^)^);
          DBTYPE_BSTR,
          DBTYPE_WSTR                 : if DBBINDING.cbMaxLen = 0 then
                                          RowAccessor.SetBlob(I, TZOleDBCLOB.Create(FResultSet.FRowSet,
                                            FResultSet.FLobAccessors[DBBINDING.obLength],
                                            DBTYPE_WSTR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
                                            FResultSet.FChunkSize, ConSettings))
                                        else begin
                                          Len := FLength^ shr 1;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            while (PWideChar(FData^)+Len-1)^ = ' ' do Dec(Len);
                                          RowAccessor.SetPWideChar(I, FData^, @Len);
                                        end;
          DBTYPE_BSTR or DBTYPE_BYREF,
          DBTYPE_WSTR or DBTYPE_BYREF : begin
                                          Len := FLength^ shr 1;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            while (ZPPWideChar(FData^)^+Len-1)^ = ' ' do Dec(Len);
                                          RowAccessor.SetPWideChar(I, ZPPWideChar(FData^)^, @Len);
                                        end;
          //DBTYPE_IDISPATCH = 9;
          DBTYPE_BOOL                 : RowAccessor.SetBoolean(I, PWordBool(FData^)^);
          DBTYPE_VARIANT              : case RowAccessor.GetColumnType(I) of
                                          stBoolean: RowAccessor.SetBoolean(I, POleVariant(FData^)^);
                                          stByte        : RowAccessor.SetByte(I, POleVariant(FData^)^);
                                          stShort       : RowAccessor.SetShort(I, POleVariant(FData^)^);
                                          stWord        : RowAccessor.SetWord(I, POleVariant(FData^)^);
                                          stSmall       : RowAccessor.SetSmall(I, POleVariant(FData^)^);
                                          stLongWord    : RowAccessor.SetUInt(I, POleVariant(FData^)^);
                                          stInteger     : RowAccessor.SetInt(I, POleVariant(FData^)^);
                                          stULong       : RowAccessor.SetULong(I, POleVariant(FData^)^);
                                          stLong        : RowAccessor.SetLong(I, POleVariant(FData^)^);
                                          stFloat       : RowAccessor.SetFloat(I, POleVariant(FData^)^);
                                          stDouble      : RowAccessor.SetDouble(I, POleVariant(FData^)^);
                                          stCurrency    : RowAccessor.SetCurrency(I, POleVariant(FData^)^);
                                          stBigDecimal  : RowAccessor.SetBigDecimal(I, POleVariant(FData^)^);
                                          {stDate, stTime, stTimestamp,
                                          stGUID,
                                          //now varying size types in equal order
                                          stString, stUnicodeString, stBytes,
                                          stAsciiStream, stUnicodeStream, stBinaryStream,
                                          //finally the object types
                                          stArray, stDataSet}
                                        end;
          //DBTYPE_IUNKNOWN = 13;
          //DBTYPE_DECIMAL = 14;
          DBTYPE_UI1                  : RowAccessor.SetByte(I, PByte(FData^)^);
          DBTYPE_I1                   : RowAccessor.SetShort(I, PShortInt(FData^)^);
          DBTYPE_UI2                  : RowAccessor.SetWord(I, PWord(FData^)^);
          DBTYPE_UI4                  : RowAccessor.SetUInt(I, PLongWord(FData^)^);
          DBTYPE_I8                   : RowAccessor.SetLong(I, PInt64(FData^)^);
          DBTYPE_UI8                  : RowAccessor.SetULong(I, PInt64(FData^)^);
          DBTYPE_GUID                 : RowAccessor.SetBytes(I, FData^, 16);
          DBTYPE_BYTES                : if DBBINDING.cbMaxLen = 0 then
                                          RowAccessor.SetBlob(I, TZOleDBBLOB.Create(FResultSet.FRowSet,
                                            FResultSet.FLobAccessors[DBBINDING.obLength],
                                            FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo], FResultSet.FChunkSize))
                                        else
                                          RowAccessor.SetBytes(I, FData^, FLength^);
          DBTYPE_STR                  : if DBBINDING.cbMaxLen = 0 then
                                          RowAccessor.SetBlob(I, TZOleDBCLOB.Create(FResultSet.FRowSet,
                                            FResultSet.FLobAccessors[DBBINDING.obLength],
                                            DBTYPE_STR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
                                            FResultSet.FChunkSize, ConSettings))
                                        else begin
                                          Len := FLength^;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            while (PAnsiChar(FData^)+Len-1)^ = ' ' do Dec(Len);
                                          FUniTemp := PRawToUnicode(FData^, Len, ConSettings^.ClientCodePage^.CP);
                                          RowAccessor.SetPWideChar(I, Pointer(FUniTemp), @Len);
                                        end;
          DBTYPE_STR or DBTYPE_BYREF  : begin
                                          Len := FLength^;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            while (PPAnsiChar(FData^)^+Len-1)^ = ' ' do Dec(Len);
                                          FUniTemp := PRawToUnicode(PPAnsiChar(FData^)^, Len, ConSettings^.ClientCodePage^.CP);
                                          RowAccessor.SetPWideChar(I, Pointer(FUniTemp), @Len);
                                        end;
          //DBTYPE_NUMERIC = 131;
          //DBTYPE_UDT = 132;
          DBTYPE_DBDATE               : RowAccessor.SetDate(I, EncodeDate(Abs(PDBDate(FData^)^.year), PDBDate(FData^)^.month, PDBDate(FData^)^.day));
          DBTYPE_DBTIME               : RowAccessor.SetTime(I, EncodeTime(PDBTime(FData^)^.hour, PDBTime(FData^)^.minute, PDBTime(FData^)^.second, 0));
          DBTYPE_DBTIMESTAMP          : RowAccessor.SetTimestamp(I, EncodeDate(Abs(PDBTimeStamp(FData^)^.year), PDBTimeStamp(FData^)^.month, PDBTimeStamp(FData^)^.day)
                                                                   +EncodeTime(PDBTimeStamp(FData^)^.hour, PDBTimeStamp(FData^)^.minute, PDBTimeStamp(FData^)^.second, PDBTimeStamp(FData^)^.fraction div 1000000));
          {SQL Server types only }
          DBTYPE_XML                  : RowAccessor.SetBlob(I, TZOleDBCLOB.Create(FResultSet.FRowSet,
                                            FResultSet.FLobAccessors[DBBINDING.obLength],
                                            DBTYPE_WSTR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
                                            FResultSet.FChunkSize, ConSettings));

          //DBTYPE_TABLE = 143; // introduced in SQL 2008
          DBTYPE_DBTIME2              : RowAccessor.SetTime(I, EncodeTime(PDBTime2(FData^)^.hour, PDBTime2(FData^)^.minute, PDBTime2(FData^)^.second, PDBTime2(FData^)^.fraction div 1000000));
          //DBTYPE_DBTIMESTAMPOFFSET = 146; // introduced in SQL 2008
          //DBTYPE_FILETIME = 64;
          //DBTYPE_PROPVARIANT = 138;
          //DBTYPE_VARNUMERIC = 139;

        end;
      end;
      RowsList.Add(RowAccessor.RowBuffer);
      LastRowNo := RowsList.Count;
    finally
      RowAccessor.RowBuffer := TempRow;
    end;
end;

initialization
  {init some reusable records (: }
  LobReadObj.dwFlags := STGM_READ;
  LobReadObj.iid := IID_ISequentialStream;

  LobDBBinding.obValue := 0;
  LobDBBinding.pTypeInfo := nil;
  LobDBBinding.pObject := @LobReadObj;
  LobDBBinding.pBindExt := nil;
  LobDBBinding.dwPart := DBPART_VALUE; //null is indicated by "generic" Accessor, Length isn't required we read the stream!
  LobDBBinding.dwMemOwner := DBMEMOWNER_CLIENTOWNED;
  LobDBBinding.eParamIO := DBPARAMIO_NOTPARAM;
  LobDBBinding.cbMaxLen := 0;
  LobDBBinding.dwFlags := DBCOLUMNFLAGS_ISLONG;
  LobDBBinding.wType := DBTYPE_IUNKNOWN;
  LobDBBinding.bPrecision := 0;
  LobDBBinding.bScale := 0;
//(*
{$ELSE !ENABLE_OLEDB}
implementation
{$ENDIF ENABLE_OLEDB}
//*)
end.




