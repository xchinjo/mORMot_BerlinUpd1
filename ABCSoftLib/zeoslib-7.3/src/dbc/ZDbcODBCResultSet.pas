{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes           }
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

unit ZDbcODBCResultSet;

interface

{$I ZDbc.inc}

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF OLD_FPC}ZClasses, {$ENDIF}ZSysUtils, ZDbcIntfs,
  ZCompatibility, ZDbcResultSet, ZFastCode, ZDbcResultsetMetadata, ZDbcStatement,
  ZPlainODBCDriver, ZDbcODBCCon;

type
  { eh: improve missing meta informations of SQLColumns}
  TODBCTResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  TAbstractODBCResultSet = Class(TZAbstractResultSet)
  private
    fPHSTMT: PSQLHSTMT; //direct reference the handle of the smt/metadata
    fConnection: IZODBCConnection;
    fPlainDriver: IODBC3BasePlainDriver;
    fZBufferSize, fChunkSize: Integer;
    fEnhancedColInfo: Boolean;
    fColumnCount: SQLSMALLINT;
    fMaxFetchableRows, fFetchedRowCount, fCurrentBufRowNo: SQLULEN;
    fColumnBuffers: array of TByteDynArray;
    fColumnBuffSizes: array of NativeUInt;
    fStrLen_or_Ind: SQLLEN;
    fColDataPtr: Pointer;
    fSQLTypes: TZSQLTypeArray;
    fIsUnicodeDriver: Boolean;
    fFreeHandle, fCursorOpened: Boolean;
    fFixedWidthStrings, fBoundColumns: TBooleanDynArray;
    fODBC_CTypes: array of SQLSMALLINT;
    fRowBlobs: array of IZBlob; //row wise storage of unbound lobs
    fSQL_GETDATA_EXTENSIONS: SQLUINTEGER;
    fFirstGetDataIndex: Integer;
    procedure LoadUnBoundColumns;
  protected
    procedure CheckStmtError(RETCODE: SQLRETURN);
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; virtual; abstract;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; virtual; abstract;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZColumnInfo); virtual; abstract;
    procedure InternalDecTrailingSpaces(ColumnIndex: SQLUSMALLINT); virtual; abstract;
    function InternalGetRaw(ColumnIndex: SQLUSMALLINT; CodePage: Word): RawByteString; virtual; abstract;
    function InternalGetUnicode(ColumnIndex: SQLUSMALLINT): ZWideString; virtual; abstract;
  public
    constructor Create(Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); virtual;
    constructor CreateForMetadataCall(var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; Connection: IZODBCConnection); virtual;
    procedure Open; override;
    procedure Close; override;

    function Next: Boolean; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetRawByteString(ColumnIndex: Integer): RawByteString; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetWord(ColumnIndex: Integer): Word; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetUInt(ColumnIndex: Integer): LongWord; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
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
  End;

  TODBCResultSetW = class(TAbstractODBCResultSet)
  private
    fPlainW: IODBC3UnicodePlainDriver;
  protected
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZColumnInfo); override;
    procedure InternalDecTrailingSpaces(ColumnIndex: SQLUSMALLINT); override;
    function InternalGetRaw(ColumnIndex: SQLUSMALLINT; CodePage: Word): RawByteString; override;
    function InternalGetUnicode(ColumnIndex: SQLUSMALLINT): ZWideString; override;
  public
    constructor Create(Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); override;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; override;
  end;

  TODBCResultSetA = class(TAbstractODBCResultSet)
  private
    fPlainA: IODBC3RawPlainDriver;
  protected
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZColumnInfo); override;
    procedure InternalDecTrailingSpaces(ColumnIndex: SQLUSMALLINT); override;
    function InternalGetRaw(ColumnIndex: SQLUSMALLINT; CodePage: Word): RawByteString; override;
    function InternalGetUnicode(ColumnIndex: SQLUSMALLINT): ZWideString; override;
  public
    constructor Create(Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
  end;

  TZODBCBlob = class(TZAbstractBlob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; PlainDriver: IODBC3BasePlainDriver);
  end;

  TZODBCClobA = class(TZAbstractCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; PlainDriver: IODBC3BasePlainDriver;
      ConSettings: PZConSettings);
  end;

  TZODBCClobW = class(TZAbstractCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; PlainDriver: IODBC3BasePlainDriver;
      ConSettings: PZConSettings);
  end;

const
  StringStreamTypes: Array[Boolean] of SQLSMALLINT = (SQL_C_CHAR, SQL_C_WCHAR);

implementation

uses Math,
  ZMessages, ZDbcODBCUtils, ZEncoding, ZDbcODBCStatement;

{ TAbstractODBCResultSet }

procedure TAbstractODBCResultSet.CheckStmtError(RETCODE: SQLRETURN);
begin
  CheckODBCError(RETCODE, fPHSTMT^, SQL_HANDLE_STMT, fConnection);
end;

procedure TAbstractODBCResultSet.Close;
var RETCODE: SQLRETURN;
begin
  if not Closed then begin
    if Assigned(fPHSTMT^) then
      if fFreeHandle then begin // from metadata
        CheckStmtError(fPlainDriver.FreeHandle(SQL_HANDLE_STMT, fPHSTMT^)); //free handle
        fPHSTMT^ := nil;
      end else
        if Assigned(Statement) and ((Statement as IZODBCStatement).GetMoreResultsIndicator = mriUnknown) then begin
          ResetCursor;
          CheckStmtError(fPlainDriver.FreeStmt(fPHSTMT^,SQL_UNBIND)); //discart bindings
          RETCODE := fPlainDriver.MoreResults(fPHSTMT^);
          if RETCODE = SQL_SUCCESS then
            (Statement as IZODBCStatement).SetMoreResultsIndicator(mriHasMoreResults)
          else if RETCODE = SQL_NO_DATA then
            (Statement as IZODBCStatement).SetMoreResultsIndicator(mriHasNoMoreResults)
          else CheckStmtError(RETCODE);
        end else
          CheckStmtError(fPlainDriver.FreeStmt(fPHSTMT^,SQL_UNBIND)); //discart bindings
    inherited Close;
    RowNo := LastRowNo + 1; //suppress a possible fetch approach
  end;
end;

{$IFDEF USE_SYNCOMMONS}
procedure TAbstractODBCResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  EndJSONObject: Boolean; With_DATETIME_MAGIC: Boolean; SkipNullFields: Boolean);
var C, H, I: Integer;
    P: Pointer;
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
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case fSQLTypes[C] of
        stBoolean:    JSONWriter.AddShort(JSONBool[PWordBool(PByte(fColDataPtr)^ <> 0)^]);
        stByte:       JSONWriter.AddU(PByte(fColDataPtr)^);
        stShort:      JSONWriter.Add(PShortInt(fColDataPtr)^);
        stWord:       JSONWriter.AddU(PWord(fColDataPtr)^);
        stSmall:      JSONWriter.Add(PSmallInt(fColDataPtr)^);
        stLongWord:   JSONWriter.AddU(PLongWord(fColDataPtr)^);
        stInteger:    JSONWriter.Add(PInteger(fColDataPtr)^);
        stULong:      JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PUInt64(fColDataPtr)^));
        stLong:       JSONWriter.Add(PInt64(fColDataPtr)^);
        stFloat:      JSONWriter.AddSingle(PSingle(fColDataPtr)^);
        stDouble,
        stCurrency,
        stBigDecimal: JSONWriter.AddDouble(PDouble(fColDataPtr)^);
        stBytes:      JSONWriter.WrBase64(fColDataPtr,fStrLen_or_Ind,True);
        stGUID:       begin
                        JSONWriter.Add('"');
                        JSONWriter.Add(PGUID(fColDataPtr)^);
                        JSONWriter.Add('"');
                      end;
        stTime:       begin
                        JSONWriter.Add('"');
                        if fODBC_CTypes[C] = SQL_C_BINARY then
                          JSONWriter.AddDateTime(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                            PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                        else
                          JSONWriter.AddDateTime(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                            PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
                        JSONWriter.Add('"');
                      end;
        stDate:       begin
                        JSONWriter.Add('"');
                        JSONWriter.AddDateTime(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
                        JSONWriter.Add('"');
                      end;
        stTimeStamp:  begin
                        JSONWriter.Add('"');
                        JSONWriter.AddDateTime(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
                          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute,
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
                        JSONWriter.Add('"');
                      end;
        stString, stUnicodeString: begin
            JSONWriter.Add('"');
            if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then begin
              if fFixedWidthStrings[c] then
                while (PWideChar(fColDataPtr)+(fStrLen_or_Ind shr 1)-1)^ = ' ' do Dec(fStrLen_or_Ind, 2);
              JSONWriter.AddJSONEscapeW(fColDataPtr, fStrLen_or_Ind shr 1)
            end else begin
              if fFixedWidthStrings[c] then
                while (PAnsiChar(fColDataPtr)+(fStrLen_or_Ind)-1)^ = ' ' do Dec(fStrLen_or_Ind);
              if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
                JSONWriter.AddJSONEscape(fColDataPtr, fStrLen_or_Ind)
              else begin
                FUniTemp := PRawToUnicode(fColDataPtr, fStrLen_or_Ind, ConSettings^.ClientCodePage^.CP);
                JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
              end;
            end;
            JSONWriter.Add('"');
          end;
        stAsciiStream, stUnicodeStream: begin
            JSONWriter.Add('"');
            if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or (ConSettings^.ClientCodePage^.CP <> zCP_UTF8) then begin
              P := fRowBlobs[C].GetPWideChar;
              JSONWriter.AddJSONEscapeW(P, fRowBlobs[C].Length shr 1);
            end else begin
              P := fRowBlobs[C].GetPAnsiChar(zCP_UTF8);
              JSONWriter.AddJSONEscape(P, fRowBlobs[C].Length);
            end;
            JSONWriter.Add('"');
          end;
        stBinaryStream:
          JSONWriter.WrBase64(fRowBlobs[C].GetBuffer, fRowBlobs[C].Length, True);
        else //stArray, stDataSet:
          ;
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

constructor TAbstractODBCResultSet.Create(Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
  ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True);
var Supported: SQLUSMALLINT;
begin
  inherited Create(Statement, SQL, TODBCTResultSetMetadata.Create(Connection.GetMetadata, SQL, Self), Connection.GetConSettings);
  fConnection := Connection;
  fPlainDriver := fConnection.GetPlainDriver;
  fIsUnicodeDriver := Supports(fPlainDriver, IODBC3UnicodePlainDriver);
  fPHSTMT := @StmtHandle;
  fZBufferSize := ZBufferSize;
  fChunkSize := ChunkSize;
  fConnection.CheckDbcError(fPLainDriver.GetFunctions(ConnectionHandle, SQL_API_SQLCOLATTRIBUTE, @Supported));
  fEnhancedColInfo := EnhancedColInfo and (Supported = SQL_TRUE);
  fCurrentBufRowNo := 0;
  fFreeHandle := not Assigned(StmtHandle);
  Connection.CheckDbcError(fPlainDriver.GetInfo(ConnectionHandle,
    SQL_GETDATA_EXTENSIONS, @fSQL_GETDATA_EXTENSIONS, SizeOf(SQLUINTEGER), nil));
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  fCursorOpened := True;
  Open;
end;

constructor TAbstractODBCResultSet.CreateForMetadataCall(
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; Connection: IZODBCConnection);
begin
  StmtHandle := nil;
  Create(nil, StmtHandle, ConnectionHandle, '', Connection,
    {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Connection.GetParameters.Values['internal_buffer_size'], 131072), //by default 128KB
    {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Connection.GetParameters.Values['chunk_size'], 4096), False);
  Connection.CheckDbcError(fPlainDriver.AllocHandle(SQL_HANDLE_STMT, ConnectionHandle, StmtHandle));
end;

function TAbstractODBCResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
  if Ord(fSQLTypes[ColumnIndex]) < Ord(stString) then
    Result := GetRawByteString(ColumnIndex)
  else
    if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
      Result := ''
    else begin
      {$IFNDEF GENERIC_INDEX}
      ColumnIndex := ColumnIndex-1;
      {$ENDIF}
      case fSQLTypes[ColumnIndex] of
        stString, stUnicodeString:
          Result := InternalGetRaw(ColumnIndex, ZOSCodePage);
        stAsciiStream, stUnicodeStream:
          Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetAnsiString;
        stBinaryStream:
          Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString;
        else {stArray, stDataSet:} Result := '';
      end;
    end;
end;

function TAbstractODBCResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := GetDouble(ColumnIndex);
end;

function TAbstractODBCResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
  Result := nil;
  if not IsNull(ColumnIndex) then begin //loads the lob on demand -> a second call is impossible
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    if fSQLTypes[ColumnIndex] in [stBinaryStream, stAsciiStream, stUnicodeStream] then
        Result := fRowBlobs[ColumnIndex]
    else Result := TZAbstractBlob.Create;
  end;
end;

function TAbstractODBCResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  Result := False;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^ <> 0;
      stByte:       Result := PByte(fColDataPtr)^ <> 0;
      stShort:      Result := PShortInt(fColDataPtr)^ <> 0;
      stWord:       Result := PWord(fColDataPtr)^ <> 0;
      stSmall:      Result := PSmallInt(fColDataPtr)^ <> 0;
      stLongWord:   Result := PLongWord(fColDataPtr)^ <> 0;
      stInteger:    Result := PInteger(fColDataPtr)^ <> 0;
      stULong:      Result := PUInt64(fColDataPtr)^ <> 0;
      stLong:       Result := PInt64(fColDataPtr)^ <> 0;
      stFloat:      Result := PSingle(fColDataPtr)^ <> 0;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^ <> 0;
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000) <> 0
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0) <> 0;
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day) <> 0;
      stTimeStamp:
        Result := (EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day) <> 0) and
          (EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction) <> 0);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := StrToBoolEx(PWideChar(fColDataPtr), True, fFixedWidthStrings[ColumnIndex])
          else
            Result := StrToBoolEx(PAnsiChar(fColDataPtr), True, fFixedWidthStrings[ColumnIndex])
      end;
      //stAsciiStream, stUnicodeStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToIntDef(PWideChar(fColDataPtr), 0)
          else
            Result := RawToIntDef(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  if IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := nil
  else if fColDataPtr = nil then //streamed data
    Result := nil
  else begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    SetLength(Result, fStrLen_or_Ind shl Ord((fSQLTypes[ColumnIndex] in [stString, stUnicodeString]) and fIsUnicodeDriver));
    System.Move(fColDataPtr^, Pointer(Result)^, Length(Result));
  end;

end;

function TAbstractODBCResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeSQLDateToDateTime(fColDataPtr, fStrLen_or_Ind shr 1, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := RawSQLDateToDateTime(fColDataPtr, fStrLen_or_Ind, ConSettings^.ReadFormatSettings, Failed);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            SQLStrToFloatDef(PWideChar(fColDataPtr), 0, Result, fStrLen_or_Ind)
          else
            SQLStrToFloatDef(PAnsiChar(fColDataPtr), 0, Result, fStrLen_or_Ind)
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            SQLStrToFloatDef(PWideChar(fColDataPtr), 0, Result, fStrLen_or_Ind)
          else
            SQLStrToFloatDef(PAnsiChar(fColDataPtr), 0, Result, fStrLen_or_Ind)
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToIntDef(PWideChar(fColDataPtr), 0)
          else
            Result := RawToIntDef(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToInt64Def(PWideChar(fColDataPtr), 0)
          else
            Result := RawToInt64Def(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetUInt(ColumnIndex: Integer): LongWord;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToUInt64Def(PWideChar(fColDataPtr), 0)
          else
            Result := RawToUInt64Def(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetRawByteString(
  ColumnIndex: Integer): RawByteString;
begin
  if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := ''
  else begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := BoolToRawEx(PByte(fColDataPtr)^ <> 0);
      stByte:       Result := IntToRaw(PByte(fColDataPtr)^);
      stShort:      Result := IntToRaw(PShortInt(fColDataPtr)^);
      stWord:       Result := IntToRaw(PWord(fColDataPtr)^);
      stSmall:      Result := IntToRaw(PSmallInt(fColDataPtr)^);
      stLongWord:   Result := IntToRaw(PLongWord(fColDataPtr)^);
      stInteger:    Result := IntToRaw(PInteger(fColDataPtr)^);
      stULong:      Result := IntToRaw(PUInt64(fColDataPtr)^);
      stLong:       Result := IntToRaw(PInt64(fColDataPtr)^);
      stFloat:      Result := FloatToSQLRaw(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := FloatToSQLRaw(PDouble(fColDataPtr)^);
      stBytes:      SetString(Result, PAnsiChar(fColDataPtr), fStrLen_or_Ind);
      stGUID:       Result := GUIDToRaw(PGUID(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      DateTimeToRawSQLTime(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000), ConSettings.DisplayFormatSettings, False)
                    else
                      DateTimeToRawSQLTime(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0),ConSettings.DisplayFormatSettings, False);
      stDate:
        Result := DateTimeToRawSQLDate(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day),
            ConSettings.DisplayFormatSettings, False);
      stTimeStamp:
        Result := DateTimeToRawSQLTimeStamp(
          EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction), ConSettings.DisplayFormatSettings, False);
      stString, stUnicodeString:
        Result := InternalGetRaw(ColumnIndex, ZOSCodePage);
      stAsciiStream, stUnicodeStream, stBinaryStream:
        Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString;
      else {stArray, stDataSet:} Result := '';
    end;
  end;
end;

function TAbstractODBCResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToIntDef(PWideChar(fColDataPtr), 0)
          else
            Result := RawToIntDef(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToIntDef(PWideChar(fColDataPtr), 0)
          else
            Result := RawToIntDef(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetString(ColumnIndex: Integer): String;
begin
  {$IFDEF UNICODE}
  Result := GetUnicodeString(ColumnIndex);
  {$ELSE}
  Result := GetRawByteString(ColumnIndex);
  {$ENDIF}
end;

function TAbstractODBCResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stTimeStamp:
        Result := EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction div 1000000);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeSQLTimeToDateTime(fColDataPtr, fStrLen_or_Ind shr 1, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := RawSQLTimeToDateTime(fColDataPtr, fStrLen_or_Ind, ConSettings^.ReadFormatSettings, Failed);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stDouble,
      stCurrency,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction div 1000000);
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeSQLTimeStampToDateTime(fColDataPtr, fStrLen_or_Ind shr 1, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := RawSQLTimeStampToDateTime(fColDataPtr, fStrLen_or_Ind, ConSettings^.ReadFormatSettings, Failed);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToUInt64Def(PWideChar(fColDataPtr), 0)
          else
            Result := RawToUInt64Def(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.GetUnicodeString(
  ColumnIndex: Integer): ZWideString;
begin
  if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := ''
  else begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := BoolToUnicodeEx(PByte(fColDataPtr)^ <> 0);
      stByte:       Result := IntToUnicode(PByte(fColDataPtr)^);
      stShort:      Result := IntToUnicode(PShortInt(fColDataPtr)^);
      stWord:       Result := IntToUnicode(PWord(fColDataPtr)^);
      stSmall:      Result := IntToUnicode(PSmallInt(fColDataPtr)^);
      stLongWord:   Result := IntToUnicode(PLongWord(fColDataPtr)^);
      stInteger:    Result := IntToUnicode(PInteger(fColDataPtr)^);
      stULong:      Result := IntToUnicode(PUInt64(fColDataPtr)^);
      stLong:       Result := IntToUnicode(PInt64(fColDataPtr)^);
      stFloat:      Result := FloatToSQLUnicode(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := FloatToSQLUnicode(PDouble(fColDataPtr)^);
      stBytes: begin
          SetString(fRawTemp, PAnsiChar(fColDataPtr), fStrLen_or_Ind);
          Result := ASCII7ToUnicodeString(Pointer(fRawTemp), fStrLen_or_Ind);
        end;
      stGUID:       Result := GUIDToUnicode(PGUID(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      DateTimeToUnicodeSQLTime(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000), ConSettings.DisplayFormatSettings, False)
                    else
                      DateTimeToUnicodeSQLTime(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0),ConSettings.DisplayFormatSettings, False);
      stDate:
        Result := DateTimeToUnicodeSQLDate(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day),
            ConSettings.DisplayFormatSettings, False);
      stTimeStamp:
        Result := DateTimeToUnicodeSQLTimeStamp(
          EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction), ConSettings.DisplayFormatSettings, False);
      stString, stUnicodeString:
        Result := InternalGetUnicode(ColumnIndex);
      stAsciiStream, stUnicodeStream:
        Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetUnicodeString;
      else //stBinaryStream, stArray, stDataSet:
        Result := '';
    end;
  end;
end;

function TAbstractODBCResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
begin
  if Ord(fSQLTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) < Ord(stString) then
    Result := GetRawByteString(ColumnIndex)
  else
    if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
      Result := ''
    else begin
      {$IFNDEF GENERIC_INDEX}
      ColumnIndex := ColumnIndex-1;
      {$ENDIF}
      case fSQLTypes[ColumnIndex] of
        stString, stUnicodeString:
          Result := InternalGetRaw(ColumnIndex, zCP_UTF8);
        stAsciiStream, stUnicodeStream:
          Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetUTF8String;
        stBinaryStream:
          Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString;
        else {stArray, stDataSet:} Result := '';
      end;
    end;
end;

function TAbstractODBCResultSet.GetWord(ColumnIndex: Integer): Word;
begin
  Result := 0;
  if not IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex-1;
    {$ENDIF}
    case fSQLTypes[ColumnIndex] of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PLongWord(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stDouble,
      stCurrency,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if fODBC_CTypes[ColumnIndex] = SQL_C_BINARY then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: begin
          InternalDecTrailingSpaces(ColumnIndex);
          if fIsUnicodeDriver then
            Result := UnicodeToIntDef(PWideChar(fColDataPtr), 0)
          else
            Result := RawToIntDef(PAnsiChar(fColDataPtr), 0);
        end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
    end;
  end;
end;

function TAbstractODBCResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Assert((ColumnIndex >= FirstDbcIndex) and (ColumnIndex{$IFDEF GENERIC_INDEX}<{$ELSE}<={$ENDIF} fColumnCount), SColumnIsNotAccessable);
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  fStrLen_or_Ind := PStrLen_or_IndArray(Pointer(fColumnBuffers[ColumnIndex]))^[fCurrentBufRowNo-1];
  Result := fStrLen_or_Ind = SQL_NULL_DATA;
  fColDataPtr := nil;
  if (Ord(fSQLTypes[ColumnIndex]) <= Ord(stUnicodeString)) and (not Result) then
    fColDataPtr := { Start entry }            @fColumnBuffers[ColumnIndex][
      { increase by size of indicator array } (SizeOf(SQLLEN)*fMaxFetchableRows){%H-}+
      { get entry of Data in current row }    (fColumnBuffSizes[ColumnIndex]*(fCurrentBufRowNo-1))];
  LastWasNull := Result;
end;

procedure TAbstractODBCResultSet.LoadUnBoundColumns;
var
  ColumnIndex: Integer;
  StrLen_or_IndPtr: PSQLLEN;
begin
  for ColumnIndex := fFirstGetDataIndex to fColumnCount-1 do begin
    StrLen_or_IndPtr := Pointer(fColumnBuffers[ColumnIndex]);
    if not fBoundColumns[ColumnIndex] then //some drivers allow GetData in mixed order so check it!
      if Ord(fSQLTypes[ColumnIndex]) < Ord(stAsciiStream) then //move data to buffers
        CheckStmtError(fPlainDriver.GetData(fPHSTMT^, ColumnIndex+1, fODBC_CTypes[ColumnIndex],
          {%H-}Pointer({%H-}NativeUInt(StrLen_or_IndPtr)+SizeOf(SQLLEN)), fColumnBuffSizes[ColumnIndex], StrLen_or_IndPtr))
      else begin
        { check out length of lob }
        CheckStmtError(fPlainDriver.GetData(fPHSTMT^, ColumnIndex+1,
          fODBC_CTypes[ColumnIndex], Pointer(1){can not be nil}, 0, StrLen_or_IndPtr));
        { store the lob -> a second call to same column is impossible for some Drivers }
        if fSQLTypes[ColumnIndex] = stBinaryStream then
          fRowBlobs[ColumnIndex] := TZODBCBlob.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver)
        else if fSQLTypes[ColumnIndex] in [stAsciiStream, stUnicodeStream] then
          if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
            fRowBlobs[ColumnIndex] := TZODBCClobW.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver, ConSettings)
          else
            fRowBlobs[ColumnIndex] := TZODBCClobA.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver, ConSettings);
      end;
  end;
end;

function TAbstractODBCResultSet.Next: Boolean;
//const FetchOrientation: array[Boolean] of SQLSMALLINT = (SQL_FETCH_FIRST, SQL_FETCH_NEXT); //using FetchScroll or ExtendedFetch??
var RETCODE: SQLRETURN;
label Fail, FetchData;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (fPHSTMT^ = nil) then
    goto Fail;
  if (RowNo = 0) then begin//fetch Iteration count of rows
    if Closed then Open;
FetchData:
    fCursorOpened := True;
    fCurrentBufRowNo := 1;
    RETCODE := fPlainDriver.Fetch(fPHSTMT^);
    if fMaxFetchableRows > 1 then //block cursor mode
      case RetCode of
        SQL_NO_DATA: //SQL_NO_DATA is returned too if final block fetch is done too but less rows than demanded are fetched
          if fFetchedRowCount = 0 then //so check out the how many rows have been obtained
            goto Fail
          else
            LoadUnBoundColumns;
        SQL_INVALID_HANDLE: begin
            fPHSTMT^ := nil;
            inherited ResetCursor;
            goto fail;
          end;
        SQL_PARAM_DATA_AVAILABLE: ; //v3.8+
        else begin
            CheckStmtError(RETCODE);
            LoadUnBoundColumns;
          end;
      end
    else if RETCODE = SQL_NO_DATA then //single row fetch -> SQL_NO_DATA = end ow row set
      goto Fail
    else begin
      if (RETCODE <> SQL_PARAM_DATA_AVAILABLE) then
        CheckStmtError(RETCODE);
      fFetchedRowCount := 1;
      LoadUnBoundColumns;
    end;
  end else
    if FCurrentBufRowNo < fFetchedRowCount then
      Inc(FCurrentBufRowNo)
    else
      if (fMaxFetchableRows > 1) and (fFetchedRowCount <> fMaxFetchableRows ) then
        goto fail else
        goto FetchData;

  RowNo := RowNo + 1;
  if LastRowNo < RowNo then
    LastRowNo := RowNo;
  Result := True;
  Exit;
Fail:
  if RowNo <= LastRowNo then
    RowNo := LastRowNo + 1;
end;

procedure TAbstractODBCResultSet.Open;
var
  bufSQLLEN: SQLLEN;
  ColumnNumber: SQLUSMALLINT;
  ColumnInfo: TZColumnInfo;
  RowSize: Integer;
  LobsInResult: Boolean;
  StrBuf: TByteDynArray;
  function NoStreamedColFollows: Boolean;
  var I: Integer;
  begin
    Result := True;
    for i := High(fSQLTypes) downto low(fSQLTypes)+ColumnNumber do
      if ord(fSQLTypes[I]) <= Ord(stUnicodeString) then begin
        Result := False;
        Break;
      end;
  end;
begin
  if Closed and Assigned(fPHSTMT^) then begin
    CheckStmtError(fPlainDriver.NumResultCols(fPHSTMT^, @fColumnCount));
    if fColumnCount = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    RowSize := 0;
    LobsInResult := False;
    SetLength(fColumnBuffSizes, fColumnCount);
    SetLength(fSQLTypes, fColumnCount);
    SetLength(fColumnBuffers, fColumnCount);
    SetLength(fBoundColumns, fColumnCount);
    SetLength(fRowBlobs, fColumnCount);
    SetLength(fFixedWidthStrings, fColumnCount);
    SetLength(fODBC_CTypes, fColumnCount);
    SetLength(StrBuf, (Max(32,
      Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
        Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxSchemaNameLength,
          Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
            fConnection.GetMetaData.GetDataBaseInfo.GetMaxColumnNameLength))))+1) shl Ord(ConSettings^.ClientCodePage^.Encoding = ceUTF16));

    for ColumnNumber := 1 to fColumnCount do begin
      ColumnInfo := TZColumnInfo.Create;
      ColumnsInfo.Add(ColumnInfo); //add first -> memleaksave
      if fEnhancedColInfo then begin
        ColumnInfo.CharOctedLength := ColNumAttribute(ColumnNumber, SQL_DESC_OCTET_LENGTH);
        ColumnInfo.AutoIncrement := ColNumAttribute(ColumnNumber, SQL_DESC_AUTO_UNIQUE_VALUE) = SQL_TRUE;
        ColumnInfo.CaseSensitive := ColNumAttribute(ColumnNumber, SQL_DESC_CASE_SENSITIVE) = SQL_TRUE;
        ColumnInfo.ColumnDisplaySize := ColNumAttribute(ColumnNumber, SQL_DESC_DISPLAY_SIZE);
        bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_NULLABLE);
        if bufSQLLEN = SQL_NULLABLE then
          ColumnInfo.Nullable := ntNullable
        else if bufSQLLEN = SQL_NO_NULLS then
          ColumnInfo.Nullable := ntNoNulls
        else
          ColumnInfo.Nullable := ntNullableUnknown;
        ColumnInfo.Searchable := ColNumAttribute(ColumnNumber, SQL_DESC_SEARCHABLE) <> SQL_PRED_NONE;
        bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_UPDATABLE);
        ColumnInfo.ReadOnly := bufSQLLEN = SQL_ATTR_READONLY;
        ColumnInfo.Writable := bufSQLLEN <> SQL_ATTR_READONLY;
        ColumnInfo.DefinitelyWritable := bufSQLLEN = SQL_ATTR_WRITE;

        ColumnInfo.ColumnLabel := ColStrAttribute(ColumnNumber, SQL_DESC_LABEL, StrBuf);
        ColumnInfo.ColumnName := ColStrAttribute(ColumnNumber, SQL_DESC_BASE_COLUMN_NAME, StrBuf);
        if ColumnInfo.ColumnName = '' then
          ColumnInfo.ColumnName := ColStrAttribute(ColumnNumber, SQL_DESC_NAME, StrBuf);
        if ColumnInfo.ColumnName = '' then //aggregates like SUM() don't have a columname -> skip processing
          ColumnInfo.ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
        else begin
          ColumnInfo.TableName := ColStrAttribute(ColumnNumber, SQL_DESC_BASE_TABLE_NAME, StrBuf);
          if ColumnInfo.TableName = '' then
            ColumnInfo.TableName := ColStrAttribute(ColumnNumber, SQL_DESC_TABLE_NAME, StrBuf);
          if ColumnInfo.TableName <> '' then begin //no table? -> no schema or catalog !
            ColumnInfo.SchemaName := ColStrAttribute(ColumnNumber, SQL_DESC_SCHEMA_NAME, StrBuf);
            ColumnInfo.CatalogName := ColStrAttribute(ColumnNumber, SQL_DESC_CATALOG_NAME, StrBuf);
          end;
        end;
        //ColumnInfo.DefaultValue -> not implemented
        //ColumnInfo.DefaultExpression -> not implemented
        { get signed for determing the C_DATA_TYPE }
        ColumnInfo.Signed := ColNumAttribute(ColumnNumber, SQL_DESC_UNSIGNED) = SQL_FALSE;
        { process TZSQLType }
        bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_CONCISE_TYPE);
        fFixedWidthStrings[ColumnNumber-1] := (bufSQLLEN = SQL_CHAR) or (bufSQLLEN = SQL_WCHAR);
        if bufSQLLEN = SQL_TYPE_VARIANT then begin//SQL Server type
          ColumnInfo.ColumnType := ConvertODBC_CTypeToSQLType(ColNumAttribute(ColumnNumber, SQL_CA_SS_VARIANT_TYPE), ConSettings^.CPType);
          fODBC_CTypes[ColumnNumber-1] := ConvertODBCTypeToODBC_CType(ConvertSQLTypeToODBCType(ColumnInfo.ColumnType,SQL_TYPE_VARIANT, ConSettings^.ClientCodePage^.Encoding), not ColumnInfo.Signed, ConSettings^.ClientCodePage^.Encoding);
        end else begin
          fODBC_CTypes[ColumnNumber-1] := ConvertODBCTypeToODBC_CType(bufSQLLEN, not ColumnInfo.Signed, ConSettings^.ClientCodePage^.Encoding);
          ColumnInfo.ColumnType := ConvertODBCTypeToSQLType(bufSQLLEN, not ColumnInfo.Signed, ConSettings^.CPType);
        end;
        fSQLTypes[ColumnNumber-1] := ColumnInfo.ColumnType;
        {numeric data type infos: }
        if ColumnInfo.ColumnType in [stFloat, stDouble] then begin
          ColumnInfo.Precision := ColNumAttribute(ColumnNumber, SQL_DESC_PRECISION);
          ColumnInfo.Scale := ColNumAttribute(ColumnNumber, SQL_DESC_SCALE);
          if ColumnInfo.ColumnType = stDouble then begin
            ColumnInfo.Currency := ZFastCode.Pos('MONEY', UpperCase(ColStrAttribute(ColumnNumber, SQL_DESC_TYPE_NAME, StrBuf))) > 0; //handle smallmoney oslt too
            if ColumnInfo.Currency then
              ColumnInfo.ColumnType := stCurrency;
          end
        end else if ColumnInfo.ColumnType in [stString, stUnicodeString, stBytes, stGUID] then begin
          { character / binary info}
          ColumnInfo.Precision := ColNumAttribute(ColumnNumber, SQL_DESC_LENGTH);
          if ColumnInfo.Precision = 0 then begin
             ColumnInfo.ColumnType := TZSQLType(Ord(ColumnInfo.ColumnType)+3); //switch to streamed mode
             fSQLTypes[ColumnNumber-1] := ColumnInfo.ColumnType;
          end;
          if ColumnInfo.ColumnType in [stString, stUnicodeString] then
            if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16) then
              ColumnInfo.ColumnCodePage := zCP_UTF16
            else
              ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        end;
      end else begin
        DescribeColumn(ColumnNumber, StrBuf, ColumnInfo);
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName;
        if (ColumnInfo.ColumnType in [stString, stUnicodeString, stBytes]) and
           (ColumnInfo.Precision = 0) then
             ColumnInfo.ColumnType := TZSQLType(Ord(ColumnInfo.ColumnType)+3); //switch to streamed mode
        fSQLTypes[ColumnNumber-1] := ColumnInfo.ColumnType;
      end;
      { calc buf size }
      if not (ColumnInfo.ColumnType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then begin //streams will be fetched by GetData()
        fColumnBuffSizes[ColumnNumber-1] := CalcBufSize(ColumnInfo.Precision, fODBC_CTypes[ColumnNumber-1],
          ColumnInfo.ColumnType, ConSettings^.ClientCodePage)+SizeOf(SQLLEN);
        Inc(RowSize, fColumnBuffSizes[ColumnNumber-1]);
      end else begin
        LobsInResult := True;
        ColumnInfo.Precision := 0;
      end;
    end;
    //GetData don't work with multiple fetched rows for most drivers
    fMaxFetchableRows := Max(1, (fZBufferSize div RowSize)*Ord(not LobsInResult)); //calculate max count of rows for a single fetch call
    if fMaxFetchableRows > 1 then begin
      CheckStmtError(fPlainDriver.SetStmtAttr(fPHSTMT^, SQL_ATTR_ROW_ARRAY_SIZE, {%H-}Pointer(fMaxFetchableRows), 0));
      CheckStmtError(fPlainDriver.SetStmtAttr(fPHSTMT^, SQL_ATTR_ROWS_FETCHED_PTR, @fFetchedRowCount, 0));
    end;
    fFirstGetDataIndex := fColumnCount;
    for ColumnNumber := 0 to fColumnCount -1 do
      if fColumnBuffSizes[ColumnNumber] > 0 then begin //streams will be fetched by GetData()
        SetLength(fColumnBuffers[ColumnNumber], fColumnBuffSizes[ColumnNumber]*fMaxFetchableRows);
        fColumnBuffSizes[ColumnNumber] := fColumnBuffSizes[ColumnNumber]-SizeOf(SQLLEN); // now omit indicator space again
        if (ColumnNumber = 0) or ((ColumnNumber > 0) and fBoundColumns[ColumnNumber-1]) then begin
          CheckStmtError(fPlainDriver.BindCol(fPHSTMT^, ColumnNumber+1,
            fODBC_CTypes[ColumnNumber], @fColumnBuffers[ColumnNumber][SizeOf(SQLLEN)*fMaxFetchableRows],
            fColumnBuffSizes[ColumnNumber], @fColumnBuffers[ColumnNumber][0]));
          fBoundColumns[ColumnNumber] := True;
        end;
      end else begin
        SetLength(fColumnBuffers[ColumnNumber], SizeOf(SQLLEN)); //left space for Str_Or_Ind which will be filled by LoadUnboundColumn
        { improve Invalid descriptor index error .. }
        if (fSQL_GETDATA_EXTENSIONS and SQL_GD_BOUND = SQL_GD_BOUND ) then //E: (DM) The specified column was bound.
          if (fSQL_GETDATA_EXTENSIONS and SQL_GD_ANY_COLUMN = SQL_GD_ANY_COLUMN ) //E: (DM) The number of the specified column was less than or equal to the number of the highest bound column
              or NoStreamedColFollows then begin
            CheckStmtError(fPlainDriver.BindCol(fPHSTMT^, ColumnNumber+1,
              fODBC_CTypes[ColumnNumber], nil, SQL_DATA_AT_EXEC, @fColumnBuffers[ColumnNumber][0]));
            fBoundColumns[ColumnNumber] := True;
          end;
        if not fBoundColumns[ColumnNumber] then
          fFirstGetDataIndex := Min(ColumnNumber, fFirstGetDataIndex);
      end;
    inherited Open;
  end;
end;

procedure TAbstractODBCResultSet.ResetCursor;
begin
  if Assigned(fPHSTMT^) and fCursorOpened then begin
    {CheckStmtError}(fPlainDriver.CloseCursor(fPHSTMT^)); //close cursor and discrad pending result
    fCursorOpened := False;
  end;
  inherited ResetCursor;
end;

{ TODBCResultSetW }

function TODBCResultSetW.ColNumAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT): SQLLEN;
begin
  Result := 0; //init see docs
  CheckStmtError(fPlainW.ColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
      nil, 0, nil, @Result));
end;

function TODBCResultSetW.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String;
var
  StringLength: SQLSMALLINT;
begin
  StringLength := 0;
  CheckStmtError(fPlainW.ColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
      Pointer(Buf), Length(Buf), @StringLength, nil));
  if StringLength > 0 then
    {$IFDEF UNICODE}
    System.SetString(Result, PWideChar(Pointer(Buf)), StringLength shr 1)
    {$ELSE}
    Result := PUnicodeToRaw(PWideChar(Pointer(Buf)), StringLength shr 1, ConSettings^.ClientCodePage^.CP)
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetW.Create(Statement: IZStatement; var StmtHandle: SQLHSTMT;
  ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
  ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean);
begin
  fPlainW := Connection.GetPLainDriver as IODBC3UnicodePlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection, ZBufferSize,
    ChunkSize, EnhancedColInfo);
end;

procedure TODBCResultSetW.DescribeColumn(ColumnNumber: SQLUSMALLINT;
  const Buf: TByteDynArray; var ColumnInfo: TZColumnInfo);
var
  NameLength, DataType, DecimalDigits, Nullable: SQLSMALLINT;
  ColumnSize: SQLULEN;
  {$IFDEF UNICODE}
  ColName: String;
  {$ENDIF}
begin
  CheckStmtError(fPlainW.DescribeCol(fPHSTMT^, ColumnNumber, Pointer(Buf), Length(Buf),
    @NameLength, @DataType, @ColumnSize, @DecimalDigits, @Nullable));
  if NameLength = 0 then
    ColumnInfo.ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
  else begin
    {$IFDEF UNICODE}
    System.SetString(ColName, PWideChar(Pointer(Buf)), NameLength);
    ColumnInfo.ColumnName := ColName;
    {$ELSE}
    ColumnInfo.ColumnName := PUnicodeToRaw(Pointer(Buf), NameLength, ConSettings^.ClientCodePage^.CP);
    {$ENDIF}
  end;
  ColumnInfo.Precision := ColumnSize;
  ColumnInfo.Scale := DecimalDigits;
  if Nullable = SQL_NULLABLE then
    ColumnInfo.Nullable := ntNullable
  else if Nullable = SQL_NO_NULLS then
    ColumnInfo.Nullable := ntNoNulls
  else
    ColumnInfo.Nullable := ntNullableUnknown;
  ColumnInfo.ColumnType := ConvertODBCTypeToSQLType(DataType, False, ConSettings^.CPType);
  fODBC_CTypes[ColumnNumber-1] := ConvertODBCTypeToODBC_CType(DataType, False, ConSettings^.ClientCodePage^.Encoding);
  if ColumnInfo.ColumnType in [stString, stUnicodeString] then
    if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16) then
      ColumnInfo.ColumnCodePage := zCP_UTF16
    else
      ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
end;

function TODBCResultSetW.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := nil;
    Len := 0;
  end else if (fSQLTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] in [stString, stUnicodeString]) then begin
    InternalDecTrailingSpaces(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
    Len := fStrLen_or_Ind shr 1;
    Result := fColDataPtr;
  end else if (fSQLTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] in [stAsciiStream, stUnicodeStream]) then begin
    Result := fRowBlobs[ColumnIndex].GetPWideChar;
    Len := fRowBlobs[ColumnIndex].Length shr 1
  end else
    Result := inherited GetPWideChar(ColumnIndex, Len);
end;

procedure TODBCResultSetW.InternalDecTrailingSpaces(ColumnIndex: SQLUSMALLINT);
begin
  if fFixedWidthStrings[ColumnIndex] then begin
    while (PWideChar(fColDataPtr)+(fStrLen_or_Ind shr 1)-1)^ = ' ' do
      Dec(fStrLen_or_Ind, 2);
    (PWideChar(fColDataPtr)+(fStrLen_or_Ind shr 1))^ := #0;
  end;
end;

function TODBCResultSetW.InternalGetRaw(ColumnIndex: SQLUSMALLINT;
  CodePage: Word): RawByteString;
begin
  InternalDecTrailingSpaces(ColumnIndex);
  Result := PUnicodeToRaw(fColDataPtr, fStrLen_or_Ind shr 1, CodePage);
end;

function TODBCResultSetW.InternalGetUnicode(ColumnIndex: SQLUSMALLINT): ZWideString;
begin
  InternalDecTrailingSpaces(ColumnIndex);
  SetString(Result, PWideChar(fColDataPtr), fStrLen_or_Ind shr 1);
end;

{ TODBCResultSetA }

function TODBCResultSetA.ColNumAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT): SQLLEN;
begin
  Result := 0; //init see docs
  CheckStmtError(fPlainA.ColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
      nil, 0, nil, @Result));
end;

function TODBCResultSetA.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String;
var
  StringLength: SQLSMALLINT;
begin
  StringLength := 0;
  CheckStmtError(fPlainA.ColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
       Pointer(Buf), Length(Buf), @StringLength, nil));
  if StringLength > 0 then
    {$IFDEF UNICODE}
    Result := PRawToUnicode(PAnsiChar(Pointer(Buf)), StringLength, ConSettings^.ClientCodePage^.CP)
    {$ELSE}
    System.SetString(Result, PAnsiChar(Pointer(Buf)), StringLength)
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetA.Create(Statement: IZStatement; var StmtHandle: SQLHSTMT;
  ConnectionHandle: SQLHDBC; SQL: String; Connection: IZODBCConnection;
  ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean);
begin
  fPlainA := Connection.GetPLainDriver as IODBC3RawPlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection,
    ZBufferSize, ChunkSize, EnhancedColInfo);
end;

procedure TODBCResultSetA.DescribeColumn(ColumnNumber: SQLUSMALLINT;
  const Buf: TByteDynArray; var ColumnInfo: TZColumnInfo);
var
  {$IFNDEF UNICODE}
  ColumnName: String;
  {$ENDIF}
  NameLength, DataType, DecimalDigits, Nullable: SQLSMALLINT;
  ColumnSize: SQLULEN;
begin
  CheckStmtError(fPlainA.DescribeCol(fPHSTMT^, ColumnNumber, Pointer(Buf), LEngth(Buf),
    @NameLength, @DataType, @ColumnSize, @DecimalDigits, @Nullable));
  if NameLength = 0 then
    ColumnInfo.ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
  else begin
    {$IFDEF UNICODE}
    ColumnInfo.ColumnName := PRawToUnicode(Pointer(Buf), NameLength, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    SetString(ColumnName, PAnsiChar(Pointer(Buf)), NameLength);
    ColumnInfo.ColumnName := ColumnName;
    {$ENDIF}
  end;
  ColumnInfo.Precision := ColumnSize;
  ColumnInfo.Scale := DecimalDigits;
  if Nullable = SQL_NULLABLE then
    ColumnInfo.Nullable := ntNullable
  else if Nullable = SQL_NO_NULLS then
    ColumnInfo.Nullable := ntNoNulls
  else
    ColumnInfo.Nullable := ntNullableUnknown;
  ColumnInfo.ColumnType := ConvertODBCTypeToSQLType(DataType, False, ConSettings^.CPType);
  fODBC_CTypes[ColumnNumber-1] := ConvertODBCTypeToODBC_CType(DataType, False, ConSettings^.ClientCodePage^.Encoding);
  if ColumnInfo.ColumnType in [stString, stUnicodeString] then
    if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16) then
      ColumnInfo.ColumnCodePage := zCP_UTF16
    else
      ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
end;

function TODBCResultSetA.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := nil;
    Len := 0;
  end else if (fSQLTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] in [stString, stUnicodeString]) then begin
    InternalDecTrailingSpaces(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
    Len := fStrLen_or_Ind;
    Result := fColDataPtr;
  end else if (fSQLTypes[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] in [stAsciiStream, stUnicodeStream]) then begin
    Result := fRowBlobs[ColumnIndex].GetPAnsiChar(Consettings^.ClientCodePage^.CP);
    Len := fRowBlobs[ColumnIndex].Length shr 1
  end else
    Result := inherited GetPAnsiChar(ColumnIndex, Len);
end;

procedure TODBCResultSetA.InternalDecTrailingSpaces(ColumnIndex: SQLUSMALLINT);
begin
  if fFixedWidthStrings[ColumnIndex] then begin
    while (PAnsiChar(fColDataPtr)+fStrLen_or_Ind-1)^ = ' ' do
      Dec(fStrLen_or_Ind);
    (PAnsiChar(fColDataPtr)+fStrLen_or_Ind)^ := #0;
  end;
end;

function TODBCResultSetA.InternalGetRaw(ColumnIndex: SQLUSMALLINT;
  CodePage: Word): RawByteString;
begin
  InternalDecTrailingSpaces(ColumnIndex);
  if (ConSettings^.ClientCodePage^.CP = CodePage) then
    ZSetString(fColDataPtr, fStrLen_or_Ind, Result{%H-})
  else begin
    fUniTemp := PRawToUnicode(fColDataPtr, fStrLen_or_Ind, ConSettings^.ClientCodePage^.CP);
    Result := PUnicodeToRaw(Pointer(fUniTemp), Length(fUniTemp), CodePage);
  end;
end;

function TODBCResultSetA.InternalGetUnicode(ColumnIndex: SQLUSMALLINT): ZWideString;
begin
  InternalDecTrailingSpaces(ColumnIndex);
  Result := PRawToUnicode(fColDataPtr, fStrLen_or_Ind, ConSettings^.ClientCodePage^.CP);
end;

{ TODBCTResultSetMetadata }

procedure TODBCTResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  ColumnInfo.ColumnName := '';
//  !!!skip this brings the detailed information through until someone has an idea
//  how to improve the missing UnCachedGetColumns information!
end;

{ TZODBCBlob }

constructor TZODBCBlob.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; PlainDriver: IODBC3BasePlainDriver);
var
  OffSetPtr: PAnsiChar;
  i: Integer;
begin
  inherited Create;
  if StrLen_or_IndPtr^ >= 0 then begin
    FBlobSize := StrLen_or_IndPtr^;
    GetMem(FBlobData, FBlobSize);
    OffSetPtr := FBlobData;
    for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
      Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
      Inc(OffSetPtr, ChunkSize);
    end;
    Assert(SQL_SUCCEDED(PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
  end else if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
    GetMem(FBlobData, ChunkSize);
    FBlobSize := ChunkSize;
    OffSetPtr := FBlobData;
    while (PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
      ReallocMem(FBlobData, FBlobSize + ChunkSize);
      OffSetPtr := {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(FBlobSize));
      FBlobSize := FBlobSize + ChunkSize;
    end;
    FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^;
    ReallocMem(FBlobData, FBlobSize);
  end;
end;

{ TZODBCClobA }

constructor TZODBCClobA.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer;
  PlainDriver: IODBC3BasePlainDriver; ConSettings: PZConSettings);
var
  OffSetPtr: PAnsiChar;
  i: Integer;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ >= 0 then begin
      FBlobSize := StrLen_or_IndPtr^ +1;
      GetMem(FBlobData, FBlobSize);
      OffSetPtr := FBlobData;
      for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, ChunkSize-1);
      end;
      Assert(SQL_SUCCEDED(PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      GetMem(FBlobData, ChunkSize);
      FBlobSize := ChunkSize;
      OffSetPtr := FBlobData;
      while (PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        ReallocMem(FBlobData, FBlobSize + ChunkSize);
        OffSetPtr := {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(FBlobSize-1));
        FBlobSize := FBlobSize + ChunkSize-1;
      end;
      FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^ +1;
      ReallocMem(FBlobData, FBlobSize);
    end;
    (PAnsiChar(FBlobData)+FBlobSize-1)^ := #0; //set trailing #0
  end;
end;

{ TZODBCClobW }

constructor TZODBCClobW.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer;
  PlainDriver: IODBC3BasePlainDriver; ConSettings: PZConSettings);
var
  OffSetPtr: PAnsiChar;
  I: Integer;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := zCP_UTF16;
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ >= 0 then begin
      FBlobSize := StrLen_or_IndPtr^ +2;
      GetMem(FBlobData, FBlobSize);
      OffSetPtr := FBlobData;
      for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, ChunkSize-2);
      end;
      Assert(SQL_SUCCEDED(PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      GetMem(FBlobData, ChunkSize);
      FBlobSize := ChunkSize;
      OffSetPtr := FBlobData;
      while (PlainDriver.GetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        ReallocMem(FBlobData, FBlobSize + ChunkSize);
        OffSetPtr := {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(FBlobSize){%H-}-2);
        FBlobSize := FBlobSize + ChunkSize-2;
      end;
      FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^ +2;
      ReallocMem(FBlobData, FBlobSize);
    end;
    (PWideChar(FBlobData)+(FBlobSize shr 1)-1)^ := #0; //set trailing #0
  end;
end;

end.
