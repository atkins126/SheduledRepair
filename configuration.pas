(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpassqlite                ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)
unit configuration;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, sqlite3.table, sqlite3.insert, sqlite3.schema, sqlite3.result, 
  sqlite3.result_row, utils.pair, utils.functor, container.arraylist,
  objects.common;

type
  { Config }
  TConfig = class(TCommonObject)
  private
    const
      CONFIG_TABLE_NAME = 'config';
  public
    constructor Create; override;
    destructor Destroy; override;

    { Check database table scheme. }
    function CheckSchema : Boolean; override;

    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load ({%H-}AID : Int64) : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Get config value. }
    function GetValue (AKey : String; ADefault : String) : String; overload; 
    function GetValue (AKey : String; ADefault : Integer) : Integer; overload;
    function GetValue (AKey : String; ADefault : Double) : Double; overload;

    { Store value. }
    procedure SetValue (AKey : String; AValue : String); overload;
    procedure SetValue (AKey : String; AValue : Integer); overload;
    procedure SetValue (AKey : String; AValue : Double); overload;
  private
    type
      TKeyValue = class(specialize TPair<String, String>);
      
      TKeyValueCompareFunctor = class
        (specialize TBinaryFunctor<TKeyValue, Integer>)
      public
        function Call (AValue1, AValue2 : TKeyValue) : Integer; override;
      end;
      
      TKeyValueList = class
        (specialize TArrayList<TKeyValue, TKeyValueCompareFunctor>);
  private
    FKeyValueList : TKeyValueList;
  end;

var
  Config : TConfig = nil;

implementation

{ TConfig.TKeyValueCompareFunctor }

function TConfig.TKeyValueCompareFunctor.Call (AValue1, AValue2 : TKeyValue) :
  Integer;
begin
  if AValue1.First < AValue2.First then
    Result := -1
  else if AValue2.First < AValue1.First then
    Result := 1
  else
    Result := 0;
end;

{ TConfig }

constructor TConfig.Create;
begin
  if not Assigned(Config) then
  begin
    inherited Create;
    FKeyValueList := TKeyValueList.Create;
    Config := self;
  end else
    self := Config;
end;

destructor TConfig.Destroy;
begin
  FreeAndNil(FKeyValueList);
  inherited Destroy;
end;

function TConfig.CheckSchema : Boolean;
var
  Schema : TSQLite3Schema;
begin
  Schema := TSQLite3Schema.Create;

  Schema
    .Id
    .Text('key').NotNull
    .Text('value');

  if not FTable.Exists then
    FTable.New(Schema);

  Result := FTable.CheckSchema(Schema);

  FreeAndNil(Schema);
end;

function TConfig.Table : String;
begin
  Result := CONFIG_TABLE_NAME;
end;

function TConfig.Load (AID : Int64) : Boolean;
var
  result_rows : TSQLite3Result;
  row : TSQLite3ResultRow;
begin
  FKeyValueList.Clear;

  result_rows := FTable.Select.All.Get;
  if not result_rows.FirstRow.HasRow then
    Exit(True);

  for row in result_rows do
  begin
    FKeyValueList.Append(TKeyValue.Create(row.GetStringValue('key'),
      row.GetStringValue('value')));
  end;

  Result := True;
end;

function TConfig.Save : Boolean;
var
  item : TKeyValue;
  insert : TSQLite3Insert;
begin
  if not FKeyValueList.FirstEntry.HasValue then
    Exit(True);

  insert := FTable.Insert
    .Column('key', SQLITE_TEXT)
    .Column('value', SQLITE_TEXT);

  for item in FKeyValueList do
  begin
    insert.Row.Value(item.First).Value(item.Second);
  end;

  Result := (insert.Get > 0);
end;

function TConfig.GetValue (AKey : String; ADefault : String) : String;
var
  Index : Integer;
begin
  if not FKeyValueList.FirstEntry.HasValue then
    Exit(ADefault);

  Index := FKeyValueList.IndexOf(TKeyValue.Create(AKey, ADefault));
  
  if Index <> -1 then
    Result := FKeyValueList.GetValue(Index).Second
  else
    Result := ADefault;
end;

function TConfig.GetValue (AKey : String; ADefault : Integer) : Integer;
begin
  Result := StrToIntDef(GetValue(AKey, ''), ADefault)
end;

function TConfig.GetValue (AKey : String; ADefault : Double) : Double;
begin
  Result := StrToFloatDef(GetValue(AKey, ''), ADefault);
end;

procedure TConfig.SetValue (AKey : String; AValue : String);
begin
  FKeyValueList.Append(TKeyValue.Create(AKey, AValue));
end;

procedure TConfig.SetValue (AKey : String; AValue : Integer);
begin
  FKeyValueList.Append(TKeyValue.Create(AKey, IntToStr(AValue)));
end;

procedure TConfig.SetValue (AKey : String; AValue : Double);
begin
  FKeyValueList.Append(TKeyValue.Create(AKey, FloatToStr(AValue)));
end;

initialization
  Config := TConfig.Create;
finalization
  FreeAndNil(Config);
end.
