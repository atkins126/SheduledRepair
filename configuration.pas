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
    constructor Create (AID : Int64); override;
    destructor Destroy; override;

    { Check database table scheme. }
    function CheckSchema : Boolean; override;

    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Get config value. }
    function GetValue (AKey : String; ADefault : String) : String; overload; 
    function GetValue (AKey : String; ADefault : Integer) : Integer; overload;
    function GetValue (AKey : String; ADefault : Double) : Double; overload;

    { Store value. }
    procedure SetValue (AKey : String; AValue : String); overload;
    procedure SetValue (AKey : String; AValue : Integer); overload;
    procedure SetValue (AKey : String; AValue : Double); overload;

    { Delete value. }
    function RemoveValue(AKey : String) : Boolean;
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

constructor TConfig.Create (AID : Int64);
begin
  if not Assigned(Config) then
  begin
    inherited Create (AID);
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

function TConfig.Load : Boolean;
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
begin
  Result := True;
end;

function TConfig.Delete : Boolean;
begin
  Result := True;
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
var
  updated_rows : Integer;
  index : Integer;
begin
  updated_rows := FTable.Update.Update('value', AValue).Where('key', AKey).Get;
 
  if updated_rows > 0 then
  begin
    index := FKeyValueList.IndexOf(TKeyValue.Create(AKey, AValue));
    
    if index <> -1 then
      FKeyValueList.GetValue(Index).Second := AValue
    else
      FKeyValueList.Append(TKeyValue.Create(AKey, AValue));
    Exit;
  end;
    
  FTable.Insert.Value('key', AKey).Value('value', AValue).Get;
  FKeyValueList.Append(TKeyValue.Create(AKey, AValue));
end;

procedure TConfig.SetValue (AKey : String; AValue : Integer);
begin
  SetValue(AKey, IntToStr(AValue));
end;

procedure TConfig.SetValue (AKey : String; AValue : Double);
begin
  SetValue(AKey, FloatToStr(AValue));
end;

function TConfig.RemoveValue (AKey : String) : Boolean;
var
  Index : Integer;
begin
  if not FKeyValueList.FirstEntry.HasValue then
    Exit(False);

  Index := FKeyValueList.IndexOf(TKeyValue.Create(AKey, ''));

  if Index <> -1 then
  begin
    FKeyValueList.Remove(Index);
    Exit(FTable.Delete.Where('key', AKey).Get > 0);
  end;

  Result := False;
end;

initialization
  Config := TConfig.Create(-1);
finalization
  FreeAndNil(Config);
end.
