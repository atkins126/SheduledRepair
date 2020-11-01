(******************************************************************************)
(*                               SheduledRepair                               *)
(*                                                                            *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/SheduledRepair              ivan@semenkov.pro *)
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
unit objects.quantity;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, objects.measure;

type
  TQuantity = class(TCommonObject)
  private
    const
      QUANTITY_TABLE_NAME = 'quantity';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Object deep copy. }
    procedure Assign (AQuantity : TQuantity);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Check all dependent schemes. }
    function CheckDepentSchemes : Boolean; override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;

    { Load all dependent objects. }
    function LoadDepentObjects : Boolean; override;

    { Save all dependent objects. }
    function SaveDepentObjects : Boolean; override;
  protected
    FCount : Double;
    FMeasure : TMeasure;
  public
    property Count : Double read FCount write FCount;
    property Measure : TMeasure read FMeasure write FMeasure;
  end;

implementation

{ TQuantity }

constructor TQuantity.Create (AID : Int64);
begin
  inherited Create (AID);
  FCount := 0;
  FMeasure := TMeasure.Create(-1);
end;

destructor TQuantity.Destroy;
begin
  FreeAndNil(FMeasure);
  inherited Destroy;
end;

procedure TQuantity.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Float('count').NotNull
    .Integer('measure_id').NotNull;
end;

function TQuantity.CheckDepentSchemes : Boolean;
begin
  Result := FMeasure.CheckSchema;
end;

function TQuantity.Table : String;
begin
  Result := QUANTITY_TABLE_NAME;
end;

function TQuantity.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;
  FCount := GetDoubleProperty('count');
end;

function TQuantity.LoadDepentObjects : Boolean;
begin
  Result := FMeasure.Reload(GetIntegerProperty('measure_id'));
end;

function TQuantity.SaveDepentObjects : Boolean;
begin
  Result := FMeasure.Save;
end;

function TQuantity.Save : Boolean;
begin
  SetDoubleProperty('count', FCount);
  SetIntegerProperty('measure_id', FMeasure.ID);
  Result := inherited Save;
end;

procedure TQuantity.Assign (AQuantity : TQuantity);
begin
  Count := AQuantity.Count;
  Measure := AQuantity.Measure;
end;

end.
