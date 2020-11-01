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
unit objects.measure;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema;

type
  TMeasure = class(TCommonObject)
  private
    const
      MEASURE_TABLE_NAME = 'measure';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Object deep copy. }
    procedure Assign (AMeasure : TMeasure);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;

    { Load current object form database. }
    function LoadCurrentObject : Boolean; override;
  protected
    FName : String;
  public
    property Name : String read FName write FName;
  end;

implementation

{ TCommonObject }

constructor TMeasure.Create (AID : Int64);
begin
  inherited Create (AID);
  FName := '';
end;

destructor TMeasure.Destroy;
begin
  inherited Destroy;
end;

procedure TMeasure.PrepareSchema (var ASchema : TSQLite3Schema);
begin
  ASchema
    .Id
    .Text('name').NotNull;
end;

function TMeasure.Table : String;
begin
  Result := MEASURE_TABLE_NAME;
end;

function TMeasure.LoadCurrentObject : Boolean;
begin
  Result := inherited LoadCurrentObject;
  FName := GetStringProperty('name');
end;

function TMeasure.Save : Boolean;
begin
  SetStringProperty('name', FName);
  Result := inherited Save;
end;

procedure TMeasure.Assign (AMeasure : TMeasure);
begin
  Name := AMeasure.Name;
end;

end.
