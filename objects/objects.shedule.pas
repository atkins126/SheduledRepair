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
unit objects.shedule;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, sqlite3.schema, sqlite3.result, sqlite3.result_row;

type
  TShedule = class(TCommonObject)
  private
    const
      SHEDULE_TABLE_NAME = 'shedule';
  public
    constructor Create (AID : Int64); override;
    destructor Destroy; override;
    
    { Get object database table name. }
    function Table : String; override;

    { Load object from database. }
    function Load : Boolean; override;

    { Save object to database. }
    function Save : Boolean; override;

    { Delete object from database. }
    function Delete : Boolean; override;

    { Object deep copy. }
    procedure Assign (AShedule : TShedule);
  protected
    { Prepare current object database table scheme. }
    procedure PrepareSchema (var ASchema : TSQLite3Schema); override;
  protected
    FPrevDate : TDate;
    FNextDate : TDate;
  public
    property PrevDate : TDate read FPrevDate write FPrevDate;
    property NextDate : TDate read FPrevDate write FPrevDate;
  end;

implementation

{ TPeriod }

constructor TShedule.Create (AID : Int64);
begin
  inherited Create (AID);
  FPrevDate := Now;
  FNextDate := Now;
end;

destructor TShedule.Destroy;
begin
  inherited Destroy;
end;

procedure TShedule.PrepareSchema (var ASchema : TSQLite3Schema); 
begin
  ASchema
    .Id
    .Text('prev_date')
    .Text('next_date');
end;

function TShedule.Table : String;
begin
  Result := SHEDULE_TABLE_NAME;
end;

function TShedule.Load : Boolean;
begin
  if not LoadCurrentObject then
    Exit(False);

  FPrevDate := StrToDateDef(GetStringProperty('prev_date'), Now);
  FNextDate := StrToDateDef(GetStringProperty('next_date'), Now);
  Result := True;
end;

function TShedule.Save : Boolean;
begin
  if ID <> -1 then
  begin
    Result := (UpdateRow.Update('prev_date', DateToStr(FPrevDate))
      .Update('next_date', DateToStr(FNextDate)).Get > 0);
  end else 
  begin
    Result := (InsertRow.Value('prev_date', DateToStr(FPrevDate))
      .Value('next_date', DateToStr(FNextDate)).Get > 0);
    UpdateObjectID;
  end;
end;

function TShedule.Delete : Boolean;
begin
  Result := DeleteCurrentObject;
end;

procedure TShedule.Assign (AShedule : TShedule);
begin
  FPrevDate := AShedule.PrevDate;
  FNextDate := AShedule.NextDate;
end;

end.
