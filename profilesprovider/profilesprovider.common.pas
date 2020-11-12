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
unit profilesprovider.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, container.arraylist, utils.functor,
  sqlite3.result, sqlite3.result_row, renderer.profile.objectprofile;

type
  TCommonProfilesProvider = class
  public
    constructor Create;
    destructor Destroy; override;
    
    { Load profiles. }
    function Load : Boolean; virtual;

    { Append new profile to list. }
    procedure Append (AProfile : TRendererObjectProfile);

    { Remove profile from list by index. }
    procedure Remove (AProfileIndex : Cardinal);

    { Get profile by index. }
    function GetProfile (AProfileIndex : Cardinal) : TRendererObjectProfile;
  protected
      TProfilesCompareFunctor = class
        (specialize TBinaryFunctor<TRendererObjectProfile, Integer>)
      public
        function Call (AValue1, AValue2 : TRendererObjectProfile) : Integer; 
          override;
      end;

      TProfilesList = class
        (specialize TArrayList<TRendererObjectProfile, 
        TProfilesCompareFunctor>);
  protected
    FProfilesList : TProfilesList;
  end;

implementation

{ TCommonProfilesProvider.TProfilesCompareFunctor }

function TCommonProfilesProvider.TProfilesCompareFunctor.Call (AValue1, 
  AValue2 : TRendererObjectProfile) : Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else 
    Result := 0;
end;

{ TCommonProfilesProvider }

constructor TCommonProfilesProvider.Create;
begin
  FProfilesList := TProfilesList.Create;
end;

destructor TCommonProfilesProvider.Destroy;
begin
  FreeAndNil(FProfilesList);
  inherited Destroy;
end;

procedure TCommonProfilesProvider.Append (AProfile : TRendererObjectProfile);
begin
  FProfilesList.Append(AProfile);
end;

procedure TCommonProfilesProvider.Remove (AProfileIndex : Cardinal);
begin
  if AProfileIndex < FProfilesList.Length then
    FProfilesList.Remove(AProfileIndex);
end;

function TCommonProfilesProvider.GetProfile (AProfileIndex : Cardinal) : 
  TRendererObjectProfile;
begin
  if AProfileIndex < FProfilesList.Length then
    Exit(FProfilesList.Value[AProfileIndex]);
  
  Result := nil;
end;

function TCommonProfilesProvider.Load : Boolean;
var
  Table : TSQLite3Table;
  ResultRows : TSQLite3Result;
  Row : TSQLite3ResultRow;
  Profile : TRendererObjectProfile;
begin
  Profile := TRendererObjectProfile.Create(-1);

  Table := TSQLite3Table.Create(DB.Errors, DB.Handle, Profile.Table);
  ResultRows := Table.Select.Field('id').Get;

  FreeAndNil(Profile);

  for Row in ResultRows do
  begin
    Profile := TRendererObjectProfile.Create(Row.GetIntegerValue('id'));
    FObjectsList.Append(Profile);
  end;

  Result := True;
end;

end.
