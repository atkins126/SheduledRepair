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
unit dataproviders.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, database, sqlite3.table, container.arraylist, utils.functor,
  sqlite3.result, sqlite3.result_row, Classes, rules.chain, 
  renderer.profile.objectprofile, objects.common;

type
  TCommonDataProvider = class
  public
    constructor Create;
    destructor Destroy; override;
    
    function Load : Boolean; virtual; abstract;
    
    function CreateObject : TCommonObject; virtual; abstract;
    function GetObject (AIndex : Cardinal) : TCommonObject;
    function DeleteObject (AIndex : Cardinal) : Boolean;
    function GetObjectProfile (AIndex : Cardinal) : TRendererObjectProfile;
  public
    type
      TObjectCompareFunctor = class
        (specialize TBinaryFunctor<TCommonObject, Integer>)
      public
        function Call (AValue1, AValue2 : TCommonObject) : Integer; override;
      end;

      TObjectsList = class
        (specialize TArrayList<TCommonObject, TObjectCompareFunctor>);
  public
    function GetEnumerator : TObjectsList.TIterator;
  protected
    FObjectsList : TObjectsList;
  end;

implementation

{ TCommonDataProvider.TObjectCompareFunctor }

function TCommonDataProvider.TObjectCompareFunctor.Call (AValue1, AValue2 : T) :
  Integer;
begin
  if AValue1.ID < AValue2.ID then
    Result := -1
  else if AValue2.ID < AValue1.ID then
    Result := 1
  else 
    Result := 0;
end;

{ TCommonDataProvider }

constructor TCommonDataProvider.Create;
begin
  FObjectsList := TObjectsList.Create;
end;

destructor TCommonDataProvider.Destroy;
begin
  FreeAndNil(FObjectsList);
  inherited Destroy;
end;

function TCommonDataProvider.GetEnumerator : TObjectsList.TIterator;
begin
  Result := FObjectsList.GetEnumerator;
end;

function TCommonDataProvider.GetObject (AIndex : Cardinal) : TCommonObject;
begin
  
end;

function TCommonDataProvider.DeleteObject (AIndex : Cardinal) : Boolean;
begin
  
end;

function TCommonDataProvider.GetObjectProfile (AIndex : Cardinal) :
  TRendererObjectProfile;
begin
  
end;

function TCommonDataProvider.GetEnumerator : TObjectsList.TIterator;
begin
  
end;

end.
