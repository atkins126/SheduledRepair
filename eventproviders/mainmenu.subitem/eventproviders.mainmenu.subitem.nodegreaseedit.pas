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
unit eventproviders.mainmenu.subitem.nodegreaseedit;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, eventproviders.common, objects.common, objects.greasebundle;

type
  TMainMenuSubitemNodeGreaseEditEventProvider = class(TCommonEventProvider)
  public
    constructor Create; override;
  private
    function NodeGreaseEditClickEvent ({%H-}AObject : TCommonObject) : 
      Boolean;
  end;

implementation

uses
  dataprovider, mainmenuprovider;

{ TMainMenuSubitemNodeGreaseEditEventProvider }

constructor TMainMenuSubitemNodeGreaseEditEventProvider.Create;
begin
  inherited Create;
  
  Register(EVENT_OBJECT_CLICK, @NodeGreaseEditClickEvent);
end;

function TMainMenuSubitemNodeGreaseEditEventProvider
  .NodeGreaseEditClickEvent (AObject : TCommonObject) : Boolean;
var
  EntityObject : TCommonObject;
begin
  EntityObject := Provider.GetSelectedObject;
  if Assigned(EntityObject) then
    Provider.ShowEditor(TGreaseBundle(EntityObject));

  Result := True;
end;

end.
