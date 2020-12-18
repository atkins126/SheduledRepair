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
unit eventproviders.common;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, objects.common, container.hashtable, utils.functor;

type
  TCommonEventProvider = class
  public
    const
      EVENT_OBJECT_CLICK                                                 = 0;
      EVENT_OBJECT_DOUBLE_CLICK                                          = 1;
      EVENT_OBJECT_SELECT                                                = 2;
      EVENT_OBJECT_UNSELECT                                              = 3;
      EVENT_OBJECT_ATTACH_DYNAMIC_MENU                                   = 4;
      EVENT_OBJECT_DETACH_DYNAMIC_MENU                                   = 5;
    type
      TObjectEvent = function (AObject : TCommonObject) : Boolean of object;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Append new event. }
    procedure Register (AEventID : Integer; AEvent : TObjectEvent);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Remove event. }
    procedure Remove (AEventID : Integer);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Run exists event. } 
    function Fire (AEventID : Integer; AObject : TCommonObject) : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    type
      TEventCompareFunctor = class(specialize TUnsortableFunctor<Integer>);
      TEvents = class(specialize THashTable<Integer, TObjectEvent, 
        TEventCompareFunctor>);
  protected
    FEvents : TEvents;
  end;

implementation

{ TCommonEventProvider }

constructor TCommonEventProvider.Create;
begin
  FEvents := TEvents.Create(@HashInteger);
end;

destructor TCommonEventProvider.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FEvents);
end;

procedure TCommonEventProvider.Register (AEventID : Integer; AEvent :
  TObjectEvent);
begin
  FEvents.Insert(AEventID, AEvent);
end;

procedure TCommonEventProvider.Remove (AEventID : Integer);
begin
  FEvents.Remove(AEventID);
end;

function TCommonEventProvider.Fire (AEventID : Integer; AObject : 
  TCommonObject) : Boolean;
var
  Event : TObjectEvent;
begin
  try
    Event := FEvents.Search(AEventID);
    Exit(Event(AObject));
  except
    on E: EKeyNotExistsException do
      { Do nothing. }
  end;
  Result := False;
end;

end.
