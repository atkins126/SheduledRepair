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
unit renderers.virtualtreeview;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, VirtualTrees, objects.common, renderers.virtualtreeview,
  dataproviders.common, profilesprovider.common, renderers.common;

type
  TRenderer = class (specialize TVirtualTreeViewRenderer<TCommonObject>)
  public
    constructor Create (ATreeView : TVirtualDrawTree);
    destructor Destroy; override;

    procedure Update (ADataProvider : TCommonDataProvider; AProfileProvider :
      TCommonProfilesProvider; ARenderer : TCommonRenderer);
  protected
    { Get tree item height. }
    function ItemHeight (ANode : PVirtualNode; ACanvas : TCanvas; AIndex : 
      Cardinal; AItemType : Integer; AData : TCommonObject) : Cardinal; 
      override;
    
    { Draw tree item. }
    procedure ItemDraw (ANode : PVirtualNode; AColumn : TColumnIndex; 
      AItemType : Integer; ACanvas : TCanvas; ACellRect : TRect; AContentRect : 
      TRect; AState : TItemStates; AData : TCommonObject); override; 

    { Update VirtualTree data. }
    procedure UpdateData; 
  protected
    FDataProvider : TCommonDataProvider;
    FProfileProvider : TCommonProfilesProvider;
    FRenderer : TCommonRenderer;
  end;

implementation

{ TRenderer }

constructor TRenderer.Create (ATreeView : TVirtualDrawTree);
begin
  inherited Create(ATreeView, []);
  FDataProvider := nil;
  FProfileProvider := nil;
  FRenderer := nil;
end;

destructor TRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure TRenderer.Update (ADataProvider : TCommonDataProvider; 
  AProfileProvider : TCommonProfilesProvider; ARenderer : TCommonRenderer);
begin
  FDataProvider := ADataProvider;
  FProfileProvider := AProfileProvider;
  FRenderer := ARenderer;

  { Reload data to VirtualTree. }  
  UpdateData;
end;

procedure TRenderer.UpdateData;
begin
  FTreeView.BeginUpdate;
  FTreeView.Clear;  

    

  FTreeView.EndUpdate;
end;

end.