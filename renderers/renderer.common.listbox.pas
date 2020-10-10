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
unit renderer.common.listbox;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, StdCtrls, Controls, Graphics, Types;

type
  PCustomListBox = ^TCustomListBox;  

  TCommonListBoxRenderer = class
  public
    constructor Create (AListBox : PCustomListBox);
  protected
    procedure DrawItem (ACanvas : TCanvas; AIndex : Integer; ARect : TRect;
      AState : TOwnerDrawState); virtual; abstract;
    function DrawItemHeight (AIndex : Integer) : Integer; virtual;  
  private
    procedure OnDrawItem(AControl: TWinControl; AIndex: Integer;
      ARect: TRect; AState: TOwnerDrawState);
    procedure OnMeasureItem (AControl: TWinControl; AIndex: Integer;
        var AHeight: Integer);
  private
    FListBox : PCustomListBox;
    FItemHeight : Integer;
    FItemWidth : Integer;

    procedure SetItemHeight (AHeight : Integer);
    procedure SetItemWidth (AWidth : Integer);
  public
    property ItemHeight : Integer read FItemHeight write SetItemHeight;
    property ItemWidth : Integer read FItemWidth write SetItemWidth;
  end;

implementation

{ TCommonListBoxRenderer }

constructor TCommonListBoxRenderer.Create (AListBox : PCustomListBox);
begin
  FItemHeight := 50;
  FListBox := AListBox;
  with FListBox^ do
  begin
    BorderStyle := bsNone;
    Style := TListBoxStyle.lbVirtual;
    OnDrawItem := TDrawItemEvent(@Self.OnDrawItem);
    OnMeasureItem := @Self.OnMeasureItem;
    ItemHeight := FItemHeight;
    ScrollWidth := 0;
  end;
end;

procedure TCommonListBoxRenderer.OnDrawItem(AControl: TWinControl; AIndex:
  Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  with AControl as TCustomListBox do
    DrawItem(Canvas, AIndex, ARect, AState);
end;

procedure TCommonListBoxRenderer.OnMeasureItem(AControl: TWinControl; AIndex: 
  Integer; var AHeight: Integer);
begin
  AHeight := DrawItemHeight(AIndex);
end;

function TCommonListBoxRenderer.DrawItemHeight (AIndex : Integer) : Integer;
begin
  Result := FItemHeight;
end;

procedure TCommonListBoxRenderer.SetItemHeight (AHeight : Integer);
begin
  FItemHeight := AHeight;
  FListBox^.ItemHeight := AHeight;
end;

procedure TCommonListBoxRenderer.SetItemWidth (AWidth : Integer);
begin
  FItemWidth := AWidth;
  FListBox^.Width := AWidth;
end;

end.
