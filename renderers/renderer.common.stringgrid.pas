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
unit renderer.common.stringgrid;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, Classes, StdCtrls, Controls, Graphics, Types, Math, Grids;

type
  PCustomStringGrid = ^TCustomStringGrid;  

  TCommonStringGridRenderer = class
  public
    constructor Create (AStringGrid : PCustomStringGrid);
  protected  
    procedure OnSelect (aCol, aRow : Integer); virtual; abstract;
    procedure OnDraw (ACanvas : TCanvas; aCol, aRow : Integer; aRect : TRect; 
      aState : TGridDrawState); virtual; abstract;
    procedure OnMouseMove (X, Y : Integer); virtual; abstract;
    procedure OnMouseLeave; virtual; abstract;
  private  
    procedure SelectCell ({%H-}ASender: TObject; aCol, aRow: Integer;
      var ACanSelect: Boolean);
    procedure DrawCell ({%H-}ASender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure MouseMove ({%H-}Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure MouseLeave ({%H-}Sender: TObject);
  private
    FStringGrid : PCustomStringGrid;
  protected
    property StringGrid : PCustomStringGrid read FStringGrid;
  end;

implementation

{ TCommonStringGridRenderer }

constructor TCommonStringGridRenderer.Create (AStringGrid : PCustomStringGrid);
begin
  FStringGrid := AStringGrid;

  with FStringGrid^ do
  begin
    DefaultDrawing := False;
    ExtendedSelect := False;
    FixedCols := 0;
    FixedRows := 0;
    Options := [goSmoothScroll];

    OnSelectCell := @Self.SelectCell;
    OnDrawCell := @Self.DrawCell;
    OnMouseMove := @Self.MouseMove;
    OnMouseLeave := @Self.MouseLeave;
  end;
end;

procedure TCommonStringGridRenderer.SelectCell (ASender : TObject; aCol, aRow : 
  Integer; var ACanSelect : Boolean);
begin
  OnSelect(aCol, aRow);
  ACanSelect := False;
end;

procedure TCommonStringGridRenderer.DrawCell (ASender: TObject; aCol, aRow: 
  Integer; aRect: TRect; aState: TGridDrawState);
var
  oldBrush : TBrush;
  oldPen : TPen;
begin
  with ASender as TCustomStringGrid do
  begin
    oldBrush := Canvas.Brush;
    oldPen := Canvas.Pen;

    OnDraw(Canvas, aCol, aRow, aRect, aState);

    Canvas.Brush := oldBrush;
    Canvas.Pen := oldPen;
  end;
end;

procedure TCommonStringGridRenderer.MouseMove (Sender: TObject; Shift: 
  TShiftState; X, Y: Integer);
begin
  OnMouseMove(X, Y);  
end;

procedure TCommonStringGridRenderer.MouseLeave (Sender: TObject);
begin
  OnMouseLeave;
end;

end.
