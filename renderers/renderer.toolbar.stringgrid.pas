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
unit renderer.toolbar.stringgrid;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, StdCtrls, ImgList, Graphics, Types, Grids, LCLType, math,
  renderer.common.stringgrid, utils.functor, container.arraylist;

type
  PCustomImageList = ^TCustomImageList;

  { Toolbar base item. }
  TToolbarItem = class
  protected
    procedure OnDraw (ACanvas : TCanvas; aRect : TRect; aState : 
      TGridDrawState); virtual; abstract;  
  private
    FHeight : Integer;
    FWidth : Integer;
    
  public
    property Height : Integer read FHeight write FHeight;
    property Width : Integer read FWidth write FWidth;
  end;  

  { Menu header item. }
  TMenuHeaderItem = class(TToolbarItem)
  public
    constructor Create;
    destructor Destroy; override;
  protected
    procedure OnDraw (ACanvas : TCanvas; aRect : TRect; aState : 
      TGridDrawState); override;
  private
    FPicture : TPicture;
  public
    property Picture : TPicture read FPicture;
  end;

  { Toolbar renderer for string grid. }
  TToolbarStringGridRenderer = class(TCommonStringGridRenderer)
  public
    constructor Create (AStringGrid : PCustomStringGrid);
    destructor Destroy; override;

    { Append new tool bar item. }
    procedure Append (AToolItem : TToolbarItem);
  protected  
    procedure OnSelect (aCol, aRow : Integer); override;
    procedure OnDraw (ACanvas : TCanvas; aCol, aRow : Integer; aRect : TRect; 
      aState : TGridDrawState); override;
    procedure OnMouseMove (X, Y : Integer); override;
    procedure OnMouseLeave; override;
  private
    type
      TToolbarItemCompareFunctor = class
        (specialize TUnsortableFunctor<TToolbarItem>);

      TToolbarItemsList = class
        (specialize TArrayList<TToolbarItem, TToolbarItemCompareFunctor>);  
  private
    FToolbarItems : TToolbarItemsList;
  public

  end;

implementation

{ TToolbarStringGridRenderer }

constructor TToolbarStringGridRenderer.Create (AStringGrid : PCustomStringGrid);
begin
  inherited Create(AStringGrid);
  FToolbarItems := TToolbarItemsList.Create;  

  with StringGrid^ do
  begin
    ColCount := FToolbarItems.Length;
    RowCount := 1;
    ScrollBars := ssNone;
  end;
end;

destructor TToolbarStringGridRenderer.Destroy;
begin
  FreeAndNil(FToolbarItems);
  inherited Destroy;
end;

procedure TToolbarStringGridRenderer.Append (AToolItem : TToolbarItem);
var
  Index : Integer;
  MaxHeight : Integer;
begin
  FToolbarItems.Append(AToolItem);
  StringGrid^.ColCount := FToolbarItems.Length;
  MaxHeight := 0;

  for Index := 0 to FToolbarItems.Length - 1 do
  begin
    StringGrid^.Columns.Add;
    MaxHeight := Max(MaxHeight, FToolbarItems.Value[Index].Height);

    StringGrid^.Columns.Items[Index].Width := FToolbarItems.Value[Index].Width;
    StringGrid^.RowHeights[Index] := FToolbarItems.Value[Index].Height;
  end;

  StringGrid^.Height := MaxHeight;
end;

procedure TToolbarStringGridRenderer.OnSelect (aCol, aRow : Integer); 
begin
  
end;

procedure TToolbarStringGridRenderer.OnDraw (ACanvas : TCanvas; aCol, aRow : 
  Integer; aRect : TRect; aState : TGridDrawState);
begin
  if not FToolbarItems.FirstEntry.HasValue then
    Exit;

  FToolbarItems.Value[aCol].OnDraw(ACanvas, aRect, aState);
end;

procedure TToolbarStringGridRenderer.OnMouseMove (X, Y : Integer);
begin
  
end;

procedure TToolbarStringGridRenderer.OnMouseLeave; 
begin
  
end;

{ TMenuHeaderItem }

constructor TMenuHeaderItem.Create;
begin
  FPicture := TPicture.Create;
  Height := 50;
  Width := 200;
end;

destructor TMenuHeaderItem.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TMenuHeaderItem.OnDraw (ACanvas : TCanvas; aRect : TRect; aState : 
  TGridDrawState);
begin
  ACanvas.Brush.Color := clWindow;
  ACanvas.FillRect(aRect);
end;

end.
