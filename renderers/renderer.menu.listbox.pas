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
unit renderer.menu.listbox;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, StdCtrls, ImgList, renderer.common.listbox, Graphics, Types,
  LCLType;

type
  PCustomImageList = ^TCustomImageList;

  TMenuListBoxRenderer = class(TCommonListBoxRenderer)
  public
    type
      TMenuSide = (MENU_LEFT, MENU_RIGHT);
  public
    constructor Create (AListBox : PCustomListBox; AImageList : 
      PCustomImageList);
  protected
    procedure DrawItem (ACanvas : TCanvas; AIndex : Integer; ARect : TRect;
      AState : TOwnerDrawState); override;
  private
    FImageList : PCustomImageList;
    FMenuSide : TMenuSide;
    FMenuHover : Boolean;

    procedure SetMenuSide (ASide : TMenuSide);
  public
    property MenuSide : TMenuSide read FMenuSide write SetMenuSide;
    property MenuHover : Boolean read FMenuHover write FMenuHover;
  end;

implementation

{ TMenuListBoxRenderer }

constructor TMenuListBoxRenderer.Create (AListBox : PCustomListBox; 
  AImageList : PCustomImageList);
var
 i : Integer;
begin
  inherited Create(AListBox);
  AListBox^.Options := [];
  FImageList := AImageList;
  
  AListBox^.Items.Clear;
  for i := 0 to (AImageList^.Count div 2) - 1 do
  begin
    AListBox^.Items.Add(IntToStr(i));
  end;

  FMenuSide := MENU_LEFT;
  FMenuHover := False;
  ItemHeight := FImageList^.Height + 20;
  ItemWidth := FImageList^.Width + 30;
end;

procedure TMenuListBoxRenderer.SetMenuSide (ASide : TMenuSide);
begin
  FMenuSide := ASide;
end;

procedure TMenuListBoxRenderer.DrawItem (ACanvas : TCanvas; AIndex : Integer;
  ARect : TRect; AState : TOwnerDrawState);
var
  oldBrush : TBrush;
  oldPen : TPen;
begin
  oldBrush := ACanvas.Brush;
  oldPen := ACanvas.Pen;

  ACanvas.Brush.Color := clDefault;
  ACanvas.Pen.Color := clBlack;
  ACanvas.FillRect(ARect);

  case FMenuSide of
    MENU_LEFT :
      begin
        if (odSelected in AState) or (FMenuHover and (ItemHover = AIndex)) then
        begin
          ACanvas.Brush.Color := clMenuBar;
          ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Left + 10,
            ARect.Bottom, 10, 10);
          ACanvas.FillRect(ARect.Left, ARect.Top, ARect.Left + 5,
            ARect.Bottom);
          ACanvas.Line(ARect.Left, ARect.Top, ARect.Left + 5, ARect.Top);
          ACanvas.Line(ARect.Left, ARect.Bottom - 1, ARect.Left + 5,
            ARect.Bottom - 1);
        end;

        if ((odBackgroundPainted in AState) and (odSelected in AState)) or
           ((FMenuHover and (ItemHover = AIndex))) then
        begin
          FImageList^.Draw(ACanvas, ARect.Left + 20, ARect.Top + 10,
            AIndex * 2 + 1);
        end;

        if ((odBackgroundPainted in AState) and not (odSelected in AState)) or
           ((FMenuHover and (ItemHover <> AIndex))) then
        begin
          FImageList^.Draw(ACanvas, ARect.Left + 20, ARect.Top + 10,
            AIndex * 2);
        end;
      end;
    MENU_RIGHT :
      begin
        if odSelected in AState then
        begin
          ACanvas.Brush.Color := clMenuBar;
          ACanvas.RoundRect(ARect.Right - 10, ARect.Top, ARect.Right,
            ARect.Bottom, 10, 10);
          ACanvas.FillRect(ARect.Right - 5, ARect.Top, ARect.Right,
            ARect.Bottom);
          ACanvas.Line(ARect.Right - 5, ARect.Top, ARect.Right, ARect.Top);
          ACanvas.Line(ARect.Right - 5, ARect.Bottom - 1, ARect.Right,
            ARect.Bottom - 1);
        end;

        if (odBackgroundPainted in AState) and (odSelected in AState) then
        begin
          FImageList^.Draw(ACanvas, ARect.Left + 10, ARect.Top + 10,
            AIndex * 2 + 1);
        end;

        if (odBackgroundPainted in AState) and not (odSelected in AState) then
        begin
          FImageList^.Draw(ACanvas, ARect.Left + 10, ARect.Top + 10,
            AIndex * 2);
        end;
      end;
  end;

  ACanvas.Brush := oldBrush;
  ACanvas.Pen := oldPen;
end;

end.
