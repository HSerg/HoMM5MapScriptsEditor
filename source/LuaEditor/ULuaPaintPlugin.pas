// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit ULuaPaintPlugin;

interface

uses
  Classes, SynEdit, ULuaFrame, Graphics, Windows;

type
  TLuaPaintPlugin = class(TSynEditPlugin)
  protected
    FLuaFrame: TLuaFrame;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
    
  public
    constructor Create(ALuaFrame: TLuaFrame);
  end;

implementation

{ TLuaPaintPlugin }

procedure TLuaPaintPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
const
  NONE_IMAGE = -1;
  ERROR_IMAGE = 4;
  WARNING_IMAGE = 5;
var
  LH, X, Y: integer;
  LI: TScriptLineStatus;
  ImgIndex: integer;
  LineNo: integer;
begin
  FirstLine := FLuaFrame.synedtScript.RowToLine(FirstLine);
  LastLine := FLuaFrame.synedtScript.RowToLine(LastLine);
  X := 14;
  LH := FLuaFrame.synedtScript.LineHeight;

  for LineNo := FirstLine to LastLine do
    begin
      Y := (LH - FLuaFrame.imglGutterGlyphs.Height) div 2
           + LH * (FLuaFrame.synedtScript.LineToRow(LineNo) - FLuaFrame.synedtScript.TopLine);

      LI := FLuaFrame.GetLineStatus(LineNo);
      
      if slsError = LI then
        ImgIndex := ERROR_IMAGE
      else
      if slsWarning = LI then
        ImgIndex := WARNING_IMAGE
      else
        ImgIndex := NONE_IMAGE;

      if ImgIndex >= 0 then
        FLuaFrame.imglGutterGlyphs.Draw(ACanvas, X, Y, ImgIndex);
    end;
end;

constructor TLuaPaintPlugin.Create(ALuaFrame: TLuaFrame);
begin
  inherited Create(ALuaFrame.synedtScript);
  FLuaFrame := ALuaFrame;
end;

procedure TLuaPaintPlugin.LinesDeleted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

procedure TLuaPaintPlugin.LinesInserted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

end.
