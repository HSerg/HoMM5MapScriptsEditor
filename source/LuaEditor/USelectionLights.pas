// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USelectionLights;

interface

uses
  Windows, SysUtils, Graphics, SynEdit, SynEditHighlighter;

type
  RLightBlock = record
    IsTokenBlock: boolean; 
    BufferCoord: TBufferCoord;
  end;

  TSelectionLights = class
  protected
    FHighlighter: TSynCustomHighlighter;
    FBufferCoords: array of RLightBlock;
    FWord: string;
    FText: string;
    FTextChanged: boolean;
    FEditor: TSynEdit;
    FSelectionBufferCoord: TBufferCoord;

    procedure DetectChanges();
    procedure ParseLua();
    procedure ParseDelimText();
    procedure PaintLights(Canvas: TCanvas);

    procedure AppendLightBlock(const AIsTokenBlock: boolean;
      const ABufferCoord: TBufferCoord);
    function FindLightBlock(const ABufferCoord: TBufferCoord): integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Paint(AEditor: TSynEdit; Canvas: TCanvas; TransientType: TTransientType);

    property Editor: TSynEdit read FEditor write FEditor;
    property TextChanged: boolean read FTextChanged write FTextChanged;
  end;

implementation

uses
  SynHighlighterLua, LuaSyntax;

{ TSelectionLights }

procedure TSelectionLights.AppendLightBlock(const AIsTokenBlock: boolean;
  const ABufferCoord: TBufferCoord);
var
  old_size: integer;
begin
  old_size := Length(FBufferCoords);
  SetLength(FBufferCoords, old_size+1);
  FBufferCoords[old_size].IsTokenBlock := AIsTokenBlock;
  FBufferCoords[old_size].BufferCoord := ABufferCoord;
end;

constructor TSelectionLights.Create;
begin
  FHighlighter := TSynLuaSyn.Create(nil);
  FTextChanged := true; 
end;

destructor TSelectionLights.Destroy;
begin
  FHighlighter.Free;
end;

procedure TSelectionLights.DetectChanges;
begin
  FTextChanged := (FText <> Editor.Text);
end;

function TSelectionLights.FindLightBlock(
  const ABufferCoord: TBufferCoord): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FBufferCoords) - 1 do
    if (FBufferCoords[i].BufferCoord.Char = ABufferCoord.Char) and
       (FBufferCoords[i].BufferCoord.Line = ABufferCoord.Line) then
      begin
        Result := i;
        break;
      end;
end;

procedure TSelectionLights.Paint(AEditor: TSynEdit; Canvas: TCanvas;
  TransientType: TTransientType);
begin
  FEditor := AEditor;
  if TransientType = ttBefore then
    ParseLua()
  else
    begin
      ParseLua();
      PaintLights(Canvas);
    end;
end;

procedure TSelectionLights.PaintLights(Canvas: TCanvas);
const
  BGR_COLOR: TColor = clSilver;
  BGR_COLOR2: TColor = clYellow;
var
  i: Integer;
  Pix: TPoint;
  displayCoord: TDisplayCoord;
begin
  if Length(FBufferCoords) = 0 then
    exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Assign(Editor.Font);

  for i := 0 to Length(FBufferCoords) - 1 do
    begin
      if (FSelectionBufferCoord.Line = FBufferCoords[i].BufferCoord.Line) and
         (FSelectionBufferCoord.Char = FBufferCoords[i].BufferCoord.Char) then
        continue;
      displayCoord := Editor.BufferToDisplayPos(FBufferCoords[i].BufferCoord);
      if Editor.TopLine-1 > displayCoord.Row then
        continue;
      if Editor.TopLine+Editor.LinesInWindow-1+1 < displayCoord.Row then
        continue;
      Pix := Editor.RowColumnToPixels(displayCoord);
      if FBufferCoords[i].IsTokenBlock then
        Canvas.Brush.Color := BGR_COLOR2
      else
        Canvas.Brush.Color := BGR_COLOR;
      Canvas.TextOut(Pix.X, Pix.Y, FWord);
    end;
end;

procedure TSelectionLights.ParseDelimText;
const
  END_OF_TOKEN_CHR = '"''{}()[],;+-/*<>=. '#9;
var
  line: integer;
  textLine: string;
  token: string;
  i: Integer;
  P: TBufferCoord;
begin
  for line := 0 to Editor.Lines.Count-1 do
    begin
      textLine := Editor.Lines[line];
      if Pos(FWord, textLine) = 0 then
        continue;
      token := '';
      for i := 1 to Length(textLine) do
        begin
          if pos(textLine[i], END_OF_TOKEN_CHR) = 0 then
            begin
              token := token + textLine[i];
              continue;
            end;
          if FWord = token then
            begin
              P.Line := line+1;
              P.Char := i-Length(FWord);
              if FindLightBlock(P) = -1 then
                AppendLightBlock(true, P);
            end;
          token := '';
        end;
      if FWord = token then
        begin
          P.Line := line+1;
          P.Char := Length(textLine)-Length(FWord)+1;
          if FindLightBlock(P) = -1 then
            AppendLightBlock(true, P);
        end;
    end;
end;

procedure TSelectionLights.ParseLua();
var
  line: Integer;
  P: TBufferCoord;
  word: string;
begin
  if not FTextChanged and (FWord = Trim(Editor.SelText)) then
    exit;

  FWord := Trim(Editor.SelText);
  if FTextChanged then
    FText := Editor.Text;
  SetLength(FBufferCoords, 0);

  if not Editor.SelAvail then
    exit;
  if FWord = '' then
    exit;
  if (Editor.WordAtCursor <> '') and (FWord <> Editor.WordAtCursor) then
    exit;
  if (Editor.WordAtCursor = '') and (FWord <> Editor.GetWordAtRowCol(Editor.PrevWordPos)) then
    exit;

  FSelectionBufferCoord := Editor.BlockBegin;

  for line := 0 to Editor.Lines.Count-1 do
    begin
      if Pos(FWord, Editor.Lines[line]) = 0 then
        continue;

      FHighlighter.ResetRange;
      FHighlighter.SetLine(Editor.Lines[line], line);
      while not FHighlighter.GetEol do
        begin
          if (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkIdentifier)) or
             (FHighlighter.GetTokenKind = Ord(LuaSyntax.tkKey)) then
            begin
              word := FHighlighter.GetToken();
              if word = FWord then
                begin
                  P.Line := line+1;
                  P.Char := FHighlighter.GetTokenPos()+1;
                  AppendLightBlock(false, P);
                end
            end;

          FHighlighter.Next;
        end;
    end;

  ParseDelimText();

  FTextChanged := false;
end;

end.
