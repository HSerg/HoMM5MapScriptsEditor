// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit USynEditHelper;

interface

uses
  Controls, SynEdit;

type
  TSynEditHelper = class helper for TSynEdit
  protected
    procedure SetOnContextPopupHack(const Value: TContextPopupEvent);
    function GetOnContextPopupHack: TContextPopupEvent;
  public
    property OnContextPopupHack: TContextPopupEvent read GetOnContextPopupHack write SetOnContextPopupHack;
  end;

implementation

function TSynEditHelper.GetOnContextPopupHack: TContextPopupEvent;
begin
  Result := OnContextPopup;
end;

procedure TSynEditHelper.SetOnContextPopupHack(const Value: TContextPopupEvent);
begin
  OnContextPopup := Value;
end;

end.
