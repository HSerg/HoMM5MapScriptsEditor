// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UCommonTabbedEditor;

interface

uses
  Forms, Classes;

type
  ICommonTabbedEditor = interface
    ['{91B435F3-0B76-42B6-ADCF-EC6676AA154B}']

    function getTitle(): string;
    function getFullFileName(): string;
    function getImageIndex(): integer;
    function getModified(): boolean;

    function getFrame: TFrame;

    function getChangeModified(): TNotifyEvent;
    procedure setChangeModified(ANotifyEvent: TNotifyEvent);
    property OnChangeModified: TNotifyEvent read getChangeModified write setChangeModified;

    function SaveChanges(): boolean;
  end;

implementation

end.
