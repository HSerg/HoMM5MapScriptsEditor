// Copyright (c) 2004-2016 Sergey Klochkov. Contacts: <hserg@sklabs.ru>
// License: http://opensource.org/licenses/GPL-3.0
// The project web site is located on http://hmm5.sklabs.ru

unit UCommonDM;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TCommonDM = class(TDataModule)
    imglstXML: TImageList;
    imlActions: TImageList;
    imglstFileIcons: TImageList;
    imglstFileOverlayIcons: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CommonDM: TCommonDM;

implementation

{$R *.dfm}

end.
