unit UDBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Controls;

type

  { TDataModuleMain }

  TDataModuleMain = class(TDataModule)
    IBConnection: TIBConnection;
    ImageList: TImageList;
    SQLTransaction: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{$R *.lfm}

{ TDataModuleMain }

end.
