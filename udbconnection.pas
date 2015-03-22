unit UDBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil;

type

  { TDataModuleMain }

  TDataModuleMain = class(TDataModule)
    IBConnection: TIBConnection;
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
