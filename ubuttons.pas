unit UButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UEditForm, Dialogs, UMetadata, Grids;

type

  TEOnClick = procedure (ACol, ARow, ARecord: Integer) of object;

  { TMainButton }

  TMainButton = class
  private
    FImage: TPortableNetworkGraphic;
    FX, FY, FCol, FRow, FRecord: Integer;
  public
    OnClick: TEOnClick;
    constructor Create(ACol, ARow, ARecord: Integer);
    procedure CheckClick(AX, AY: Integer);
  end;

  { TEditButton }

  TEditButton = class(TMainButton)
  public
    constructor Create(ACol, ARow, ARecord: Integer);
    procedure Print(ACanvas: TCanvas; AX, AY: Integer);
  end;

  { TExpandButton }

  TExpandButton = class(TMainButton)
  public
    constructor Create(ACol, ARow, ARecord: Integer);
    procedure Print(ACanvas: TCanvas; AX, AY: Integer);
  end;

  { TDeleteButton }

  TDeleteButton = class(TMainButton)
  public
    constructor Create(ACol, ARow, ARecord: Integer);
    procedure Print(ACanvas: TCanvas; AX, AY: Integer);
  end;

implementation

{ TDeleteButton }

constructor TDeleteButton.Create(ACol, ARow, ARecord: Integer);
begin
  inherited Create(ACol, ARow, ARecord);
  FImage.LoadFromFile('Icons/SDelete.png');
end;

procedure TDeleteButton.Print(ACanvas: TCanvas; AX, AY: Integer);
begin
  FX := AX - (FImage.Width + 2) * 2;
  FY := AY + 2;
  ACanvas.Draw(FX, FY, FImage);
end;

{ TExpandButton }

constructor TExpandButton.Create(ACol, ARow, ARecord: Integer);
begin
  inherited Create(ACol, ARow, ARecord);
  FImage.LoadFromFile('Icons/SExpand.png');
end;

procedure TExpandButton.Print(ACanvas: TCanvas; AX, AY: Integer);
begin
  FX := AX - FImage.Width - 2;
  FY := AY - FImage.Height - 2;
  ACanvas.Draw(FX, FY, FImage);
end;

{ TEditButton }

constructor TEditButton.Create(ACol, ARow, ARecord: Integer);
begin
  inherited Create(ACol, ARow, ARecord);
  FImage.LoadFromFile('Icons/SEdit.png');
end;

procedure TEditButton.Print(ACanvas: TCanvas; AX, AY: Integer);
begin
  FX := AX - FImage.Width - 2;
  FY := AY + 2;
  ACanvas.Draw(FX, FY, FImage);
end;

{ TMainButton }

constructor TMainButton.Create(ACol, ARow, ARecord: Integer);
begin
  FImage := TPortableNetworkGraphic.Create;
  FCol := ACol;
  FRow := ARow;
  FRecord := ARecord;
end;

procedure TMainButton.CheckClick(AX, AY: Integer);
begin
  if (AX >= FX) and (AX <= FX + FImage.Width)
  and (AY >= FY) and (AY <= FY + FImage.Height) then
    OnClick(FCol, FRow, FRecord);
end;

end.

