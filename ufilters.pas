unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, ExtCtrls, StdCtrls, UMetadata, Buttons;

type

  { TMainFilter }

  TMainFilter = Class
  private
    FSpeedButton: TSpeedButton;
    procedure Change(Sender: TObject);
  public
    constructor Create(ASpeedButton: TSpeedButton);
    constructor Free; virtual;
    function GetColumn: Integer; virtual;
    function GetFilterType: String; virtual;
    function GetFilterValue: String; virtual;
  end;

  { TCBColumnName }

  TCBColumnName =class(TMainFilter)
  private
    FComboBoxColumnName: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetColumn: Integer; override;
  end;

  { TCBFilterType }

  TCBFilterType =class(TMainFilter)
  private
    FComboBoxFilterType: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterType: String; override;
  end;

  { TEFilterValue }

  TEFilterValue =class(TMainFilter)
  private
     FEditFilterValue: TEdit;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterValue: String; override;
  end;

implementation

{ TCBColumnName }

constructor TCBColumnName.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
var
  i: Integer;
begin
  Inherited Create(ASpeedButton);
  FComboBoxColumnName := TComboBox.Create(AWinControl);
  with FComboBoxColumnName do
  begin
    Visible := True;
    Width := 80;
    Left := 10;
    Height := 23;
    AWinControl.Tag := AWinControl.Tag + Height + 5;
    Top := AWinControl.Tag;
    Parent := AWinControl;
    ReadOnly := True;
    for i := 0 to Tables[ACurTable].FieldsCount - 1 do
      if Tables[ACurTable].Fields[i] is TMyJoinedField then
        Items.Add((Tables[ACurTable].Fields[i] as TMyJoinedField).JoinedFieldCaption)
      else
        Items.Add(Tables[ACurTable].Fields[i].Caption);
    ItemIndex := 0;
    OnChange := @Change;
  end;
end;

constructor TCBColumnName.Free;
begin
  FComboBoxColumnName.Free;
end;

function TCBColumnName.GetColumn: Integer;
begin
  Result := FComboBoxColumnName.ItemIndex;
end;

{ TCBFilterType }

constructor TCBFilterType.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
var
  i: Integer;
begin
  Inherited Create(ASpeedButton);
  FComboBoxFilterType := TComboBox.Create(AWinControl);
  with FComboBoxFilterType do
  begin
    Width := 80;
    Left := 110;
    Height := 23;
    Top := AWinControl.Tag;
    Parent := AWinControl;
    ReadOnly := True;
    Items.Add('<');
    Items.Add('<=');
    Items.Add('>');
    Items.Add('>=');
    Items.Add('<>');
    Items.Add('=');
    Items.Add('Подстрока');
    ItemIndex := 0;
    OnChange := @Change;
  end;
end;

constructor TCBFilterType.Free;
begin
  FComboBoxFilterType.Free;
end;

function TCBFilterType.GetFilterType: String;
begin
  if FComboBoxFilterType.Items[FComboBoxFilterType.ItemIndex] = 'Подстрока' then
    Result := 'LIKE'
  else
    Result := FComboBoxFilterType.Items[FComboBoxFilterType.ItemIndex];
end;

{ TEFilterValue }
constructor TEFilterValue.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
begin
  Inherited Create(ASpeedButton);
  FEditFilterValue := TEdit.Create(AWinControl);
  with FEditFilterValue do
  begin
    Width := 80;
    Height := 23;
    Left := 200;
    Top := AWinControl.Tag;
    Parent := AWinControl;
    OnChange := @Change;
  end;
end;

constructor TEFilterValue.Free;
begin
  FEditFilterValue.Parent.Tag := FEditFilterValue.Parent.Tag - FEditFilterValue.Height - 5;
  FEditFilterValue.Free;
end;

function TEFilterValue.GetFilterValue: String;
begin
  Result := FEditFilterValue.Text;
end;

{ TMainFilter }

procedure TMainFilter.Change(Sender: TObject);
begin
  FSpeedButton.Down := False;
end;

constructor TMainFilter.Create(ASpeedButton: TSpeedButton);
begin
  FSpeedButton := ASpeedButton;
end;

constructor TMainFilter.Free;
begin

end;

function TMainFilter.GetColumn: Integer;
begin

end;

function TMainFilter.GetFilterType: String;
begin

end;

function TMainFilter.GetFilterValue: String;
begin

end;

end.

