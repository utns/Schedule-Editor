unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, ExtCtrls, StdCtrls, UMetadata;

type

  { TMainFilter }

  TMainFilter = Class
  public
    //constructor Create(AWinControl: TWinControl; ACurTable: Integer);
    constructor Free; virtual;
    function GetColumn: Integer; virtual;
    function GetFilterType: String; virtual;
    function GetFilterValue: String; virtual;
  end;

  { TCBColumnName }

  TCBColumnName =class(TMainFilter)
  private
    ComboBoxColumnName: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer);
    constructor Free; override;
    function GetColumn: Integer; override;
  end;

  { TCBFilterType }

  TCBFilterType =class(TMainFilter)
  private
    ComboBoxFilterType: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer);
    constructor Free; override;
    function GetFilterType: String; override;
  end;

  { TEFilterValue }

  TEFilterValue =class(TMainFilter)
  private
     EditFilterValue: TEdit;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer);
    constructor Free; override;
    function GetFilterValue: String; override;
  end;

implementation

{ TCBColumnName }

constructor TCBColumnName.Create(AWinControl: TWinControl; ACurTable: Integer);
var
  i: Integer;
begin
  ComboBoxColumnName := TComboBox.Create(AWinControl);
  with ComboBoxColumnName do
  begin
    Visible := True;
    Width := 80;
    Left := 10;
    Height := 23;
    AWinControl.Tag := AWinControl.Tag + Height + 5;
    Top := AWinControl.Tag;
    Parent := AWinControl;
    for i := 0 to Tables[ACurTable].FieldsCount - 1 do
      if Tables[ACurTable].Fields[i] is TMyJoinedField then
        Items.Add((Tables[ACurTable].Fields[i] as TMyJoinedField).JoinedFieldCaption)
      else
        Items.Add(Tables[ACurTable].Fields[i].Caption);
    ItemIndex := 0;
  end;
end;

constructor TCBColumnName.Free;
begin
  ComboBoxColumnName.Free;
end;

function TCBColumnName.GetColumn: Integer;
begin
  Result := ComboBoxColumnName.ItemIndex;
end;

{ TCBFilterType }

constructor TCBFilterType.Create(AWinControl: TWinControl; ACurTable: Integer);
var
  i: Integer;
begin
  ComboBoxFilterType := TComboBox.Create(AWinControl);
  with ComboBoxFilterType do
  begin
    Width := 80;
    Left := 110;
    Height := 23;
    Top := AWinControl.Tag;
    Parent := AWinControl;
    Items.Add('<');
    Items.Add('<=');
    Items.Add('>');
    Items.Add('>=');
    Items.Add('<>');
    Items.Add('=');
    ItemIndex := 0;
  end;
end;

constructor TCBFilterType.Free;
begin
  ComboBoxFilterType.Free;
end;

function TCBFilterType.GetFilterType: String;
begin
  Result := ComboBoxFilterType.Items[ComboBoxFilterType.ItemIndex];
end;

{ TEFilterValue }

constructor TEFilterValue.Create(AWinControl: TWinControl; ACurTable: Integer);
begin
  EditFilterValue := TEdit.Create(AWinControl);
  with EditFilterValue do
  begin
    Width := 80;
    Height := 23;
    Left := 200;
    Top := AWinControl.Tag;
    Parent := AWinControl;
  end;
end;

constructor TEFilterValue.Free;
begin
  EditFilterValue.Parent.Tag := EditFilterValue.Parent.Tag - EditFilterValue.Height - 5;
  EditFilterValue.Free;
end;

function TEFilterValue.GetFilterValue: String;
begin
  Result := EditFilterValue.Text;
end;

{ TMainFilter }

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

