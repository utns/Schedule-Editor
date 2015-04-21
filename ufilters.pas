unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, ExtCtrls, StdCtrls, UMetadata, Buttons, UDBConnection;

type

  { TMainFilter }

  TMainFilter = class
  private
    FSpeedButton: TSpeedButton;
    procedure Change(Sender: TObject);
  public
    constructor Create(ASpeedButton: TSpeedButton);
    constructor Free; virtual;
  end;

  { TCBColumnName }

  TCBColumnName = class(TMainFilter)
  private
    FComboBoxColumnName: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetColumn: Integer;
  end;

  { TCBFilterType }

  TCBFilterType = class(TMainFilter)
  private
    FComboBoxFilterType: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterType: String;
  end;

  { TCBAndOr }

  TCBAndOr = class(TMainFilter)
  private
    FComboBoxAndOr: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterAndOr: String;
  end;

  { TEFilterValue }

  TEFilterValue = class(TMainFilter)
  private
    FEditFilterValue: TEdit;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterValue: String;
  end;

  { TBBDeleteFilter }

  TBBDeleteFilter = class(TMainFilter)
  public
    FBitBtnDelete: TBitBtn;
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    Constructor Free; override;
  end;

  { TPanelFilter }

  TPanelFilter = class
  private
    FPanel: TPanel;
    FCBColumnName: TCBColumnName;
    FCBFilterType: TCBFilterType;
    FCBAndOr: TCBAndOr;
    FEFilterValue: TEFilterValue;
    procedure SetTag(AValue: PtrInt);
    procedure SetTop(AValue: Integer);
  public
    FSBDeleteFilter: TBBDeleteFilter;
    Constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton; ATag: Integer);
    Constructor Free;
    function GetColumn: Integer;
    function GetFilterType: String;
    function GetFilterValue: String;
    function GetFilterAndOr: String;
    property Tag: PtrInt write SetTag;
    property Top: Integer write SetTop;
  end;

implementation

{ TPanelFilter }

procedure TPanelFilter.SetTag(AValue: PtrInt);
begin
  FPanel.Tag := AValue;
end;

procedure TPanelFilter.SetTop(AValue: Integer);
begin
  FPanel.Top := AValue;
end;

constructor TPanelFilter.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton; ATag: Integer);
begin
  FPanel := TPanel.Create(AWinControl);
  with FPanel do
  begin
    Width := 445;
    Left := 0;
    Height := 33;
    Parent := AWinControl;
    Tag := ATag;
    Top := Tag * Height;
  end;
  if ATag > 0 then
  begin
    FCBAndOr := TCBAndOr.Create(FPanel, ACurTable, ASpeedButton);
    FSBDeleteFilter := TBBDeleteFilter.Create(FPanel, ACurTable, ASpeedButton);
  end;
  FCBColumnName := TCBColumnName.Create(FPanel, ACurTable, ASpeedButton);
  FCBFilterType := TCBFilterType.Create(FPanel, ACurTable, ASpeedButton);
  FEFilterValue := TEFilterValue.Create(FPanel, ACurTable, ASpeedButton);
end;

constructor TPanelFilter.Free;
begin
  FPanel.Free;
end;

function TPanelFilter.GetColumn: Integer;
begin
  Result := FCBColumnName.GetColumn;
end;

function TPanelFilter.GetFilterType: String;
begin
  Result := FCBFilterType.GetFilterType;
end;

function TPanelFilter.GetFilterValue: String;
begin
  Result := FEFilterValue.GetFilterValue;
end;

function TPanelFilter.GetFilterAndOr: String;
begin
  Result := FCBAndOr.GetFilterAndOr;
end;

{ TBBDeleteFilter }

constructor TBBDeleteFilter.Create(AWinControl: TWinControl;
  ACurTable: Integer; ASpeedButton: TSpeedButton);
var
  PNG: TPortableNetworkGraphic;
  BMP: TBitmap;
begin
  Inherited Create(ASpeedButton);
  FBitBtnDelete := TBitBtn.Create(AWinControl);
  with FBitBtnDelete do
  begin
    Width := 25;
    Left := 410;
    Height := 23;
    Visible := True;
    {PNG := TPortableNetworkGraphic.Create;
    BMP := TBitmap.Create;
    PNG.LoadFromFile('Icons\Remove.png');
    BMP.Assign(PNG);
    Glyph := BMP;
    Spacing := 0;
    Layout := blGlyphTop;
    BMP.Free;
    PNG.Free; }
    Parent := AWinControl;
    Top := 5;
    Hint := 'Удалить фильтр';
    ShowHint := True;
  end;
end;

constructor TBBDeleteFilter.Free;
begin
  FBitBtnDelete.Free;
end;

{ TCBAndOr }

constructor TCBAndOr.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
begin
  Inherited Create(ASpeedButton);
  FComboBoxAndOr := TComboBox.Create(AWinControl);
  with FComboBoxAndOr do
  begin
    Width := 50;
    Left := 10;
    Height := 23;
    Top := 5;
    Parent := AWinControl;
    ReadOnly := True;
    Items.Add('И');
    Items.Add('Или');
    ItemIndex := 0;
    OnChange := @Change;
  end;
end;

constructor TCBAndOr.Free;
begin
  FComboBoxAndOr.Free;
end;

function TCBAndOr.GetFilterAndOr: String;
begin
  if FComboBoxAndOr.ItemIndex = 0 then
    Result := 'AND'
  else
    Result := 'OR';
end;

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
    Width := 90;
    Left := 65;
    Height := 23;
    Top := 5;
    Parent := AWinControl;
    ReadOnly := True;
    with Tables[ACurTable] do
      for i := 0 to FieldsCount - 1 do
        if Fields[i] is TMyJoinedField then
          Items.Add((Fields[i] as TMyJoinedField).JoinedFieldCaption)
        else
          Items.Add(Fields[i].Caption);
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
begin
  Inherited Create(ASpeedButton);
  FComboBoxFilterType := TComboBox.Create(AWinControl);
  with FComboBoxFilterType do
  begin
    Width := 90;
    Left := 160;
    Height := 23;
    Top := 5;
    Parent := AWinControl;
    ReadOnly := True;
    Items.Add('<');
    Items.Add('<=');
    Items.Add('>');
    Items.Add('>=');
    Items.Add('<>');
    Items.Add('=');
    Items.Add('Содержит');
    Items.Add('Начинается с');
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
  case FComboBoxFilterType.Items[FComboBoxFilterType.ItemIndex] of
    'Содержит': Result := 'Substring';
    'Начинается с': Result := 'Begin';
    else Result := FComboBoxFilterType.Items[FComboBoxFilterType.ItemIndex];
  end
end;

{ TEFilterValue }
constructor TEFilterValue.Create(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
begin
  Inherited Create(ASpeedButton);
  FEditFilterValue := TEdit.Create(AWinControl);
  with FEditFilterValue do
  begin
    Width := 150;
    Height := 23;
    Left := 255;
    Top := 5;
    Parent := AWinControl;
    OnChange := @Change;
  end;
end;

constructor TEFilterValue.Free;
begin
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

end.

