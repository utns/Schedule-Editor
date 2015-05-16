unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,Graphics, Dialogs, DbCtrls,
  ExtCtrls, StdCtrls, UMetadata, Buttons, UDBConnection, sqldb;

type

  { TBasicFilter }

  TBasicFilter = class
  private
    FSpeedButton: TSpeedButton;
    procedure Change(Sender: TObject);
  public
    constructor Create(ASpeedButton: TSpeedButton);
    constructor Free; virtual;
  end;

  { TCBColumnName }

  TCBColumnName = class(TBasicFilter)
  private
    FComboBoxColumnName: TComboBox;
    FStringList: TStringList;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetColumn: Integer;
  end;

  { TCBFilterType }

  TCBFilterType = class(TBasicFilter)
  private
    FComboBoxFilterType: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterType: String;
  end;

  { TCBAndOr }

  TCBAndOr = class(TBasicFilter)
  private
    FComboBoxAndOr: TComboBox;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterAndOr: String;
  end;

  { TEFilterValue }

  TEFilterValue = class(TBasicFilter)
  private
    FEditFilterValue: TEdit;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ASpeedButton: TSpeedButton);
    constructor Free; override;
    function GetFilterValue: String;
  end;

  { TBBDeleteFilter }

  TBBDeleteFilter = class(TBasicFilter)
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

  { TMainFilter }

  TMainFilter = class
  private

    FCurTable: Integer;
    FSpeedButton: TSpeedButton;
    procedure ButtonDeleteFilterMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    FFilters: array of TPanelFilter;
    procedure AddNewFilters(AWinControl: TWinControl;
      ACurTable: Integer; ASpeedButton: TSpeedButton);
    procedure SetParams(ASQLQuery: TSQLQuery);
    function CreateSqlFilter: String;
    constructor Create;
  end;

implementation

{ TMainFilter }

procedure TMainFilter.ButtonDeleteFilterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, PanelTag: Integer;
begin
  PanelTag := (Sender as TBitBtn).Parent.Tag;
  FFilters[PanelTag].Free;
  for i := PanelTag to High(FFilters) - 1 do
  begin
    FFilters[i] := FFilters[i + 1];
    FFilters[i].Tag := i;
    FFilters[i].Top := i * 33;
  end;
  SetLength(FFilters, Length(FFilters) - 1);
  FSpeedButton.Down := False;
end;

procedure TMainFilter.AddNewFilters(AWinControl: TWinControl; ACurTable: Integer;
  ASpeedButton: TSpeedButton);
begin
  FSpeedButton := ASpeedButton;
  FCurTable := ACurTable;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TPanelFilter.Create(AWinControl, ACurTable, ASpeedButton, High(FFilters));
  if High(FFilters) > 0 then
    FFilters[High(FFilters)].FSBDeleteFilter.FBitBtnDelete.OnMouseUp := @ButtonDeleteFilterMouseUp;
end;

procedure TMainFilter.SetParams(ASQLQuery: TSQLQuery);
var
  i: Integer;
  FilterValue: String;
begin
  for i := 0 to High(FFilters) do
    with FFilters[i] do
    begin
      if GetFilterValue <> '' then
      begin
        case GetFilterType of
          'Substring': FilterValue := '%' + GetFilterValue + '%';
          'Begin': FilterValue := GetFilterValue + '%';
          else FilterValue := GetFilterValue;
        end;
        ASQLQuery.ParamByName('p' + IntToStr(i)).AsString := FilterValue;
      end;
    end;
end;

function TMainFilter.CreateSqlFilter: String;
var
  i: Integer;
  s, FilterType: String;
begin
  Result := '';
  with Tables[FCurTable] do
  begin
    with FFilters[0] do
    begin
      s := ' WHERE %s.%s %s :p%d ';
      case GetFilterType of
       'Substring', 'Begin': FilterType := 'LIKE';
       else FilterType := GetFilterType;
      end;

      if GetFilterValue <> '' then
        if Fields[GetColumn] is TMyJoinedField then
          Result += Format(s, [(Fields[GetColumn] as TMyJoinedField).ReferencedTable,
            (Fields[GetColumn] as TMyJoinedField).JoinedFieldName,
            FilterType, 0])
        else
          Result += Format(s, [Name, Fields[GetColumn].Name, FilterType, 0]);
    end;

    for i := 1 to High(FFilters) do
    begin
      with FFilters[i] do
      begin
        s := ' %s %s.%s %s :p%d';
        case GetFilterType of
          'Substring', 'Begin': FilterType := 'LIKE';
          else FilterType := GetFilterType;
        end;

        if GetFilterValue <> '' then
          if Fields[GetColumn] is TMyJoinedField then
            Result += Format(s, [GetFilterAndOr,
              (Fields[GetColumn] as TMyJoinedField).ReferencedTable,
              (Fields[GetColumn] as TMyJoinedField).JoinedFieldName,
              FilterType, i])
          else
            Result += Format(s, [GetFilterAndOr, Name, Fields[GetColumn].Name,
              FilterType, i]);
      end;
    end;
  end;
end;

constructor TMainFilter.Create;
begin
  inherited;
  SetLength(FFilters, 0);
end;

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
    PNG := TPortableNetworkGraphic.Create;
    BMP := TBitmap.Create;
    PNG.LoadFromFile('Icons/Remove.png');
    BMP.Assign(PNG);
    Glyph := BMP;
    Spacing := 0;
    Layout := blGlyphTop;
    BMP.Free;
    PNG.Free;
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
  FStringList := TStringList.Create;
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
        if (Fields[i] is TMyJoinedField) and ((Fields[i] as TMyJoinedField).JoinedVisible) then
        begin
          Items.Add((Fields[i] as TMyJoinedField).JoinedFieldCaption);
          FStringList.Append(IntToStr(i));
        end
        else
          if Fields[i].Visible then
          begin
            Items.Add(Fields[i].Caption);
            FStringList.Append(IntToStr(i));
          end;
    ItemIndex := 0;
    OnChange := @Change;
  end;
end;

constructor TCBColumnName.Free;
begin
  FComboBoxColumnName.Free;
  FStringList.Free;
end;

function TCBColumnName.GetColumn: Integer;
begin
  Result := StrToInt(FStringList.Strings[FComboBoxColumnName.ItemIndex]);
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

{ TBasicFilter }

procedure TBasicFilter.Change(Sender: TObject);
begin
  FSpeedButton.Down := False;
end;

constructor TBasicFilter.Create(ASpeedButton: TSpeedButton);
begin
  FSpeedButton := ASpeedButton;
end;

constructor TBasicFilter.Free;
begin

end;

end.

