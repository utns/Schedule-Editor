unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, ExtCtrls, Buttons,
  PairSplitter, UMetadata, UFilters;

type

  { TFormListView }

  TFormListView = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SpeedButtonAddFilter: TSpeedButton;
    SpeedButtonDeleteFilter: TSpeedButton;
    SpeedButtonOK: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddFilterClick(Sender: TObject);
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure AddNewFilters;
    procedure SpeedButtonAddFilterClick(Sender: TObject);
    procedure SpeedButtonDeleteFilterClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    function CreateSqlFilter: String;
    procedure SetParams;
  private
    Filters: array of TMainFilter;
    CurSortColumn: Integer;
    CurSortType: Integer;
    { private declarations }
  public
    { public declarations }
  end;

var
  FormListView: TFormListView;

implementation

{$R *.lfm}

{ TFormListView }

procedure TFormListView.CreateNew(AName, ACaption: String; ATag: Integer);
var
  AForm: TFormListView;
begin
  AForm := TFormListView.Create(Application);
  with AForm do
  begin
    Caption := ACaption;
    Name := AName;
    Tag := ATag;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[ATag].CreateSQlQuery(-1, -1, ''));
    SQLQuery.Open;
    SetTableColumns;
    AddNewFilters;
    Show;
    CurSortType := 0;
    CurSortColumn := -1;
  end;
end;

procedure TFormListView.DBGridTitleClick(Column: TColumn);
begin
  Mouse.CursorPos := Point(Mouse.CursorPos.x + 1, Mouse.CursorPos.y);
  if (CurSortColumn <> Column.Index) and (CurSortColumn <> -1) then
  begin
    CurSortColumn := Column.Index;
    CurSortType := 0;
  end;
  if (CurSortColumn = Column.Index) or (CurSortColumn = -1) then
  begin
    CurSortColumn := Column.Index;
    CurSortType := (CurSortType + 1) mod 3;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CurSortColumn, CurSortType, CreateSqlFilter));
    SetParams;
    SQLQuery.Open;
    SetTableColumns;
    DBGrid.Columns[CurSortColumn].Width := DBGrid.Columns[CurSortColumn].Width + 16;
    DBGrid.Columns[CurSortColumn].Title.ImageIndex := CurSortType - 1;
    if CurSortType = 0 then
    begin
      DBGrid.Columns[CurSortColumn].Width := DBGrid.Columns[CurSortColumn].Width - 16;
      CurSortColumn := -1;
    end;
  end;
end;

procedure TFormListView.ButtonAddFilterClick(Sender: TObject);
begin
  AddNewFilters;
end;

procedure TFormListView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormListView.SetTableColumns;
var
  i, j: Integer;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
    for j := 0 to Tables[Tag].FieldsCount - 1 do
    begin
      if DBGrid.Columns[i].Title.Caption = UpperCase(Tables[Tag].Fields[j].Name)  then
        with Tables[Tag].Fields[j] do
        begin
          DBGrid.Columns[i].Title.Caption := Caption;
          DBGrid.Columns[i].Width := Width;
          DBGrid.Columns[i].Visible := Visible;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;

      if (Tables[Tag].Fields[j] is TMyJoinedField) and (DBGrid.Columns[i].Title.Caption =
        UpperCase((Tables[Tag].Fields[j] as TMyJoinedField).JoinedFieldName)) then
        with (Tables[Tag].Fields[j] as TMyJoinedField) do
        begin
          DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          DBGrid.Columns[i].Width := JoinedFieldWidth;
          DBGrid.Columns[i].Visible := JoinedVisible;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;
    end;
end;

procedure TFormListView.AddNewFilters;
begin
  SetLength(Filters, Length(Filters) + 4);
  if Length(Filters) > 4 then
  begin
    SpeedButtonDeleteFilter.Visible := True;
    Filters[High(Filters) - 3] := TCBAndOr.Create(ScrollBox, Tag, SpeedButtonOK);
  end;
  Filters[High(Filters) - 2] := TCBColumnName.Create(ScrollBox, Tag, SpeedButtonOK);
  Filters[High(Filters) - 1] := TCBFilterType.Create(ScrollBox, Tag, SpeedButtonOK);
  Filters[High(Filters)] := TEFilterValue.Create(ScrollBox, Tag, SpeedButtonOK);
  SpeedButtonDeleteFilter.Top := ScrollBox.Tag;
end;

procedure TFormListView.SpeedButtonAddFilterClick(Sender: TObject);
begin
  AddNewFilters;
end;

procedure TFormListView.SpeedButtonDeleteFilterClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Filters[High(Filters) - i].Free;
  SetLength(Filters, Length(Filters) - 4);
  SpeedButtonDeleteFilter.Top := ScrollBox.Tag;
  if Length(Filters) = 4 then
   SpeedButtonDeleteFilter.Visible := False;
  SpeedButtonOK.Down := False;
end;

procedure TFormListView.SpeedButtonOKClick(Sender: TObject);
begin
  if (Length(Filters) > 4) and (Filters[3].GetFilterValue = '') then
  begin
      ShowMessage('Установите значение первого фильтра.');
      SpeedButtonOK.Down := False;
  end
  else
  begin
    SpeedButtonOK.Down := True;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(-1, -1, CreateSqlFilter));
    SetParams;
    SQLQuery.Open;
    SetTableColumns;
  end;
end;

function TFormListView.CreateSqlFilter: String;
var
  i, GroupFilters: Integer;
  s, FilterType: String;
begin
  Result := '';

  s := ' WHERE %s.%s %s :p%d ';
  case Filters[2].GetFilterType of
    'Substring', 'Begin': FilterType := 'LIKE';
    else FilterType := Filters[2].GetFilterType;
  end;

  if Filters[3].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[1].GetColumn] is TMyJoinedField then
        Result += Format(s, [
          (Tables[Tag].Fields[Filters[1].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[1].GetColumn] as TMyJoinedField).JoinedFieldName,
          FilterType, 0])
      else
        Result += Format(s, [Tables[Tag].Name, Tables[Tag].Fields[Filters[1].GetColumn].Name,
          FilterType, 0]);

  for i := 1 to (High(Filters) div 4) do
  begin
    s := ' %s %s.%s %s :p%d';
    GroupFilters := 4 * i;
    case Filters[GroupFilters + 2].GetFilterType of
      'Substring', 'Begin': FilterType := 'LIKE';
      else FilterType := Filters[GroupFilters + 2].GetFilterType;
    end;

    if Filters[GroupFilters + 3].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[GroupFilters + 1].GetColumn] is TMyJoinedField then
        Result += Format(s, [
          Filters[GroupFilters].GetFilterAndOr,
          (Tables[Tag].Fields[Filters[GroupFilters + 1].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[GroupFilters + 1].GetColumn] as TMyJoinedField).JoinedFieldName,
          FilterType, i])
      else
        Result += Format(s, [Filters[GroupFilters].GetFilterAndOr, Tables[Tag].Name,
        Tables[Tag].Fields[Filters[GroupFilters + 1].GetColumn].Name, FilterType, i]);
  end;
end;

procedure TFormListView.SetParams;
var
  i, GroupFilters: Integer;
  FilterValue: String;
begin
  for i := 0 to (High(Filters) div 4) do
  begin
    GroupFilters := 4 * i;
    if Filters[GroupFilters + 3].GetFilterValue <> '' then
    begin
      case Filters[GroupFilters + 2].GetFilterType of
        'Substring': FilterValue := '%' + Filters[GroupFilters + 3].GetFilterValue + '%';
        'Begin': FilterValue := Filters[GroupFilters + 3].GetFilterValue + '%';
        else FilterValue := Filters[GroupFilters + 3].GetFilterValue;
      end;
      SQLQuery.ParamByName('p' + IntToStr(i)).AsString := FilterValue;
    end;
  end;
end;

end.

