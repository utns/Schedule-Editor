unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, ExtCtrls, StdCtrls, Buttons,
  PairSplitter, UMetadata, USQLQueries, UFilters;

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
          //DBGrid.Columns[i].Tag := j;
          DBGrid.Columns[i].Visible := Visible;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;

        if (Tables[Tag].Fields[j] is TMyJoinedField) and (DBGrid.Columns[i].Title.Caption =
          UpperCase((Tables[Tag].Fields[j] as TMyJoinedField).JoinedFieldName)) then
        with (Tables[Tag].Fields[j] as TMyJoinedField) do
        begin
          DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          //DBGrid.Columns[i].Tag := j;
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
  SpeedButtonAddFilter.Top := ScrollBox.Tag;
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
  SpeedButtonAddFilter.Top := ScrollBox.Tag;
  SpeedButtonDeleteFilter.Top := ScrollBox.Tag;
  if Length(Filters) = 4 then
   SpeedButtonDeleteFilter.Visible := False;
end;

procedure TFormListView.SpeedButtonOKClick(Sender: TObject);
begin
  SpeedButtonOK.Down := True;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(-1, -1, CreateSqlFilter));
  SetParams;
  SQLQuery.Open;
  SetTableColumns;
end;

function TFormListView.CreateSqlFilter: String;
var
  i, a: Integer;
  curs: String;
begin
  Result := '';
  curs := ' WHERE %s.%s %s :p%d ';
  if Filters[3].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[1].GetColumn] is TMyJoinedField then
        Result += Format(curs, [
          (Tables[Tag].Fields[Filters[1].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[1].GetColumn] as TMyJoinedField).JoinedFieldName,
          Filters[2].GetFilterType, a])
      else
        Result += Format(curs, [Tables[Tag].Name, Tables[Tag].Fields[Filters[a + 1].GetColumn].Name,
          Filters[a + 2].GetFilterType, a]);
  for i := 1 to (High(Filters) div 4) do
  begin
    curs := ' %s %s.%s %s :p%d';
    a := 4 * i;
    if Filters[a + 3].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[a + 1].GetColumn] is TMyJoinedField then
        Result += Format(curs, [
          Filters[a].GetFilterAndOr,
          (Tables[Tag].Fields[Filters[a + 1].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[a + 1].GetColumn] as TMyJoinedField).JoinedFieldName,
          Filters[a + 2].GetFilterType, a])
      else
        Result += Format(curs, [Tables[Tag].Name, Tables[Tag].Fields[Filters[a + 1].GetColumn].Name,
          Filters[a + 2].GetFilterType, a]);
  end;
end;

procedure TFormListView.SetParams;
var
  i, a: Integer;
  s: String;
begin
  for i := 0 to (High(Filters) div 4) do
  begin
    a := 4 * i;
    if Filters[a + 2].GetFilterType = 'LIKE' then
      s := '%' + Filters[a + 3].GetFilterValue + '%'
    else
      s := Filters[a + 3].GetFilterValue;
    if Filters[a + 3].GetFilterValue <> '' then
      SQLQuery.ParamByName('p' + IntToStr(a)).AsString := s;
  end;
end;

end.

