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
    ButtonDeleteFilter: TButton;
    ButtonAddFilter: TButton;
    CBTypeOfSort: TComboBox;
    CBOrderBy: TComboBox;
    ComboBox1: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    Edit1: TEdit;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SpeedButtonOK: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddFilterClick(Sender: TObject);
    procedure ButtonDeleteFilterClick(Sender: TObject);
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure AddCBItem;
    procedure AddNewFilters;
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
    AddCBItem;
    AddNewFilters;
    Show;
    CurSortType := 0;
    CurSortColumn := -1;
  end;
end;

procedure TFormListView.DBGridTitleClick(Column: TColumn);
begin
  if (CurSortColumn <> Column.Tag) and (CurSortColumn <> -1) then
  begin
    CurSortColumn := Column.Tag;
    CurSortType := 0;
  end;
  if (CurSortColumn = Column.Tag) or (CurSortColumn = -1) then
  begin
    CurSortColumn := Column.Tag;
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
      CurSortColumn := -1;
  end;
end;

procedure TFormListView.ButtonAddFilterClick(Sender: TObject);
begin
  AddNewFilters;
end;

procedure TFormListView.ButtonDeleteFilterClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 2 do
    Filters[High(Filters) - i].Free;
  SetLength(Filters, Length(Filters) - 3);
  ButtonAddFilter.Top := ScrollBox.Tag;
  ButtonDeleteFilter.Top := ScrollBox.Tag;
  if Length(Filters) = 3 then
   ButtonDeleteFilter.Visible := False;
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
          DBGrid.Columns[i].Tag := j;
          DBGrid.Columns[i].Visible := Visible;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;

        if (Tables[Tag].Fields[j] is TMyJoinedField) and (DBGrid.Columns[i].Title.Caption =
          UpperCase((Tables[Tag].Fields[j] as TMyJoinedField).JoinedFieldName)) then
        with (Tables[Tag].Fields[j] as TMyJoinedField) do
        begin
          DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          DBGrid.Columns[i].Tag := j;
          DBGrid.Columns[i].Width := JoinedFieldWidth;
          DBGrid.Columns[i].Visible := JoinedVisible;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;
    end;
end;

procedure TFormListView.AddCBItem;
var
  i: Integer;
begin
  for i := 0 to Tables[Tag].FieldsCount - 1 do
    if Tables[Tag].Fields[i] is TMyJoinedField then
      CBOrderBy.Items.Add((Tables[Tag].Fields[i] as TMyJoinedField).JoinedFieldCaption)
    else
      CBOrderBy.Items.Add(Tables[Tag].Fields[i].Caption);
end;

procedure TFormListView.AddNewFilters;
begin
  SetLength(Filters, Length(Filters) + 3);
  if Length(Filters) > 3 then
    ButtonDeleteFilter.Visible := True;
  Filters[High(Filters) - 2] := TCBColumnName.Create(ScrollBox, Tag, SpeedButtonOK);
  Filters[High(Filters) - 1] := TCBFilterType.Create(ScrollBox, Tag, SpeedButtonOK);
  Filters[High(Filters)] := TEFilterValue.Create(ScrollBox, Tag, SpeedButtonOK);
  ButtonAddFilter.Top := ScrollBox.Tag;
  ButtonDeleteFilter.Top := ScrollBox.Tag;
end;

procedure TFormListView.SpeedButtonOKClick(Sender: TObject);
begin
  SpeedButtonOK.Down := True;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CBOrderBy.ItemIndex, CBTypeOfSort.ItemIndex + 1, CreateSqlFilter));
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
  for i := 0 to (High(Filters) div 3) do
  begin
    if i = 0 then
      curs := ' WHERE %s.%s %s :p%d '
    else
      curs := ' AND %s.%s %s :p%d';
    a := 3 * i;
    //if Filters[a + 1].GetFilterType = 'LIKE' then
      //curs := ' AND %s.%s %s :p%d';

    if Filters[a + 2].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[a].GetColumn] is TMyJoinedField then
        Result += Format(curs, [
          (Tables[Tag].Fields[Filters[a].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[a].GetColumn] as TMyJoinedField).JoinedFieldName,
          Filters[a + 1].GetFilterType,a])
      else
        Result += Format(curs, [Tables[Tag].Name, Tables[Tag].Fields[Filters[a].GetColumn].Name,
          Filters[a + 1].GetFilterType, a]);
  end;
end;

procedure TFormListView.SetParams;
var
  i, a: Integer;
  s: String;
begin
  for i := 0 to (High(Filters) div 3) do
  begin
    a := 3 * i;
    if Filters[a + 1].GetFilterType = 'LIKE' then
      s := '%' + Filters[a + 2].GetFilterValue + '%'
    else
      s := Filters[a + 2].GetFilterValue;
    if Filters[a + 2].GetFilterValue <> '' then
      SQLQuery.ParamByName('p' + IntToStr(a)).AsString := s;
  end;
end;

end.

