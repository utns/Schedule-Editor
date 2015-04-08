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
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddFilterClick(Sender: TObject);
    procedure ButtonDeleteFilterClick(Sender: TObject);
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure AddCBItem;
    procedure AddNewFilters;
    procedure SpeedButton1Click(Sender: TObject);
  private
    Filters: array of TMainFilter;
    CurSortColumn: Integer;
    CurSortValue: Integer;
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
    CurSortValue := 0;
    CurSortColumn := -1;
  end;
end;

procedure TFormListView.DBGridTitleClick(Column: TColumn);
var
  CurColTag: Integer;
begin
  if (CurSortColumn <> Column.Tag) and (CurSortColumn <> -1) then
  begin
    CurSortColumn := Column.Tag;
    CurSortValue := 0;
  end;
  if (CurSortColumn = Column.Tag) or (CurSortColumn = -1) then
  begin
    CurSortColumn := Column.Tag;
    CurSortValue := (CurSortValue + 1) mod 3;
    CurColTag := Column.Tag;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CurSortColumn, CurSortValue, ''));
    SQLQuery.Open;
    SetTableColumns;
    DBGrid.Columns[CurSortColumn].Width := DBGrid.Columns[CurSortColumn].Width + 16;
    DBGrid.Columns[CurSortColumn].Title.ImageIndex := CurSortValue - 1;
    if CurSortValue = 0 then
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
  ButtonAddFilter.Top := ScrollBox1.Tag;
  ButtonDeleteFilter.Top := ScrollBox1.Tag;
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
          //ShowMessage(IntToStr(j) +'|'+IntToStr(i)+Caption);
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
          //ShowMessage(IntToStr(j) +'|'+IntToStr(i)+JoinedFieldCaption);
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
  Filters[High(Filters) - 2] := TCBColumnName.Create(ScrollBox1, Tag);
  Filters[High(Filters) - 1] := TCBFilterType.Create(ScrollBox1, Tag);
  Filters[High(Filters)] := TEFilterValue.Create(ScrollBox1, Tag);
  ButtonAddFilter.Top := ScrollBox1.Tag;
  ButtonDeleteFilter.Top := ScrollBox1.Tag;
end;

procedure TFormListView.SpeedButton1Click(Sender: TObject);
var
  i, a: Integer;
  s: String;
begin
  if Filters[2].GetFilterValue <> '' then
    if Tables[Tag].Fields[Filters[0].GetColumn] is TMyJoinedField then
      s := Format(' WHERE CAST(%s.%s as VARCHAR(100))%s''%s'' ', [(Tables[Tag].Fields[Filters[0].GetColumn] as TMyJoinedField).ReferencedTable,
        (Tables[Tag].Fields[Filters[0].GetColumn] as TMyJoinedField).JoinedFieldName,
        Filters[1].GetFilterType, Filters[2].GetFilterValue])
    else
      //s := Format(' WHERE CAST(%s.%s as VARCHAR(100)) %s ', [Tables[Tag].Name, Tables[Tag].Fields[Filters[0].GetColumn].Name, ':p']);
      s := Format(' WHERE CAST(%s.%s as VARCHAR(100))%s''%s'' ', [Tables[Tag].Name, Tables[Tag].Fields[Filters[0].GetColumn].Name,
        Filters[1].GetFilterType, Filters[2].GetFilterValue]);
  for i := 1 to (High(Filters) div 3) do
  begin
    a := 3 * i;
    if Filters[a + 2].GetFilterValue <> '' then
      if Tables[Tag].Fields[Filters[a].GetColumn] is TMyJoinedField then
        s += Format(' AND CAST(%s.%s as VARCHAR(100))%s''%s'' ', [(Tables[Tag].Fields[Filters[a].GetColumn] as TMyJoinedField).ReferencedTable,
          (Tables[Tag].Fields[Filters[a].GetColumn] as TMyJoinedField).JoinedFieldName,
          Filters[a + 1].GetFilterType, Filters[a + 2].GetFilterValue])
      else
        s += Format(' AND CAST(%s.%s as VARCHAR(100))%s''%s'' ', [Tables[Tag].Name, Tables[Tag].Fields[Filters[a].GetColumn].Name,
          Filters[a + 1].GetFilterType, Filters[a + 2].GetFilterValue]);
  end;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CBOrderBy.ItemIndex, CBTypeOfSort.ItemIndex + 1, s));//' WHERE :p1 :p2 :p3 '));
  ShowMessage(SQLQuery.SQL.Text);
  //SQLQuery.Prepare;
  //SQLQuery.ParamByName('p1').AsString := Filters[1].GetFilterType;
  //SQLQuery.ParamByName('p').AsString := Filters[1].GetFilterType + Filters[2].GetFilterValue;
  //ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.Open;
  SetTableColumns;
end;

end.

