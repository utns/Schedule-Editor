unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, ExtCtrls, Buttons,
  PairSplitter, StdCtrls, UMetadata, UFilters, UEditForm, UDBConnection;

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
    SpeedButtonDelete: TSpeedButton;
    SpeedButtonEdit: TSpeedButton;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonAddFilter: TSpeedButton;
    SpeedButtonOK: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddFilterClick(Sender: TObject);
    constructor Create(AName, ACaption: String; ATag: Integer);
    constructor Create(AName, ACaption: String; ATag: Integer; AFilters: array of TPanelFilter;
      AColumn1, AColumn2: Integer; AFilterValue1, AFilterValue2: String);
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonAddFilterClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure SpeedButtonEditClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    //procedure DeleteSQL;
    procedure OpenSQLQuery;
    //procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType; AID: Integer);
    procedure SQLQueryBeforeClose(DataSet: TDataSet);
  private
    MainFilter: TMainFilter;
    CurSortColumn: Integer;
    CurSortType: Integer;
    CellDblClick: Boolean;
    SelectedID: Integer;
  public

  end;

  procedure CreateNewListViewForm(AName, ACaption: String; ATag: Integer);
  procedure CreateNewListViewForm(AName, ACaption: String; ATag: Integer;
    AFilters: array of TPanelFilter; AColumn1, AColumn2: Integer;
    AFilterValue1, AFilterValue2: String);

var
  FormListView: TFormListView;
  ListViewForms: array of TFormListView;

implementation



{$R *.lfm}

{ TFormListView }

constructor TFormListView.Create(AName, ACaption: String; ATag: Integer);
begin
  Inherited Create(Application);
  Caption := ACaption;
  Name := AName;
  Tag := ATag;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.AddStrings(Tables[ATag].CreateSQlQuery(-1, -1, ''));
  SQLQuery.Open;
  SetTableColumns;
  MainFilter := TMainFilter.Create;
  //MainFilter.AddNewFilters(ScrollBox, Tag, SpeedButtonOK);
  Show;
  CurSortType := 0;
  CurSortColumn := -1;
end;

constructor TFormListView.Create(AName, ACaption: String; ATag: Integer;
  AFilters: array of TPanelFilter; AColumn1, AColumn2: Integer; AFilterValue1,
  AFilterValue2: String);
begin
  Inherited Create(Application);
  Caption := ACaption;
  Name := AName;
  Tag := ATag;
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  MainFilter := TMainFilter.Create;
  MainFilter.AddFilter(ScrollBox, Tag, SpeedButtonOK, AColumn1, AFilterValue1);
  MainFilter.AddFilter(ScrollBox, Tag, SpeedButtonOK, AColumn2, AFilterValue2);
  MainFilter.CopyFilters(ScrollBox, Tag, SpeedButtonOK, AFilters);
  SQLQuery.SQL.AddStrings(Tables[ATag].CreateSQlQuery(-1, -1, ''));//MainFilter.CreateSqlFilter));
  SQLQuery.Open;
  SetTableColumns;
  //MainFilter.AddNewFilters(ScrollBox, Tag, SpeedButtonOK);
  SpeedButtonOKClick(Self);
  Show;
  CurSortType := 0;
  CurSortColumn := -1;
end;

procedure TFormListView.DBGridCellClick(Column: TColumn);
begin
  CellDblClick := True;
end;

procedure TFormListView.DBGridDblClick(Sender: TObject);
begin
  if CellDblClick then
    CreateNewEditForm(Self.Tag, ftEdit, SQLQuery.Fields.FieldByName(Tables[Self.Tag].Fields[0].Name).Value);
end;

procedure TFormListView.DBGridTitleClick(Column: TColumn);
begin
  CellDblClick := False;
  Mouse.CursorPos := Point(Mouse.CursorPos.x + 1, Mouse.CursorPos.y);
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
    SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CurSortColumn, CurSortType, MainFilter.CreateSqlFilter));
    MainFilter.SetParams(SQLQuery);
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
  MainFilter.AddNewFilters(ScrollBox, Tag, SpeedButtonOK);
end;

procedure TFormListView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CurForm, i: Integer;
begin
  for i := 0 to High(ListViewForms) do
    if Name = ListViewForms[i].Name then
    begin
      CurForm := i;
      Break;
    end;
  for i := CurForm to High(ListViewForms) - 1 do
    ListViewForms[i] := ListViewForms[i + 1];
  SetLength(ListViewForms, Length(ListViewForms) - 1);
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
          DBGrid.Columns[i].Tag := j;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;

      if (Tables[Tag].Fields[j] is TMyJoinedField) and (DBGrid.Columns[i].Title.Caption =
        UpperCase((Tables[Tag].Fields[j] as TMyJoinedField).JoinedFieldName)) then
        with (Tables[Tag].Fields[j] as TMyJoinedField) do
        begin
          DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          DBGrid.Columns[i].Width := JoinedFieldWidth;
          DBGrid.Columns[i].Visible := JoinedVisible;
          DBGrid.Columns[i].Tag := j;
          DBGrid.Columns[i].Title.ImageIndex := -1;
        end;
    end;
end;

procedure TFormListView.SpeedButtonAddClick(Sender: TObject);
begin
  CreateNewEditForm(Self.Tag, ftAdd, 0);
end;

procedure TFormListView.SpeedButtonAddFilterClick(Sender: TObject);
begin
  MainFilter.AddNewFilters(ScrollBox, Tag, SpeedButtonOK);
end;

procedure TFormListView.SpeedButtonDeleteClick(Sender: TObject);
var
  ButtonSelected, i: Integer;
begin
  DeleteSql(Self.Tag, SQLQuery.Fields.FieldByName(Tables[Self.Tag].Fields[0].Name).Value, SQLQuery);
  OpenSQLQuery;
  {ButtonSelected := MessageDlg('Удалить выбранную запись?', mtConfirmation, mbYesNo, 0);
  if ButtonSelected = mrYes then
  begin
    for i := 0 to High(EditForms) do
      if SQLQuery.Fields.FieldByName(Tables[Self.Tag].Fields[0].Name).Value = EditForms[i].Tag then
      begin
        MessageDlg('Выбранная запись редактируется.', mtError, [mbOK], 0);
        EditForms[i].ShowOnTop;
        Exit;
      end;
    DeleteSQL;
  end;}
end;

procedure TFormListView.SpeedButtonEditClick(Sender: TObject);
begin
  CreateNewEditForm(Self.Tag, ftEdit, SQLQuery.Fields.FieldByName(Tables[Self.Tag].Fields[0].Name).Value);
end;

procedure TFormListView.SpeedButtonOKClick(Sender: TObject);
begin
  if (Length(MainFilter.FFilters) > 1) and (MainFilter.FFilters[0].GetFilterValue = '') then
  begin
      ShowMessage('Установите значение первого фильтра.');
      SpeedButtonOK.Down := False;
  end
  else
  begin
    SpeedButtonOK.Down := True;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(-1, -1, MainFilter.CreateSqlFilter));
    MainFilter.SetParams(SQLQuery);
    SQLQuery.Open;
    SetTableColumns;
  end;
end;

function IsFormOpen(AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 To High(ListViewForms) do
    if (ListViewForms[i].Name = AName) then
    begin
      Result := True;
      ListViewForms[i].ShowOnTop;
      Break;
    end;
end;

procedure CreateNewListViewForm(AName, ACaption: String; ATag: Integer);
begin
  if not(IsFormOpen(AName)) then
  begin
    SetLength(ListViewForms, Length(ListViewForms) + 1);
    ListViewForms[High(ListViewForms)] := TFormListView.Create(AName, ACaption, ATag);
    with ListViewForms[High(ListViewForms)] do
      MainFilter.AddNewFilters(ScrollBox, Tag, SpeedButtonOK);
  end;
end;

procedure CreateNewListViewForm(AName, ACaption: String; ATag: Integer;
  AFilters: array of TPanelFilter; AColumn1, AColumn2: Integer; AFilterValue1,
  AFilterValue2: String);
begin
  if not(IsFormOpen(AName)) then
  begin
    SetLength(ListViewForms, Length(ListViewForms) + 1);
    ListViewForms[High(ListViewForms)] := TFormListView.Create(AName, ACaption, ATag);
    with ListViewForms[High(ListViewForms)] do
    begin
      MainFilter.AddFilter(ScrollBox, Tag, SpeedButtonOK, AColumn1, AFilterValue1);
      MainFilter.AddFilter(ScrollBox, Tag, SpeedButtonOK, AColumn2, AFilterValue2);
      MainFilter.CopyFilters(ScrollBox, Tag, SpeedButtonOK, AFilters);
      SpeedButtonOKClick(Nil);
    end;
  end;
end;

{procedure TFormListView.DeleteSQL;
var
  s, id: String;
begin
  try
    id := IntToStr(SQLQuery.Fields.FieldByName(Tables[Tag].Fields[0].Name).Value);
    with SQLQuery do
    begin
      s := SQL.Text;
      Close;
      with Tables[Self.Tag] do
        SQL.Text := Format('DELETE FROM %s WHERE %s = %s;', [Name, Fields[0].Name, id]);
      ExecSQL;
      DataModuleMain.SQLTransaction.Commit;
      SQL.Clear;
      SQL.AddStrings(s);
      EActivateSQL;
    end;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Нельзя удалить внешний ключ.', mtError, [mbOK], 0);
      SQLQuery.SQL.Text := s;
      OpenSQLQuery;
    end;
  end;
  RefreshEditForms;
end;}

procedure TFormListView.OpenSQLQuery;
begin
  SQLQuery.Open;
  SetTableColumns;
  SQLQuery.Locate(Tables[Tag].Fields[0].Name, SelectedID, []);
end;

procedure TFormListView.SQLQueryBeforeClose(DataSet: TDataSet);
begin
  SelectedID := SQLQuery.Fields.FieldByName(Tables[Tag].Fields[0].Name).Value;
end;

procedure SetLocate(ACurTable, AID: Integer);
var
  i: Integer;
begin
  for i := 0 to High(ListViewForms) do
    if ListViewForms[i].Tag = ACurTable then
    begin
      ListViewForms[i].SQLQuery.Close;
      ListViewForms[i].SelectedID := AID;
      Exit;
    end;
end;

initialization
  ELocate := @SetLocate;

end.

