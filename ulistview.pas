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
    ButtonDelete: TButton;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SpeedButtonAddFilter: TSpeedButton;
    SpeedButtonOK: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonAddFilterClick(Sender: TObject);
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    //procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure AddNewFilters;
    procedure SpeedButtonAddFilterClick(Sender: TObject);
    procedure ButtonDeleteFilterMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonOKClick(Sender: TObject);
    function CreateSqlFilter: String;
    procedure SetParams;
    procedure SQLQueryAfterOpen(DataSet: TDataSet);
    procedure DeleteSQL;
  private
    Filters: array of TPanelFilter;
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

procedure TFormListView.ButtonEditClick(Sender: TObject);
begin
  //DBGrid.DataSource := Nil;
  EditForm.CreateNew(Self.Tag, DataSource, ftEdit, SQLQuery);
end;

procedure TFormListView.ButtonAddClick(Sender: TObject);
begin
  EditForm.CreateNew(Self.Tag, DataSource, ftAdd, SQLQuery);
end;

procedure TFormListView.ButtonDeleteClick(Sender: TObject);
var
  ButtonSelected: Integer;
begin
  ButtonSelected := MessageDlg('Удалить выбранную запись?', mtConfirmation, mbYesNo, 0);
  if ButtonSelected = mrYes then
    DeleteSQL;
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
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TPanelFilter.Create(ScrollBox, Tag, SpeedButtonOK, High(Filters));
  if High(Filters) > 0 then
    Filters[High(Filters)].FSBDeleteFilter.FBitBtnDelete.OnMouseUp := @ButtonDeleteFilterMouseUp;
end;

procedure TFormListView.SpeedButtonAddFilterClick(Sender: TObject);
begin
  AddNewFilters;
end;

procedure TFormListView.ButtonDeleteFilterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, PanelTag: Integer;
begin
  PanelTag := (Sender as TBitBtn).Parent.Tag;
  Filters[PanelTag].Free;
  for i := PanelTag to High(Filters) - 1 do
  begin
    Filters[i] := Filters[i + 1];
    Filters[i].Tag := i;
    Filters[i].Top := i * 33;
  end;
  SetLength(Filters, Length(Filters) - 1);
  SpeedButtonOK.Down := False;
end;

procedure TFormListView.SpeedButtonOKClick(Sender: TObject);
begin
  if (Length(Filters) > 1) and (Filters[0].GetFilterValue = '') then
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
  i: Integer;
  s, FilterType: String;
begin
  Result := '';
  with Tables[Tag] do
  begin
    with Filters[0] do
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

    for i := 1 to High(Filters) do
    begin
      with Filters[i] do
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

procedure TFormListView.SetParams;
var
  i: Integer;
  FilterValue: String;
begin
  for i := 0 to High(Filters) do
    with Filters[i] do
    begin
      if GetFilterValue <> '' then
      begin
        case GetFilterType of
          'Substring': FilterValue := '%' + GetFilterValue + '%';
          'Begin': FilterValue := GetFilterValue + '%';
          else FilterValue := GetFilterValue;
        end;
        SQLQuery.ParamByName('p' + IntToStr(i)).AsString := FilterValue;
      end;
    end;
end;

procedure TFormListView.SQLQueryAfterOpen(DataSet: TDataSet);
begin
  SetTableColumns;
end;

procedure TFormListView.DeleteSQL;
var
  s, id: String;
begin
  id := IntToStr(SQLQuery.Fields.FieldByName(Tables[Tag].Fields[0].Name).Value);
  with SQLQuery do
  begin
    s := SQL.Text;
    Close;
    SQL.Clear;
    SQL.AddStrings('DELETE FROM ' + Tables[Self.Tag].Name + ' WHERE ' + Tables[Self.Tag].Fields[0].Name + ' = ' + id + ';' );
    DataModuleMain.SQLTransaction.Commit;
    ExecSQL;
    SQL.Clear;
    SQL.AddStrings(s);
    Open;
  end;

end;

end.

