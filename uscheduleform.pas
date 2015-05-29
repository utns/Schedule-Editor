unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, StdCtrls, Buttons, CheckLst, DbCtrls, Menus, UMetadata, UFilters,
  math, UButtons, UEditForm, UListView, UGrid, UDBConnection;

type

  { TScheduleForm }

  TMyCaption = record
    Name: String;
    ID: Integer;
  end;

  TDragDrop = record
    DType: (dtOne, dtAll);
    RecordNum: Integer;
  end;

  TCaptionsArray = array of TMyCaption;

  TScheduleForm = class(TForm)
    CBColName: TComboBox;
    CBRowName: TComboBox;
    CheckListBox: TCheckListBox;
    CBSortType: TComboBox;
    CBOrderBy: TComboBox;
    DrawGrid: TDrawGrid;
    LabelOrderBy: TLabel;
    LabelColName: TLabel;
    LabelRowName: TLabel;
    MainMenu: TMainMenu;
    MIHideEmptyCol: TMenuItem;
    MIFieldName: TMenuItem;
    MIHideEmptyRow: TMenuItem;
    MISettings: TMenuItem;
    Panel: TPanel;
    ScrollBox: TScrollBox;
    SpeedButtonOK: TSpeedButton;
    SpeedButtonAdd: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ComboBoxChange(Sender: TObject);
    procedure CheckListBoxItemClick(Sender: TObject; Index: integer);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemHideEmptyClick(Sender: TObject);
    procedure MenuItemFieldNameClick(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FillDrawGrid;
    procedure DGEditButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGExpandButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGDeleteButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGTableButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGAddButtonClick(ACol, ARow, ARecord: Integer);
    procedure HideEmptyColRow;
    function OrderBy: string;
    procedure SetColWidth;
    procedure SetRowHeight;
    procedure SetCaptions(AIndex: Integer; var ACaptions: TCaptionsArray);
    procedure FillGrid;
    procedure SQLUpdate(ACol, ARow: Integer);
  private
    MainFilter: TMainFilter;
    RecordHeight, MouseClickCol, MouseClickRow, CurColIndex, CurRowIndex,
      CurOrderByIndex: Integer;
    ColCaptions, RowCaptions: array of TMyCaption;
    Grid: array of array of TMyCell;
    IsMouseMove: Boolean;
    DragAndDrop: TDragDrop;
    { private declarations }
  public
    { public declarations }
  end;

var
  ScheduleForm: TScheduleForm;

const
  CurTable = 8;

implementation

{$R *.lfm}


{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  with Tables[CurTable] do
    for i := 1 to FieldsCount - 1 do
      if (Fields[i] is TMyJoinedField) then
      begin
        with Fields[i] as TMyJoinedField do
        begin
          CBColName.Items.Add(JoinedFieldCaption);
          CBRowName.Items.Add(JoinedFieldCaption);
          CBOrderBy.Items.Add(JoinedFieldCaption);
          CheckListBox.Items.Add(JoinedFieldCaption);
        end;
        CheckListBox.Checked[i - 1] := True;
      end;
  CBColName.ItemIndex := 0;
  CBRowName.ItemIndex := 0;
  CBOrderBy.ItemIndex := 0;
  DrawGrid.FocusRectVisible := False;
  MainFilter := TMainFilter.Create;
  MainFilter.AddNewFilters(ScrollBox, CurTable, SpeedButtonOK);
  SpeedButtonOKClick(Self);
end;

procedure TScheduleForm.MenuItemHideEmptyClick(Sender: TObject);
begin
  HideEmptyColRow;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.MenuItemFieldNameClick(Sender: TObject);
begin
  FillDrawGrid;
  SetColWidth;
  HideEmptyColRow;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ACol, ARow: Integer;
begin
  IsMouseMove := True;
  with DrawGrid do
  begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol = 0) or (ARow = 0) then
      Hint := ''
    else
      Hint := Grid[ACol - 1][ARow - 1].GetHint;
  end
end;

procedure TScheduleForm.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  if not IsMouseMove then
  begin
    DrawGrid.MouseToCell(X, Y, ACol, ARow);
    if (ACol <> 0) and (ARow <> 0) then
      Grid[ACol - 1][ARow - 1].CheckClick(X, Y, DrawGrid.CellRect(ACol, ARow).Top, RecordHeight);
  end;
end;

procedure TScheduleForm.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  IsMouseMove := False;
  DrawGrid.MouseToCell(X, Y, ACol, ARow);
  MouseClickCol := ACol - 1;
  MouseClickRow := ARow - 1;
  if ssRight in Shift then
    with DragAndDrop do
    begin
      if ssCtrl in Shift then
        DType := dtAll
      else
        DType := dtOne;
      RecordNum :=(Y - DrawGrid.CellRect(ACol, ARow).Top) div RecordHeight;
      if Grid[MouseClickCol][MouseClickRow].RecordsCount > RecordNum then
        BeginDrag(True);
    end;
end;

procedure TScheduleForm.CheckListBoxItemClick(Sender: TObject; Index: integer);
begin
  if CheckedCount(CheckListBox) = 0 then
    CheckListBox.Checked[Index] := True;
  SetColWidth;
  SetRowHeight;
  HideEmptyColRow;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.DrawGridDblClick(Sender: TObject);
var
  j: Integer;
begin
  if (MouseClickRow = -1) and (MouseClickCol <> -1) then
    with DrawGrid do
    begin
      ColWidths[MouseClickCol + 1] := Canvas.TextWidth(ColCaptions[MouseClickCol].Name) + 2;
      for j := 0 to High(Grid[MouseClickCol + 1]) do
        ColWidths[MouseClickCol + 1] := max(ColWidths[MouseClickCol + 1],
          Grid[MouseClickCol][j].MaxWidth(Canvas, CheckListBox) + 2);
    end;
  if (MouseClickCol <> -1) and (MouseClickRow <> -1) then
    if (Grid[MouseClickCol][MouseClickRow].RecordsCount <> 0) then
      DGExpandButtonClick(MouseClickCol, MouseClickRow, -1)
    else
      if (ColCaptions[0].ID <> RowCaptions[0].ID) or
        ((ColCaptions[0].ID = RowCaptions[0].ID) and (MouseClickRow = MouseClickCol)) then
        DGAddButtonClick(MouseClickCol, MouseClickRow, -1);
end;

procedure TScheduleForm.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  ACol, ARow: Integer;
begin
  DrawGrid.MouseToCell(X, Y, ACol, ARow);
  SQLUpdate(ACol - 1, ARow - 1);
end;

procedure TScheduleForm.DrawGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ACol, ARow: Integer;
begin
  DrawGrid.MouseToCell(X, Y, ACol, ARow);
  if (ACol > 0) and (ARow > 0) and ((CurRowIndex <> CurColIndex) or
    ((CurRowIndex = CurColIndex) and (ACol = ARow))) then
    Accept := True
  else
    Accept := False;
end;

procedure TScheduleForm.ComboBoxChange(Sender: TObject);
begin
  SpeedButtonOK.Down := False;
end;

procedure TScheduleForm.SpeedButtonAddClick(Sender: TObject);
begin
  MainFilter.AddNewFilters(ScrollBox, CurTable, SpeedButtonOK);
end;

procedure TScheduleForm.SpeedButtonOKClick(Sender: TObject);
begin
  CurColIndex := CBColName.ItemIndex;
  CurRowIndex := CBRowName.ItemIndex;
  CurOrderByIndex := CBOrderBy.ItemIndex;
  FillDrawGrid;
  SetRowHeight;
  SetColWidth;
  HideEmptyColRow;
  DrawGrid.Invalidate;
  SpeedButtonOK.Down := True;
end;

procedure TScheduleForm.DGEditButtonClick(ACol, ARow, ARecord: Integer);
begin
  CreateNewEditForm(CurTable, ftEdit, Grid[ACol][ARow].Records[ARecord].ID,
    CurColIndex, CurRowIndex);
end;

procedure TScheduleForm.DGExpandButtonClick(ACol, ARow, ARecord: Integer);
var
  MaxHeight: Integer;
begin
  MaxHeight := RecordHeight * Grid[ACol][ARow].RecordsCount;
  if DrawGrid.RowHeights[ARow + 1] = MaxHeight then
    DrawGrid.RowHeights[ARow + 1] := RecordHeight
  else
    DrawGrid.RowHeights[ARow + 1] := MaxHeight;
end;

procedure TScheduleForm.DGDeleteButtonClick(ACol, ARow, ARecord: Integer);
begin
  DeleteSql(CurTable, Grid[ACol][ARow].Records[ARecord].ID, SQLQuery);
end;

procedure TScheduleForm.DGTableButtonClick(ACol, ARow, ARecord: Integer);
var
  FormCaption, FormName: string;
begin
  FormName := 'Schedule' + IntToStr(ACol) + IntToStr(ARow);
  FormCaption := 'Расписание';
  CreateNewListViewForm(FormName, FormCaption, CurTable, ftScheduleListView, MainFilter.FFilters,
  CurColIndex + 1, CurRowIndex + 1, ColCaptions[ACol].Name,
  RowCaptions[ARow].Name, ColCaptions[ACol].ID, RowCaptions[ARow].ID);
end;

procedure TScheduleForm.DGAddButtonClick(ACol, ARow, ARecord: Integer);
begin
  CreateNewEditForm(CurTable, ftAdd, 0, CurColIndex, CurRowIndex);
  EditForms[High(EditForms)].SetColRowCB(CurColIndex, ColCaptions[ACol].ID,
  CurRowIndex, RowCaptions[ARow].ID);
end;

procedure TScheduleForm.FillDrawGrid;
var
  i, j: Integer;
begin
  for i := 0 to High(Grid) do
    for j := 0 to High(Grid[i]) do
      Grid[i][j].Free;
  SetCaptions(CurColIndex, ColCaptions);
  SetCaptions(CurRowIndex, RowCaptions);
  DrawGrid.ColCount := Length(ColCaptions) + 1;
  DrawGrid.RowCount := Length(RowCaptions) + 1;
  SetLength(Grid, Length(ColCaptions));
  for i := 0 to High(Grid) do
    SetLength(Grid[i], Length(RowCaptions));
  for i := 0 to High(Grid) do
    for j := 0 to High(Grid[i]) do
      Grid[i][j] := TMyCell.Create(i, j, @DGExpandButtonClick, @DGTableButtonClick,
      @DGAddButtonClick);
  FillGrid;
end;

procedure TScheduleForm.FillGrid;
var
  i, CurCol, CurRow: Integer;
  StringList: TStringList;
begin
  with SQLQuery do
    begin
      Close;
      with Tables[CurTable] do
        SQL.Text := SqlSelect + SqlInnerJoin + MainFilter.CreateSqlFilter + OrderBy;
        MainFilter.SetParams(SQLQuery);
      Open;
    end;
    SQLQuery.First;

  CurCol := 0;
  CurRow := 0;
  StringList := TStringList.Create;
  with SQLQuery do
  begin
    while (not EOF) and (CurRow <= High(RowCaptions)) do
    begin
      if FieldByName((Tables[CurTable].Fields[CurRowIndex + 1] as TMyJoinedField).ReferencedField).Value
        = RowCaptions[CurRow].ID then
      begin
        if FieldByName((Tables[CurTable].Fields[CurColIndex + 1] as TMyJoinedField).ReferencedField).Value
          = ColCaptions[CurCol].ID then
        begin
          StringList.Clear;
          with Tables[CurTable] do
          begin
            for i := 1 to FieldsCount - 1 do
                with (Fields[i] as TMyJoinedField) do
                  if MIFieldName.Checked then
                    StringList.Add(Format('%s: %s', [JoinedFieldCaption, FieldByName(JoinedFieldName).Text]))
                  else
                  StringList.Add(FieldByName(JoinedFieldName).Text);
            StringList.Add('');
            Grid[CurCol][CurRow].AddRecord(StringList, FieldByName(Fields[0].Name).AsInteger,
              @DGEditButtonClick, @DGDeleteButtonClick);
          end;
          Next;
        end else
          if CurCol = High(ColCaptions) then
          begin
            CurCol := 0;
            Inc(CurRow);
          end
          else
            inc(CurCol);
      end else
      begin
        inc(CurRow);
        CurCol := 0;
      end;
    end;
  end;
  StringList.Free;
end;

procedure TScheduleForm.StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  CountChecked: Integer;
begin
  with DrawGrid.Canvas do
  begin
    CountChecked := CheckedCount(CheckListBox);
    RecordHeight := (CountChecked + 1) * TextHeight('A');
    FillRect(aRect);
    if (aCol = 0) and (aRow <> 0) then
      TextOut(aRect.Left + 1, aRect.Top, RowCaptions[aRow - 1].Name);
    if (aCol <> 0) and (aRow = 0) then
      TextOut(aRect.Left + 1, aRect.Top, ColCaptions[aCol - 1].Name);
  end;
  if (ACol <> 0) and (ARow <> 0) then
    Grid[aCol - 1][aRow - 1].Print(DrawGrid.Canvas, aRect, CheckListBox);
end;

procedure TScheduleForm.HideEmptyColRow;
var
  i, j: Integer;
  Empty: Boolean;
begin
    for i := 0 to High(ColCaptions) do
    begin
      Empty := True;
      for j := 0 to High(RowCaptions)  do
        if Grid[i][j].RecordsCount > 0 then
          Empty := False;
      if Empty then
        if MIHideEmptyCol.Checked then
          DrawGrid.ColWidths[i + 1] := 0
        else
          DrawGrid.ColWidths[i + 1] := DrawGrid.Canvas.TextWidth(ColCaptions[i].Name) + 2;
    end;

    for j := 0 to High(RowCaptions) do
    begin
      Empty := True;
      for i := 0 to High(ColCaptions) do
        if Grid[i][j].RecordsCount > 0 then
          Empty := False;
      if Empty then
        if MIHideEmptyRow.Checked then
          DrawGrid.RowHeights[j + 1] := 0
        else
          DrawGrid.RowHeights[j + 1] := RecordHeight;
    end;
end;

procedure TScheduleForm.SetColWidth;
var
  i, j: Integer;
begin
  with DrawGrid do
  begin
    for i := 0 to High(Grid) do
    begin
      ColWidths[i + 1] := Canvas.TextWidth(ColCaptions[i].Name) + 2;
      for j := 0 to High(Grid[i]) do
        ColWidths[i + 1] := max(ColWidths[i + 1], Grid[i][j].MaxWidth(Canvas, CheckListBox) + 2);
    end;
    ColWidths[0] := 15;
    for i := 0 to High(RowCaptions) do
      ColWidths[0] := Max(ColWidths[0], Canvas.TextWidth(RowCaptions[i].Name) + 2);
  end;
end;

procedure TScheduleForm.SetRowHeight;
var
  ATextHeight, i: Integer;
begin
  ATextHeight := DrawGrid.Canvas.TextHeight('A');
  for i := 0 to High(RowCaptions) do
    DrawGrid.RowHeights[i + 1] := (CheckedCount(CheckListBox) + 1) * ATextHeight;
  DrawGrid.RowHeights[0] := ATextHeight;
end;

procedure TScheduleForm.SetCaptions(AIndex: Integer;
  var ACaptions: TCaptionsArray);
begin
  with SQLQuery do
  begin
    Close;
    with (Tables[CurTable].Fields[AIndex + 1] as TMyJoinedField) do
      SQL.Text := Format('SELECT %s, %s, %s FROM %s ORDER BY 3', [
        JoinedFieldName, ReferencedField, FieldOrderBy(ReferencedTable), ReferencedTable]);
    Open;
  end;
  SetLength(ACaptions, 0);
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    SetLength(ACaptions, Length(ACaptions) + 1);
    ACaptions[High(ACaptions)].Name := SQLQuery.Fields.FieldByNumber(1).Value;
    ACaptions[High(ACaptions)].ID := SQLQuery.Fields.FieldByNumber(2).Value;
    SQLQuery.Next;
  end;
end;

function TScheduleForm.OrderBy: string;
begin
  with Tables[CurTable] do
    Result := Format(' ORDER BY %s, %s, %s',
      [FieldOrderBy((Fields[CurRowIndex + 1] as TMyJoinedField).ReferencedTable),
      FieldOrderBy((Fields[CurColIndex + 1] as TMyJoinedField).ReferencedTable),
      FieldOrderBy((Fields[CurOrderByIndex + 1] as TMyJoinedField).ReferencedTable)]);
  if CBSortType.ItemIndex = 1 then
    Result += ' DESC';
end;

procedure TScheduleForm.SQLUpdate(ACol, ARow: Integer);
var
  s: String;
  i: Integer;
begin
    SQLQuery.Close;
    with Tables[CurTable] do
    begin
      s := Format('UPDATE %s SET %s = %d',[Name,
        (Fields[CurRowIndex + 1] as TMyJoinedField).ReferencedField, RowCaptions[ARow].ID]);
      if CurRowIndex <> CurColIndex then
         s += Format(', %s = %d', [(Fields[CurColIndex + 1] as TMyJoinedField).ReferencedField,
           ColCaptions[ACol].ID]);
      s += ' WHERE ';
      with DragAndDrop do
        if DType = dtOne then
          s += Format('%s = %d', [Fields[0].Name, Grid[MouseClickCol][MouseClickRow].Records[DragAndDrop.RecordNum].ID])
        else
        begin
          for i := 0 to Grid[MouseClickCol][MouseClickRow].RecordsCount - 1 do
            s += Format('%s = %d OR ', [Fields[0].Name, Grid[MouseClickCol][MouseClickRow].Records[i].ID]);
          Delete(s, Length(s) - 3, 3);
        end;
    end;
    SQLQuery.SQL.Text := s;
    SQLQuery.ExecSQL;
    DataModuleMain.SQLTransaction.Commit;
    EActivateSQL;
    RefreshEditForms;
end;

end.

