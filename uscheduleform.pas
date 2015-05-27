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

  //TDragType = (dtOne, dtAll);
  TDragDrop = record
    DType: (dtOne, dtAll);
    StartCol, StartRow, RecordNum: Integer;
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
    procedure MIFieldNameClick(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FillDrawGrid;
    procedure DGEditButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGExpandButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGDeleteButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGTableButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGAddButtonClick(ACol, ARow, ARecord: Integer);
    function CalcChecked: Integer;
    procedure HideEmptyColRow;
    function OrderBy: string;
    procedure SetColWidth;
    procedure SetCaptions(AComboBox: TComboBox; var ACaptions: TCaptionsArray);
    procedure FillGrid;
    procedure SQLUpdate(ACol, ARow: Integer);
  private
    MainFilter: TMainFilter;
    RecordHeight: Integer;
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
  FillDrawGrid;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.MenuItemHideEmptyClick(Sender: TObject);
begin
  //SetColWidth;
  FillDrawGrid;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.MIFieldNameClick(Sender: TObject);
begin
  FillDrawGrid;
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
  if ssRight in Shift then
  begin
    with DragAndDrop do
    begin
      if ssCtrl in Shift then
        DType := dtAll
      else
        DType := dtOne;
       RecordNum :=(Y - DrawGrid.CellRect(ACol, ARow).Top) div RecordHeight;
       StartCol := ACol - 1;
       StartRow := ARow - 1;
    end;
    BeginDrag(True);
  end;
end;

procedure TScheduleForm.CheckListBoxItemClick(Sender: TObject; Index: integer);
begin
  if CalcChecked = 0 then
    CheckListBox.Checked[Index] := True;
  FillDrawGrid;
  DrawGrid.Invalidate;
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
  if (ACol > 0) and (ARow > 0) then
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
  FillDrawGrid;
  DrawGrid.Invalidate;
  SpeedButtonOK.Down := True;
end;

procedure TScheduleForm.StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  ATextHeight, CountChecked: Integer;
begin
  with DrawGrid.Canvas do
  begin
    ATextHeight := TextHeight('A');
    CountChecked := CalcChecked;
    RecordHeight := (CountChecked + 1) * ATextHeight;
    FillRect(aRect);
    if (aCol = 0) and (aRow = 0) then
    begin
      DrawGrid.DefaultRowHeight := (CountChecked + 1) * ATextHeight;
      DrawGrid.RowHeights[0] := ATextHeight;
      HideEmptyColRow;
    end;
    if (aCol = 0) and (aRow <> 0) then
      TextOut(aRect.Left, aRect.Top, RowCaptions[aRow - 1].Name);
    if (aCol <> 0) and (aRow = 0) then
      TextOut(aRect.Left, aRect.Top, ColCaptions[aCol - 1].Name);
  end;
  if (ACol <> 0) and (ARow <> 0) then
    Grid[aCol - 1][aRow - 1].Print(DrawGrid.Canvas, aRect);
end;

procedure TScheduleForm.StringGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  DrawGrid.Invalidate;
  //CanSelect := False;
end;

procedure TScheduleForm.FillDrawGrid;
var
  i, j: Integer;
begin
  for i := 0 to High(Grid) do
    for j := 0 to High(Grid[i]) do
      Grid[i][j].Free;
  SetCaptions(CBColName, ColCaptions);
  SetCaptions(CBRowName, RowCaptions);
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
  SetColWidth;
  for i := 0 to High(RowCaptions) do
    DrawGrid.RowHeights[i + 1] := (CalcChecked + 1) * DrawGrid.Canvas.TextHeight('A');
end;

procedure TScheduleForm.DGEditButtonClick(ACol, ARow, ARecord: Integer);
begin
  CreateNewEditForm(CurTable, ftEdit, Grid[ACol][ARow].Records[ARecord].ID);
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
  FormCaption := 'Расписание';//: ' + ColCaptions[ACol].Name + ', ' + RowCaptions[ARow].Name;
  CreateNewListViewForm(FormName, FormCaption, CurTable, MainFilter.FFilters,
  CBColName.ItemIndex + 1, CBRowName.ItemIndex + 1, ColCaptions[ACol].Name,
  RowCaptions[ARow].Name);
end;

procedure TScheduleForm.DGAddButtonClick(ACol, ARow, ARecord: Integer);
begin
  CreateNewEditForm(CurTable, ftAdd, 0);
  EditForms[High(EditForms)].SetColRowCB(CBColName.ItemIndex, ColCaptions[ACol].ID,
  CBRowName.ItemIndex, RowCaptions[ARow].ID);
end;

function TScheduleForm.CalcChecked: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Checked[i] then
      inc(Result);
end;

procedure TScheduleForm.HideEmptyColRow;
var
  i, j: Integer;
  Empty: Boolean;
begin
  if MIHideEmptyCol.Checked then
    for i := 0 to High(ColCaptions) do
    begin
      Empty := True;
      for j := 0 to High(RowCaptions)  do
        if Grid[i][j].RecordsCount > 0 then
          Empty := False;
      if Empty then
        DrawGrid.ColWidths[i + 1] := 0;
    end;

  if MIHideEmptyRow.Checked then
    for j := 0 to High(RowCaptions) do
    begin
      Empty := True;
      for i := 0 to High(ColCaptions) do
        if Grid[i][j].RecordsCount > 0 then
          Empty := False;
      if Empty then
        DrawGrid.RowHeights[j + 1] := 0;
    end;
end;

function TScheduleForm.OrderBy: string;
begin
  with Tables[CurTable] do
    Result := Format(' ORDER BY %s, %s, %s',
      [FieldOrderBy((Fields[CBRowName.ItemIndex + 1] as TMyJoinedField).ReferencedTable),
      FieldOrderBy((Fields[CBColName.ItemIndex + 1] as TMyJoinedField).ReferencedTable),
      FieldOrderBy((Fields[CBOrderBy.ItemIndex + 1] as TMyJoinedField).ReferencedTable)]);
  if CBSortType.ItemIndex = 1 then
    Result += ' DESC';
end;

procedure TScheduleForm.SetColWidth;
var
  i, j: Integer;
begin
  with DrawGrid do
  begin
    DefaultColWidth := 50;
    for i := 0 to High(Grid) do
    begin
      ColWidths[i + 1] := Canvas.TextWidth(ColCaptions[i].Name) + 2;
      for j := 0 to High(Grid[i]) do
        ColWidths[i + 1] := max(ColWidths[i + 1], Grid[i][j].MaxWidth(Canvas) + 2);
    end;
    for i := 0 to High(RowCaptions) do
      ColWidths[0] := Max(ColWidths[0], Canvas.TextWidth(RowCaptions[i].Name) + 2);
  end;
end;

procedure TScheduleForm.SetCaptions(AComboBox: TComboBox;
  var ACaptions: TCaptionsArray);
begin
  with SQLQuery do
  begin
    Close;
    with (Tables[CurTable].Fields[AComboBox.ItemIndex + 1] as TMyJoinedField) do
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

procedure TScheduleForm.FillGrid;
var
  i, j, CurCol, CurRow: Integer;
  StringList: TStringList;
  s: string;
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
    while not EOF do
    begin
      if FieldByName((Tables[CurTable].Fields[CBRowName.ItemIndex + 1] as TMyJoinedField).ReferencedField).Value
        = RowCaptions[CurRow].ID then
      begin
        if FieldByName((Tables[CurTable].Fields[CBColName.ItemIndex + 1] as TMyJoinedField).ReferencedField).Value
          = ColCaptions[CurCol].ID then
        begin
          StringList.Clear;
          with Tables[CurTable] do
          begin
            for i := 1 to FieldsCount - 1 do
              if CheckListBox.Checked[i - 1] then
                with (Fields[i] as TMyJoinedField) do
                begin
                  if MIFieldName.Checked then
                    StringList.Add(Format('%s: %s', [JoinedFieldCaption, FieldByName(JoinedFieldName).Text]))
                  else
                    StringList.Add(FieldByName(JoinedFieldName).Text);
                end;
            StringList.Add('');
            Grid[CurCol][CurRow].AddRecord(StringList, FieldByName(Fields[0].Name).AsInteger,
              @DGEditButtonClick, @DGDeleteButtonClick);
          end;
          Next;
        end else
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

procedure TScheduleForm.SQLUpdate(ACol, ARow: Integer);
var
  s: String;
  i: Integer;
begin
    SQLQuery.Close;
    with Tables[CurTable] do
    begin
      s := Format('UPDATE %s SET %s = %d, %s = %d WHERE ',[Name,
        (Fields[CBRowName.ItemIndex + 1] as TMyJoinedField).ReferencedField, RowCaptions[ARow].ID,
        (Fields[CBColName.ItemIndex + 1] as TMyJoinedField).ReferencedField, ColCaptions[ACol].ID]);
      with DragAndDrop do
        if DragAndDrop.DType = dtOne then
          s += Format('%s = %d', [Fields[0].Name, Grid[StartCol][StartRow].Records[DragAndDrop.RecordNum].ID])
        else
        begin
          for i := 0 to Grid[StartCol][StartRow].RecordsCount - 1 do
            s += Format('%s = %d OR ', [Fields[0].Name, Grid[StartCol][StartRow].Records[i].ID]);
          Delete(s, Length(s) - 3, 3);
        end;
    end;
    SQLQuery.SQL.Text := s;
    //'UPDATE SCHEDULES SET TEACHERID = 107, WEEKDAYID = 607 WHERE PAIRID = 501 OR PAIRID = 502';
    SQLQuery.ExecSQL;
    DataModuleMain.SQLTransaction.Commit;
    EActivateSQL;
    RefreshEditForms;
  //s := 'UPDATE ' + Tables[CurTable].Name + ' SET';
end;

end.

