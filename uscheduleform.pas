unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, StdCtrls, Buttons, CheckLst, UMetadata, UFilters, math, UButtons,
  UEditForm;

type

  { TScheduleForm }

  TMyCaption = record
    Name: String;
    ID: Integer;
  end;

  { TMyRecord }

  TMyRecord = class
  private
    FStringList: TStringList;
    FEditButton: TEditButton;
    FDeleteButton: TDeleteButton;
    FID, FHeight: Integer;
  public
    procedure Print(ACanvas: TCanvas; ARect: TRect; ANumber: Integer);
    procedure CheckClick(AX, AY: Integer);
    function MaxWidth(ACanvas: TCanvas): Integer;
    constructor Create(AStringList: TStringList; AID, ACol, ARow, ARecord: Integer;
      AEditButtonOnClick, ADeleteButtonOnClick: TEOnClick);
  end;

  { TMyCell }

  TMyCell = class
  private
    FRecords: array of TMyRecord;
    FCol, FRow: Integer;
    FExpandButton: TExpandButton;
  public
    procedure AddRecord(AStringList: TStringList; AID: Integer;
      AEditButtonOnClick, ADeleteButtonOnClick: TEOnClick);
    procedure Print(ACanvas: TCanvas; ARect: TRect);
    procedure CheckClick(AX, AY, ATop, AHeight: Integer);
    function GetHint: String;
    function MaxWidth(ACanvas: TCanvas): Integer;
    constructor Create(ACol, ARow: Integer; AExpandButtonOnClick: TEOnClick);
  end;

  TScheduleForm = class(TForm)
    CBColName: TComboBox;
    CBRowName: TComboBox;
    CheckListBox: TCheckListBox;
    DrawGrid: TDrawGrid;
    LabelColName: TLabel;
    LabelRowName: TLabel;
    Panel: TPanel;
    ScrollBox: TScrollBox;
    SpeedButtonOK: TSpeedButton;
    SpeedButtonAdd: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure CheckListBoxItemClick(Sender: TObject; Index: integer);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FillGrid;
    procedure DGEditButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGExpandButtonClick(ACol, ARow, ARecord: Integer);
    procedure DGDeleteButtonClick(ACol, ARow, ARecord: Integer);
    function CalcChecked: Integer;
    procedure HideEmptyColRow;
  private
    MainFilter: TMainFilter;
    RecordHeight: Integer;
    ColCaptions, RowCaptions: array of TMyCaption;
    Grid: array of array of TMyCell;
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

{ TMyRecord }

procedure TMyRecord.Print(ACanvas: TCanvas; ARect: TRect; ANumber: Integer);
var
  i, ATextHeight: Integer;
begin
  with ACanvas do
  begin
    ATextHeight := TextHeight('A');
    for i := 0 to FStringList.Count - 1 do
      TextOut(ARect.Left, ARect.Top + (i + FStringList.Count * ANumber) * ATextHeight,
        FStringList.Strings[i]);
  end;
  FEditButton.Print(ACanvas, ARect.Right, ARect.Top + FStringList.Count * ANumber * ATextHeight);
  FDeleteButton.Print(ACanvas, ARect.Right, ARect.Top + FStringList.Count * ANumber * ATextHeight);
  //FHeight := FStringList.Count * ATextHeight;
end;

procedure TMyRecord.CheckClick(AX, AY: Integer);
begin
  FEditButton.CheckClick(AX, AY);
  FDeleteButton.CheckClick(AX, AY);
end;

function TMyRecord.MaxWidth(ACanvas: TCanvas): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FStringList.Count - 1 do
    Result := Max(Result, ACanvas.TextWidth(FStringList.Strings[i]));
end;

constructor TMyRecord.Create(AStringList: TStringList; AID, ACol, ARow,
  ARecord: Integer; AEditButtonOnClick, ADeleteButtonOnClick: TEOnClick);
begin
  FStringList := TStringList.Create;
  FStringList.Text := AStringList.Text;
  FEditButton := TEditButton.Create(ACol, ARow, ARecord);
  FEditButton.OnClick := AEditButtonOnClick;
  FDeleteButton := TDeleteButton.Create(ACol, ARow, ARecord);
  FDeleteButton.OnClick := ADeleteButtonOnClick;
  FID := AID;
end;

{ TMyCell }

procedure TMyCell.AddRecord(AStringList: TStringList; AID: Integer;
  AEditButtonOnClick, ADeleteButtonOnClick: TEOnClick);
begin
  SetLength(FRecords, Length(FRecords) + 1);
  FRecords[High(FRecords)] := TMyRecord.Create(AStringList, AID, FCol, FRow,
  High(FRecords), AEditButtonOnClick, ADeleteButtonOnClick);
end;

procedure TMyCell.Print(ACanvas: TCanvas; ARect: TRect);
var
  i: Integer;
begin
  for i := 0 to High(FRecords) do
    FRecords[i].Print(ACanvas, ARect, i);
  if Length(FRecords) > 0 then
    FExpandButton.Print(ACanvas, ARect.Right, ARect.Bottom);
end;

procedure TMyCell.CheckClick(AX, AY, ATop, AHeight: Integer);
var
  i: Integer;
begin
  if High(FRecords) >= (AY - ATop) div AHeight then
    FRecords[(AY - ATop) div AHeight].CheckClick(AX, AY);
  FExpandButton.CheckClick(AX, AY);
end;

function TMyCell.GetHint: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FRecords) do
    Result += FRecords[i].FStringList.Text;
end;

function TMyCell.MaxWidth(ACanvas: TCanvas): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FRecords) do
    Result := Max(Result, FRecords[i].MaxWidth(ACanvas));
end;

constructor TMyCell.Create(ACol, ARow: Integer; AExpandButtonOnClick: TEOnClick
  );
begin
  FCol := ACol;
  FRow := ARow;
  FExpandButton := TExpandButton.Create(ACol, ARow, -1);
  FExpandButton.OnClick := AExpandButtonOnClick;
  SetLength(FRecords, 0);
end;

{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  with Tables[CurTable] do
    for i := 1 to FieldsCount - 1 do
      if (Fields[i] is TMyJoinedField) then
      begin
        CBColName.Items.Add((Fields[i] as TMyJoinedField).JoinedFieldCaption);
        CBRowName.Items.Add((Fields[i] as TMyJoinedField).JoinedFieldCaption);
        CheckListBox.Items.Add((Fields[i] as TMyJoinedField).JoinedFieldCaption);
        CheckListBox.Checked[i - 1] := True;
      end
      else
      begin
        CBColName.Items.Add(Fields[i].Caption);
        CBRowName.Items.Add(Fields[i].Caption);
        CheckListBox.Items.Add(Fields[i].Caption);
        CheckListBox.Checked[i - 1] := True;
      end;
  CBColName.ItemIndex := 0;
  CBRowName.ItemIndex := 0;
  DrawGrid.FocusRectVisible := False;
  MainFilter := TMainFilter.Create;
  MainFilter.AddNewFilters(ScrollBox, CurTable, SpeedButtonOK);
  FillGrid;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ACol, ARow: Integer;
begin
  with DrawGrid do
  begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol = 0) or (ARow = 0) then
      Hint := ''
    else
      Hint := Grid[ACol - 1][ARow - 1].GetHint;
  end
end;

procedure TScheduleForm.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  DrawGrid.MouseToCell(X, Y, ACol, ARow);
  if (ACol <> 0) and (ARow <> 0) then
    Grid[ACol - 1][ARow - 1].CheckClick(X, Y, DrawGrid.CellRect(ACol, ARow).Top, RecordHeight);
end;

procedure TScheduleForm.CheckListBoxItemClick(Sender: TObject; Index: integer);
begin
  FillGrid;
  DrawGrid.Invalidate;
end;

procedure TScheduleForm.SpeedButtonAddClick(Sender: TObject);
begin
  MainFilter.AddNewFilters(ScrollBox, CurTable, SpeedButtonOK);
end;

procedure TScheduleForm.SpeedButtonOKClick(Sender: TObject);
begin
  FillGrid;
  DrawGrid.Invalidate;
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
      //HideEmptyColRow;
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

procedure TScheduleForm.FillGrid;
var
  i, j, CurCol, CurRow: Integer;
  StringList: TStringList;
begin
  with SQLQuery do
  begin
    Close;
    with (Tables[CurTable].Fields[CBColName.ItemIndex + 1] as TMyJoinedField) do
      SQL.Text := Format('SELECT %s, %s FROM %s ORDER BY 1', [
        JoinedFieldName, ReferencedField, ReferencedTable]);
    Open;
  end;
  SetLength(ColCaptions, 0);
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    SetLength(ColCaptions, Length(ColCaptions) + 1);
    ColCaptions[High(ColCaptions)].Name := SQLQuery.Fields.FieldByNumber(1).Value;
    ColCaptions[High(ColCaptions)].ID := SQLQuery.Fields.FieldByNumber(2).Value;
    SQLQuery.Next;
  end;
  DrawGrid.ColCount := Length(ColCaptions) + 1;


  with SQLQuery do
  begin
    Close;
    with (Tables[CurTable].Fields[CBRowName.ItemIndex + 1] as TMyJoinedField) do
      SQL.Text := Format('SELECT  %s, %s FROM %s ORDER BY 1', [
        JoinedFieldName, ReferencedField, ReferencedTable]);
    Open;
  end;
  SetLength(RowCaptions, 0);
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    SetLength(RowCaptions, Length(RowCaptions) + 1);
    RowCaptions[High(RowCaptions)].Name := SQLQuery.Fields.FieldByNumber(1).Value;
    RowCaptions[High(RowCaptions)].ID := SQLQuery.Fields.FieldByNumber(2).Value;
    SQLQuery.Next;
  end;
  DrawGrid.RowCount := Length(RowCaptions) + 1;

  SetLength(Grid, Length(ColCaptions));
  for i := 0 to High(Grid) do
    SetLength(Grid[i], Length(RowCaptions));

  for i := 0 to High(Grid) do
    for j := 0 to High(Grid[i]) do
      Grid[i][j] := TMyCell.Create(i, j, @DGExpandButtonClick);

  /////////////
  with SQLQuery do
  begin
    Close;
    with Tables[CurTable] do
      SQL.Text := SqlSelect + SqlInnerJoin + MainFilter.CreateSqlFilter + Format(' ORDER BY %s, %s',
        [(Fields[CBRowName.ItemIndex + 1] as TMyJoinedField).JoinedFieldName, (Fields[CBColName.ItemIndex + 1] as TMyJoinedField).JoinedFieldName]);
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
                  StringList.Add(Format('%s: %s', [JoinedFieldCaption, FieldByName(JoinedFieldName).Text]));//FieldByName((Fields[i] as TMyJoinedField).JoinedFieldName).Text);
              StringList.Add('-------');
            Grid[CurCol][CurRow].AddRecord(StringList, FieldByName(Fields[0].Name).AsInteger,
              @DGEditButtonClick, @DGDeleteButtonClick);
          end;
          Next;
        end
        else
          inc(CurCol);
      end
      else
      begin
        inc(CurRow);
        CurCol := 0;
      end;
    end;
  end;

  with DrawGrid do
  begin
    DefaultColWidth := 50;
    for i := 0 to High(Grid) do
      for j := 0 to High(Grid[i]) do
        ColWidths[i + 1] := max(ColWidths[i + 1], Grid[i][j].MaxWidth(Canvas) + 2);
    for i := 0 to High(RowCaptions) do
      ColWidths[0] := Max(ColWidths[0], Canvas.TextWidth(RowCaptions[i].Name) + 2);
  end;
  StringList.Free;
end;

procedure TScheduleForm.DGEditButtonClick(ACol, ARow, ARecord: Integer);
begin
  CreateNewEditForm(CurTable, ftEdit, Grid[ACol][ARow].FRecords[ARecord].FID);
end;

procedure TScheduleForm.DGExpandButtonClick(ACol, ARow, ARecord: Integer);
var
  MaxHeight: Integer;
begin
  MaxHeight := RecordHeight * Length(Grid[ACol][ARow].FRecords);
  if DrawGrid.RowHeights[ARow + 1] = MaxHeight then
    DrawGrid.RowHeights[ARow + 1] := RecordHeight
  else
    DrawGrid.RowHeights[ARow + 1] := MaxHeight;
end;

procedure TScheduleForm.DGDeleteButtonClick(ACol, ARow, ARecord: Integer);
begin
  DeleteSql(CurTable, Grid[ACol][ARow].FRecords[ARecord].FID, SQLQuery);
  FillGrid;
  DrawGrid.Invalidate;
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
  for i := 0 to High(Grid) do
  begin
    Empty := True;
    for j := 0 to High(Grid[i]) do
      if Length(Grid[i][j].FRecords) > 0 then
      begin
        Empty := False;
        Break;
      end;
    if Empty then
      DrawGrid.ColWidths[i + 1] := 0;
  end;

  for j := 0 to High(Grid[0]) do
  begin
    Empty := True;
    for i := 0 to High(Grid) do
      if Length(Grid[i][j].FRecords) > 0 then
      begin
        Empty := False;
        Break;
      end;
    if Empty then
      DrawGrid.RowHeights[j + 1] := 0;
  end;
end;

end.

