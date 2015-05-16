unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, StdCtrls, Buttons, CheckLst, UMetadata, UFilters;

type

  { TForm1 }

  TMyCaption = record
    Name: String;
    ID: Integer;
  end;

  { TMyRecord }

  TMyRecord = class
    FStringList: TStringList;
    FID: Integer;
  public
    procedure Print(ACanvas: TCanvas; ARect: TRect; AOffset: Integer);
    constructor Create(AStringList: TStringList; AID: Integer);
  end;

  { TMyCell }

  TMyCell = class
  private
    FRecords: array of TMyRecord;
  public
    procedure AddRecord(AStringList: TStringList; AID: Integer);
    procedure Print(ACanvas: TCanvas; ARect: TRect);
    function GetHint: String;
    constructor Create;
  end;

  TForm1 = class(TForm)
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
  private
    MainFilter: TMainFilter;
    ColCaptions, RowCaptions: array of TMyCaption;
    Grid: array of array of TMyCell;
    function CalcChecked: Integer;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  CurTable = 8;

implementation

{$R *.lfm}

{ TMyRecord }

procedure TMyRecord.Print(ACanvas: TCanvas; ARect: TRect; AOffset: Integer);
var
  i, ATextHeight: Integer;
begin
  with ACanvas do
  begin
    ATextHeight := TextHeight('A');
    for i := 0 to FStringList.Count - 1 do
      TextOut(ARect.Left, ARect.Top + (i + FStringList.Count * AOffset) * ATextHeight,
        FStringList.Strings[i]) ;
  end;
end;

constructor TMyRecord.Create(AStringList: TStringList; AID: Integer);
begin
  FStringList := TStringList.Create;
  FStringList.Text := AStringList.Text;
  FID := AID;
end;

{ TMyCell }

procedure TMyCell.AddRecord(AStringList: TStringList; AID: Integer);
begin
  SetLength(FRecords, Length(FRecords) + 1);
  FRecords[High(FRecords)] := TMyRecord.Create(AStringList, AID);
end;

procedure TMyCell.Print(ACanvas: TCanvas; ARect: TRect);
var
  i: Integer;
begin
  for i := 0 to High(FRecords) do
    FRecords[i].Print(ACanvas, ARect, i);
end;

function TMyCell.GetHint: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(FRecords) do
    Result += FRecords[i].FStringList.Text;
end;

constructor TMyCell.Create;
begin
  SetLength(FRecords, 0);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
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

procedure TForm1.DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TForm1.SpeedButtonAddClick(Sender: TObject);
begin
  MainFilter.AddNewFilters(ScrollBox, CurTable, SpeedButtonOK);
end;

procedure TForm1.SpeedButtonOKClick(Sender: TObject);
begin
  FillGrid;
  DrawGrid.Invalidate;
end;

procedure TForm1.StringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  ATextHeight, i, k, j, CountChecked: Integer;
begin
  with DrawGrid.Canvas do
  begin
    ATextHeight := TextHeight('A');
    CountChecked := CalcChecked;
    FillRect(aRect);
    if (aCol = 0) and (aRow = 0) then
    begin
      SQLQuery.First;
      DrawGrid.DefaultRowHeight := CountChecked * ATextHeight;
    end;
    if (aCol = 0) and (aRow <> 0) then
      TextOut(aRect.Left, aRect.Top, RowCaptions[aRow - 1].Name);
    if (aCol <> 0) and (aRow = 0) then
      TextOut(aRect.Left, aRect.Top, ColCaptions[aCol - 1].Name);
  end;
    if (ACol <> 0) and (ARow <> 0) then
      Grid[aCol - 1][aRow - 1].Print(DrawGrid.Canvas, aRect);
    {begin
      k := 0;
      while ((SQLQuery.FieldByName((Tables[CurTable].Fields[CBColName.ItemIndex + 1] as TMyJoinedField).ReferencedField).Value
          = ColCaptions[aCol - 1].ID) and
          (SQLQuery.FieldByName((Tables[CurTable].Fields[CBRowName.ItemIndex + 1] as TMyJoinedField).ReferencedField).Value
          = RowCaptions[aRow - 1].ID)) and (not SQLQuery.EOF) do
      begin
        j := 0;
        with Tables[CurTable] do
          for i := 1 to FieldsCount - 1 do
            if CheckListBox.Checked[i - 1] then
            begin
              inc(j);
              TextOut(aRect.Left, aRect.Top + (j - 1 + k * (CountChecked + 1)) * ATextHeight,
                SQLQuery.FieldByName((Fields[i] as TMyJoinedField).JoinedFieldName).Text);
            end;
        TextOut(aRect.Left, aRect.Top + (j + k * (CountChecked + 1)) * ATextHeight, '---------------');
        inc(k);
        SQLQuery.Next;
      end;
    end;}

end;

procedure TForm1.StringGridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  DrawGrid.Invalidate;
  //CanSelect := False;
end;

procedure TForm1.FillGrid;
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
  DrawGrid.ColCount := SQLQuery.RecordCount + 1;
  SetLength(ColCaptions, 0);
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    SetLength(ColCaptions, Length(ColCaptions) + 1);
    ColCaptions[High(ColCaptions)].Name := SQLQuery.Fields.FieldByNumber(1).Value;
    ColCaptions[High(ColCaptions)].ID := SQLQuery.Fields.FieldByNumber(2).Value;
    SQLQuery.Next;
  end;


  with SQLQuery do
  begin
    Close;
    with (Tables[CurTable].Fields[CBRowName.ItemIndex + 1] as TMyJoinedField) do
      SQL.Text := Format('SELECT  %s, %s FROM %s ORDER BY 1', [
        JoinedFieldName, ReferencedField, ReferencedTable]);
    Open;
  end;
  SetLength(RowCaptions, 0);
  DrawGrid.RowCount := SQLQuery.RecordCount + 1;
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    SetLength(RowCaptions, Length(RowCaptions) + 1);
    RowCaptions[High(RowCaptions)].Name := SQLQuery.Fields.FieldByNumber(1).Value;
    RowCaptions[High(RowCaptions)].ID := SQLQuery.Fields.FieldByNumber(2).Value;
    SQLQuery.Next;
  end;

  SetLength(Grid, Length(ColCaptions));
  for i := 0 to High(Grid) do
    SetLength(Grid[i], Length(RowCaptions));

  for i := 0 to High(Grid) do
    for j := 0 to High(Grid[i]) do
      Grid[i][j] := TMyCell.Create;

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
                StringList.Add(FieldByName((Fields[i] as TMyJoinedField).JoinedFieldName).Text);
              StringList.Add('-------');
            Grid[CurCol][CurRow].AddRecord(StringList, FieldByName(Fields[0].Name).AsInteger);
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
  StringList.Free;
end;

function TForm1.CalcChecked: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to CheckListBox.Count - 1 do
    if CheckListBox.Checked[i] then
      inc(Result);
end;

end.

