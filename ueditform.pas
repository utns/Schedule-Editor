unit UEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, DbCtrls, ExtCtrls, UMetadata, StdCtrls, UDBConnection, UEditors;

type

  { TEditForm }

  TEditForm = class(TForm)
    BitBtnCancel: TBitBtn;
    BitBtnSave: TBitBtn;
    MainSQLQuery: TSQLQuery;
    SourceSQLQuery: TSQLQuery;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    constructor Create(ACurTable: Integer; AFormType: TFormType; AID: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SQLInsert;
    procedure SQLUpdate;
    function GetNewID: String;
    procedure ApplyQuery(AQuery: String);
    function IsEqualValues(ATag: Integer): Boolean;
    function IsEmptyFields: Boolean;
    procedure RefreshEditors;
    procedure SetColRowCB(AColIndex, AColID, ARowIndex, ARowID: Integer);
    procedure SetLabelsColor(AFirstLabel, ASecondLabel: Integer);
  private
    FormType: TFormType;
    CurTable: Integer;
    Editors: array of TMainEditor;
    { private declarations }
  public
    { public declarations }
  end;



type
  TEvent = procedure;
  TELocate = procedure(ACurTable, AID: Integer);

var
  EditForm: TEditForm;
  EActivateSQL: TEvent;
  ELocate: TELocate;
  EditForms: array of TEditForm;
  LastEditFormTableID: Integer;

procedure DeleteSql(ACurTable, AID: Integer; ASQLQuery: TSQLQuery);
procedure RefreshEditForms;
procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType; AID: Integer);
procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType;
  AID, AFirstLabel, ASecondLabel: Integer);

implementation

procedure DeleteSql(ACurTable, AID: Integer; ASQLQuery: TSQLQuery);
var
  i, ButtonSelected: Integer;
  s: String;
begin
  ButtonSelected := MessageDlg('Удалить выбранную запись?', mtConfirmation, mbYesNo, 0);
  if ButtonSelected = mrYes then
  begin
    for i := 0 to High(EditForms) do
      if AID = EditForms[i].Tag then
      begin
        MessageDlg('Выбранная запись редактируется.', mtError, [mbOK], 0);
        EditForms[i].ShowOnTop;
        Exit;
      end;
    try
      with ASQLQuery do
      begin
        s := SQL.Text;
        Close;
        with Tables[ACurTable] do
          SQL.Text := Format('DELETE FROM %s WHERE %s = %s;', [Name, Fields[0].Name, IntToStr(AID)]);
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
        ASQLQuery.SQL.Text := s;
      end;
    end;
    RefreshEditForms
  end;
end;

procedure RefreshEditForms;
var
  i: Integer;
begin
  for i := 0 to High(EditForms) do
    EditForms[i].RefreshEditors;
end;

function IsFormOpen(AID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if AID <> 0 then
    for i := 0 to High(EditForms) do
      if AID = EditForms[i].Tag then
      begin
        Result := True;
        EditForms[i].ShowOnTop;
        Exit;
      end;
end;

procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType;
  AID: Integer);
var
  i: Integer;
begin
  if not IsFormOpen(AID) then
  begin
    SetLength(EditForms, Length(EditForms) + 1);
    EditForms[High(EditForms)] := TEditForm.Create(ACurTable, AFormType, AID);
  end;
end;

procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType; AID,
  AFirstLabel, ASecondLabel: Integer);
begin
  if not IsFormOpen(AID) then
  begin
    SetLength(EditForms, Length(EditForms) + 1);
    EditForms[High(EditForms)] := TEditForm.Create(ACurTable, AFormType, AID);
    EditForms[High(EditForms)].SetLabelsColor(AFirstLabel, ASecondLabel);
  end;
end;

{$R *.lfm}

{ TEditForm }

constructor TEditForm.Create(ACurTable: Integer; AFormType: TFormType;
  AID: Integer);
var
  i: Integer;
begin
  Inherited Create(Application);
  SourceSQLQuery.Close;
  SourceSQLQuery.SQL.Clear;
  SourceSQLQuery.SQL.Add(Tables[ACurTable].CreateSQlQuery(-1, -1, ''));
  SourceSQLQuery.Open;
  SourceSQLQuery.Locate(Tables[ACurTable].Fields[0].Name, AID, []);
  CurTable := ACurTable;
  FormType := AFormType;
  Tag := AID;
  if (AFormType = ftEdit) then
    Caption := 'Редактирование записи'
  else
    Caption := 'Добавление записи';
  for i := 1 to Tables[ACurTable].FieldsCount - 1 do
  begin
    SetLength(Editors, Length(Editors) + 1);
    if Tables[ACurTable].Fields[i] is TMyJoinedField then
    begin
      if (Tables[ACurTable].Fields[i] as TMyJoinedField).JoinedVisible then
        Editors[High(Editors)] := TMyLookupCB.Create(Self, ACurTable, i, AFormType, SourceSQLQuery);
    end
    else
      if Tables[ACurTable].Fields[i].Visible then
        Editors[High(Editors)] := TMyEdit.Create(Self, ACurTable, i, AFormType, SourceSQLQuery);
  end;
  BitBtnSave.Top := 5 + 28 * i;
  BitBtnCancel.Top := 5 + 28 * i;
  Height := BitBtnCancel.Top + BitBtnCancel.Height + 5;
  Show;
end;

procedure TEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CurForm, i: Integer;
begin
  for i := 0 to High(EditForms) do
    if (Tag = EditForms[i].Tag) and (CurTable = EditForms[i].CurTable) then
    begin
      CurForm := i;
      Break;
    end;
  for i := CurForm to High(EditForms) - 1 do
    EditForms[i] := EditForms[i + 1];
  SetLength(EditForms, Length(EditForms) - 1);
  CloseAction := caFree;
end;

procedure TEditForm.BitBtnSaveClick(Sender: TObject);
begin
  if IsEmptyFields then
  begin
    MessageDlg('Заполните пустые поля.', mtError, [mbOK], 0);
    Exit;
  end;
  try
    if FormType = ftAdd then
      SQLInsert
    else
      SQLUpdate;
  except
    on E: EVariantError do
    begin
      MessageDlg('Неправильный тип поля.', mtError, [mbOK], 0);
      EActivateSQL;
      Exit;
    end;
  end;
  Close;
  LastEditFormTableID := CurTable;
  DataModuleMain.SQLTransaction.Commit;
  EActivateSQL;
  RefreshEditForms;
end;

procedure TEditForm.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.SQLInsert;
var
  i: Integer;
  s, NewID: String;
begin
  NewID := GetNewID;
  ELocate(CurTable, StrToInt(NewID));
  s := Format('INSERT INTO %s VALUES (%s', [Tables[CurTable].Name, NewID]);
  for i := 0 to High(Editors) do
    s += ', :p' + IntToStr(i);
  s += ');';
  ApplyQuery(s);
end;

procedure TEditForm.SQLUpdate;
var
  i: integer;
  s: string;
  ChangedField: Boolean;
begin
  ELocate(CurTable, StrToInt(SourceSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[0].Name).Value));
  ChangedField := False;
  s := 'UPDATE ' + Tables[CurTable].Name + ' SET';
  for i := 1 to Tables[CurTable].FieldsCount - 1 do
    if  not IsEqualValues(i) then
    begin
      s +=  Format(' %s = :p%d ,', [Tables[CurTable].Fields[i].Name, i]);
      ChangedField := True;
    end;
  Delete(s, Length(s), 1);
  s += Format(' WHERE %s = %s' , [Tables[CurTable].Fields[0].Name,
    SourceSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[0].Name).Value]);


  if ChangedField then
    with MainSQLQuery do
    begin
      Close;
      SQL.Clear;
      SQL.AddStrings(s);
      for i := 0 to High(Editors) do
        if not IsEqualValues(i + 1) then
          ParamByName('p' + IntToStr(i + 1)).AsString := Editors[i].GetValue;
      ExecSQL;
    end;
end;

function TEditForm.GetNewID: String;
begin
  with MainSQLQuery do
  begin
    Close;
    SQL.Text := 'SELECT NEXT VALUE FOR MainSequence FROM RDB$DATABASE;';
    Open;
    Result := Fields[0].AsString;
  end;
end;

procedure TEditForm.ApplyQuery(AQuery: String);
var
  i: Integer;
begin
  with MainSQLQuery do
  begin
    Close;
    SQL.Clear;
    SQL.AddStrings(AQuery);
    for i := 0 to High(Editors) do
      Params[i].AsString := Editors[i].GetValue;
    ExecSQL;
  end;
end;

function TEditForm.IsEqualValues(ATag: Integer): Boolean;
var
  CurValue: String;
begin
  Result := False;
    if Editors[ATag - 1] is TMyEdit then
      CurValue := SourceSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[ATag].Name).Value
    else
      CurValue := SourceSQLQuery.Fields.FieldByName((Tables[CurTable].Fields[ATag] as TMyJoinedField).JoinedFieldName).Value;
    if Editors[ATag - 1].GetText = CurValue then
      Result := True;
end;

function TEditForm.IsEmptyFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Editors) do
    if Editors[i].GetValue = '' then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TEditForm.RefreshEditors;
var
  i: Integer;
begin
  SourceSQLQuery.Close;
  SourceSQLQuery.SQL.Clear;
  SourceSQLQuery.SQL.Add(Tables[CurTable].CreateSQlQuery(-1, -1, ''));
  SourceSQLQuery.Open;
  SourceSQLQuery.Locate(Tables[CurTable].Fields[0].Name, Tag, []);
  for i := 0 to High(Editors) do
    if Editors[i] is TMyLookupCB then
      Editors[i].Refresh;
end;

procedure TEditForm.SetColRowCB(AColIndex, AColID, ARowIndex, ARowID: Integer);
begin
  (Editors[ARowIndex] as TMyLookupCB).SetValue(IntToStr(ARowID));
  (Editors[AColIndex] as TMyLookupCB).SetValue(IntToStr(AColID));
end;

procedure TEditForm.SetLabelsColor(AFirstLabel, ASecondLabel: Integer);
begin
  Editors[AFirstLabel].LabelColor := clRed;
  Editors[ASecondLabel].LabelColor := clRed;
end;

end.

