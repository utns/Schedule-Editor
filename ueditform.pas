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
    procedure  CreateNew(ACurTable: Integer; AFormType: TFormType; AID: Integer);
    procedure SQLInsert;
    procedure SQLUpdate;
    function GetNewID: String;
    procedure ApplyQuery(AQuery: String);
    function IsEqualValues(ATag: Integer): Boolean;
    function IsEmptyFields: Boolean;
  private
    ListViewDataSource: TDataSource;
    //SourceSQLQuery: TSQLQuery;
    FormType: TFormType;
    CurTable: Integer;
    Editors: array of TMainEditor;
    { private declarations }
  public
    { public declarations }
  end;

type
  TEvent = procedure of object;

var
  EditForm: TEditForm;
  EActivateSQL: TEvent;

implementation

{$R *.lfm}

{ TEditForm }

procedure TEditForm.CreateNew(ACurTable: Integer; AFormType: TFormType; AID: Integer);
var
  AEditForm: TEditForm;
  i: Integer;
begin
  AEditForm := TEditForm.Create(Application);
  with AEditForm do
  begin
    SourceSQLQuery.Close;
    SourceSQLQuery.SQL.Clear;
    SourceSQLQuery.SQL.Add(Tables[ACurTable].CreateSQlQuery(-1, -1, ''));
    SourceSQLQuery.Open;
    SourceSQLQuery.Locate(Tables[ACurTable].Fields[0].Name, AID, []);
    CurTable := ACurTable;
    FormType := AFormType;
    Tag := AID;
    Caption := 'Карточка';
    for i := 1 to Tables[ACurTable].FieldsCount - 1 do
    begin
        SetLength(Editors, Length(Editors) + 1);
      if Tables[ACurTable].Fields[i] is TMyJoinedField then
      begin
        if (Tables[ACurTable].Fields[i] as TMyJoinedField).JoinedVisible then
          Editors[High(Editors)] := TMyLookupCB.Create(AEditForm, ACurTable, i, AFormType, SourceSQLQuery);
      end
      else
        if Tables[ACurTable].Fields[i].Visible then
          Editors[High(Editors)] := TMyEdit.Create(AEditForm, ACurTable, i, AFormType, SourceSQLQuery);
    end;
    BitBtnSave.Top := 5 + 28 * i;
    BitBtnCancel.Top := 5 + 28 * i;
    Height := BitBtnCancel.Top + BitBtnCancel.Height + 5;
    ShowModal;
  end;
end;

procedure TEditForm.BitBtnSaveClick(Sender: TObject);
begin
  if IsEmptyFields then
  begin
    MessageDlg('Заполните пустые поля.', mtError, [mbNo], 0);
    Exit;
  end;
  if FormType = ftAdd then
    SQLInsert
  else
    SQLUpdate;
  Close;
  DataModuleMain.SQLTransaction.Commit;
  EActivateSQL;
  //SourceSQLQuery.Locate('ScheduleID', 45, []);
end;

procedure TEditForm.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.SQLInsert;
var
  i: Integer;
  s: String;
begin
  s := Format('INSERT INTO %s VALUES (%s', [Tables[CurTable].Name, GetNewID]);
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

end.

