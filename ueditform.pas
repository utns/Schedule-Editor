unit UEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, DbCtrls, ExtCtrls, UMetadata, StdCtrls, UDBConnection;

type

  { TEditForm }

  TFormType = (ftDelete, ftEdit, ftAdd);

  TEditForm = class(TForm)
    BitBtnCancel: TBitBtn;
    BitBtnSave: TBitBtn;
    MainDataSource: TDataSource;
    MainSQLQuery: TSQLQuery;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure  CreateNew(ACurTable: Integer; ADataSource: TDataSource; AFormType: TFormType; ASQLQuery: TSQLQuery);
    procedure CreateCB(ACurField: Integer);
    procedure CreateEdit(ACurField: Integer);
    procedure CreateLabel(ACurField: Integer);
    procedure SQLInsert;
    procedure SQLUpdate;
    function GetNewID: String;
    function GetCBItemID(ADBLookupCB: TDBLookupComboBox): Integer;
    procedure ApplyQuery(AQuery: String);
    function IsEqualValues(ATag: Integer): Boolean;
    function IsEmptyFields: Boolean;
  private
    ListViewDataSource: TDataSource;
    ListViewSQLQuery: TSQLQuery;
    FormType: TFormType;
    CurTable: Integer;
    Editors: array of TWinControl;
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

procedure TEditForm.CreateNew(ACurTable: Integer; ADataSource: TDataSource;
  AFormType: TFormType; ASQLQuery: TSQLQuery);
var
  AEditForm: TEditForm;
  i: Integer;
begin
  AEditForm := TEditForm.Create(Application);
  with AEditForm do
  begin
    ListViewDataSource := ADataSource;
    ListViewSQLQuery := ASQLQuery;
    CurTable := ACurTable;
    FormType := AFormType;
    Tag := ACurTable;
    Caption := 'Карточка';
    for i := 1 to Tables[ACurTable].FieldsCount - 1 do
    begin
      if Tables[ACurTable].Fields[i] is TMyJoinedField then
      begin
        if (Tables[ACurTable].Fields[i] as TMyJoinedField).JoinedVisible then
          CreateCB(i);
      end
      else
        if Tables[ACurTable].Fields[i].Visible then
          CreateEdit(i);
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
end;

procedure TEditForm.BitBtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.CreateCB(ACurField: Integer);
var
  FDBEdit: TDBEdit;
  FLookupCB: TDBLookupComboBox;
  FSQLQuery: TSQLQuery;
  FDataSource: TDataSource;
begin
  CreateLabel(ACurField);
  FSQLQuery := TSQLQuery.Create(Self);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    SQL.AddStrings(Format('SELECT %s FROM %s', [(Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName,
      (Tables[CurTable].Fields[ACurField] as TMyJoinedField).ReferencedTable]));
    Open;
  end;

  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FSQLQuery;

  FLookupCB := TDBLookupComboBox.Create(Self);
  with FLookupCB do
  begin
    Visible := True;
    Width := 250;
    Left := 110;
    Height := 23;
    Tag := ACurField;
    Top := -23 + 28 * ACurField;
    Parent := Self;
    ReadOnly := True;
    KeyField := (Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName;
    ListSource := FDataSource;
    if FormType = ftEdit then
    begin
      ItemIndex := Items.IndexOf(ListViewSQLQuery.Fields.
        FieldByName((Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName).Value);
    end;
  end;
  SetLength(Editors, Length(Editors) + 1);
  Editors[High(Editors)] := FLookupCB;
  FSQLQuery.Free;
  FDataSource.Free;
end;

procedure TEditForm.CreateEdit(ACurField: Integer);
var
  FEdit: TEdit;
begin
  CreateLabel(ACurField);
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Visible := True;
    Width := 250;
    Left := 110;
    Height := 23;
    Top := -23 + 28 * ACurField;
    Parent := Self;
    if FormType = ftEdit then
    begin
      Text := ListViewSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[ACurField].Name).Value;
    end;
  end;
  SetLength(Editors, Length(Editors) + 1);
  Editors[High(Editors)] := FEdit;
end;

procedure TEditForm.CreateLabel(ACurField: Integer);
var
  FLable: TLabel;
begin
  FLable := TLabel.Create(Self);
  with FLable do
  begin
    Visible := True;
    Width := 100;
    Left := 5;
    Height := 23;
    Top := -23 + 28 * ACurField;
    Parent := Self;
    if Tables[CurTable].Fields[ACurField] is TMyJoinedField then
      Text := (Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldCaption
    else
      Text := Tables[CurTable].Fields[ACurField].Caption;
  end;
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
    ListViewSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[0].Name).Value]);

  if ChangedField then
    with MainSQLQuery do
    begin
      Close;
      SQL.Clear;
      SQL.AddStrings(s);
      for i := 0 to High(Editors) do
        if not IsEqualValues(i + 1) then
          if Editors[i] is TEdit then
            ParamByName('p' + IntToStr(i + 1)).AsString := (Editors[i] as TEdit).Text
          else
            ParamByName('p' + IntToStr(i + 1)).AsInteger :=  GetCBItemID((Editors[i] as TDBLookupComboBox));
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
  {with MainSQLQuery do
  begin
    SQL.Clear;
    SQL.AddStrings(Format('SELECT MAX(%s) FROM %s', [Tables[CurTable].Fields[0].Name,
      Tables[CurTable].Name]));
    Open;
    Result := IntToStr(Fields[0].AsInteger + 1);
  end;}
end;

function TEditForm.GetCBItemID(ADBLookupCB: TDBLookupComboBox): Integer;
var
  FSQLQuery: TSQLQuery;
begin
  if ADBLookupCB.ItemIndex = -1 then
  begin
    Result := -1;
    Exit;
  end;
  FSQLQuery := TSQLQuery.Create(Self);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Text := Format('SELECT first 1 skip %d %s FROM %s', [ADBLookupCB.ItemIndex,
      (Tables[CurTable].Fields[ADBLookupCB.Tag] as TMyJoinedField).ReferencedField,
      (Tables[CurTable].Fields[ADBLookupCB.Tag] as TMyJoinedField).ReferencedTable]);
    Open;
    Result := Fields[0].AsInteger;
    Free;
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
      if Editors[i] is TEdit then
        Params[i].AsString := (Editors[i] as TEdit).Text
      else
        Params[i].AsInteger :=  GetCBItemID((Editors[i] as TDBLookupComboBox));
    ExecSQL;
  end;
end;

function TEditForm.IsEqualValues(ATag: Integer): Boolean;
begin
  Result := False;
    if Editors[ATag - 1] is TEdit then
    begin
      if (Editors[ATag - 1] as TEdit).Text = ListViewSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[ATag].Name).Value then
      begin
        Result := True;
      end;
    end else
      if (Editors[ATag - 1] as TDBLookupComboBox).Items[(Editors[ATag - 1] as TDBLookupComboBox).ItemIndex] =
        ListViewSQLQuery.Fields.FieldByName((Tables[CurTable].Fields[ATag] as TMyJoinedField).JoinedFieldName).Value then
      begin
        Result := True;
      end;
end;

function TEditForm.IsEmptyFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Editors) do
    if Editors[i] is TEdit then
    begin
      if (Editors[i] as TEdit).Text = '' then
      begin
        Result := True;
        Exit;
      end
    end else
      if GetCBItemID((Editors[i] as TDBLookupComboBox)) = -1 then
      begin
        Result := True;
        Exit;
      end;
end;

end.

