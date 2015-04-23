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
    procedure BitBtnSaveClick(Sender: TObject);
    procedure  CreateNew(ACurTable: Integer; ADataSource: TDataSource; AFormType: TFormType; ASQLQuery: TSQLQuery);
    procedure CreateCB(ACurField: Integer);
    procedure CreateEdit(ACurField: Integer);
    procedure CreateLabel(ACurField: Integer);
    procedure SQLInsert;
    procedure SQLUpdate;
    function GetNewID: String;
    function GetCBItemID(ADBLookupCB: TDBLookupComboBox): Integer;
    procedure CreateSQL(AQuery: String);
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

var
  EditForm: TEditForm;

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
    Show;
  end;



end;

procedure TEditForm.BitBtnSaveClick(Sender: TObject);
begin
  if FormType = ftAdd then
    SQLInsert
  else
    SQLUpdate;
  Close;
  DataModuleMain.SQLTransaction.Commit;
  ListViewSQLQuery.Open;
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
  s := 'INSERT INTO ' + Tables[CurTable].Name + ' VALUES (' + GetNewID;
  for i := 0 to High(Editors) do
    s += ', :p' + IntToStr(i);
  s += ');';
  CreateSQL(s);
end;

procedure TEditForm.SQLUpdate;
var
  i: integer;
  s: string;
begin
  s := 'UPDATE ' + Tables[CurTable].Name + ' SET ' + Tables[CurTable].Fields[1].Name + ' = :p1';
  for i := 2 to Tables[CurTable].FieldsCount - 1 do
    s += ', ' + Tables[CurTable].Fields[i].Name + ' = :p' + IntToStr(i);
  s += ' WHERE ' + Tables[CurTable].Fields[0].Name + ' = '
    +  IntToStr(ListViewSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[0].Name).Value);
  CreateSQL(s);
end;

function TEditForm.GetNewID: String;
begin
  with MainSQLQuery do
  begin
    SQL.Clear;
    SQL.AddStrings(Format('SELECT MAX(%s) FROM %s', [Tables[CurTable].Fields[0].Name,
      Tables[CurTable].Name]));
    Open;
    Result := IntToStr(Fields[0].AsInteger + 1);
  end;
end;

function TEditForm.GetCBItemID(ADBLookupCB: TDBLookupComboBox): Integer;
var
  FSQLQuery: TSQLQuery;
begin
  FSQLQuery := TSQLQuery.Create(Self);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    SQL.AddStrings(Format('SELECT first 1 skip %d %s FROM %s', [ADBLookupCB.ItemIndex ,(Tables[CurTable].Fields[ADBLookupCB.Tag] as TMyJoinedField).ReferencedField,
      (Tables[CurTable].Fields[ADBLookupCB.Tag] as TMyJoinedField).ReferencedTable]));
    Open;
    Result := Fields[0].AsInteger;
    Free;
  end;
end;

procedure TEditForm.CreateSQL(AQuery: String);
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

end.

