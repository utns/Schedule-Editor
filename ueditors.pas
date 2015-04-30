unit UEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DbCtrls, Controls, UMetadata, sqldb, db, StdCtrls, UDBConnection, Forms, Dialogs, variants;

type



  { TMainEditor }

  TMainEditor = class
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ACurField: Integer);
    function GetValue: String; virtual;
    function GetText: String; virtual;
    procedure Refresh; virtual;
  end;

  { TMyLookupCB }

  TMyLookupCB = class(TMainEditor)
  private
    FDBLookupComboBox: TDBLookupComboBox;
    FStringList: TStringList;
    FSQLQuery: TSQLQuery;
    FDataSource: TDataSource;
    FCurTable: Integer;
    procedure CreateSQLQueryAndDataSource(AWinControl: TWinControl; ACurTable: Integer; ACurField: Integer);
    procedure CreateStringList;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ACurField: Integer;
      AFormType: TFormType; ASourceSQLQuery: TSQLQuery);
    function GetValue: String; override;
    function GetText: String; override;
    procedure Refresh; override;
  end;

  { TMyEdit }

  TMyEdit = class(TMainEditor)
  private
    FEdit: TEdit;
  public
    constructor Create(AWinControl: TWinControl; ACurTable: Integer; ACurField: Integer;
      AFormType: TFormType; ASourceSQLQuery: TSQLQuery);
    function GetValue: String; override;
    function GetText: String; override;
  end;

implementation

{ TMyEdit }

constructor TMyEdit.Create(AWinControl: TWinControl; ACurTable: Integer;
  ACurField: Integer; AFormType: TFormType; ASourceSQLQuery: TSQLQuery);
begin
  inherited Create(AWinControl, ACurTable, ACurField);
  FEdit := TEdit.Create(AWinControl);
  with FEdit do
  begin
    Visible := True;
    Width := 250;
    Left := 140;
    Height := 23;
    Top := -23 + 28 * ACurField;
    Parent := AWinControl;
    if AFormType = ftEdit then
    begin
      Text := ASourceSQLQuery.Fields.FieldByName(Tables[ACurTable].Fields[ACurField].Name).Value;
    end;
  end;
end;

function TMyEdit.GetValue: String;
begin
  Result := FEdit.Text;
end;

function TMyEdit.GetText: String;
begin
  Result := FEdit.Text;
end;

{ TMainEditor }

constructor TMainEditor.Create(AWinControl: TWinControl; ACurTable: Integer;
  ACurField: Integer);
var
  FLable: TLabel;
begin
  FLable := TLabel.Create(AWinControl);
  with FLable do
  begin
    Visible := True;
    Width := 100;
    Left := 5;
    Height := 23;
    Top := -23 + 28 * ACurField;
    Parent := AWinControl;
    if Tables[ACurTable].Fields[ACurField] is TMyJoinedField then
      Caption := (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldCaption
    else
      Caption := Tables[ACurTable].Fields[ACurField].Caption;
  end;
end;

function TMainEditor.GetValue: String;
begin

end;

function TMainEditor.GetText: String;
begin

end;

procedure TMainEditor.Refresh;
begin

end;

{ TMyLookupCB }

procedure TMyLookupCB.CreateSQLQueryAndDataSource(AWinControl: TWinControl;
  ACurTable: Integer; ACurField: Integer);
begin
  FSQLQuery := TSQLQuery.Create(AWinControl);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    SQL.AddStrings(Format('SELECT %s, %s FROM %s', [(Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedField,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedTable]));
    Open;
  end;
  FDataSource := TDataSource.Create(AWinControl);
  FDataSource.DataSet := FSQLQuery;
end;

procedure TMyLookupCB.CreateStringList;
var
  i: Integer;
begin
  FStringList := TStringList.Create;
  for i := 0 to FSQLQuery.RecordCount - 1 do
  begin
    FStringList.Add(FSQLQuery.Fields.FieldByNumber(1).Value);
    FSQLQuery.Next;
  end;
end;

constructor TMyLookupCB.Create(AWinControl: TWinControl; ACurTable: Integer;
  ACurField: Integer; AFormType: TFormType; ASourceSQLQuery: TSQLQuery);
var
  i: Integer;
begin
  inherited Create(AWinControl, ACurTable, ACurField);
  CreateSQLQueryAndDataSource(AWinControl, ACurTable, ACurField);

  {FSQLQuery := TSQLQuery.Create(AWinControl);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    SQL.AddStrings(Format('SELECT %s, %s FROM %s', [(Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedField,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedTable]));
    Open;
  end;

  FDataSource := TDataSource.Create(AWinControl);
  FDataSource.DataSet := FSQLQuery; }

  FCurTable := ACurTable;

  FDBLookupComboBox := TDBLookupComboBox.Create(AWinControl);
  with FDBLookupComboBox do
  begin
    Visible := True;
    Width := 250;
    Left := 140;
    Height := 23;
    Tag := ACurField;
    Top := -23 + 28 * ACurField;
    Parent := AWinControl;
    Style := csDropDownList;
    KeyField := (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName;
    ListSource := FDataSource;
    if AFormType = ftEdit then
    begin
      ItemIndex := Items.IndexOf(ASourceSQLQuery.Fields.
        FieldByName((Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName).Value);
    end;
  end;

  CreateStringList;
  {FStringList := TStringList.Create;
  for i := 0 to FSQLQuery.RecordCount - 1 do
  begin
    FStringList.Add(FSQLQuery.Fields.FieldByNumber(1).Value);
    FSQLQuery.Next;
  end; }

  FSQLQuery.Free;
  FDataSource.Free;
end;

function TMyLookupCB.GetValue: String;
begin
  if FDBLookupComboBox.ItemIndex = -1 then
  begin
    Result := '';
    Exit;
  end;
  Result := FStringList.Strings[FDBLookupComboBox.ItemIndex];
end;

function TMyLookupCB.GetText: String;
begin
  Result := FDBLookupComboBox.Items[FDBLookupComboBox.ItemIndex];
end;

procedure TMyLookupCB.Refresh;
var
  LastID: String;
  i: Integer;
begin
  LastID := GetValue;
  with FDBLookupComboBox do
    CreateSQLQueryAndDataSource(Parent, FCurTable, Tag);
  CreateStringList;
  FDBLookupComboBox.ListSource := FDataSource;
  FDBLookupComboBox.ItemIndex := -1;
  for i := 0 to FStringList.Count - 1 do
    if FStringList.Strings[i] = LastID then
    begin
      FDBLookupComboBox.ItemIndex := i;
    end;
  {FSQLQuery := TSQLQuery.Create(AWinControl);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    SQL.AddStrings(Format('SELECT %s, %s FROM %s', [(Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedField,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName,
      (Tables[ACurTable].Fields[ACurField] as TMyJoinedField).ReferencedTable]));
    Open;
  end;

  FDataSource := TDataSource.Create(AWinControl);
  FDataSource.DataSet := FSQLQuery; }
end;

end.

