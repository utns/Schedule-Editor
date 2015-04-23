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
    BitBtn1: TBitBtn;
    DataSource1: TDataSource;
    DBLookupComboBox1: TDBLookupComboBox;
    SQLQuery1: TSQLQuery;
    procedure BitBtn1Click(Sender: TObject);
    procedure  CreateNew(ACurTable: Integer; ADataSource: TDataSource; AFormType: TFormType; ASQLQuery: TSQLQuery);
    procedure CreateCB(ACurField: Integer);
    procedure CreateEdit(ACurField: Integer);
    procedure CreateLable(ACurField: Integer);
  private
    ListViewDataSource: TDataSource;
    ListViewSQLQuery: TSQLQuery;
    FormType: TFormType;
    CurTable: Integer;
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
    for i := 0 to Tables[ACurTable].FieldsCount - 1 do
    begin
      if Tables[ACurTable].Fields[i] is TMyJoinedField then
        CreateCB(i)
      else
        CreateEdit(i);
      CreateLable(i);
    end;
    Show;
  end;



end;

procedure TEditForm.BitBtn1Click(Sender: TObject);
begin
  //DataModuleMain.SQLTransaction.Commit;
end;

procedure TEditForm.CreateCB(ACurField: Integer);
var
  FDBEdit: TDBEdit;
  FLookupCB: TDBLookupComboBox;
  FSQLQuery: TSQLQuery;
  FDataSource: TDataSource;
begin
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
    Top := 5 + 28 * ACurField;
    Parent := Self;
    KeyField := (Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName;
    ListSource := FDataSource;
    if FormType = ftEdit then
    begin
      ItemIndex := Items.IndexOf(ListViewSQLQuery.Fields.
      FieldByName((Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldName).Value);
    end;
  end;
end;

procedure TEditForm.CreateEdit(ACurField: Integer);
var
  FEdit: TEdit;
begin
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Visible := True;
    Width := 250;
    Left := 110;
    Height := 23;
    Top := 5 + 28 * ACurField;
    Parent := Self;
    if FormType = ftEdit then
    begin
      Text := ListViewSQLQuery.Fields.FieldByName(Tables[CurTable].Fields[ACurField].Name).Value;
    end;
  end;
end;

procedure TEditForm.CreateLable(ACurField: Integer);
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
    Top := 5 + 28 * ACurField;
    Parent := Self;
    if Tables[CurTable].Fields[ACurField] is TMyJoinedField then
      Text := (Tables[CurTable].Fields[ACurField] as TMyJoinedField).JoinedFieldCaption
    else
      Text := Tables[CurTable].Fields[ACurField].Caption;
  end;
end;

end.

