unit UEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, DbCtrls, ExtCtrls, UMetadata, StdCtrls, UDBConnection;

type

  { TEditForm }

  TEditForm = class(TForm)
    BitBtn1: TBitBtn;
    DataSource1: TDataSource;
    DBComboBox1: TDBComboBox;
    DBLookupComboBox1: TDBLookupComboBox;
    Panel1: TPanel;
    SQLQuery1: TSQLQuery;
    procedure  CreateNew(AForm: TForm);
    function CreateCB(ACurField: Integer; AForm: TEditForm): TDBLookupComboBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

{ TEditForm }

procedure TEditForm.CreateNew(AForm: TForm);
var
  AEditForm: TEditForm;
  ACB: TDBLookupComboBox;
  i: Integer;
begin
  AEditForm := TEditForm.Create(Application);
  AEditForm.Tag := AForm.Tag;
  //CreateCB(0, AEditForm);
  for i := 0 to Tables[AForm.Tag].FieldsCount - 1 do
  begin
    CreateCB(i, AEditForm);
  end;
  AEditForm.Show;
end;

function TEditForm.CreateCB(ACurField: Integer; AForm: TEditForm
  ): TDBLookupComboBox;
var
  FCB: TDBLookupComboBox;
  FSQLQuery: TSQLQuery;
  FDataSource: TDataSource;
begin
  FSQLQuery := TSQLQuery.Create(AForm);
  with FSQLQuery do
  begin
    DataBase := DataModuleMain.IBConnection;
    Transaction := DataModuleMain.SQLTransaction;
    SQL.Clear;
    if Tables[AForm.Tag].Fields[ACurField] is TMyJoinedField then
      SQL.AddStrings(Format('SELECT %s FROM %s', [(Tables[AForm.Tag].Fields[ACurField] as TMyJoinedField).JoinedFieldName,
        (Tables[AForm.Tag].Fields[ACurField] as TMyJoinedField).ReferencedTable]))
    else
      SQL.AddStrings(Format('SELECT %s FROM %s', [Tables[AForm.Tag].Fields[ACurField].Name, Tables[AForm.Tag].Name]));
    Open;
  end;

  FDataSource := TDataSource.Create(AForm);
  FDataSource.DataSet := FSQLQuery;

  FCB := TDBLookupComboBox.Create(AForm);
  with FCB do
  begin
    Visible := True;
    Width := 200;
    Left := 65;
    Height := 23;
    Top := 5 + 28 * ACurField;
    Parent := AForm;
    if Tables[AForm.Tag].Fields[ACurField] is TMyJoinedField then
      KeyField := (Tables[AForm.Tag].Fields[ACurField] as TMyJoinedField).JoinedFieldName
    else
      KeyField := Tables[AForm.Tag].Fields[ACurField].Name;
    ListSource := FDataSource;
  end;
end;

end.

