unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, UMetadata, USQLQueries;

type

  { TFormListView }

  TFormListView = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    procedure CreateNew(AName, ACaption: String);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure EditTable(AForm: TFormListView; AName: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormListView: TFormListView;

implementation

{$R *.lfm}

{ TFormListView }

procedure TFormListView.CreateNew(AName, ACaption: String);
var
  AForm: TFormListView;
begin
  AForm := TFormListView.Create(Application);
  AForm.Caption := ACaption;
  AForm.Name := AName;
  AForm.SQLQuery.Close;
  AForm.SQLQuery.SQL.Clear;
  AForm.SQLQuery.SQL.AddStrings(EditSQlQuery(AName));
  AForm.SQLQuery.Open;
  EditTable(AForm, AName);
  AForm.Show;
end;

procedure TFormListView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormListView.EditTable(AForm: TFormListView; AName: String);
var
  i, j, CurTab: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = AName then
      CurTab := i;
  for i := 0 to AForm.DBGrid.Columns.Count - 1 do
    for j := 0 to Tables[CurTab].GetFieldsLength - 1 do
      if AForm.DBGrid.Columns[i].Title.Caption = UpperCase(Tables[CurTab].Fields[j].Name)  then
      begin
        AForm.DBGrid.Columns[i].Title.Caption := Tables[CurTab].Fields[j].Caption;
        AForm.DBGrid.Columns[i].Width := Tables[CurTab].Fields[j].Width;
      end;
end;

end.
