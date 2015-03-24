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
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns(AForm: TFormListView; ACurTab: Integer);
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

procedure TFormListView.CreateNew(AName, ACaption: String; ATag: Integer);
var
  AForm: TFormListView;
begin
  AForm := TFormListView.Create(Application);
  AForm.Caption := ACaption;
  AForm.Name := AName;
  AForm.SQLQuery.Close;
  AForm.SQLQuery.SQL.Clear;
  AForm.SQLQuery.SQL.AddStrings(Tables[ATag].CreateSQlQuery);
  AForm.SQLQuery.Open;
  SetTableColumns(AForm, ATag);
  AForm.Show;
end;

procedure TFormListView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormListView.SetTableColumns(AForm: TFormListView; ACurTab: Integer);
var
  i, j: Integer;
begin
  for i := 0 to AForm.DBGrid.Columns.Count - 1 do
    for j := 0 to Tables[ACurTab].GetFieldsLength - 1 do
      if AForm.DBGrid.Columns[i].Title.Caption = UpperCase(Tables[ACurTab].Fields[j].Name)  then
      begin
        AForm.DBGrid.Columns[i].Title.Caption := Tables[ACurTab].Fields[j].Caption;
        AForm.DBGrid.Columns[i].Width := Tables[ACurTab].Fields[j].Width;
        AForm.DBGrid.Columns[i].Visible := Tables[ACurTab].Fields[j].Visible;
      end;
end;

end.
