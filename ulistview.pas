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
    for j := 0 to Tables[ACurTab].FieldsCount - 1 do
    begin
      if AForm.DBGrid.Columns[i].Title.Caption = UpperCase(Tables[ACurTab].Fields[j].Name)  then
        with Tables[ACurTab].Fields[j] do
        begin
          AForm.DBGrid.Columns[i].Title.Caption := Caption;
          AForm.DBGrid.Columns[i].Width := Width;
          AForm.DBGrid.Columns[i].Visible := Visible;
        end;
      if (Tables[ACurTab].Fields[j] is TMyJoinedField) and (AForm.DBGrid.Columns[i].Title.Caption =
      UpperCase((Tables[ACurTab].Fields[j] as TMyJoinedField).JoinedFieldName))  then
        with (Tables[ACurTab].Fields[j] as TMyJoinedField) do
        begin
          AForm.DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          AForm.DBGrid.Columns[i].Width := JoinedFieldWidth;
          AForm.DBGrid.Columns[i].Visible := JoinedVisible;
        end;
    end;
end;

end.
