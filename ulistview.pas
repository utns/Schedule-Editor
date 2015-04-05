unit UListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, DBGrids, ExtCtrls, StdCtrls, Buttons, UMetadata,
  USQLQueries, UFilters;

type

  { TFormListView }

  TFormListView = class(TForm)
    ButtonDeleteFilter: TButton;
    ButtonAddFilter: TButton;
    CBTypeOfSort: TComboBox;
    CBOrderBy: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure ButtonAddFilterClick(Sender: TObject);
    procedure ButtonDeleteFilterClick(Sender: TObject);
    procedure CreateNew(AName, ACaption: String; ATag: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetTableColumns;
    procedure AddCBItem;
    procedure AddNewFilters;
    procedure SpeedButton1Click(Sender: TObject);
  private
    Filters: array of TMainFilter;
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
  with AForm do
  begin
    Caption := ACaption;
    Name := AName;
    Tag := ATag;
    SQLQuery.Close;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(Tables[ATag].CreateSQlQuery(-1, -1, ''));
    SQLQuery.Open;
    SetTableColumns;
    AddCBItem;
    AddNewFilters;
    Show;
  end;
end;

procedure TFormListView.ButtonAddFilterClick(Sender: TObject);
begin
  AddNewFilters;
end;

procedure TFormListView.ButtonDeleteFilterClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 2 do
    Filters[High(Filters) - i].Free;
  SetLength(Filters, Length(Filters) - 3);
  ButtonAddFilter.Top := ScrollBox1.Tag;
  ButtonDeleteFilter.Top := ScrollBox1.Tag;
  if Length(Filters) = 3 then
   ButtonDeleteFilter.Visible := False;
end;

procedure TFormListView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormListView.SetTableColumns;
var
  i, j: Integer;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
    for j := 0 to Tables[Tag].FieldsCount - 1 do
    begin
      if DBGrid.Columns[i].Title.Caption = UpperCase(Tables[Tag].Fields[j].Name)  then
        with Tables[Tag].Fields[j] do
        begin
          DBGrid.Columns[i].Title.Caption := Caption;
          DBGrid.Columns[i].Width := Width;
          DBGrid.Columns[i].Visible := Visible;
        end;
      if (Tables[Tag].Fields[j] is TMyJoinedField) and (DBGrid.Columns[i].Title.Caption =
      UpperCase((Tables[Tag].Fields[j] as TMyJoinedField).JoinedFieldName))  then
        with (Tables[Tag].Fields[j] as TMyJoinedField) do
        begin
          DBGrid.Columns[i].Title.Caption := JoinedFieldCaption;
          DBGrid.Columns[i].Width := JoinedFieldWidth;
          DBGrid.Columns[i].Visible := JoinedVisible;
        end;
    end;
end;

procedure TFormListView.AddCBItem;
var
  i: Integer;
begin
  for i := 0 to Tables[Tag].FieldsCount - 1 do
    if Tables[Tag].Fields[i] is TMyJoinedField then
      CBOrderBy.Items.Add((Tables[Tag].Fields[i] as TMyJoinedField).JoinedFieldCaption)
    else
      CBOrderBy.Items.Add(Tables[Tag].Fields[i].Caption);
end;

procedure TFormListView.AddNewFilters;
begin
  SetLength(Filters, Length(Filters) + 3);
  ButtonDeleteFilter.Visible := True;
  Filters[High(Filters) - 2] := TCBColumnName.Create(ScrollBox1, Tag);
  Filters[High(Filters) - 1] := TCBFilterType.Create(ScrollBox1, Tag);
  Filters[High(Filters)] := TEFilterValue.Create(ScrollBox1, Tag);
  ButtonAddFilter.Top := ScrollBox1.Tag;
  ButtonDeleteFilter.Top := ScrollBox1.Tag;
end;

procedure TFormListView.SpeedButton1Click(Sender: TObject);
var
  s: String;
begin
  if Filters[2].GetFilterValue <> '' then
    if Tables[Tag].Fields[Filters[0].GetColumn] is TMyJoinedField then
      s := Format(' Where %s.%s %s %s' ,[(Tables[Tag].Fields[Filters[0].GetColumn] as TMyJoinedField).ReferencedTable,
        (Tables[Tag].Fields[Filters[0].GetColumn] as TMyJoinedField).JoinedFieldName,
        Filters[1].GetFilterType, Filters[2].GetFilterValue])
    else
      s := Format(' Where %s.%s %s %s' ,[Tables[Tag].Name , Tables[Tag].Fields[Filters[0].GetColumn].Name,
        Filters[1].GetFilterType, Filters[2].GetFilterValue]);
  SQLQuery.Close;
  SQLQuery.SQL.Clear;
  SQLQuery.SQL.AddStrings(Tables[Tag].CreateSQlQuery(CBOrderBy.ItemIndex, CBTypeOfSort.ItemIndex, s));
  SQLQuery.Open;
  SetTableColumns;
end;

end.
