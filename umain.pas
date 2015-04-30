unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, UMetadata, sqldb, UListView, UAbout, UEditForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItemAbout: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemReference: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure AddReferenceItem(AName, ACaption: String; ATag: Integer);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    function IsFormOpen(AName: String): Boolean;
    procedure ActivateSQL;
    procedure CreateNewEditForm(ACurTable: Integer; AFormType: TFormType; AID: Integer);
    procedure RefreshEditForms;
  private
    ListViewForms: array of TFormListView;
    EditForms: array of TEditForm;
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    AddReferenceItem(Tables[i].Name, Tables[i].Caption, i);
 EActivateSQL := @ActivateSQL;
 ECreateNewEditForm := @CreateNewEditForm;
 ERefreshEditors := @RefreshEditForms;
end;

procedure TMainForm.AddReferenceItem(AName, ACaption: String; ATag: Integer);
var
  AReferenceItem: TMenuItem;
begin
  AReferenceItem := TMenuItem.Create(MainMenu);
  AReferenceItem.Caption := ACaption;
  AReferenceItem.Name := AName;
  AReferenceItem.Tag := ATag;
  AReferenceItem.OnClick := @MenuItemClick;
  MenuItemReference.Add(AReferenceItem);
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TMainForm.MenuItemClick(Sender: TObject);
begin
  if not(IsFormOpen((Sender as TMenuItem).Name)) then
  begin
    SetLength(ListViewForms, Length(ListViewForms) + 1);
    with (Sender as TMenuItem) do
      ListViewForms[High(ListViewForms)] := TFormListView.Create(Name, Caption, Tag);
  end;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  if MessageDlg('Выйти из программы?', mtConfirmation, mbYesNo, 0) = mrYes then
    MainForm.Close;
end;

function TMainForm.IsFormOpen(AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 To High(ListViewForms) do
    if (ListViewForms[i].Name = AName) then
    begin
      Result := True;
      ListViewForms[i].ShowOnTop;
      Break;
    end;
end;

procedure TMainForm.ActivateSQL;
var
  i: Integer;
begin
  for i := 0 to High(ListViewForms) do
    ListViewForms[i].OpenSQLQuery;
end;

procedure TMainForm.CreateNewEditForm(ACurTable: Integer; AFormType: TFormType;
  AID: Integer);
var
  i: Integer;
  FormName: String;
  IsFormExists: Boolean = False;
begin
  FormName := 'EditForm' + IntToStr(AID);
  for i := 0 to High(EditForms) do
    if FormName = EditForms[i].Name then
    begin
      IsFormExists := True;
      EditForms[i].ShowOnTop;
      Exit;
    end;
  SetLength(EditForms, Length(EditForms) + 1);
  EditForms[High(EditForms)] := TEditForm.Create(ACurTable, AFormType, AID);
end;

procedure TMainForm.RefreshEditForms;
var
  i: Integer;
begin
  for i := 0 to High(EditForms) do
    EditForms[i].RefreshEditors;
end;

end.

