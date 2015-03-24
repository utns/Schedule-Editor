unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyField }

  TMyField = class
  private
    FName, FCaption: String;
    FWidth: Integer;
    FVisible: Boolean;
  public
    Constructor Create(AName, ACaption: String; AWidth: Integer; AVisible: Boolean);
    property Name: String read FName;
    property Caption: String read FCaption;
    property Width: Integer read FWidth;
    property Visible: Boolean read FVisible;
  end;

  { TMyJoinedField }

  TMyJoinedField = class(TMyField)
  private
    FReferencedTable, FReferencedField, FJoinedFieldName, FJoinedFieldCaption: String;
    FJoinedVisible: Boolean;
  public
    Constructor Create(AName, ACaption, AReferencedTable, AReferencedField, AVisField: String; AWidth: Integer);
    property ReferencedTable: String read FReferencedTable;
    property ReferencedField: String read FReferencedField;
    property JoinedFieldName: String read FJoinedFieldName;
    property JoinedFieldCaption: String read FJoinedFieldCaption;
    property JoinedVisible: Boolean read FJoinedVisible;
  end;

  { TMyTable }

  TMyTable = class
  private
    FName, FCaption: String;
    FFields: array of TMyField;
    function GetField(Index: Integer): TMyField;
  public
    Constructor Create(AName, ACaption: String);
    function CreateSqlQuery: String;
    procedure AddNewField(AName, ACaption: String; AWidth: Integer; AVisible: Boolean);
    procedure AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField: String;
      AWidth: Integer);
    function GetFieldsLength: Integer;
    property Name: String read FName;
    property Caption: String read FCaption;
    property Fields[Index: Integer]: TMyField read GetField;
  end;

  var
    Tables: array of TMyTable;

implementation

function RegisterTable(AName, ACaption: String): TMyTable;
var
  ATable: TMyTable;
begin
  SetLength(Tables, Length(Tables) + 1);
  ATable := TMyTable.Create(AName, ACaption);
  Tables[High(Tables)] := ATable;
  Result := ATable;
end;

procedure RegisterTableField(ATableName, AName, ACaption: String;
  AWidth: Integer; AVisible: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
    begin
      Tables[i].AddNewField(AName, ACaption, AWidth, AVisible);
      Break;
    end;
end;

procedure RegisterTableField(ATableName, AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField: String;
  AWidth: Integer);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
    begin                                   //плохо
      Tables[i].AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField, AWidth);
      Break;
    end;
end;

{ TMyJoinedField }

constructor TMyJoinedField.Create(AName, ACaption, AReferencedTable, AReferencedField,
  AVisField: String; AWidth: Integer);
begin
  inherited Create(AName, ACaption, AWidth, True);
  FReferencedTable := AReferencedTable;
  FReferencedField := AReferencedField;
  FJoinedFieldName := AVisField;
end;

{ TMyField }

constructor TMyField.Create(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean);
begin
  Inherited Create;
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FVisible := AVisible;
end;

{ TMyTable }

function TMyTable.GetField(Index: Integer): TMyField;
begin
  Result := FFields[Index];
end;

constructor TMyTable.Create(AName, ACaption: String);
begin
  Inherited Create;
  FName := AName;
  FCaption := ACaption;
end;

function TMyTable.CreateSqlQuery: String;
var
  i: Integer;
begin
  if (Fields[0] is TMyJoinedField) then
    Result := 'Select ' + (Fields[0] as TMyJoinedField).ReferencedTable
    + '.' + (Fields[0] as TMyJoinedField).JoinedFieldName + ' AS ' + Fields[0].Name
  else
    Result := 'Select ' + Name + '.' + Fields[0].Name;

  for i := 1 to GetFieldsLength - 1 do
    if (Fields[i] is TMyJoinedField) then
      Result += ', ' + (Fields[i] as TMyJoinedField).ReferencedTable
        + '.' + (Fields[i] as TMyJoinedField).JoinedFieldName + ' AS ' + Fields[i].Name
    else
      Result += ', ' + Name + '.' + Fields[i].Name;

  Result += ' FROM ' + Name;

  for i := 0 to GetFieldsLength - 1 do
    if Fields[i] is TMyJoinedField then
      Result += ' INNER JOIN ' + (Fields[i] as TMyJoinedField).ReferencedTable
        + ' ON ' + Name + '.' + Fields[i].Name
        + ' = ' + (Fields[i] as TMyJoinedField).ReferencedTable + '.'
        + (Fields[i] as TMyJoinedField).ReferencedField;
  Result += ' ORDER BY 1, 2';
end;

procedure TMyTable.AddNewField(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyField.Create(AName, ACaption, AWidth, AVisible);
end;

procedure TMyTable.AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName,
  AVisField: String; AWidth: Integer);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyJoinedField.Create(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField, AWidth);
end;

function TMyTable.GetFieldsLength: Integer;
begin
  Result := Length(FFields);
end;


initialization
  with RegisterTable('EducActivities', 'Учебная деятельность') do
  begin
    AddNewField('EducID', 'ИД', 35, True);
    AddNewField('EducName', 'Название', 100, True);
  end;

  with RegisterTable('Teachers', 'Преподаватели') do
  begin
    AddNewField('TeacherID', 'ИД', 35, True);
    AddNewField('TeacherInitials', 'Ф.И.О. преподавателя', 250, True);
  end;

  with RegisterTable('Groups', 'Группы') do
  begin
    AddNewField('GroupID', 'ИД', 35, True);
    AddNewField('GroupNumber', 'Номер', 55, True);
    AddNewField('GroupName', 'Название группы', 285, True);
  end;

  with RegisterTable('Students', 'Студенты') do
  begin
    AddNewField('StudentID', 'ИД', 35, True);
    AddNewField('StudentInitials', 'Ф.И.О. студента', 250, True);
    RegisterTableField('Students', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);
  end;

  with RegisterTable('Subjects', 'Предметы') do
  begin
    AddNewField('SubjectID', 'ИД', 35, True);
    AddNewField('SubjectName', 'Название предмета', 250, True);
  end;

  with RegisterTable('Audiences', 'Аудитории') do
  begin
    AddNewField('AudienceID', 'ИД', 35, True);
    AddNewField('AudienceNumber', 'Номер', 150, True);
  end;

  with RegisterTable('Pairs', 'Пары') do
  begin
    AddNewField('PairID', 'ИД', 35, True);
    AddNewField('PairBegin', 'Начало', 55, True);
    AddNewField('PairEnd', 'Конец', 50, True);
    AddNewField('PairNumber', 'Номер', 45, True);
  end;

  with RegisterTable('WeekDays', 'Дни недели') do
  begin
    AddNewField('WeekDayID', 'ИД', 35, True);
    AddNewField('WeekDayName', 'Название', 90, True);
  end;

  RegisterTable('Schedules', 'Расписание');
  RegisterTableField('Schedules', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);
  RegisterTableField('Schedules', 'WeekDayID', 'День недели', 'WeekDays', 'WeekDayID', 'WeekDayName', 90);
  RegisterTableField('Schedules', 'PairID', '№ пары', 'Pairs', 'PairID', 'PairNumber', 55);
  RegisterTableField('Schedules', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 190);
  RegisterTableField('Schedules', 'EducID', 'Форма занятия', 'EducActivities', 'EducID', 'EducName', 100);
  RegisterTableField('Schedules', 'TeacherID', 'Преподаватель', 'Teachers', 'TeacherID', 'TeacherInitials', 250);
  RegisterTableField('Schedules', 'AudienceID', 'Аудитория', 'Audiences', 'AudienceID', 'AudienceNumber', 70);

  RegisterTable('Teachers_Subjects', 'Предметы преподавателя');
  RegisterTableField('Teachers_Subjects', 'TeacherID', 'Преподаватель', 'Teachers', 'TeacherID'
    ,'TeacherInitials', 250);
  RegisterTableField('Teachers_Subjects', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 240);

  RegisterTable('Group_Subjects', 'Предметы групп');
  RegisterTableField('Group_Subjects', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);
  RegisterTableField('Group_Subjects', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 190);
end.
