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
    FJoinedWidth: Integer;
    FJoinedVisible: Boolean;
  public
    constructor Create(AName, ACaption: String; AWidth: Integer; AVisible: Boolean;
      AReferencedTable, AReferencedField, AJoinedFieldName, AJoinedFieldCaption: String;
      AJoinedWidth: Integer; AJoinedVisible: Boolean);
    property ReferencedTable: String read FReferencedTable;
    property ReferencedField: String read FReferencedField;
    property JoinedFieldName: String read FJoinedFieldName;
    property JoinedFieldCaption: String read FJoinedFieldCaption;
    property JoinedFieldWidth: Integer read FJoinedWidth;
    property JoinedVisible: Boolean read FJoinedVisible;
  end;

  { TMyTable }

  TMyTable = class
  private
    FName, FCaption: String;
    FFields: array of TMyField;
    function GetField(Index: Integer): TMyField;
  public
    constructor Create(AName, ACaption: String);
    function CreateSqlQuery(ASortColumn, ATypeSort: Integer; AWhere: String): String;
    function SqlSelect: String;
    function SqlInnerJoin: String;
    function SqlOrderBy(ASortColumn, ATypeSort: Integer): String;
    procedure AddNewField(AName, ACaption: String; AWidth: Integer; AVisible: Boolean);
    procedure AddNewField(AName, ACaption: String; AWidth: Integer; AVisible: Boolean;
      AReferencedTable, AReferencedField, AJoinedFieldName, AJoinedFieldCaption: String;
      AJoinedWidth: Integer; AJoinedVisible: Boolean);
    function FieldsCount: Integer;
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

procedure RegisterTableField(ATableName, AName, ACaption: String; AWidth: Integer; AVisible: Boolean;
      AReferencedTable, AReferencedField, AJoinedFieldName, AJoinedFieldCaption: String;
      AJoinedWidth: Integer; AJoinedVisible: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
    begin
      Tables[i].AddNewField(AName, ACaption, AWidth, AVisible, AReferencedTable, AReferencedField,
        AJoinedFieldName, AJoinedFieldCaption, AJoinedWidth, AJoinedVisible);
      Break;
    end;
end;

{ TMyJoinedField }

constructor TMyJoinedField.Create(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean; AReferencedTable, AReferencedField, AJoinedFieldName,
  AJoinedFieldCaption: String; AJoinedWidth: Integer; AJoinedVisible: Boolean);
begin
  inherited Create(AName, ACaption, AWidth, AVisible);
  FReferencedTable := AReferencedTable;
  FReferencedField := AReferencedField;
  FJoinedFieldName := AJoinedFieldName;
  FJoinedFieldCaption := AJoinedFieldCaption;
  FJoinedWidth := AJoinedWidth;
  FJoinedVisible := AJoinedVisible;
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

function TMyTable.CreateSqlQuery(ASortColumn, ATypeSort: Integer; AWhere: String
  ): String;
begin
  Result := SqlSelect;
  Result += SqlInnerJoin;
  Result += AWhere;
  Result += SqlOrderBy(ASortColumn, ATypeSort);
end;

function TMyTable.SqlSelect: String;
var
  i: Integer;
begin
  if (Fields[0] is TMyJoinedField) then
    Result := 'Select ' + (Fields[0] as TMyJoinedField).ReferencedTable
    + '.' + (Fields[0] as TMyJoinedField).JoinedFieldName
  else
    Result := 'Select ' + Name + '.' + Fields[0].Name;

  for i := 1 to FieldsCount - 1 do
  begin

  if (Fields[i] is TMyJoinedField) then
    Result += ', ' + (Fields[i] as TMyJoinedField).ReferencedTable
      + '.' + (Fields[i] as TMyJoinedField).JoinedFieldName
    else
      Result += ', ' + Name + '.' + Fields[i].Name;
  end;

  Result += ' FROM ' + Name;
end;

function TMyTable.SqlInnerJoin: String;
var
  i: Integer;
begin
  for i := 0 to FieldsCount - 1 do
    if Fields[i] is TMyJoinedField then
      Result += ' INNER JOIN ' + (Fields[i] as TMyJoinedField).ReferencedTable
        + ' ON ' + Name + '.' + Fields[i].Name
        + ' = ' + (Fields[i] as TMyJoinedField).ReferencedTable + '.'
        + (Fields[i] as TMyJoinedField).ReferencedField;
end;

function TMyTable.SqlOrderBy(ASortColumn, ATypeSort: Integer): String;
var
  i: Integer;
begin
  if (ASortColumn >= 0) and (ATypeSort > 0) then
  begin
    if (Fields[ASortColumn] is TMyJoinedField) then
      Result := ' ORDER BY ' + (Fields[ASortColumn] as TMyJoinedField).JoinedFieldName
    else
      Result := ' ORDER BY ' + Fields[ASortColumn].Name;
    if ATypeSort = 2 then
      Result += ' DESC';
  end;
end;

procedure TMyTable.AddNewField(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyField.Create(AName, ACaption, AWidth, AVisible);
end;

procedure TMyTable.AddNewField(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean; AReferencedTable, AReferencedField, AJoinedFieldName,
  AJoinedFieldCaption: String; AJoinedWidth: Integer; AJoinedVisible: Boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyJoinedField.Create(AName, ACaption, AWidth, AVisible, AReferencedTable, AReferencedField,
    AJoinedFieldName, AJoinedFieldCaption, AJoinedWidth, AJoinedVisible);
end;

function TMyTable.FieldsCount: Integer;
begin
  Result := Length(FFields);
end;


initialization
  with RegisterTable('EducActivities', 'Учебная деятельность') do
  begin
    AddNewField('EducID', 'ИД', 35, False);
    AddNewField('EducName', 'Название', 100, True);
  end;

  with RegisterTable('Teachers', 'Преподаватели') do
  begin
    AddNewField('TeacherID', 'ИД', 35, False);
    AddNewField('TeacherInitials', 'Ф.И.О. преподавателя', 250, True);
  end;

  with RegisterTable('Groups', 'Группы') do
  begin
    AddNewField('GroupID', 'ИД', 35, False);
    AddNewField('GroupNumber', 'Номер', 55, True);
    AddNewField('GroupName', 'Название группы', 285, True);
  end;

  with RegisterTable('Students', 'Студенты') do
  begin
    AddNewField('StudentID', 'ИД', 35, False);
    AddNewField('StudentInitials', 'Ф.И.О. студента', 250, True);
    AddNewField('GroupID', 'ИД группы', 75, False, 'Groups', 'GroupID', 'GroupNumber', 'Группа', 55, True);
  end;

  with RegisterTable('Subjects', 'Предметы') do
  begin
    AddNewField('SubjectID', 'ИД', 35, False);
    AddNewField('SubjectName', 'Название предмета', 250, True);
  end;

  with RegisterTable('Audiences', 'Аудитории') do
  begin
    AddNewField('AudienceID', 'ИД', 35, False);
    AddNewField('AudienceNumber', 'Номер', 150, True);
  end;

  with RegisterTable('Pairs', 'Пары') do
  begin
    AddNewField('PairID', 'ИД', 35, False);
    AddNewField('PairBegin', 'Начало', 55, True);
    AddNewField('PairEnd', 'Конец', 50, True);
    AddNewField('PairNumber', 'Номер', 45, True);
  end;

  with RegisterTable('WeekDays', 'Дни недели') do
  begin
    AddNewField('WeekDayID', 'ИД', 35, False);
    AddNewField('WeekDayName', 'Название', 90, True);
  end;

  with RegisterTable('Schedules', 'Расписание') do
  begin
    AddNewField('ScheduleID', 'ИД', 35, False);
    AddNewField('GroupID', 'ИД группы', 75, False, 'Groups', 'GroupID', 'GroupNumber', 'Группа', 55, True);
    AddNewField('WeekDayID', 'ИД дня', 75, False, 'WeekDays', 'WeekDayID', 'WeekDayName', 'День недели', 90, True);
    AddNewField('PairID', 'ИД пары', 75, False, 'Pairs', 'PairID', 'PairNumber', '№ пары', 55, True);
    AddNewField('SubjectID', 'ИД предмета', 75, False, 'Subjects', 'SubjectID', 'SubjectName', 'Предмет', 190, True);
    AddNewField('EducID', 'ИД занятия', 75, False, 'EducActivities', 'EducID', 'EducName', 'Тип занатия', 100, True);
    AddNewField('TeacherID', 'ИД преподавателя', 75, False, 'Teachers', 'TeacherID', 'TeacherInitials', 'Преподаватель', 250, True);
    AddNewField('AudienceID', 'ИД аудитории', 75, False, 'Audiences', 'AudienceID', 'AudienceNumber', 'Аудитория', 70, True);
  end;

  with RegisterTable('Teachers_Subjects', 'Предметы преподавателя') do
  begin
    AddNewField('ID', 'ИД', 35, False);
    AddNewField('TeacherID', 'ИД преподавателя', 75, False, 'Teachers', 'TeacherID', 'TeacherInitials', 'Преподаватель', 250, True);
    AddNewField('SubjectID', 'ИД предмета', 75, False, 'Subjects', 'SubjectID', 'SubjectName', 'Предмет', 190, True);
  end;

  with RegisterTable('Group_Subjects', 'Предметы групп') do
  begin
    AddNewField('ID', 'ИД', 35, False);
    AddNewField('GroupID', 'ИД группы', 75, False, 'Groups', 'GroupID', 'GroupNumber', 'Группа', 55, True);
    AddNewField('SubjectID', 'ИД предмета', 75, False, 'Subjects', 'SubjectID', 'SubjectName', 'Предмет', 190, True);
  end;

end.

