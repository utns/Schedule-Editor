unit USQLQueries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata;

function EditSQlQuery(ACurTab: Integer): String;

implementation

function EditSQlQuery(ACurTab: Integer): String;
var
  i: Integer;
begin
  with Tables[ACurTab] do
  begin
    if (Fields[0] is TMyJoinedField) then
      Result := 'Select ' + (Fields[0] as TMyJoinedField).JoinedTable
      + '.' + (Fields[0] as TMyJoinedField).VisField + ' AS ' + Fields[0].Name
    else
      Result := 'Select ' + Name + '.' + Fields[0].Name;

    for i := 1 to GetFieldsLength - 1 do
      if (Fields[i] is TMyJoinedField) then
        Result += ', ' + (Fields[i] as TMyJoinedField).JoinedTable
          + '.' + (Fields[i] as TMyJoinedField).VisField + ' AS ' + Fields[i].Name
      else
        Result += ', ' + Name + '.' + Fields[i].Name;

    Result += ' FROM ' + Name;

    for i := 0 to GetFieldsLength - 1 do
      if Fields[i] is TMyJoinedField then
        Result += ' INNER JOIN ' + (Fields[i] as TMyJoinedField).JoinedTable
          + ' ON ' + Name + '.' + Fields[i].Name
          + ' = ' + (Fields[i] as TMyJoinedField).JoinedTable + '.'
          + (Fields[i] as TMyJoinedField).JoinedField;
    Result += ' ORDER BY 1, 2';
  end;
end;

end.

