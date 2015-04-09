CREATE DATABASE 'D:\1FBD\Schedule\testBD\Schedule.fdb' user 'SYSDBA' password 'masterkey' DEFAULT CHARACTER SET WIN1251;
CREATE TABLE EducActivities
(
    EducID   INTEGER, 
    EducName VARCHAR (100) 
);
CREATE TABLE Teachers
( 
	TeacherID       INTEGER ,
	TeacherInitials VARCHAR (100)
);
CREATE TABLE Groups
(
	GroupID     INTEGER ,
	GroupNumber VARCHAR (100) ,
	GroupName   VARCHAR (100)  
);
CREATE TABLE Students
( 
	StudentID       INTEGER ,
	StudentInitials VARCHAR (100) ,
	GroupID         INTEGER 
);
CREATE TABLE Subjects
( 
	SubjectID   INTEGER ,
    SubjectName VARCHAR (100) 
);
CREATE TABLE Audiences
(
    AudienceID     INTEGER ,
	AudienceNumber VARCHAR (100)  
);
CREATE TABLE Pairs
(
	PairID     INTEGER ,
	PairBegin  VARCHAR (100) ,
	PairEnd    VARCHAR (100) ,
	PairNumber INTEGER  
);
CREATE TABLE WeekDays
(
	WeekDayID     INTEGER ,
	WeekDayName   VARCHAR (100) , 
	WeekDayNumber INTEGER  
);
CREATE TABLE Schedules
(
	GroupID    INTEGER ,
	WeekDayID  INTEGER ,
	PairID     INTEGER ,
	SubjectID  INTEGER ,
	EducID     INTEGER ,
	TeacherID  INTEGER ,
	AudienceID INTEGER 
);
CREATE TABLE Teachers_Subjects
(
	TeacherID INTEGER,
	SubjectID INTEGER
);
CREATE TABLE Group_Subjects
(
	GroupID INTEGER,
	SubjectID INTEGER
);

INSERT INTO EducActivities VALUES (1, '������');
INSERT INTO EducActivities VALUES (2, '��������');

INSERT INTO Teachers VALUES (101, '������ ��������� ���������');
INSERT INTO Teachers VALUES (102, '������� ����� ����������');
INSERT INTO Teachers VALUES (103, '��������� ���� �������������');
INSERT INTO Teachers VALUES (104, '����� ����� ������� ');
INSERT INTO Teachers VALUES (105, '������ ����� ���������');
INSERT INTO Teachers VALUES (106, '��� �������� ��������������');
INSERT INTO Teachers VALUES (107, '�������� �������� �������');
INSERT INTO Teachers VALUES (108, '������������� �����������');
INSERT INTO Teachers VALUES (109, '�������� ������ ���������');
INSERT INTO Teachers VALUES (110, '����������  ������� ������������');
INSERT INTO Teachers VALUES (111, '��������� ���� �����������');
INSERT INTO Teachers VALUES (112, '��� �.�.');
INSERT INTO Teachers VALUES (113, '�������� �.�.');
INSERT INTO Teachers VALUES (114, '������� ����� ����������');
INSERT INTO Teachers VALUES (115, '���������� ������� ����������');
INSERT INTO Teachers VALUES (116, '�������� ������� ��������');
INSERT INTO Teachers VALUES (117, '������� ����� �������������');
INSERT INTO Teachers VALUES (118, '��������� ����� ����������');
INSERT INTO Teachers VALUES (119, '������ ����� ����������');
INSERT INTO Teachers VALUES (120, '������� ������� ���������');
INSERT INTO Teachers VALUES (121, '��� ������� ������������');

INSERT INTO Groups VALUES (701, '�8103a', '���������� ���������� � �����������');
INSERT INTO Groups VALUES (702, '�8103�', '���������� ���������� � �����������');
INSERT INTO Groups VALUES (703, '�8203�', '���������� ���������� � �����������');
INSERT INTO Groups VALUES (704, '�8203�', '���������� ���������� � �����������');

INSERT INTO Students VALUES (201,  '������������ ������� ����������', 701);
INSERT INTO Students VALUES (202,  '�������� ����� ��������', 701);
INSERT INTO Students VALUES (203,  '������ ������ ���������', 701);
INSERT INTO Students VALUES (204,  '�������� ������ ����������', 701);
INSERT INTO Students VALUES (205,  '������� ������ ���������', 701);
INSERT INTO Students VALUES (206,  '���������� ���� ������������', 701);
INSERT INTO Students VALUES (207,  '������ ����� ���������', 701);
INSERT INTO Students VALUES (208,  '���� ������� ��������', 701);
INSERT INTO Students VALUES (209,  '�������� ������� ������������', 701);
INSERT INTO Students VALUES (210, '���������� ����� ��������', 701);
INSERT INTO Students VALUES (211, '������� ��������� ����������', 701);
INSERT INTO Students VALUES (212, '������ ����� ����������', 701);
INSERT INTO Students VALUES (213, '�������� ���������� ����������', 701);
INSERT INTO Students VALUES (214, '���� ���� ���������', 701);
INSERT INTO Students VALUES (215, '��������� ����� �������������', 701);
INSERT INTO Students VALUES (216, '����� ����� ����������', 701);
INSERT INTO Students VALUES (217, '�������� ����� ���������', 701);
INSERT INTO Students VALUES (218, '���������� ������� �����������', 701);
INSERT INTO Students VALUES (219, '����� ���� ����������', 701);
INSERT INTO Students VALUES (220, '������� ������� ���������', 701);
INSERT INTO Students VALUES (221, '���������� ��� ��������������', 701);
INSERT INTO Students VALUES (222, '��������� ������ �������������', 701);
INSERT INTO Students VALUES (223, '��������� ������� �������������', 701);
INSERT INTO Students VALUES (224, '������� ����� ���������', 701);
INSERT INTO Students VALUES (225, '�������� ������ ����������', 701);
INSERT INTO Students VALUES (226, '��������� ������ �������', 701);
INSERT INTO Students VALUES (227, '���������� ����� ���������', 701);
INSERT INTO Students VALUES (228, '����� ���������� ��������', 701);
INSERT INTO Students VALUES (229, '������� ���� �������������', 701);
INSERT INTO Students VALUES (230, '����� ������� �����������', 701);
INSERT INTO Students VALUES (231, '��������� ����� ����������', 701);
INSERT INTO Students VALUES (232, '���������� ����� ����������', 701);
INSERT INTO Students VALUES (233, '���� ������ ���������', 701);
INSERT INTO Students VALUES (234, '������ ������� ����������', 701);
INSERT INTO Students VALUES (235, '����� ������� ���������', 701);

INSERT INTO Subjects VALUES (301, '�������������� ������');
INSERT INTO Subjects VALUES (302, '������� � ���������');
INSERT INTO Subjects VALUES (303, '��������� �� ���');
INSERT INTO Subjects VALUES (304, '���������� ��������');
INSERT INTO Subjects VALUES (305, '����������� ����');
INSERT INTO Subjects VALUES (306, '���� ������');
INSERT INTO Subjects VALUES (307, '���������� ����������');
INSERT INTO Subjects VALUES (308, '����� � ������ ����������������');
INSERT INTO Subjects VALUES (309, '���������');
INSERT INTO Subjects VALUES (310, '��������-��������������� ������');
INSERT INTO Subjects VALUES (311, '������');
INSERT INTO Subjects VALUES (312, '���������������� ���������');
INSERT INTO Subjects VALUES (313, '����������� ������');
INSERT INTO Subjects VALUES (314, '������������ �����������������');
INSERT INTO Subjects VALUES (315, '��������� ������');
INSERT INTO Subjects VALUES (316, '������������� ������');

INSERT INTO Audiences VALUES (401,  'D734');
INSERT INTO Audiences VALUES (402,  'D549a');
INSERT INTO Audiences VALUES (403,  'D547');
INSERT INTO Audiences VALUES (404,  'D743');
INSERT INTO Audiences VALUES (405,  'D547');
INSERT INTO Audiences VALUES (406,  'D732');
INSERT INTO Audiences VALUES (407,  'D542');
INSERT INTO Audiences VALUES (408, 'D747');
INSERT INTO Audiences VALUES (409, '������������� ���');
INSERT INTO Audiences VALUES (410, 'D738');
INSERT INTO Audiences VALUES (411, 'D820');
INSERT INTO Audiences VALUES (412, 'D548');
INSERT INTO Audiences VALUES (413, 'D549'); 
INSERT INTO Audiences VALUES (414, 'D818');
INSERT INTO Audiences VALUES (415, 'D820');
INSERT INTO Audiences VALUES (416, 'D734a'); 

INSERT INTO Pairs VALUES (501, '8.30',  '10.10', 1);
INSERT INTO Pairs VALUES (502, '10.10', '11.40', 2);
INSERT INTO Pairs VALUES (503, '11.50', '13.20', 3);
INSERT INTO Pairs VALUES (504, '13.30', '15.00', 4);
INSERT INTO Pairs VALUES (505, '15.10', '16.40', 5);
INSERT INTO Pairs VALUES (506, '16.50', '18.20', 6);

INSERT INTO WeekDays VALUES (601, '�����������', 1);
INSERT INTO WeekDays VALUES (602, '�������', 2);
INSERT INTO WeekDays VALUES (603, '�����', 3);
INSERT INTO WeekDays VALUES (604, '�������', 4);
INSERT INTO WeekDays VALUES (605, '�������', 5);
INSERT INTO WeekDays VALUES (606, '�������', 6);
INSERT INTO WeekDays VALUES (607, '�����������', 7);

INSERT INTO Schedules VALUES (701, 601, 501, 303, 2, 107, 401);
INSERT INTO Schedules VALUES (701, 601, 502, 303, 2, 107, 401);

INSERT INTO Schedules VALUES (701, 602, 501, 302, 2, 106, 403);
INSERT INTO Schedules VALUES (701, 602, 502, 307, 2, 106, 407);
INSERT INTO Schedules VALUES (701, 602, 503, 301, 2, 103, 408);
INSERT INTO Schedules VALUES (701, 602, 504, 307, 2, 106, 403);

INSERT INTO Schedules VALUES (701, 603, 502, 307, 2, 106, 408);
INSERT INTO Schedules VALUES (701, 603, 503, 302, 1, 106, 408);
INSERT INTO Schedules VALUES (701, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (701, 603, 505, 304, 2, 104, 409);

INSERT INTO Schedules VALUES (701, 604, 501, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (701, 604, 502, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (701, 604, 503, 308, 1, 104, 410);

INSERT INTO Schedules VALUES (701, 605, 502, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (701, 605, 503, 301, 2, 103, 407);

INSERT INTO Schedules VALUES (701, 606, 502, 306, 2, 101, 401);
INSERT INTO Schedules VALUES (701, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (701, 606, 504, 305, 2, 110, 406);
INSERT INTO Schedules VALUES (701, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (702, 601, 501, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (702, 601, 502, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (702, 601, 503, 305, 2, 111, 404);
INSERT INTO Schedules VALUES (702, 601, 504, 302, 2, 106, 412);

INSERT INTO Schedules VALUES (702, 602, 502, 307, 1, 106, 407);
INSERT INTO Schedules VALUES (702, 602, 503, 302, 2, 106, 403);

INSERT INTO Schedules VALUES (702, 603, 503, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (702, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (702, 603, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (702, 604, 502, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (702, 604, 503, 308, 1, 104, 410);
INSERT INTO Schedules VALUES (702, 604, 504, 303, 2, 112, 401);
INSERT INTO Schedules VALUES (702, 604, 505, 303, 2, 112, 401);

INSERT INTO Schedules VALUES (702, 605, 501, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (702, 605, 502, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (702, 605, 503, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (702, 605, 502, 306, 2, 113, 401);

INSERT INTO Schedules VALUES (702, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (702, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (703, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (703, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (703, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (703, 601, 504, 303, 2, 101, 413);
INSERT INTO Schedules VALUES (703, 601, 505, 303, 2, 101, 413);

INSERT INTO Schedules VALUES (703, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (703, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (703, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (703, 603, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (703, 603, 504, 312, 2, 116, 411);
INSERT INTO Schedules VALUES (703, 603, 505, 305, 2, 117, 404);

INSERT INTO Schedules VALUES (703, 604, 501, 313, 1, 103, 407);
INSERT INTO Schedules VALUES (703, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (703, 604, 503, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (703, 604, 504, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (703, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (703, 605, 501, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (703, 605, 502, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (703, 605, 503, 316, 2, 120, 406);


INSERT INTO Schedules VALUES (704, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (704, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (704, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (704, 601, 504, 310, 2, 107, 406);
INSERT INTO Schedules VALUES (704, 601, 505, 310, 2, 107, 406);

INSERT INTO Schedules VALUES (704, 602, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (704, 602, 504, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (704, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (704, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (704, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (704, 603, 504, 305, 2, 117, 412);

INSERT INTO Schedules VALUES (704, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (704, 604, 503, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (704, 604, 504, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (704, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (704, 605, 502, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (704, 605, 503, 316, 2, 120, 406);
INSERT INTO Schedules VALUES (704, 605, 504, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (704, 605, 505, 304, 2, 108, 409);


INSERT INTO Teachers_Subjects VALUES (101, 306);
INSERT INTO Teachers_Subjects VALUES (103, 301);
INSERT INTO Teachers_Subjects VALUES (104, 308);
INSERT INTO Teachers_Subjects VALUES (106, 302);
INSERT INTO Teachers_Subjects VALUES (106, 307);
INSERT INTO Teachers_Subjects VALUES (107, 303);
INSERT INTO Teachers_Subjects VALUES (108, 304);
INSERT INTO Teachers_Subjects VALUES (109, 308);
INSERT INTO Teachers_Subjects VALUES (110, 305);
INSERT INTO Teachers_Subjects VALUES (111, 305);
INSERT INTO Teachers_Subjects VALUES (112, 303);
INSERT INTO Teachers_Subjects VALUES (113, 306);
INSERT INTO Teachers_Subjects VALUES (114, 309);
INSERT INTO Teachers_Subjects VALUES (105, 310);
INSERT INTO Teachers_Subjects VALUES (115, 311);
INSERT INTO Teachers_Subjects VALUES (117, 305);
INSERT INTO Teachers_Subjects VALUES (101, 303);
INSERT INTO Teachers_Subjects VALUES (103, 313);
INSERT INTO Teachers_Subjects VALUES (116, 312);
INSERT INTO Teachers_Subjects VALUES (121, 315);
INSERT INTO Teachers_Subjects VALUES (119, 314);
INSERT INTO Teachers_Subjects VALUES (118, 315);
INSERT INTO Teachers_Subjects VALUES (120, 316);

INSERT INTO Group_Subjects VALUES (701, 301);
INSERT INTO Group_Subjects VALUES (701, 302);
INSERT INTO Group_Subjects VALUES (701, 303);
INSERT INTO Group_Subjects VALUES (701, 304);
INSERT INTO Group_Subjects VALUES (701, 305);
INSERT INTO Group_Subjects VALUES (701, 306);
INSERT INTO Group_Subjects VALUES (701, 307);
INSERT INTO Group_Subjects VALUES (701, 308);
INSERT INTO Group_Subjects VALUES (702, 301);
INSERT INTO Group_Subjects VALUES (702, 302);
INSERT INTO Group_Subjects VALUES (702, 303);
INSERT INTO Group_Subjects VALUES (702, 304);
INSERT INTO Group_Subjects VALUES (702, 305);
INSERT INTO Group_Subjects VALUES (702, 306);
INSERT INTO Group_Subjects VALUES (702, 307);
INSERT INTO Group_Subjects VALUES (702, 308);
INSERT INTO Group_Subjects VALUES (703, 309);
INSERT INTO Group_Subjects VALUES (703, 310);
INSERT INTO Group_Subjects VALUES (703, 304);
INSERT INTO Group_Subjects VALUES (703, 311);
INSERT INTO Group_Subjects VALUES (703, 312);
INSERT INTO Group_Subjects VALUES (703, 305);
INSERT INTO Group_Subjects VALUES (703, 313);
INSERT INTO Group_Subjects VALUES (703, 314);
INSERT INTO Group_Subjects VALUES (703, 315);
INSERT INTO Group_Subjects VALUES (703, 303);
INSERT INTO Group_Subjects VALUES (703, 316);
INSERT INTO Group_Subjects VALUES (704, 309);
INSERT INTO Group_Subjects VALUES (704, 310);
INSERT INTO Group_Subjects VALUES (704, 304);
INSERT INTO Group_Subjects VALUES (704, 311);
INSERT INTO Group_Subjects VALUES (704, 312);
INSERT INTO Group_Subjects VALUES (704, 305);
INSERT INTO Group_Subjects VALUES (704, 313);
INSERT INTO Group_Subjects VALUES (704, 314);
INSERT INTO Group_Subjects VALUES (704, 315);
INSERT INTO Group_Subjects VALUES (704, 303);
INSERT INTO Group_Subjects VALUES (704, 316);

