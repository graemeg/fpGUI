{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
        This unit allows you to generate random data. This is very handy
        for testing purposes.
        This code originates from the InstantObjects project, but has been
        adapted for our needs.
}

unit RandomData;

{$mode objfpc}{$H+}

interface

type
  TGender = (gnMale, gnFemale);

function RandomName: string;
function RandomCompanyName: string;
function RandomEmail(const PersonName, DomainName: string): string;
function RandomFirstName(Gender: TGender): string;
function RandomFullName(Gender: TGender): string;
function RandomLastName: string;
function RandomNumber(Len: Integer): string;
function RandomStreet: string;
function RandomCity: string;

implementation

uses
  SysUtils;

const
  Letters = ['a'..'z'];
  Vowels = ['a', 'e', 'i', 'o', 'u', 'y'];
  Consonants = Letters - Vowels;

  SmartPrefixes: array[0..19] of string = (
    'cor', 'pri', 'pre', 'neo', 'new', 'or', 'hi', 'inter',
    'u', 'i', 'core', 'xo', 'xe', 'xor', 'xer', 'qua', 'gen',
    'in', 're', 'tri');

  SmartSuffixes: array[0..12] of string = (
    'teq', 'tex', 'tec', 'nix', 'lix', 'tor', 'max', 'time', 'qua',
    'paq', 'pac', 'nic', 'nec');

  FirstNames: array[TGender, 0..33] of string = ((
    'Anthony', 'Brian', 'Charles', 'Christopher', 'Daniel', 'David', 'Donald',
    'Edward', 'George', 'James', 'Jason', 'Jeff', 'John', 'Joseph', 'Kenneth',
    'Kevin', 'Mark', 'Michael', 'Paul', 'Richard', 'Robert', 'Ronald', 'Steven',
    'Thomas', 'William', 'Graeme', 'Jaco', 'Martin', 'Sam', 'Dale', 'Johan',
    'Zayn', 'Xiam', 'René'), (
    'Barbara', 'Betty', 'Carol', 'Deborah', 'Donna', 'Dorothy', 'Elizabeth',
    'Helen', 'Jennifer', 'Karen', 'Kimberly', 'Laura', 'Linda', 'Lisa',
    'Margaret', 'Maria', 'Mary', 'Michelle', 'Nancy', 'Patricia', 'Ruth',
    'Sandra', 'Sarah', 'Sharon', 'Susan', 'Debbie', 'Anthea', 'Theonette',
    'Amelia', 'Adrie', 'Petro', 'Yolanda', 'Zelda', 'René'));

  LastNames: array[0..53] of string = (
    'Adams', 'Allen', 'Anderson', 'Baker', 'Brown', 'Campbell', 'Carter',
    'Clark', 'Collins', 'Davis', 'Edwards', 'Evans', 'Garcia', 'Gonzalez',
    'Green', 'Hall', 'Harris', 'Hernandez', 'Hill', 'Jackson', 'Johnson',
    'Jones', 'King', 'Lee', 'Lewis', 'Lopez', 'Martin', 'Martinez', 'Miller',
    'Mitchell', 'Moore', 'Nelson', 'Parker', 'Perez', 'Phillips', 'Roberts',
    'Robinson', 'Rodriguez', 'Scott', 'Smith', 'Taylor', 'Thomas', 'Thompson',
    'Turner', 'Walker', 'White', 'Williams', 'Wilson', 'Wright', 'Young',
    'Geldenhuys', 'Welgens', 'Botha', 'Guthrie');

  Adjectives: array[0..19] of string = (
    'Intelligent', 'Smart', 'Wise', 'Bright', 'Essential', 'Basic', 'Happy',
    'Precise', 'Fast', 'Express', 'Simple', 'Good', 'Deluxe', 'Perfect',
    'Star', 'Future', 'Millenium', 'Solid', 'Sure', 'Great');

  CompanySuffixes: array[0..19] of string = (
    'Computers', 'Computing', 'Software', 'Technologies', 'Consulting', 'Food',
    'Furniture', 'Textiles', 'Wear', 'Toys', 'Adventures', 'Services', 'Tools',
    'Accessories', 'Entertainment', 'Flowers', 'Equipment', 'Items',
    'Architecture', 'Knowledge');

  CompanyTypes: array[0..2] of string = (
    'Corp.', 'Inc.', 'Ltd.');

  StreetPrefixes: array[0..3] of string = (
    'North', 'East', 'South', 'West');

  StreetBeginnings: array[0..16] of string = (
    'Salt', 'Sun', 'Moon', 'Stone', 'Rock', 'Clear', 'White', 'Green',
    'Corn', 'Shore', 'Cotton', 'Oak', 'Water', 'Bright', 'New', 'Old',
    'Apple');

  StreetEndings: array[0..6] of string = (
    'wood', 'dale', 'land', 'bridge', 'lake', 'hill', 'field');

  StreetTypes: array[0..7] of string = (
    'Street', 'Road', 'Avenue', 'Place', 'Court', 'Boulevard', 'Drive',
    'Parkway');

  Cities: array[0..36] of string = (
    'Alabama', 'Atlanta', 'Boston', 'Chicago', 'Cincinnati', 'Dallas', 'Denver',
    'Detroit', 'Houston', 'Indianapolis', 'Kansas City', 'Las Vegas',
    'Los Angeles', 'Memphis', 'Miami', 'Minneapolis', 'Nashville',
    'New Orleans', 'New York', 'Oklahoma', 'Omaha', 'Philadelphia',
    'Pittsburgh', 'Portland', 'San Diego', 'San Fransisco', 'Seattle', 'Tampa',
    'Utah', 'Washington', 'Somerset West', 'Cape Town', 'Bloemfontein',
    'Durban', 'Pretoria', 'London', 'Paris');

  DomainSuffixes: array[0..6] of string = (
    'com', 'org', 'net', 'edu', 'gov', 'co.za', 'co.uk');


function RandomStr(List: array of string): string;
begin
  Result := List[Random(Length(List))];
end;

function RandomLetter: Char;
begin
  Result := Chr(Ord('a') + Random(Ord('z') - Ord('a')));
end;

function RandomConsonant: Char;
begin
  repeat
    Result := RandomLetter;
  until Result in Consonants;
end;

function RandomVowel: Char;
begin
  repeat
    Result := RandomLetter;
  until Result in Vowels;
end;

function RandomName: string;

  function SmartName: string;
  begin
    Result := RandomStr(SmartPrefixes) + RandomStr(SmartSuffixes);
  end;

  function CombiName: string;
  begin
    Result := RandomConsonant + RandomVowel;
    if Odd(Random(2)) then
      Result := Result + RandomConsonant;
    Result := Result + RandomStr(SmartSuffixes);
  end;

  function TripletName: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to 3 do
    begin
      Result := Result + RandomConsonant + RandomVowel;
      if Random(10) = 1 then
        Result := Result + RandomConsonant;
    end;
  end;

begin
  case Random(3) of
    0: Result := SmartName;
    1: Result := CombiName;
    2: Result := TripletName;
  end;
  Result[1] := UpCase(Result[1]);
end;

function RandomCompanyName: string;
begin
  if Random(3) = 0 then
    Result := RandomStr(Adjectives)
  else
    Result := RandomName;
  Result := Result + ' ' + RandomStr(CompanySuffixes) + ' ' +
    RandomStr(CompanyTypes);
end;

function RandomEmail(const PersonName, DomainName: string): string;
var
  Name, FirstName, LastName, Suffix: string;
  I: Integer;
begin
  I := Pos(' ', PersonName);
  if I > 0 then
  begin
    FirstName := Copy(PersonName, 1, Pred(I));
    LastName := PersonName;
    Delete(LastName, 1, I);
  end else
  begin
    FirstName := PersonName;
    LastName := '';
  end;
  case Random(3) of
    0: Name := FirstName + '.' + LastName;
    1: Name := Copy(FirstName, 1, 1) + Copy(LastName, 1, 1);
    2: Name := Copy(FirstName, 1, 1) + '_' + LastName;
  end;
  if Random(10) <> 0 then // 9 of 10 have .com
    Suffix := DomainSuffixes[0] else
    Suffix := RandomStr(DomainSuffixes);
  Result := LowerCase(Name + '@' + DomainName + '.' + Suffix);
end;

function RandomFirstName(Gender: TGender): string;
begin
  Result := FirstNames[Gender, Random(Length(FirstNames[gnMale]))];
end;

function RandomFullName(Gender: TGender): string;
begin
  Result := RandomFirstName(Gender) + ' ' + RandomLastName;
end;

function RandomLastName: string;
begin
  Result := RandomStr(LastNames);
end;

function RandomNumber(Len: Integer): string;
begin
  Result := '';
  while Len > 0 do
  begin
    Result := Result + Chr(Ord('0') + Random(10));
    Dec(Len);
  end;
end;

function RandomStreet: string;
begin
  if Random(8) = 0 then
    Result := RandomStr(StreetPrefixes) + ' '
  else
    Result := '';
  Result := Result
    + RandomStr(StreetBeginnings)
    + RandomStr(StreetEndings) + ' '
    + RandomStr(StreetTypes) + ' '
    + IntToStr((Random(499) + 1) div (Random(9) + 1) + 1);
end;

function RandomCity: string;
begin
  Result := RandomStr(Cities);
end;


end.
