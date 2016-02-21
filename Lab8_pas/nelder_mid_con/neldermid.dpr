program neldermid;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
       alfa = 3.0;
        betta = 0.5;
        gamma = 2.0;
        delta = 0.5;
      eps = 0.005;
      d1 = 3.0;
      d2 = 1.0;
      d3 = 2.0;
type
  mas = array [1..10] of real;
  mas2 = array [1..10] of mas;
      function fi(x: mas): real;
      begin
        Result := Sqr(x[1] - d1) / 4.0 + Sqr(x[2] - d2) / 9.0 + d3;
      end;
      function argmin(n: integer; x: mas2): integer;
      var
      i: integer;
      min: real;

begin
  min := fi(x[1]);
  Result := 1;
  for i := 2 to n + 1 do
    if fi(x[i]) < min then
      begin
        min := fi(x[i]);
        Result := i;
      end;
end;
      function argmax(n: integer; x: mas2): integer;
    var
      i: integer;
      max: real;
    begin
      max := fi(x[1]);
      Result := 1;
      for i := 2 to n + 1 do
        if fi(x[i]) > max then
      begin
        max := fi(x[i]);
        Result := i;
      end;
      end;
      var
      x: mas2;
    iter, n, h, l, i, j, k: integer;
      s: real;
      myFile: Text;
    label M;
    procedure print;
    begin
    AssignFile(myFile, 'f.txt');
     ReWrite(myFile);
  WriteLn(myFile,'Deyctvie ', iter);
   CloseFile(myFile);
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn('   x[1]    fi(x[1])    x[2]   fi(x[2])    x[3]   fi(x[3])    x[4]   fi(x[4]) ');
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn(' ', x[1, 1]:8:5, ' ', fi(x[1]):9:5,
          ' ', x[2, 1]:8:5, ' ', fi(x[2]):9:5,
          ' ', x[3, 1]:8:5, ' ', fi(x[3]):9:5,
          ' ', x[4, 1]:8:5, ' ', fi(x[4]):9:5, ' ');
  WriteLn(' ', x[1, 2]:8:5, '          ',
          ' ', x[2, 2]:8:5, '          ',
          ' ', x[3, 2]:8:5, '          ',
          ' ', x[4, 2]:8:5, '           ');
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn('   x[5]   fi(x[5])    x[6]   fi(x[6])    x[7]   fi(x[7])    h   l  ');
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn(' ', x[5, 1]:8:5, ' ', fi(x[5]):9:5,
          ' ', x[6, 1]:8:5, ' ', fi(x[6]):9:5,
          ' ', x[7, 1]:8:5, ' ', fi(x[7]):9:5,
          ' ', h:4, ' ', l:3, ' ');
  WriteLn(' ', x[5, 2]:8:5, '          ',
          ' ', x[6, 2]:8:5, '          ',
          ' ', x[7, 2]:8:5, '          ',
          '          ');
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
end;
begin
  for i := 1 to 10 do
    for j := 1 to 10 do
      x[i, j] := 0;
  n := 2;
  x[1, 1] := 0;
  x[1, 2] := 0;
  x[2, 1] := 1;
  x[2, 2] := 0;
  x[3, 1] := 0;
  x[3, 2] := 1;
  iter := 0;
  print;
  M:
    h := argmax(n, x);
    l := argmin(n, x);
    iter := iter + 1;
    for i := 1 to n do
      begin
        s := 0;
        for j := 1 to n + 1 do
          s := s + x[j, i];
        x[n + 2, i] := (s - x[h, i]) / n;
      end;
    for i := 1 to n do
      x[n + 3, i] := x[n + 2, i] + alfa * (x[n + 2, i] - x[h, i]);
    if fi(x[n + 3]) <= fi(x[l]) then
      begin //1
        for i := 1 to n do
          x[n + 4, i] := x[n + 2, i] + gamma * (x[n + 3, i] - x[n + 2, i]);
        if fi(x[n + 4]) < fi(x[l]) then
          x[h] := x[n + 4]
        else
          x[h] := x[n + 3];
        print;
        Goto M;
      end; //1
    if fi(x[n + 3]) < fi(x[h]) then
      begin //2
        for i := 1 to n do
          x[h, i] := x[n + 2, i] + betta * (x[h, i] - x[n + 2, i]);
        x[7] := x[h];
        print;
        Goto M;
      end;//2
    for k := 1 to n + 1 do
      for i := 1 to n do
        x[k, i] := x[l, i] + delta * (x[k, i] - x[l, i]);
    s := 0;
    for k := 1 to n + 1 do
      s := s + Sqr(fi(x[k]) - fi(x[n + 2]));
    s := Sqrt(1 / (n + 1) * s);
    print;
    if s > eps then
      Goto M;
      WriteLn('Min:');
  //for i := 1 to n do
   // WriteLn('x', i, '=', x[n + 2, i]:8:5);
  WriteLn('f(x)=', fi(x[n + 2]):8:5);
  AssignFile(myFile, 'f.txt');
     ReWrite(myFile);
  WriteLn(myFile,'Min:');
 // for i := 1 to n do
   // WriteLn(myFile,'x', i, '=', x[n + 2, i]:8:5);
  WriteLn(MyFile,'f(x)=', fi(x[n + 2]):8:5);
  CloseFile(myFile);
  ReadLn;
end.
