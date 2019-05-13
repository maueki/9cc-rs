#!/bin/bash
try() {
  expected="$1"
  input="$2"

  ./target/debug/9cc "$input" > /tmp/tmp.s
  gcc -o /tmp/tmp /tmp/tmp.s test.c
  /tmp/tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual"
    exit 1
  fi
}

cargo build
try 0  "int main () { return 0;}"
try 42 "int main() { return 42;}"
try 41 "int main() { return  12 + 34 - 5;}"
try 47 "int main() { return 5+6*7;}"
try 15 "int main() { return 5*(9-6);}"
try 4  "int main() { return (3+5)/2;}"
try 3  "int main() { int a; int b; a=1;b=2;return a+b;}"
try 6  "int main() { int foo; int bar; foo = 1;bar = 2 + 3;return foo + bar;}"
try 3  "int main() { int a; int b; a=1;b=2;if (a!=b) return a+b; else return 0;}"
try 0  "int main() { int a; int b; a=1;b=1;if (a!=b) return a+b; else return 0;}"
try 3  "int main() { int a; int b; a=1;b=1;if (a==b) { int c; c = 1; return a+b+c;} else return 0;}"
try 0  "int main() {foo(); return 0;}"
try 1  "int main() {return sub(3,2);}"
try 2  "int div(int a, int b) { return a/b; } int main() { return div(6,3); }"
try 3  "int main() {int x; x=3; int *y; y=&x; return *y;}"
try 4  "int main() {int x; x=3; int *y; y=&x; *y=4; return x;}"
try 8  "int main() {int *p; alloc4(&p, 1, 2, 4, 8); int *q; q = p + 3; return *q;}"
#TODO: to improve gen.rs# try 1  "int main() { int a; a=1; { int a; a=2;} return a;}"

echo OK
