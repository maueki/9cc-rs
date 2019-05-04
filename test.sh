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
try 0 "0;"
try 42 "42;"
try 41 " 12 + 34 - 5; "
try 47 "5+6*7;"
try 15 "5*(9-6);"
try 4 "(3+5)/2;"
try 3 "a=1;b=2;a+b;"
try 3 "a=1;b=2;return a+b;"
try 6 "foo = 1;bar = 2 + 3;return foo + bar;"
try 3  "a=1;b=2;if (a!=b) return a+b; else return 0;"
try 0  "a=1;b=1;if (a!=b) return a+b; else return 0;"
try 3  "a=1;b=1;if (a==b) { c = 1; return a+b+c;} else return 0;"
try 0  "foo(); return 0;"
try 1 "return sub(3,2);" 
echo OK
