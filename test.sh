#!/bin/bash
try() {
  expected="$1"
  input="$2"

  ./target/debug/9cc "$input" > /tmp/tmp.s
  gcc -o /tmp/tmp /tmp/tmp.s
  /tmp/tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 42 42
try 41 " 12 + 34 - 5 "
try 47 "5+6*7"
try 15 "5*(9-6)"
try 4 "(3+5)/2"

echo OK
