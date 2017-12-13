#!/bin/bash

# Test script for lus2rs

compiler=$1

score=0
max=0
verbose=0

echo "***** Testing $1 *****"

echo

compile () {
if [[ $verbose != 0 ]]; then
  echo Compiling $1 $2
  ./$compiler $1 $2;
else
  ./$compiler $1 $2 > /dev/null 2>&1;
fi;
}

part1 () {
  score=0
  max=0

  echo "Part 1: syntactic analysis"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
  	"1")
    	echo
    	echo "FAIL on "$f" (should have been accepted)";;
  	"0") score=`expr $score + 1`;;
  	*)
    	echo
      echo "COMPILER FAIL on "$f"";;
    esac
  done
  echo

  percent=`expr 100 \* $score / $max`;
  echo -n "Syntactic analysis: $score/$max : $percent%";
  echo
}

part2 () {
  score=0
  max=0

  echo
  echo "Part 2: Typing"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
  	"1")
    	echo
    	echo "FAIL on "$f" (should have been accepted)";;
  	"0") score=`expr $score + 1`;;
  	*)
    	echo
      echo "COMPILER FAIL on "$f"";;
    esac
  done
  echo

  percent=`expr 100 \* $score / $max`;
  echo -n "Typing: $score/$max : $percent%";
}

part3 () {
  score=0
  max=0

  echo
  echo "Part 3: Clocking"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --clock-only $f;
    case $? in
  	"1")
    	echo
    	echo "FAIL on "$f" (should have been accepted)";;
  	"0") score=`expr $score + 1`;;
  	*)
    	echo
      echo "COMPILER FAIL on "$f"";;
    esac
  done
  echo

  percent=`expr 100 \* $score / $max`;
  echo -n "Clocking: $score/$max : $percent%";
}

case $2  in
    "-v")
      verbose=1;
      part1;
      part2;
      part3;;
    *)
      part1;
      part2;
      part3;;
esac
echo
