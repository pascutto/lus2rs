#!/bin/bash

shopt -s nullglob

# Test script for lus2rs

option=$2
compiler=$1
score=0
max=0
verbose=0

echo "Test de $1"

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

echo -n "Good cases "
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

echo -n "Part 1: $score/$max : $percent%"; }

case $option in
    "-v" )
      verbose=1;
      part1;;
    * )
      part1;;
esac
echo
