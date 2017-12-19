#!/bin/bash

# Test script for lus2rs

compiler=$1

score=0
max=0
verbose=0

echo "***** Testing $1 *****"
echo

syntactic () {
  score=0
  max=0

  echo "Part 1: syntactic analysis"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --parse-only $f
      ./$compiler --parse-only $f;
    else
      ./$compiler --parse-only $f > /dev/null 2>&1;
    fi;

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

typing () {
  score=0
  max=0

  echo
  echo "Part 2: Typing"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --type-only $f
      ./$compiler --type-only $f;
    else
      ./$compiler --type-only $f > /dev/null 2>&1;
    fi;

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
  echo
}

initializing () {
  score=0
  max=0

  echo
  echo "Part 3: Initialization analysis"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --init-only $f
      ./$compiler --init-only $f;
    else
      ./$compiler --init-only $f > /dev/null 2>&1;
    fi;

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
  echo -n "Initialization: $score/$max : $percent%";
  echo
}

clocking () {
  score=0
  max=0

  echo
  echo "Part 4: Clocking (no init)"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --clock-only --no-init $f
      ./$compiler --clock-only --no-init $f;
    else
      ./$compiler --clock-only --no-init $f > /dev/null 2>&1;
    fi;

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
  echo
}

normalizing () {
  score=0
  max=0

  echo
  echo "Part 5  : Normalizing (no init)"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --norm-only --no-init $f
      ./$compiler --norm-only --no-init $f;
    else
      ./$compiler --norm-only --no-init $f > /dev/null 2>&1;
    fi;

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
  echo -n "Normalizing: $score/$max : $percent%";
  echo
}

scheduling () {
  score=0
  max=0

  echo
  echo "Part 6: Scheduling (no init)"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --schedule-only --no-init $f
      ./$compiler --schedule-only --no-init $f;
    else
      ./$compiler --schedule-only --no-init $f > /dev/null 2>&1;
    fi;

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
  echo -n "Scheduling: $score/$max : $percent%";
  echo
}

translating () {
  score=0
  max=0

  echo
  echo "Part 7: Translating (no init)"

  for f in examples/*.lus; do
    echo -n ".";
    max=`expr $max + 1`;

    if [[ $verbose != 0 ]]; then
      echo Compiling --obj-only --no-init $f
      ./$compiler --obj-only --no-init $f;
    else
      ./$compiler --obj-only --no-init $f > /dev/null 2>&1;
    fi;

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
  echo -n "Translating: $score/$max : $percent%";
  echo
}

case $2  in
    "-v")
      verbose=1;
      syntactic;
      typing;
      initializing;
      clocking;
      normalizing;
      scheduling;
      translating;;
    *)
      syntactic;
      typing;
      initializing;
      clocking;
      normalizing;
      scheduling;
      translating;;
esac
echo
