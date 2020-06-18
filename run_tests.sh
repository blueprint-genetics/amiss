#!/bin/bash

set -e

for TEST in tests/*
do
	echo "Running test $TEST"
	Rscript $TEST
done

