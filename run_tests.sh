#!/bin/bash

set -e

for TEST in tests/*
do
	Rscript $TEST
done

