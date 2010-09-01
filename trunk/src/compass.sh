#!/bin/bash

COMPASS_LIB="compass.lisp"

if [ -z "$1" ]
then
    echo "No evaluations supplied. Bailing!"
else
    sbcl --noinform --load $COMPASS_LIB --eval "$1" --eval "(quit)"
fi

