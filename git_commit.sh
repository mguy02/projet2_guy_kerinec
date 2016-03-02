#!/bin/bash

git add --all .
git commit -m "${1:-No message provided}"
git push

exit
