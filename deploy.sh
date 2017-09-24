#!/bin/bash
git stash

git checkout dev

stack exec site rebuild

git fetch --all
git checkout -b master
