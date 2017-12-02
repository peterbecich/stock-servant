#!/bin/bash

echo "pull stock-servant"

# https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

git --git-dir=$DIR/../.git pull origin master

echo "pull submodules"

git --git-dir=$DIR/../.git submodule update --init --remote


