#!/bin/bash

echo "Watching..."
find src-ghcjs app -type f | grep .hs | entr sh -c 'sh build.sh'
