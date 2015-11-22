#!/bin/bash

echo "Watching..."
find src app -type f | grep .hs | entr sh -c 'sh build.sh'
