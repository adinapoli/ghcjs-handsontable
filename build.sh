#!/bin/bash

stack build
echo "Copy all.js..."
cp $(stack path --dist-dir)/build/ghcjs-handsontable-demo/ghcjs-handsontable-demo.jsexe/all.js demo/
echo "All done."
