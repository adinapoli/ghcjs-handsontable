#!/bin/bash

stack build
echo "Copy all.js..."
cp $(stack path --dist-dir)/build/csv-editor/csv-editor.jsexe/all.js demo/
echo "All done."
