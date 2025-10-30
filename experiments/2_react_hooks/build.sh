#!/bin/bash
rm -rf public
mkdir -p public
spago build
esbuild --bundle html/index.js --outfile=public/index.js
cp html/index.html public/index.html
