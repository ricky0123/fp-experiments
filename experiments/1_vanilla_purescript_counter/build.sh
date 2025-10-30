#!/usr/bin/env bash

rm -rf build
spago build
npx esbuild src/index.js --bundle --outfile=build/index.js
cp src/index.html build
