#!/bin/bash

set -e

npm install
spago install
spago test
esbuild --bundle index.js --outfile=dist/index.js --servedir=dist
