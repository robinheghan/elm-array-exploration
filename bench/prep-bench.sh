#!/bin/sh

set -e

elm-make --yes --output bench.js Bencher.elm

echo "var Benchmark = require('benchmark');\n" > main.js
cat bench.js >> main.js


node main.js

