#!/bin/sh

set -e

elm-make --yes --output test.js Tester.elm
node test.js
