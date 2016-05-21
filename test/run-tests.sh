#!/bin/sh

set -e

elm-make --yes --output test.js Test.elm
node test.js
