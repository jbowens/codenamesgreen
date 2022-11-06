#!/bin/bash

set -ex

go install github.com/jbowens/codenamesgreen/cmd/greenapid@latest
parcel build src/index.html
cp -R src/images dist/
cp src/robots.txt dist/
