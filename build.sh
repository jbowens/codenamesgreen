#!/bin/bash

set -ex

go install github.com/jbowens/codenamesgreen/cmd/greenapid
parcel build src/index.html
cp -R src/images dist/images
