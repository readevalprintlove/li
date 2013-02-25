#!/usr/bin/env bash

set -ex

DATE=`date '+%Y%m%d%H%M%S'`

VERSION_TEMPLATE="li.cabal.template"
VERSION_FILE="li.cabal"

# The command `git describe --match v0.3` will return a string like
#
# v0.3-856-g329708b
#
# where 856 is the number of commits since the v0.3 tag. It will always
# find the v0.3 tag and will always return the total number of commits (even
# if the tag is v0.3.1).
REVISION=`git --no-replace-objects describe --match v0.3`

# Extract the version number from the string. Do this in two steps so
# it is a little easier to understand.
REVISION=${REVISION:5} # drop the first 5 characters
REVISION=${REVISION:0:${#REVISION}-9} # drop the last 9 characters

TAG=r$REVISION

sed -e s/LI-VERSION/0.0.0-$REVISION/ < "$VERSION_TEMPLATE" > "$VERSION_FILE"

cabal build
