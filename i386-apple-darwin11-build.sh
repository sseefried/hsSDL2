#!/bin/bash

#
# hsc2hs (when in cross-compiling) mode takes forever to compile this project.
# Thus, we store a copy of the generated code in src-ios-simulator-and-armv7
# and copy it into the build directory
#

#
# Requires ghc-ios-scripts to be in path.
# See (https://github.com/sseefried/ghc-ios-scripts
#
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SCRIPT=setup-ios-i386-wrapper.sh

$SCRIPT configure
[ $? -eq 0 ] || exit 1

BUILDDIR=$THIS_DIR/dist/i386-apple-darwin11

if [ ! -d $BUILDDIR ]; then
  echo "'$BUILDIR' does not exist"
fi

cd src-ios-simulator-and-armv7

for i in $(find . -name '*.hs'); do
  DIR=$BUILDDIR/build/$(dirname $i)
  mkdir -p $DIR
  cp $i $DIR
done

cd $THIS_DIR

$SCRIPT build

echo "Now use '$SCRIPT install' to install the library"