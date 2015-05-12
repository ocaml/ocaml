#! /bin/sh

# Build for iOS

if [ -z "$platform" ]; then
    platform=iPhoneOS
fi
# or iPhoneSimulator

if [ -z "$arch" ]; then
    arch=armv7
fi
# valid for phone: armv6, armv7, armv7s, arm64
# valid for simulator: i386, x86_64

if [ -z "$sdk" ]; then
    sdk=8.3
fi


export PLT="/Applications/Xcode.app/Contents/Developer/Platforms/${platform}.platform"
export SDK="/Developer/SDKs/${platform}${sdk}.sdk"

if [ ! -d "$PLT" ]; then
    echo "Error: not found: $PLT" >&2; exit
fi
if [ ! -d "$PLT$SDK" ]; then
    echo "Error: not found: $PLT$SDK" >&2; exit
fi

./configure \
        -cc "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk" \
        -as "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk -c" \
        -aspp "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk -c" \
        -libs "-Wl,-syslibroot,$PLT$SDK" \
        -no-curses -no-shared-libs \
        -no-debugger -no-ocamldoc -no-ocamlbuild -no-graph \
        -target $arch-apple-darwin \
        "$@" || exit

make cross-all || exit
make cross-opt || exit
