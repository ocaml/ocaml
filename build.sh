#! /bin/sh

# Build for iOS

platform=iPhoneOS
# or iPhoneSimulator

arch=armv7
# valid for phone: armv6, armv7, armv7s, arm64
# valid for simulator: i386, x86_64

sdk=8.1


export PLT="/Applications/Xcode.app/Contents/Developer/Platforms/${platform}.platform"
export SDK="/Developer/SDKs/${platform}${sdk}.sdk"

./configure \
        -cc "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk" \
        -as "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk -c" \
        -aspp "gcc -arch $arch -isysroot $PLT$SDK -miphoneos-version-min=$sdk -c" \
        -libs "-Wl,-syslibroot,$PLT$SDK" \
        -no-curses -no-pthread -no-shared-libs \
        -no-debugger -no-ocamldoc -no-ocamlbuild -no-graph \
        -target $arch-apple-darwin \
        "$@" || exit

make cross-all || exit
make cross-opt || exit
