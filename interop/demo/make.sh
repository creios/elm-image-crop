#!/usr/bin/env bash
pushd ..
elm-make src/ImageCrop/Interop.elm --output=demo/image-crop.interop.js --debug
popd
