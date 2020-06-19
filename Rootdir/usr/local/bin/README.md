# Introduction
This directory contain multiple version of pre-compiled ccls<\br>

# Compile manually

1. on macOS
```
brew install llvm
git clone https://github.com/MaskRay/ccls && cd ccls
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release DUSE_SYSTEM_RAPIDJSON=OFF -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm/lib/cmake
cmake --build Release
```
2. on Linux(For example ubuntu)

