mkdir CMakeBuild
cd CMakeBuild
cmake ../CMakeList
cmake --build .
cd ..
java -jar ~/Downloads/javacpp-platform-1.5.8-bin/javacpp.jar treesitter/TreeSitterNativeConfig.java
java -jar ~/Downloads/javacpp-platform-1.5.8-bin/javacpp.jar treesitter/TreeSitterNative.java
jar cf treesitter.jar treesitter/*.class treesitter/*/*.dylib
mv -f treesitter.jar ../../src-dench/lib
cd treesitter
rm *.class