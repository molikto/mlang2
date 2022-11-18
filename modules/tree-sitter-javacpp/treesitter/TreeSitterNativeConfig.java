package treesitter;

import org.bytedeco.javacpp.*;
import org.bytedeco.javacpp.annotation.*;
import org.bytedeco.javacpp.tools.*;

// download javacpp. go inside this dir and then
// java -jar ~/Downloads/javacpp-platform-1.5.8-bin/javacpp.jar TreeSitterNativeConfig.java
@Properties(
    value = @Platform(
        value = "macosx-arm64",
        includepath = {"tree-sitter/lib/include/tree_sitter/", "."},
        include = {"api.h", "parser.h", "grammar.h"},
        linkpath = {"CMakeBuild"},
        link = {"treesitter"}
    ),
    target = "treesitter.TreeSitterNative"
)
public class TreeSitterNativeConfig implements InfoMapper {
    public void map(InfoMap infoMap) {
    }
}

