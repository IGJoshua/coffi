package coffi.ffi;

import java.lang.foreign.*;

/**
 * Loading libraries with the {@link System#load} and {@link System#loadLibrary}
 * relies on the classloader, which Clojure messes with. This class exists to
 * have a consistent classloader, providing a consistent way to load libraries
 * and symbols from them.
 */
public class Loader {

    static SymbolLookup lookup = Linker.nativeLinker().defaultLookup().or(SymbolLookup.loaderLookup());

    /**
     * Loads a library from a given absolute file path.
     *
     * @param filepath The absolute file path of the library to load
     */
    public static void loadLibrary(String filepath) {
        System.load(filepath);
    }

    /**
     * Loads a library on the system loadpath with the given name.
     *
     * @param libname The library name, stripped of platform-specific prefixes and suffixes.
     */
    public static void loadSystemLibrary(String libname) {
        System.loadLibrary(libname);
    }

    /**
     * Load the memory address of a symbol.
     *
     * First attempts to load the symbol from system libraries, like libc, and
     * afterwards attempts to load the symbol from other libraries.
     *
     * @param symbol The name of the symbol to load from a library.
     */
    public static MemorySegment findSymbol(String symbol) {
        return lookup.find(symbol).orElse(null);
    }
}
