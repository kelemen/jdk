package com.sun.tools.javac.parser;

/**
 * Creates new parsers compatible with the parent parser used to parse the complete
 * source code.
 *
 * <p><b>This is NOT part of any supported API.
 * If you write code that depends on this, you do so at your own risk.
 * This code and its internal interfaces are subject to change or
 * deletion without notice.</b>
 */
public interface ChildParserFactory {
    JavacParser newParser(JavaTokenizer input);
}
