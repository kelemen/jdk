/*
 * Copyright (c) 1999, 2020, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package com.sun.tools.javac.parser;

import com.sun.tools.javac.code.Lint;
import com.sun.tools.javac.code.Lint.LintCategory;
import com.sun.tools.javac.code.Preview;
import com.sun.tools.javac.code.Source;
import com.sun.tools.javac.code.Source.Feature;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.parser.Tokens.Comment.CommentStyle;
import com.sun.tools.javac.resources.CompilerProperties.Errors;
import com.sun.tools.javac.resources.CompilerProperties.Warnings;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.util.*;
import com.sun.tools.javac.util.JCDiagnostic.*;

import java.nio.CharBuffer;
import java.util.Set;
import java.util.regex.Pattern;

import static com.sun.tools.javac.parser.Tokens.*;
import static com.sun.tools.javac.util.LayoutCharacters.EOI;

/**
 * The lexical analyzer maps an input stream consisting of UTF-8 characters and unicode
 * escape sequences into a token sequence.
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 */
public class JavaTokenizer extends UnicodeReader {
    /**
     * If true then prints token information after each nextToken().
     */
    private static final boolean scannerDebug = false;

    /**
     * This character is used to mark the places where the string expressions
     * will be inserted into. This is required to keep text blocks being processed
     * by the {@link String#stripIndent()} method work properly.
     * <p>
     * The only fact we exploit about this character is that it cannot be
     * expressed by any other means than to write it plainly into the string
     * with the exception of unicode escaping (which is done by {@link UnicodeReader}), and that
     * transformations done by the text block normalization treats it as non-indenting character.
     */
    private static final char ESCAPED_EXPRESSION_MARKER = '#';

    /**
     * The parser factory to parse subtoken expressions.
     */
    private final ChildParserFactory childParserFactory;

    /**
     * Sentinal for non-value.
     */
    private int NOT_FOUND = -1;

    /**
     * The source language setting. Copied from scanner factory.
     */
    private Source source;

    /**
     * The preview language setting. Copied from scanner factory.
     */
    private Preview preview;

    /**
     * The log to be used for error reporting. Copied from scanner factory.
     */
    private final Log log;

    /**
     * The token factory. Copied from scanner factory.
     */
    private final Tokens tokens;

    /**
     * The names factory. Copied from scanner factory.
     */
    private final Names names;

    /**
     * The token kind, set by nextToken().
     */
    protected TokenKind tk;

    /**
     * The token's radix, set by nextToken().
     */
    protected int radix;

    /**
     * The token's name, set by nextToken().
     */
    protected Name name;

    /**
     * The position where a lexical error occurred;
     */
    protected int errPos;

    /**
     * true if is a text block, set by nextToken().
     */
    protected boolean isTextBlock;

    /**
     * true if contains escape sequences, set by nextToken().
     */
    protected boolean hasEscapeSequences;

    /**
     * Marks the index of the {@link #ESCAPED_EXPRESSION_MARKER} character
     * a subsequent escaped expression is to put and replace.
     */
    protected int nextExpressionMarkerIndex;

    protected boolean unclosedExpression;

    /**
     * The current expressions escaped within the scanned string.
     * The location of expressions are marked by {@link #ESCAPED_EXPRESSION_MARKER}.
     */
    protected List<EscapedExpression> escapedExpressions;

    /**
     * Buffer for building literals, used by nextToken().
     */
    protected StringBuilder sb;

    /**
     * Origin scanner factory.
     */
    protected ScannerFactory fac;

    /**
     * The set of lint options currently in effect. It is initialized
     * from the context, and then is set/reset as needed by Attr as it
     * visits all the various parts of the trees during attribution.
     */
    protected Lint lint;

    /**
     * Construct a Java token scanner from the input character buffer.
     *
     * @param fac  the factory which created this Scanner.
     * @param cb   the input character buffer.
     * @param childParserFactory the parser factory to parse subtoken expressions.
     */
    protected JavaTokenizer(ScannerFactory fac, CharBuffer cb, ChildParserFactory childParserFactory) {
        this(fac, JavacFileManager.toArray(cb), cb.limit(), childParserFactory);
    }

    /**
     * Construct a Java token scanner from the input character array.
     *
     * @param fac     the factory which created this Scanner
     * @param array   the input character array.
     * @param length  The length of the meaningful content in the array.
     * @param childParserFactory the parser factory to parse subtoken expressions.
     */
    protected JavaTokenizer(ScannerFactory fac, char[] array, int length, ChildParserFactory childParserFactory) {
        super(fac, array, length);
        this.childParserFactory = childParserFactory;
        this.fac = fac;
        this.log = fac.log;
        this.names = fac.names;
        this.tokens = fac.tokens;
        this.source = fac.source;
        this.preview = fac.preview;
        this.lint = fac.lint;
        this.errPos = Position.NOPOS;
        this.escapedExpressions = List.nil();
        this.sb = new StringBuilder(256);
    }

    protected JavaTokenizer(JavaTokenizer parent) {
        super(parent);

        this.childParserFactory = parent.childParserFactory;
        this.source = parent.source;
        this.preview = parent.preview;
        this.log = parent.log;
        this.tokens = parent.tokens;
        this.names = parent.names;
        this.tk = parent.tk;
        this.radix = parent.radix;
        this.name = parent.name;
        this.errPos = Position.NOPOS;
        this.isTextBlock = false;
        this.escapedExpressions = List.nil();
        this.hasEscapeSequences = false;
        this.sb = new StringBuilder(256);
        this.fac = parent.fac;
        this.lint = parent.lint;
    }

    public JavaTokenizer copy() {
        return new JavaTokenizer(this);
    }

    /**
     * Check the source level for a lexical feature.
     *
     * @param pos      position in input buffer.
     * @param feature  feature to verify.
     */
    protected void checkSourceLevel(int pos, Feature feature) {
        if (preview.isPreview(feature) && !preview.isEnabled()) {
            //preview feature without --preview flag, error
            lexError(DiagnosticFlag.SOURCE_LEVEL, pos, preview.disabledError(feature));
        } else if (!feature.allowedInSource(source)) {
            //incompatible source level, error
            lexError(DiagnosticFlag.SOURCE_LEVEL, pos, feature.error(source.name));
        } else if (preview.isPreview(feature)) {
            //use of preview feature, warn
            preview.warnPreview(pos, feature);
        }
    }

    /**
     * Report an error at the given position using the provided arguments.
     *
     * @param pos  position in input buffer.
     * @param key  error key to report.
     */
    protected void lexError(int pos, JCDiagnostic.Error key) {
        log.error(pos, key);
        tk = TokenKind.ERROR;
        errPos = pos;
    }

    /**
     * Report an error at the given position using the provided arguments.
     *
     * @param flags  diagnostic flags.
     * @param pos    position in input buffer.
     * @param key    error key to report.
     */
    protected void lexError(DiagnosticFlag flags, int pos, JCDiagnostic.Error key) {
        log.error(flags, pos, key);
        if (flags != DiagnosticFlag.SOURCE_LEVEL) {
            tk = TokenKind.ERROR;
        }
        errPos = pos;
    }

    /**
     * Report an error at the given position using the provided arguments.
     *
     * @param lc     lint category.
     * @param pos    position in input buffer.
     * @param key    error key to report.
     */
    protected void lexWarning(LintCategory lc, int pos, JCDiagnostic.Warning key) {
        DiagnosticPosition dp = new SimpleDiagnosticPosition(pos) ;
        log.warning(lc, dp, key);
    }

    /**
     * Add a character to the literal buffer.
     *
     * @param ch  character to add.
     */
    protected void put(char ch) {
        sb.append(ch);
    }

    /**
     * Add a codepoint to the literal buffer.
     *
     * @param codePoint  codepoint to add.
     */
    protected void putCodePoint(int codePoint) {
        sb.appendCodePoint(codePoint);
    }

    /**
     * Add current character or codepoint to the literal buffer.
     */
    protected void put() {
        if (isSurrogate()) {
            putCodePoint(getCodepoint());
        } else {
            put(get());
        }
    }

    /**
     * Add a string to the literal buffer.
     */
    protected void put(String string) {
        sb.append(string);
    }

    /**
     * Add current character or codepoint to the literal buffer then return next character.
     */
    protected char putThenNext() {
        put();

        return next();
    }

    /**
     * If either ch1 or ch2 matches the current character then add current character
     * to the literal buffer and then advance.
     *
     * @param ch1  first character to match.
     * @param ch2  second character to match.
     *
     * @return true if either ch1 or ch2 matches current character.
     */
    protected boolean acceptOneOfThenPut(char ch1, char ch2) {
        if (isOneOf(ch1, ch2)) {
            put(get());
            next();

            return true;
        }

        return false;
    }

    /**
     * Test if the current character is a line terminator.
     *
     * @return true if current character is a line terminator.
     */
    private boolean isEOLN() {
        return isOneOf('\n', '\r');
    }

    /**
     * Skip and process a line terminator sequence.
     */
    private void skipLineTerminator() {
        int start = position();
        accept('\r');
        accept('\n');
        processLineTerminator(start, position());
    }

    private void scanEscapedExpression(int previousLiteralPartPos) {
        JavacParser parser = childParserFactory.newParser(copy());
        JCTree.JCExpression expression = parser.parseExpression();
        Token expressionClosingToken = parser.token();

        JCTree.JCExpression firstExpressionError = null;

        if (expressionClosingToken.kind != TokenKind.RBRACE) {
            firstExpressionError = parser.syntaxError(expressionClosingToken.pos, List.of(expression), Errors.MissingEscapedExpressionTermination);
        }

        int targetPos = expressionClosingToken.pos;
        while (position() < targetPos) {
            if (!isAvailable()) {
                Assert.error("Cannot reach the end of escaped expression.");
            } else if (isEOLN()) {
                if (firstExpressionError == null) {
                    firstExpressionError = parser.syntaxError(position(), List.of(expression), Errors.IllegalLineEndInEscapedExpression);
                }
            }
            next();
        }

        if (firstExpressionError != null) {
            expression = firstExpressionError;
        }

        escapedExpressions = escapedExpressions
                .prepend(new EscapedExpression(nextExpressionMarkerIndex, expression, previousLiteralPartPos));
        nextExpressionMarkerIndex++;
        unclosedExpression = true;

        put(ESCAPED_EXPRESSION_MARKER);
    }

    /**
     * Processes the current character and places in the literal buffer. If the current
     * character is a backslash then the next character is validated as a proper
     * escape character. Conversion of escape sequences takes place at end of nextToken().
     *
     * @param pos position of the first character in literal.
     * @param allowEscapedExpression if \{expression} should be allowed or not
     */
    private void scanLitChar(int pos, boolean allowEscapedExpression) {
        char firstCh = get();
        boolean escaping = firstCh == '\\';
        if (escaping) {
            int previousLiteralPartPos = position();
            char secondCh = next();
            if (secondCh == '{' && allowEscapedExpression) {
                next();
                scanEscapedExpression(previousLiteralPartPos);
                return;
            }
            put(firstCh);
        }

        if (escaping) {
            hasEscapeSequences = true;

            switch (get()) {
                case '0': case '1': case '2': case '3':
                case '4': case '5': case '6': case '7':
                    char leadch = get();
                    putThenNext();

                    if (inRange('0', '7')) {
                        putThenNext();

                        if (leadch <= '3' && inRange('0', '7')) {
                            putThenNext();
                        }
                    }
                    break;

                case 'b':
                case 't':
                case 'n':
                case 'f':
                case 'r':
                case '\'':
                case '\"':
                case '\\':
                    putThenNext();
                    break;

                case 's':
                    checkSourceLevel(position(), Feature.TEXT_BLOCKS);
                    putThenNext();
                    break;

                case '\n':
                case '\r':
                    if (isTextBlock) {
                        skipLineTerminator();
                        // Normalize line terminator.
                        put('\n');
                    } else {
                        lexError(position(), Errors.IllegalEscChar);
                    }
                    break;

                default:
                    lexError(position(), Errors.IllegalEscChar);
                    break;
            }
        } else {
            if (unclosedExpression && firstCh == '}') {
                next();
                unclosedExpression = false;
                escapedExpressions.head.nextLiteralStartPos = position();
            } else {
                if (firstCh == ESCAPED_EXPRESSION_MARKER) {
                    nextExpressionMarkerIndex++;
                }
                putThenNext();
            }
        }
    }

    /**
     * Scan a string literal or text block.
     *
     * @param pos  position of the first character in literal.
     */
    private void scanString(int pos) {
        // Assume the best.
        tk = Tokens.TokenKind.STRINGLITERAL;
        unclosedExpression = false;
        nextExpressionMarkerIndex = 0;
        escapedExpressions = List.nil();
        // Track the end of first line for error recovery.
        int firstEOLN = NOT_FOUND;
        // Check for text block delimiter.
        isTextBlock = accept("\"\"\"");

        if (isTextBlock) {
            // Check if preview feature is enabled for text blocks.
            checkSourceLevel(pos, Feature.TEXT_BLOCKS);

            // Verify the open delimiter sequence.
            // Error if the open delimiter sequence is not """<white space>*<LineTerminator>.
            skipWhitespace();

            if (isEOLN()) {
                skipLineTerminator();
            } else {
                lexError(position(), Errors.IllegalTextBlockOpen);
                return;
            }

            // While characters are available.
            while (isAvailable()) {
                if (accept("\"\"\"")) {
                    return;
                }

                if (isEOLN()) {
                    skipLineTerminator();
                    // Add normalized line terminator to literal buffer.
                    put('\n');

                    // Record first line terminator for error recovery.
                    if (firstEOLN == NOT_FOUND) {
                        firstEOLN = position();
                    }
                } else {
                    // Add character to string buffer.
                    scanLitChar(pos, true);
                }
            }
        } else {
            // Skip first quote.
            next();

            // While characters are available.
            while (isAvailable()) {
                if (accept('\"')) {
                    return;
                }

                if (isEOLN()) {
                    // Line terminator in string literal is an error.
                    // Fall out to unclosed string literal error.
                    break;
                } else {
                    // Add character to string buffer.
                    scanLitChar(pos, true);
                }
            }
        }

        // String ended without close delimiter sequence.
        lexError(pos, isTextBlock ? Errors.UnclosedTextBlock : Errors.UnclosedStrLit);

        if (firstEOLN  != NOT_FOUND) {
            // Reset recovery position to point after text block open delimiter sequence.
            reset(firstEOLN);
        }
    }

    /**
     * Scan sequence of digits.
     *
     * @param pos         position of the first character in literal.
     * @param digitRadix  radix of numeric literal.
     */
    private void scanDigits(int pos, int digitRadix) {
        int leadingUnderscorePos = is('_') ? position() : NOT_FOUND;
        int trailingUnderscorePos;

        do {
            if (!is('_')) {
                put();
                trailingUnderscorePos = NOT_FOUND;
            } else {
                trailingUnderscorePos = position();
            }

            next();
        } while (digit(pos, digitRadix) >= 0 || is('_'));

        if (leadingUnderscorePos != NOT_FOUND) {
            lexError(leadingUnderscorePos, Errors.IllegalUnderscore);
        } else if (trailingUnderscorePos != NOT_FOUND) {
            lexError(trailingUnderscorePos, Errors.IllegalUnderscore);
        }
    }

    /**
     * Read fractional part of hexadecimal floating point number.
     *
     * @param pos  position of the first character in literal.
     */
    private void scanHexExponentAndSuffix(int pos) {
        if (acceptOneOfThenPut('p', 'P')) {
            skipIllegalUnderscores();
            acceptOneOfThenPut('+', '-');
            skipIllegalUnderscores();

            if (digit(pos, 10) >= 0) {
                scanDigits(pos, 10);
            } else {
                lexError(pos, Errors.MalformedFpLit);
            }
        } else {
            lexError(pos, Errors.MalformedFpLit);
        }

        if (acceptOneOfThenPut('f', 'F')) {
            tk = TokenKind.FLOATLITERAL;
            radix = 16;
        } else {
            acceptOneOfThenPut('d', 'D');
            tk = TokenKind.DOUBLELITERAL;
            radix = 16;
        }
    }

    /**
     * Read fractional part of floating point number.
     *
     * @param pos  position of the first character in literal.
     */
    private void scanFraction(int pos) {
        skipIllegalUnderscores();

        if (digit(pos, 10) >= 0) {
            scanDigits(pos, 10);
        }

        int index = sb.length();

        if (acceptOneOfThenPut('e', 'E')) {
            skipIllegalUnderscores();
            acceptOneOfThenPut('+', '-');
            skipIllegalUnderscores();

            if (digit(pos, 10) >= 0) {
                scanDigits(pos, 10);
                return;
            }

            lexError(pos, Errors.MalformedFpLit);
            sb.setLength(index);
        }
    }

    /**
     * Read fractional part and 'd' or 'f' suffix of floating point number.
     *
     * @param pos  position of the first character in literal.
     */
    private void scanFractionAndSuffix(int pos) {
        radix = 10;
        scanFraction(pos);

        if (acceptOneOfThenPut('f', 'F')) {
             tk = TokenKind.FLOATLITERAL;
        } else {
            acceptOneOfThenPut('d', 'D');
            tk = TokenKind.DOUBLELITERAL;
        }
    }

    /**
     * Read fractional part and 'd' or 'f' suffix of hexadecimal floating point number.
     *
     * @param pos  position of the first character in literal.
     */
    private void scanHexFractionAndSuffix(int pos, boolean seendigit) {
        radix = 16;
        Assert.check(is('.'));
        putThenNext();
        skipIllegalUnderscores();

        if (digit(pos, 16) >= 0) {
            seendigit = true;
            scanDigits(pos, 16);
        }

        if (!seendigit)
            lexError(pos, Errors.InvalidHexNumber);
        else
            scanHexExponentAndSuffix(pos);
    }

    /**
     * Skip over underscores and report as a error if found.
     */
    private void skipIllegalUnderscores() {
        if (is('_')) {
            lexError(position(), Errors.IllegalUnderscore);
            skip('_');
        }
    }

    /**
     * Read a number. (Spec. 3.10)
     *
     * @param pos    position of the first character in literal.
     * @param radix  the radix of the number; one of 2, 8, 10, 16.
     */
    private void scanNumber(int pos, int radix) {
        // for octal, allow base-10 digit in case it's a float literal
        this.radix = radix;
        int digitRadix = (radix == 8 ? 10 : radix);
        int firstDigit = digit(pos, Math.max(10, digitRadix));
        boolean seendigit = firstDigit >= 0;
        boolean seenValidDigit = firstDigit >= 0 && firstDigit < digitRadix;

        if (seendigit) {
            scanDigits(pos, digitRadix);
        }

        if (radix == 16 && is('.')) {
            scanHexFractionAndSuffix(pos, seendigit);
        } else if (seendigit && radix == 16 && isOneOf('p', 'P')) {
            scanHexExponentAndSuffix(pos);
        } else if (digitRadix == 10 && is('.')) {
            putThenNext();
            scanFractionAndSuffix(pos);
        } else if (digitRadix == 10 && isOneOf('e', 'E', 'f', 'F', 'd', 'D')) {
            scanFractionAndSuffix(pos);
        } else {
            if (!seenValidDigit) {
                switch (radix) {
                case 2:
                    lexError(pos, Errors.InvalidBinaryNumber);
                    break;
                case 16:
                    lexError(pos, Errors.InvalidHexNumber);
                    break;
                }
            }
            // If it is not a floating point literal,
            // the octal number should be rescanned correctly.
            if (radix == 8) {
                sb.setLength(0);
                reset(pos);
                scanDigits(pos, 8);
            }

            if (acceptOneOf('l', 'L')) {
                tk = TokenKind.LONGLITERAL;
            } else {
                tk = TokenKind.INTLITERAL;
            }
        }
    }

    /**
     * Determines if the sequence in the literal buffer is a token (keyword, operator.)
     */
    private void checkIdent() {
        name = names.fromString(sb.toString());
        tk = tokens.lookupKind(name);
    }

    /**
     * Read an identifier. (Spec. 3.8)
     */
    private void scanIdent() {
        putThenNext();

        do {
            switch (get()) {
            case 'A': case 'B': case 'C': case 'D': case 'E':
            case 'F': case 'G': case 'H': case 'I': case 'J':
            case 'K': case 'L': case 'M': case 'N': case 'O':
            case 'P': case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X': case 'Y':
            case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e':
            case 'f': case 'g': case 'h': case 'i': case 'j':
            case 'k': case 'l': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x': case 'y':
            case 'z':
            case '$': case '_':
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                break;

            case '\u0000': case '\u0001': case '\u0002': case '\u0003':
            case '\u0004': case '\u0005': case '\u0006': case '\u0007':
            case '\u0008': case '\u000E': case '\u000F': case '\u0010':
            case '\u0011': case '\u0012': case '\u0013': case '\u0014':
            case '\u0015': case '\u0016': case '\u0017':
            case '\u0018': case '\u0019': case '\u001B':
            case '\u007F':
                next();
                continue;

            case '\u001A': // EOI is also a legal identifier part
                if (isAvailable()) {
                    next();
                    continue;
                }

                checkIdent();
                return;

            default:
                boolean isJavaIdentifierPart;

                if (isASCII()) {
                    // all ASCII range chars already handled, above
                    isJavaIdentifierPart = false;
                } else {
                    if (Character.isIdentifierIgnorable(get())) {
                        next();
                        continue;
                    }

                    isJavaIdentifierPart = isSurrogate()
                            ? Character.isJavaIdentifierPart(getCodepoint())
                            : Character.isJavaIdentifierPart(get());
                }

                if (!isJavaIdentifierPart) {
                    checkIdent();
                    return;
                }
            }

            putThenNext();
        } while (true);
    }

    /**
     * Return true if ch can be part of an operator.
     *
     * @param ch  character to check.
     *
     * @return true if ch can be part of an operator.
     */
    private boolean isSpecial(char ch) {
        switch (ch) {
        case '!': case '%': case '&': case '*': case '?':
        case '+': case '-': case ':': case '<': case '=':
        case '>': case '^': case '|': case '~':
        case '@':
            return true;

        default:
            return false;
        }
    }

    /**
     * Read longest possible sequence of special characters and convert to token.
     */
    private void scanOperator() {
        while (true) {
            put();
            TokenKind newtk = tokens.lookupKind(sb.toString());

            if (newtk == TokenKind.IDENTIFIER) {
                sb.setLength(sb.length() - 1);
                break;
            }

            tk = newtk;
            next();

            if (!isSpecial(get())) {
                break;
            }
        }
    }

    /**
     * Read token (main entrypoint.)
     */
    public Token readToken() {
        sb.setLength(0);
        name = null;
        radix = 0;
        isTextBlock = false;
        hasEscapeSequences = false;

        int pos;
        List<Comment> comments = null;

        try {
            loop: while (true) {
                pos = position();

                switch (get()) {
                case ' ':  // (Spec 3.6)
                case '\t': // (Spec 3.6)
                case '\f': // (Spec 3.6)
                    skipWhitespace();
                    processWhiteSpace(pos, position());
                    break;

                case '\n': // (Spec 3.4)
                    next();
                    processLineTerminator(pos, position());
                    break;

                case '\r': // (Spec 3.4)
                    next();
                    accept('\n');
                    processLineTerminator(pos, position());
                    break;

                case 'A': case 'B': case 'C': case 'D': case 'E':
                case 'F': case 'G': case 'H': case 'I': case 'J':
                case 'K': case 'L': case 'M': case 'N': case 'O':
                case 'P': case 'Q': case 'R': case 'S': case 'T':
                case 'U': case 'V': case 'W': case 'X': case 'Y':
                case 'Z':
                case 'a': case 'b': case 'c': case 'd': case 'e':
                case 'f': case 'g': case 'h': case 'i': case 'j':
                case 'k': case 'l': case 'm': case 'n': case 'o':
                case 'p': case 'q': case 'r': case 's': case 't':
                case 'u': case 'v': case 'w': case 'x': case 'y':
                case 'z':
                case '$': case '_': // (Spec. 3.8)
                    scanIdent();
                    break loop;

                case '0': // (Spec. 3.10)
                    next();

                    if (acceptOneOf('x', 'X')) {
                        skipIllegalUnderscores();
                        scanNumber(pos, 16);
                    } else if (acceptOneOf('b', 'B')) {
                        skipIllegalUnderscores();
                        scanNumber(pos, 2);
                    } else {
                        put('0');

                        if (is('_')) {
                            int savePos = position();
                            skip('_');

                            if (digit(pos, 10) < 0) {
                                lexError(savePos, Errors.IllegalUnderscore);
                            }
                        }

                        scanNumber(pos, 8);
                    }
                    break loop;

                case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':  // (Spec. 3.10)
                    scanNumber(pos, 10);
                    break loop;

                case '.': // (Spec. 3.12)
                    if (accept("...")) {
                        put("...");
                        tk = TokenKind.ELLIPSIS;
                    } else {
                        next();
                        int savePos = position();

                        if (accept('.')) {
                            lexError(savePos, Errors.IllegalDot);
                        } else if (digit(pos, 10) >= 0) {
                            put('.');
                            scanFractionAndSuffix(pos); // (Spec. 3.10)
                        } else {
                            tk = TokenKind.DOT;
                        }
                    }
                    break loop;

                case ',': // (Spec. 3.12)
                    next();
                    tk = TokenKind.COMMA;
                    break loop;

                case ';': // (Spec. 3.12)
                    next();
                    tk = TokenKind.SEMI;
                    break loop;

                case '(': // (Spec. 3.12)
                    next();
                    tk = TokenKind.LPAREN;
                    break loop;

                case ')': // (Spec. 3.12)
                    next();
                    tk = TokenKind.RPAREN;
                    break loop;

                case '[': // (Spec. 3.12)
                    next();
                    tk = TokenKind.LBRACKET;
                    break loop;

                case ']': // (Spec. 3.12)
                    next();
                    tk = TokenKind.RBRACKET;
                    break loop;

                case '{': // (Spec. 3.12)
                    next();
                    tk = TokenKind.LBRACE;
                    break loop;

                case '}': // (Spec. 3.12)
                    next();
                    tk = TokenKind.RBRACE;
                    break loop;

                case '/':
                    next();

                    if (accept('/')) { // (Spec. 3.7)
                        skipToEOLN();

                        if (isAvailable()) {
                            comments = appendComment(comments, processComment(pos, position(), CommentStyle.LINE));
                        }
                        break;
                    } else if (accept('*')) { // (Spec. 3.7)
                        boolean isEmpty = false;
                        CommentStyle style;

                        if (accept('*')) {
                            style = CommentStyle.JAVADOC;

                            if (is('/')) {
                                isEmpty = true;
                            }
                        } else {
                            style = CommentStyle.BLOCK;
                        }

                        if (!isEmpty) {
                            while (isAvailable()) {
                                if (accept('*')) {
                                    if (is('/')) {
                                        break;
                                    }
                                } else {
                                    next();
                                }
                            }
                        }

                        if (accept('/')) {
                            comments = appendComment(comments, processComment(pos, position(), style));

                            break;
                        } else {
                            lexError(pos, Errors.UnclosedComment);

                            break loop;
                        }
                    } else if (accept('=')) {
                        tk = TokenKind.SLASHEQ; // (Spec. 3.12)
                    } else {
                        tk = TokenKind.SLASH; // (Spec. 3.12)
                    }
                    break loop;

                case '\'': // (Spec. 3.10)
                    next();

                    if (accept('\'')) {
                        lexError(pos, Errors.EmptyCharLit);
                    } else {
                        if (isEOLN()) {
                            lexError(pos, Errors.IllegalLineEndInCharLit);
                        }

                        scanLitChar(pos, false);

                        if (accept('\'')) {
                            tk = TokenKind.CHARLITERAL;
                        } else {
                            lexError(pos, Errors.UnclosedCharLit);
                        }
                    }
                    break loop;

                case '\"': // (Spec. 3.10)
                    scanString(pos);
                    break loop;

                default:
                    if (isSpecial(get())) {
                        scanOperator();
                    } else {
                        boolean isJavaIdentifierStart;

                        if (isASCII()) {
                            // all ASCII range chars already handled, above
                            isJavaIdentifierStart = false;
                        } else {
                            isJavaIdentifierStart = isSurrogate()
                                    ? Character.isJavaIdentifierStart(getCodepoint())
                                    : Character.isJavaIdentifierStart(get());
                        }

                        if (isJavaIdentifierStart) {
                            scanIdent();
                        } else if (digit(pos, 10) >= 0) {
                            scanNumber(pos, 10);
                        } else if (is((char)EOI) || !isAvailable()) {
                            tk = TokenKind.EOF;
                            pos = position();
                        } else {
                            String arg;

                            if (isSurrogate()) {
                                int codePoint = getCodepoint();
                                char hi = Character.highSurrogate(codePoint);
                                char lo = Character.lowSurrogate(codePoint);
                                arg = String.format("\\u%04x\\u%04x", (int) hi, (int) lo);
                            } else {
                                char ch = get();
                                arg = (32 < ch && ch < 127) ? String.format("%s", ch) :
                                                              String.format("\\u%04x", (int) ch);
                            }

                            lexError(pos, Errors.IllegalChar(arg));
                            next();
                        }
                    }
                    break loop;
                }
            }

            int endPos = position();

            if (tk.tag == Token.Tag.DEFAULT) {
                return new Token(tk, pos, endPos, comments);
            } else  if (tk.tag == Token.Tag.NAMED) {
                return new NamedToken(tk, pos, endPos, name, comments);
            } else {
                // Get characters from string buffer.
                String string = sb.toString();

                // If a text block.
                if (isTextBlock) {
                    // Verify that the incidental indentation is consistent.
                    if (lint.isEnabled(LintCategory.TEXT_BLOCKS)) {
                        Set<TextBlockSupport.WhitespaceChecks> checks =
                                TextBlockSupport.checkWhitespace(string);
                        if (checks.contains(TextBlockSupport.WhitespaceChecks.INCONSISTENT)) {
                            lexWarning(LintCategory.TEXT_BLOCKS, pos,
                                    Warnings.InconsistentWhiteSpaceIndentation);
                        }
                        if (checks.contains(TextBlockSupport.WhitespaceChecks.TRAILING)) {
                            lexWarning(LintCategory.TEXT_BLOCKS, pos,
                                    Warnings.TrailingWhiteSpaceWillBeRemoved);
                        }
                    }
                    // Remove incidental indentation.
                    try {
                        string = string.stripIndent();
                    } catch (Exception ex) {
                        // Error already reported, just use unstripped string.
                    }
                }

                // Translate escape sequences if present.
                if (hasEscapeSequences) {
                    try {
                        string = string.translateEscapes();
                    } catch (Exception ex) {
                        // Error already reported, just use untranslated string.
                    }
                }

                if (tk.tag == Token.Tag.STRING) {
                    if (escapedExpressions.isEmpty()) {
                        // Build string token.
                        return new StringToken(tk, pos, endPos, string, comments);
                    } else {
                        return buildInterpolatedStringToken(pos, endPos, string, comments);
                    }
                } else {
                    // Build numeric token.
                    return new NumericToken(tk, pos, endPos, string, radix, comments);
                }
            }
        } finally {
            int endPos = position();

            if (scannerDebug) {
                    System.out.println("nextToken(" + pos
                                       + "," + endPos + ")=|" +
                                       new String(getRawCharacters(pos, endPos))
                                       + "|");
            }
        }
    }


    private InterpolatedStringToken buildInterpolatedStringToken(
            int pos,
            int endPos,
            String string,
            List<Comment> comments) {

        List<EscapedExpression> orderedEscapedExpressions = escapedExpressions.reverse();
        // Just allow them to be garbage collected
        escapedExpressions = List.nil();

        List<Object> stringParts = List.nil();

        int previousLiteralPartStartPos = pos;

        // Indexing into `string`
        int markerIndex = -1;
        int partStartIndex = 0;
        int charIndex = 0;

        expressionLoop:
        while (!orderedEscapedExpressions.isEmpty()) {
            EscapedExpression head = orderedEscapedExpressions.head;
            orderedEscapedExpressions = orderedEscapedExpressions.tail;
            if (head.markerIndex < 0) {
                stringParts = stringParts.prepend(head.expression);
                continue;
            }

            for (; charIndex < string.length(); charIndex++) {
                if (string.charAt(charIndex) == ESCAPED_EXPRESSION_MARKER) {
                    markerIndex++;
                    if (markerIndex == head.markerIndex) {
                        if (charIndex > partStartIndex) {
                            stringParts = stringParts.prepend(new StringToken(
                                    tk,
                                    previousLiteralPartStartPos,
                                    head.previousLiteralPartEndPos,
                                    string.substring(partStartIndex, charIndex),
                                    List.nil()
                            ));
                        }
                        charIndex++;

                        previousLiteralPartStartPos = head.nextLiteralStartPos;
                        partStartIndex = charIndex;

                        stringParts = stringParts.prepend(head.expression);
                        continue expressionLoop;
                    }
                }
            }
        }

        if (string.length() > partStartIndex) {
            stringParts = stringParts.prepend(new StringToken(
                    tk,
                    previousLiteralPartStartPos,
                    endPos,
                    string.substring(partStartIndex),
                    List.nil()
            ));
        }

        return new InterpolatedStringToken(pos, endPos, comments, stringParts.reverse());
    }

    /**
     * Appends a comment to the list of comments preceding the current token.
     *
     * @param comments  existing list of comments.
     * @param comment   comment to append.
     *
     * @return new list with comment prepended to the existing list.
     */
    List<Comment> appendComment(List<Comment> comments, Comment comment) {
        return comments == null ?
                List.of(comment) :
                comments.prepend(comment);
    }

    /**
     * Return the position where a lexical error occurred.
     *
     * @return position in the input buffer of where the error occurred.
     */
    public int errPos() {
        return errPos;
    }

    /**
     * Set the position where a lexical error occurred.
     *
     * @param pos  position in the input buffer of where the error occurred.
     */
    public void errPos(int pos) {
        errPos = pos;
    }

    /**
     * Called when a complete comment has been scanned. pos and endPos
     * will mark the comment boundary.
     *
     * @param pos     position of the opening / in the input buffer.
     * @param endPos  position + 1 of the closing / in the input buffer.
     * @param style   style of comment.
     *
     * @return the constructed BasicComment.
     */
    protected Tokens.Comment processComment(int pos, int endPos, CommentStyle style) {
        if (scannerDebug) {
            System.out.println("processComment(" + pos
                                + "," + endPos + "," + style + ")=|"
                                + new String(getRawCharacters(pos, endPos))
                                + "|");
        }

        char[] buf = getRawCharacters(pos, endPos);

        return new BasicComment(style, fac, buf, pos);
    }

    /**
     * Called when a complete whitespace run has been scanned. pos and endPos
     * will mark the whitespace boundary.
     *
     * (Spec 3.6)
     *
     * @param pos     position in input buffer of first whitespace character.
     * @param endPos  position + 1 in input buffer of last whitespace character.
     */
    protected void processWhiteSpace(int pos, int endPos) {
        if (scannerDebug) {
            System.out.println("processWhitespace(" + pos
                                + "," + endPos + ")=|" +
                                new String(getRawCharacters(pos, endPos))
                                + "|");
        }
    }

    /**
     * Called when a line terminator has been processed.
     *
     * @param pos     position in input buffer of first character in sequence.
     * @param endPos  position + 1 in input buffer of last character in sequence.
     */
    protected void processLineTerminator(int pos, int endPos) {
        if (scannerDebug) {
            System.out.println("processTerminator(" + pos
                                + "," + endPos + ")=|" +
                                new String(getRawCharacters(pos, endPos))
                                + "|");
        }
    }

    /**
     * Build a map for translating between line numbers and positions in the input.
     *
     * @return a LineMap
     */
    public Position.LineMap getLineMap() {
        return Position.makeLineMap(getRawCharacters(), length(), false);
    }

    protected static class EscapedExpression {
        /**
         * The index of the {@link #ESCAPED_EXPRESSION_MARKER} from the beginning of the string,
         * including the characters that are actually part of the string as well.
         * <p>
         * {@code -1} if this is an error token.
         */
        protected final int markerIndex;
        protected final JCTree.JCExpression expression;

        protected final int previousLiteralPartEndPos;
        protected int nextLiteralStartPos;

        public EscapedExpression(int markerIndex, JCTree.JCExpression expression, int previousLiteralPartEndPos) {
            this.previousLiteralPartEndPos = previousLiteralPartEndPos;
            this.markerIndex = markerIndex;
            this.expression = expression;
            this.nextLiteralStartPos = -1;
        }
    }

    /**
     * Scan a documentation comment; determine if a deprecated tag is present.
     * Called once the initial /, * have been skipped, positioned at the second *
     * (which is treated as the beginning of the first line).
     * Stops positioned at the closing '/'.
     */
    protected static class BasicComment extends PositionTrackingReader implements Comment {
        /**
         * Style of comment
         *   LINE starting with //
         *   BLOCK starting with /*
         *   JAVADOC starting with /**
         */
        CommentStyle cs;

        /**
         * true if comment contains @deprecated at beginning of a line.
         */
        protected boolean deprecatedFlag = false;

        /**
         * true if comment has been fully scanned.
         */
        protected boolean scanned = false;

        /**
         * Constructor.
         *
         * @param cs      comment style
         * @param sf      Scan factory.
         * @param array   Array containing contents of source.
         * @param offset  Position offset in original source buffer.
         */
        protected BasicComment(CommentStyle cs, ScannerFactory sf, char[] array, int offset) {
            super(sf, array, offset);
            this.cs = cs;
        }

        /**
         * Return comment body text minus comment adornments or null if not scanned.
         *
         * @return comment body text.
         */
        public String getText() {
            return null;
        }

        /**
         * Return buffer position in original buffer mapped from buffer position in comment.
         *
         * @param pos  buffer position in comment.
         *
         * @return buffer position in original buffer.
         */
        public int getSourcePos(int pos) {
            return -1;
        }

        /**
         * Return style of comment.
         *   LINE starting with //
         *   BLOCK starting with /*
         *   JAVADOC starting with /**
         *
         * @return
         */
        public CommentStyle getStyle() {
            return cs;
        }

        /**
         * true if comment contains @deprecated at beginning of a line.
         *
         * @return true if comment contains @deprecated.
         */
        public boolean isDeprecated() {
            if (!scanned && cs == CommentStyle.JAVADOC) {
                scanDocComment();
            }

            return deprecatedFlag;
        }

        /**
         * Scan JAVADOC comment for details.
         */
        protected void scanDocComment() {
            try {
                boolean deprecatedPrefix = false;
                accept("/**");

                forEachLine:
                while (isAvailable()) {
                    // Skip optional WhiteSpace at beginning of line
                    skipWhitespace();

                    // Skip optional consecutive Stars
                    while (accept('*')) {
                        if (is('/')) {
                            return;
                        }
                    }

                    // Skip optional WhiteSpace after Stars
                    skipWhitespace();

                    // At beginning of line in the JavaDoc sense.
                    deprecatedPrefix = deprecatedFlag || accept("@deprecated");

                    if (deprecatedPrefix && isAvailable()) {
                        if (Character.isWhitespace(get())) {
                            deprecatedFlag = true;
                        } else if (accept('*')) {
                            if (is('/')) {
                                deprecatedFlag = true;
                                return;
                            }
                        }
                    }

                    // Skip rest of line
                    while (isAvailable()) {
                        switch (get()) {
                            case '*':
                                next();

                                if (is('/')) {
                                    return;
                                }

                                break;
                            case '\r': // (Spec 3.4)
                            case '\n': // (Spec 3.4)
                                accept('\r');
                                accept('\n');
                                continue forEachLine;

                            default:
                                next();
                                break;
                        }
                    } // rest of line
                } // forEachLine
                return;
            } finally {
                scanned = true;
            }
        }
    }
}
