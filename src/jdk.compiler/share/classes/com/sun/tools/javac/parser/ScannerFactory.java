/*
 * Copyright (c) 1999, 2021, Oracle and/or its affiliates. All rights reserved.
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

import java.nio.CharBuffer;

import com.sun.tools.javac.code.Lint;
import com.sun.tools.javac.code.Preview;
import com.sun.tools.javac.code.Source;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Names;
import java.util.Objects;


/**
 * A factory for creating scanners.
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own
 *  risk.  This code and its internal interfaces are subject to change
 *  or deletion without notice.</b>
 */
public class ScannerFactory {
    /** The context key for the scanner factory. */
    public static final Context.Key<ScannerFactory> scannerFactoryKey = new Context.Key<>();

    /** Get the Factory instance for this context. */
    public static ScannerFactory instance(Context context) {
        ScannerFactory instance = context.get(scannerFactoryKey);
        if (instance == null)
            instance = new ScannerFactory(context);
        return instance;
    }

    final Log log;
    final Names names;
    final Source source;
    final Preview preview;
    final Tokens tokens;
    final Lint lint;

    /** Create a new scanner factory. */
    protected ScannerFactory(Context context) {
        context.put(scannerFactoryKey, this);
        this.log = Log.instance(context);
        this.names = Names.instance(context);
        this.source = Source.instance(context);
        this.preview = Preview.instance(context);
        this.tokens = Tokens.instance(context);
        this.lint = Lint.instance(context);
    }

    public Scanner newScanner(CharSequence input, boolean keepDocComments, boolean keepEndPos, boolean keepLineMap, boolean parseModuleInfo, ParserFactory parserFactory) {
        ChildParserFactory childParserFactory = new ChildParserFactoryImpl(keepDocComments, keepEndPos, keepLineMap, parseModuleInfo, parserFactory);
        return newScanner(input, keepDocComments, childParserFactory);
    }

    public Scanner newScanner(CharSequence input, boolean keepDocComments, ChildParserFactory childParserFactory) {
        if (input instanceof CharBuffer charBuffer) {
            if (keepDocComments)
                return newScanner(new JavadocTokenizer(this, charBuffer, childParserFactory));
            else
                return newScanner(new JavaTokenizer(this, charBuffer, childParserFactory));
        } else {
            char[] array = input.toString().toCharArray();
            return newScanner(array, array.length, keepDocComments, childParserFactory);
        }
    }

    public Scanner newScanner(char[] input, int inputLength, boolean keepDocComments, ChildParserFactory childParserFactory) {
        if (keepDocComments)
            return newScanner(new JavadocTokenizer(this, input, inputLength, childParserFactory));
        else
            return newScanner(new JavaTokenizer(this, input, inputLength, childParserFactory));
    }

    private Scanner newScanner(JavaTokenizer tokenizer) {
        return new Scanner(this, tokenizer);
    }

    private class ChildParserFactoryImpl implements ChildParserFactory {
        private final ParserFactory parserFactory;
        private final boolean keepDocComments;
        private final boolean keepEndPos;
        private final boolean keepLineMap;
        private final boolean parseModuleInfo;

        public ChildParserFactoryImpl(
                boolean keepDocComments,
                boolean keepEndPos,
                boolean keepLineMap,
                boolean parseModuleInfo,
                ParserFactory parserFactory) {

            this.parserFactory = parserFactory;
            this.keepDocComments = keepDocComments;
            this.keepEndPos = keepEndPos;
            this.keepLineMap = keepLineMap;
            this.parseModuleInfo = parseModuleInfo;
        }

        @Override
        public JavacParser newParser(JavaTokenizer childInput) {
            Scanner scanner = newScanner(childInput);
            return parserFactory.newParser(scanner, keepDocComments, keepEndPos, keepLineMap, parseModuleInfo);
        }
    }
}
