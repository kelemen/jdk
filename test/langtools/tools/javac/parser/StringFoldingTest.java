/*
 * Copyright (c) 2011, 2015, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
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

/*
 * @test
 * @bug 7068902 8139751
 * @summary verify that string folding can be enabled or disabled
 * @modules jdk.compiler
 */

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.LiteralTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.SimpleTreeVisitor;
import com.sun.source.util.TreeScanner;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class StringFoldingTest {
    final JavaCompiler tool;

    public StringFoldingTest() {
        tool = ToolProvider.getSystemJavaCompiler();
    }

    static class JavaSource extends SimpleJavaFileObject {

        final String source;

        JavaSource(String source) {
            super(URI.create("myfo:/C.java"), JavaFileObject.Kind.SOURCE);
            this.source = source;
        }

        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return source;
        }
    }

    public static void main(String... args) throws IOException {
        StringFoldingTest t = new StringFoldingTest();
        t.run(false);
        t.run(true);
    }

    void run(boolean disableStringFolding) throws IOException {
        run("class C {String X=\"F\" + \"O\" + \"L\" + \"D\" + \"E\" + \"D\";}", disableStringFolding);
        run("class C {String X=\"\\{\"F\"}\\{\"O\"}\\{\"L\"}\\{\"D\"}\\{\"E\"}\\{\"D\"}\";}", disableStringFolding);
        run("class C {String X=\"\\{\"F\" + \"O\"}\\{\"L\"}\\{\"D\" + \"E\" + \"\\{\"D\"}\"}\";}", disableStringFolding);
        run("class C {String v=\"x\";String X=v + (\"F\" + \"OLDED\") + \"z\";}", disableStringFolding);
        run("class C {String X=\"\\{\"F\"}O\\{\"\\{\"L\"}\"}\\{\"\\{\"\\{\"D\"}\"}\"}\\{\"\\{\"\\{\"\\{\"E\"}\"}\"}\"}\\{\"\\{\"\\{\"\\{\"\\{\"D\"}\"}\"}\"}\"}\";}", disableStringFolding);
    }

    void run(String sourceCodeStr, boolean disableStringFolding) throws IOException {
        List<String> argsList = new ArrayList<String>();
        if (disableStringFolding) {
            argsList.add("-XDallowStringFolding=false");
        }
        JavacTask ct = (JavacTask)tool.getTask(null, null, null,
                argsList,
                null,
                Arrays.asList(new JavaSource(sourceCodeStr)));
        Iterable<? extends CompilationUnitTree> trees = ct.parse();
        String text = trees.toString();
        System.out.println(text);

        if (disableStringFolding) {
            if (text.contains("FOLDED") || scanForLiteral(trees, "FOLDED"::equals)) {
                throw new AssertionError("Expected no string folding");
            }
            if (!text.contains("\"F\"") || !scanForLiteral(trees, "F"::equals)) {
                throw new AssertionError("Expected content not found");
            }
        } else {
            if (!text.contains("FOLDED") || !scanForLiteral(trees, "FOLDED"::equals)) {
                throw new AssertionError("Expected string folding");
            }
        }
    }

    boolean scanForLiteral(Iterable<? extends CompilationUnitTree> trees, Predicate<? super String> filter) {
        for (CompilationUnitTree tree : trees) {
            Boolean found = tree.accept(new TreeScanner<>() {
                @Override
                public Boolean reduce(Boolean r1, Boolean r2) {
                    return (r1 != null && r1) || (r2 != null && r2);
                }

                @Override
                public Boolean scan(Tree tree, Boolean value) {
                    if (tree == null) {
                        return false;
                    }
                    if (value != null && value) {
                        return true;
                    }

                    Boolean acceptedLiteral = tree.accept(new SimpleTreeVisitor<>(){
                        @Override
                        public Boolean visitLiteral(LiteralTree node, Boolean literalValue) {
                            if (node.getKind() == Tree.Kind.STRING_LITERAL) {
                                return filter.test(node.getValue().toString());
                            } else {
                                return false;
                            }
                        }
                    }, false);
                    if (acceptedLiteral != null && acceptedLiteral) {
                        return true;
                    }

                    return super.scan(tree, false);
                }
            }, false);
            if (found != null && found) {
                return true;
            }
        }
        return false;
    }
}
