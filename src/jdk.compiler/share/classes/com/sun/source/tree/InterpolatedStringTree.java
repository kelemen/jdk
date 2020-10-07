package com.sun.source.tree;

import java.util.List;

/**
 * A tree node an {@code interpolated string}.
 *
 * For example:
 * <pre>
 *   "Prefix \{a * b} Middle \{c} Suffix"
 * </pre>
 *
 * @jls a.b Interpolated Strings
 *
 * @since xx
 */
public interface InterpolatedStringTree extends ExpressionTree {
    /**
     * Returns all the parts of the string in the order to be concatenated.
     * This includes the string literal parts as well. However, it does
     * not have to contain any string literal. For example: {@code "\{myVar}"} will
     * not have empty string parts, only a single one for {@code myVar}.
     *
     * @return the parts to be concatenated
     */
    List<? extends ExpressionTree> getStringParts();
}
