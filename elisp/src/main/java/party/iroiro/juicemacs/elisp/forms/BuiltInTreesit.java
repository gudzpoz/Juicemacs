package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;

import java.util.List;

public class BuiltInTreesit extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInTreesitFactory.getFactories();
    }

    /**
     * <pre>
     * Return non-nil if LANGUAGE exists and is loadable.
     *
     * If DETAIL is non-nil, return (t . nil) when LANGUAGE is available,
     * (nil . DATA) when unavailable.  DATA is the signal data of
     * `treesit-load-language-error'.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-language-available-p", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitLanguageAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitLanguageAvailableP(Object language, Object detail) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the language ABI version of the tree-sitter library.
     *
     * By default, report the latest ABI version supported by the library for
     * loading language support modules.  The library is backward-compatible
     * with language modules which use older ABI versions; if MIN-COMPATIBLE
     * is non-nil, return the oldest compatible ABI version.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-library-abi-version", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitLibraryAbiVersion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitLibraryAbiVersion(Object minCompatible) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the ABI version of the tree-sitter grammar for LANGUAGE.
     * Return nil if a grammar library for LANGUAGE is not available.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-language-abi-version", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitLanguageAbiVersion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitLanguageAbiVersion(Object language) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the absolute file name of the grammar file for LANGUAGE.
     *
     * If LANGUAGE isn't loaded yet, load it first.  If the language can't be
     * loaded or the file name couldn't be determined, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-grammar-location", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitGrammarLocation extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitGrammarLocation(Object language) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a tree-sitter parser.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a tree-sitter node.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a compiled tree-sitter query.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-compiled-query-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitCompiledQueryP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitCompiledQueryP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if OBJECT is a generic tree-sitter query.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-query-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitQueryP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitQueryP(Object object) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the language of QUERY.
     * QUERY has to be a compiled query.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-query-language", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitQueryLanguage extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitQueryLanguage(Object query) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the parser to which NODE belongs.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-parser", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeParser extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeParser(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create and return a parser in BUFFER for LANGUAGE with TAG.
     *
     * The parser is automatically added to BUFFER's parser list, as returned
     * by `treesit-parser-list'.  LANGUAGE is a language symbol.  If BUFFER
     * is nil or omitted, it defaults to the current buffer.  If BUFFER
     * already has a parser for LANGUAGE with TAG, return that parser, but if
     * NO-REUSE is non-nil, always create a new parser.
     *
     * TAG can be any symbol except t, and defaults to nil.  Different
     * parsers can have the same tag.
     *
     * If that buffer is an indirect buffer, its base buffer is used instead.
     * That is, indirect buffers use their base buffer's parsers.  Lisp
     * programs should widen as necessary should they want to use a parser in
     * an indirect buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-create", minArgs = 1, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FTreesitParserCreate extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserCreate(Object language, Object buffer, Object noReuse, Object tag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete PARSER from its buffer's parser list.
     * See `treesit-parser-list' for the buffer's parser list.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-delete", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserDelete extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserDelete(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return BUFFER's parser list, filtered by LANGUAGE and TAG.
     *
     * BUFFER defaults to the current buffer.  If that buffer is an indirect
     * buffer, its base buffer is used instead.  That is, indirect buffers
     * use their base buffer's parsers.
     *
     * If LANGUAGE is non-nil, only return parsers for that language.
     *
     * The returned list only contain parsers with TAG.  TAG defaults to nil.
     * If TAG is t, include parsers in the returned list regardless of their
     * tag.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-list", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTreesitParserList extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserList(Object buffer, Object language, Object tag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the buffer of PARSER.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-buffer", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserBuffer extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserBuffer(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return PARSER's language symbol.
     * This symbol is the one used to create the parser.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-language", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserLanguage extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserLanguage(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return PARSER's tag.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-tag", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserTag extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserTag(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the root node of PARSER.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-root-node", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserRootNode extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserRootNode(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Limit PARSER to RANGES.
     *
     * RANGES is a list of (BEG . END), each (BEG . END) defines a region in
     * which the parser should operate.  Regions must not overlap, and the
     * regions should come in order in the list.  Signal
     * `treesit-set-range-error' if the argument is invalid, or something
     * else went wrong.  If RANGES is nil, the PARSER is to parse the whole
     * buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-set-included-ranges", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitParserSetIncludedRanges extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserSetIncludedRanges(Object parser, Object ranges) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the ranges set for PARSER.
     * If no ranges are set for PARSER, return nil.
     * See also `treesit-parser-set-included-ranges'.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-included-ranges", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserIncludedRanges extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserIncludedRanges(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the list of after-change notifier functions for PARSER.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-notifiers", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitParserNotifiers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserNotifiers(Object parser) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Add FUNCTION to the list of PARSER's after-change notifiers.
     * FUNCTION must be a function symbol, rather than a lambda form.
     * FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
     * of cons cells of the form (START . END), where START and END are buffer
     * positions.  PARSER is the parser issuing the notification.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-add-notifier", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitParserAddNotifier extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserAddNotifier(Object parser, Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Remove FUNCTION from the list of PARSER's after-change notifiers.
     *   FUNCTION must be a function symbol, rather than a lambda form.
     * FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
     * of cons of the form (START . END), where START and END are buffer
     * positions.  PARSER is the parser issuing the notification.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parser-remove-notifier", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitParserRemoveNotifier extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParserRemoveNotifier(Object parser, Object function) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Parse STRING using a parser for LANGUAGE.
     *
     * Return the root node of the result parse tree.  DO NOT use this function
     * in a loop: this function is intended for one-off use and isn't
     * optimized; for heavy workload, use a temporary buffer instead.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-parse-string", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitParseString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitParseString(Object string, Object language) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the NODE's type as a string.
     * If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-type", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeType extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeType(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the NODE's start position in its buffer.
     * If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-start", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeStart extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeStart(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the NODE's end position in its buffer.
     * If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-end", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeEnd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeEnd(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the string representation of NODE.
     * If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-string", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeString(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the immediate parent of NODE.
     * Return nil if NODE has no parent.  If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-parent", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeParent extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeParent(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the Nth child of NODE.
     *
     * Return nil if there is no Nth child.  If NAMED is non-nil, look for
     * named child only.  NAMED defaults to nil.  If NODE is nil, return
     * nil.
     *
     * N could be negative, e.g., -1 represents the last child.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-child", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeChild extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeChild(Object node, Object n, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if NODE has PROPERTY, nil otherwise.
     *
     * PROPERTY could be `named', `missing', `extra', `outdated',
     * `has-error', or `live'.
     *
     * Named nodes correspond to named rules in the language definition,
     * whereas "anonymous" nodes correspond to string literals in the
     * language definition.
     *
     * Missing nodes are inserted by the parser in order to recover from
     * certain kinds of syntax errors, i.e., should be there but not there.
     *
     * Extra nodes represent things like comments, which are not required the
     * language definition, but can appear anywhere.
     *
     * A node is "outdated" if the parser has reparsed at least once after
     * the node was created.
     *
     * A node "has error" if itself is a syntax error or contains any syntax
     * errors.
     *
     * A node is "live" if its parser is not deleted and its buffer is
     * live.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-check", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeCheck extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeCheck(Object node, Object property) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the field name of the Nth child of NODE.
     *
     * Return nil if there's no Nth child, or if it has no field.
     * If NODE is nil, return nil.
     *
     * N counts all children, i.e., named ones and anonymous ones.
     *
     * N could be negative, e.g., -1 represents the last child.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-field-name-for-child", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeFieldNameForChild extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeFieldNameForChild(Object node, Object n) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the number of children of NODE.
     *
     * If NAMED is non-nil, count named children only.  NAMED defaults to
     * nil.  If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-child-count", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeChildCount extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeChildCount(Object node, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the child of NODE with FIELD-NAME (a string).
     * Return nil if there is no such child.  If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-child-by-field-name", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeChildByFieldName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeChildByFieldName(Object node, Object fieldName) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the next sibling of NODE.
     *
     * Return nil if there is no next sibling.  If NAMED is non-nil, look for named
     * siblings only.  NAMED defaults to nil.  If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-next-sibling", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeNextSibling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeNextSibling(Object node, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the previous sibling of NODE.
     *
     * Return nil if there is no previous sibling.  If NAMED is non-nil, look
     * for named siblings only.  NAMED defaults to nil.  If NODE is nil,
     * return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-prev-sibling", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodePrevSibling extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodePrevSibling(Object node, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the first child of NODE for buffer position POS.
     *
     * Specifically, return the first child that extends beyond POS.
     * Return nil if there is no such child.
     * If NAMED is non-nil, look for named children only.  NAMED defaults to nil.
     * Note that this function returns an immediate child, not the smallest
     * (grand)child.  If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-first-child-for-pos", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeFirstChildForPos extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeFirstChildForPos(Object node, Object pos, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the smallest node that covers buffer positions BEG to END.
     *
     * The returned node is a descendant of NODE.
     * Return nil if there is no such node.
     * If NAMED is non-nil, look for named child only.  NAMED defaults to nil.
     * If NODE is nil, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-descendant-for-range", minArgs = 3, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeDescendantForRange extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeDescendantForRange(Object node, Object beg, Object end, Object named) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if NODE1 and NODE2 refer to the same node.
     * If any one of NODE1 and NODE2 is nil, return nil.
     * This function uses the same equivalence metric as `equal', and returns
     * non-nil if NODE1 and NODE2 refer to the same node in a syntax tree
     * produced by tree-sitter.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-eq", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeEq extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeEq(Object node1, Object node2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Expand PATTERN to its string form.
     *
     * PATTERN can be
     *
     *     :anchor
     *     :?
     *     :*
     *     :+
     *     :equal
     *     :match
     *     (TYPE PATTERN...)
     *     [PATTERN...]
     *     FIELD-NAME:
     *     &#64;CAPTURE-NAME
     *     (_)
     *     _
     *     \"TYPE\"
     *
     * See Info node `(elisp)Pattern Matching' for detailed explanation.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-pattern-expand", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitPatternExpand extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitPatternExpand(Object pattern) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Expand sexp QUERY to its string form.
     *
     * A PATTERN in QUERY can be
     *
     *     :anchor
     *     :?
     *     :*
     *     :+
     *     :equal
     *     :match
     *     (TYPE PATTERN...)
     *     [PATTERN...]
     *     FIELD-NAME:
     *     &#64;CAPTURE-NAME
     *     (_)
     *     _
     *     \"TYPE\"
     *
     * See Info node `(elisp)Pattern Matching' for detailed explanation.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-query-expand", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitQueryExpand extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitQueryExpand(Object query) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Compile QUERY to a compiled query.
     *
     * Querying with a compiled query is much faster than an uncompiled one.
     * LANGUAGE is the language this query is for.
     *
     * If EAGER is non-nil, immediately load LANGUAGE and compile the query.
     * Otherwise defer the compilation until the query is first used.
     *
     * Signal `treesit-query-error' if QUERY is malformed or something else
     * goes wrong.  (This only happens if EAGER is non-nil.)
     * You can use `treesit-query-validate' to validate and debug a query.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-query-compile", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTreesitQueryCompile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitQueryCompile(Object language, Object query, Object eager) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Query NODE with patterns in QUERY.
     *
     * Return a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
     * assigned to the node in PATTERN.  NODE is the captured node.
     *
     * QUERY is either a string query, a sexp query, or a compiled query.
     * See Info node `(elisp)Pattern Matching' for how to write a query in
     * either string or sexp form.  When using repeatedly, a compiled query
     * is much faster than a string or sexp one, so it is recommend to
     * compile your query if it will be used repeatedly.
     *
     * BEG and END, if both non-nil, specify the region of buffer positions
     * in which the query is executed.  Any matching node whose span overlaps
     * with the region between BEG and END are captured, it doesn't have to
     * be completely in the region.
     *
     * If NODE-ONLY is non-nil, return a list of nodes.
     *
     * Besides a node, NODE can be a parser, in which case the root node of
     * that parser is used.  NODE can also be a language symbol, in which case
     * the root node of a parser for that language is used.  If such a parser
     * doesn't exist, it is created.
     *
     * Signal `treesit-query-error' if QUERY is malformed or something else
     * goes wrong.  You can use `treesit-query-validate' to validate and debug
     * the query.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-query-capture", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTreesitQueryCapture extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitQueryCapture(Object node, Object query, Object beg, Object end, Object nodeOnly) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Traverse the parse tree of NODE depth-first using PREDICATE.
     *
     * Traverse the subtree of NODE, and match PREDICATE with each node along
     * the way.
     *
     * PREDICATE can be a regexp string that matches against each node's
     * type, a predicate function, and more.  See `treesit-thing-settings'
     * for the possible predicates.  PREDICATE can also be a thing defined in
     * `treesit-thing-settings'.  Using an undefined thing doesn't raise an
     * error.
     *
     * By default, only traverse named nodes, but if ALL is non-nil, traverse
     * all nodes.  If BACKWARD is non-nil, traverse backwards.  If DEPTH is
     * non-nil, only traverse nodes up to that number of levels down in the
     * tree.  If DEPTH is nil, default to 1000.
     *
     * Return the first matched node, or nil if none matches.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-search-subtree", minArgs = 2, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FTreesitSearchSubtree extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitSearchSubtree(Object node, Object predicate, Object backward, Object all, Object depth) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Search for node matching PREDICATE in the parse tree of START.
     *
     * Start traversing the tree from node START, and match PREDICATE with
     * each node (except START itself) along the way.
     *
     * PREDICATE can be a regexp string that matches against each node's
     * type, a predicate function, and more.  See `treesit-thing-settings'
     * for the possible predicates.  PREDICATE can also be a thing defined in
     * `treesit-thing-settings'.  Using an undefined thing doesn't raise an
     * error.
     *
     * By default, only search for named nodes, but if ALL is non-nil, search
     * for all nodes.  If BACKWARD is non-nil, search backwards.
     *
     * Return the first matched node, or nil if none matches.
     *
     * For a tree like below, where START is marked by S, traverse as
     * numbered from 1 to 12:
     *
     *                 12
     *                 |
     *        S--------3----------11
     *        |        |          |
     *   o--o-+--o  1--+--2    6--+-----10
     *   |  |                  |        |
     *   o  o                +-+-+   +--+--+
     *                       |   |   |  |  |
     *                       4   5   7  8  9
     *
     * Note that this function doesn't traverse the subtree of START, and it
     * always traverse leaf nodes first, then upwards.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-search-forward", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FTreesitSearchForward extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitSearchForward(Object start, Object predicate, Object backward, Object all) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create a sparse tree of ROOT's subtree.
     *
     * This takes the subtree under ROOT, and combs it so only the nodes that
     * match PREDICATE are left, like picking out grapes on the vine.
     *
     * PREDICATE can be a regexp string that matches against each node's
     * type, a predicate function, and more.  See `treesit-thing-settings'
     * for the possible predicates.  PREDICATE can also be a thing defined in
     * `treesit-thing-settings'.  Using an undefined thing doesn't raise an
     * error.
     *
     * For a subtree on the left that consist of both numbers and letters, if
     * PREDICATE is "is letter", the returned tree is the one on the right.
     *
     *         a                 a              a
     *         |                 |              |
     *     +---+---+         +---+---+      +---+---+
     *     |   |   |         |   |   |      |   |   |
     *     b   1   2         b   |   |      b   c   d
     *         |   |     =&gt;      |   |  =&gt;      |
     *         c   +--+          c   +          e
     *         |   |  |          |   |
     *      +--+   d  4       +--+   d
     *      |  |              |
     *      e  5              e
     *
     * If PROCESS-FN is non-nil, it should be a function of one argument.  In
     * that case, instead of returning the matched nodes, pass each node to
     * PROCESS-FN, and use its return value instead.
     *
     * If non-nil, DEPTH is the number of levels to go down the tree from
     * ROOT.  If DEPTH is nil or omitted, it defaults to 1000.
     *
     * Each node in the returned tree looks like (NODE . (CHILD ...)).  The
     * root of this tree might be nil, if ROOT doesn't match PREDICATE.
     *
     * If no node matches PREDICATE, return nil.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-induce-sparse-tree", minArgs = 2, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FTreesitInduceSparseTree extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitInduceSparseTree(Object root, Object predicate, Object processFn, Object depth) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Check whether NODE matches PREDICATE.
     *
     * PREDICATE can be a symbol representing a thing in
     * `treesit-thing-settings', or a predicate, like regexp matching node
     * type, etc.  See `treesit-thing-settings' for more details.
     *
     * Return non-nil if NODE matches PREDICATE, nil otherwise.
     *
     * Signals `treesit-invalid-predicate' if there's no definition of THING
     * in `treesit-thing-settings', or if PREDICATE is malformed.  If
     * IGNORE-MISSING is non-nil, don't signal an error for missing THING
     * definition, but still signal for malformed PREDICATE.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-node-match-p", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FTreesitNodeMatchP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitNodeMatchP(Object node, Object predicate, Object ignoreMissing) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return information about the subtree of NODE.
     *
     * Return a list (MAX-DEPTH MAX-WIDTH COUNT), where MAX-DEPTH is the
     * maximum depth of the subtree, MAX-WIDTH is the maximum number of
     * direct children of nodes in the subtree, and COUNT is the number of
     * nodes in the subtree, including NODE.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-subtree-stat", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FTreesitSubtreeStat extends ELispBuiltInBaseNode {
        @Specialization
        public static Void treesitSubtreeStat(Object node) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if tree-sitter support is built-in and available.
     * </pre>
     */
    @ELispBuiltIn(name = "treesit-available-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FTreesitAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean treesitAvailableP() {
            // TODO
            return false;
        }
    }
}
