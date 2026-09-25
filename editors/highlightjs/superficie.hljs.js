/**
 * Superficie language definition for highlight.js
 *
 * Designed to be close to Clojure's highlighting while adding
 * superficie-specific syntax (|>, .>, :=, =>, x -> body, x[i] <- v,
 * | match arms, end blocks).
 *
 * Source of truth: editors/highlightjs/superficie.hljs.js in the superficie
 * repository; the npm release copies it to dist/.
 *
 * Usage:
 *   <script src="highlight.min.js"></script>
 *   <script src="superficie.hljs.js"></script>
 *   <script>hljs.highlightAll();</script>
 *
 * Then use: <pre><code class="language-superficie">...</code></pre>
 */
(function () {
  'use strict';

  function superficie(hljs) {
    // --- Clojure core built-ins (subset matching hljs clojure grammar) ---
    var CLOJURE_BUILTINS =
      'apply assoc assoc-in atom concat conj cons contains? count dec deref ' +
      'dissoc drop empty? every? filter filterv first flatten fn? frequencies ' +
      'get get-in group-by hash-map hash-set identity inc interleave interpose ' +
      'into iterate juxt keys keyword keyword? last list list? map map? mapcat ' +
      'mapv max merge merge-with meta min name namespace neg? next nil? not-any? ' +
      'not-empty not-every? nth partial partition partition-all partition-by ' +
      'peek pos? pr-str print println pprint quot range re-find re-matches ' +
      're-seq reduce reduce-kv reductions remove repeat repeatedly replace ' +
      'reset! rest reverse second select-keys seq seq? set set? shuffle ' +
      'slurp some some? sort sort-by split-at split-with str subs subvec ' +
      'swap! symbol symbol? take take-last take-while update update-in ' +
      'val vals vec vector vector? zero? zipmap require import use refer ' +
      'comp complement constantly memoize';

    // --- Superficie block keywords ---
    var BLOCK_KEYWORDS = 'defmacro defn defn- fn fn- def defonce ns ' +
      'defmulti defmethod defprotocol defrecord deftype reify proxy ' +
      'if if-not if-let if-some when when-not when-let when-some when-first ' +
      'let letfn binding with-open with-redefs loop cond condp case match ' +
      'for doseq dotimes do try catch finally while ' +
      'end else new not throw recur and or mod rem in';

    // A Clojure symbol may contain - / ! ? * . etc., so \b is not a symbol
    // boundary: without these, `map` would match inside par/map-void! and
    // `zero` inside add-zero.
    var SYM_CHAR = "[A-Za-z0-9_\\-!?*+<>=&'/.$%#]";
    var SYM_BEFORE = '(?<!' + SYM_CHAR + '|:)';
    var SYM_AFTER = "(?![A-Za-z0-9_\\-!?*+<>=&'/.$%#])";

    function wordsPattern(words) {
      return words.split(' ').map(function (w) {
        return w.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
      }).join('|');
    }

    // --- Atoms ---

    var NUMBER = {
      scope: 'number',
      relevance: 0,
      variants: [
        { match: /[-+]?0[xX][0-9a-fA-F]+N?/ },
        { match: /[-+]?[0-9]+\/[0-9]+N?/ },
        { match: /[-+]?[0-9]+((\.[0-9]*([eE][+-]?[0-9]+)?M?)|([eE][+-]?[0-9]+M?|M))/ },
        { match: /[-+]?([1-9][0-9]*|0)N?/ }
      ]
    };

    var CHARACTER = {
      scope: 'string',  // characters render like strings in Clojure
      variants: [
        { match: /\\u[0-9a-fA-F]{4}/ },
        { match: /\\(newline|space|tab|formfeed|backspace|return)/ },
        { match: /\\\S/, relevance: 0 }
      ]
    };

    var REGEX = { scope: 'regexp', begin: /#"/, end: /"/, contains: [hljs.BACKSLASH_ESCAPE] };
    var STRING = hljs.inherit(hljs.QUOTE_STRING_MODE, { illegal: null });
    var COMMENT = hljs.COMMENT(';', '$', { relevance: 0 });
    var LITERAL = { scope: 'literal', match: /\b(true|false|nil)\b/ };

    // --- Clojure keywords :foo, ::bar, :ns/name ---
    var KEY = {
      scope: 'symbol',
      match: /:{1,2}[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/
    };

    // --- Superficie-specific operators ---
    // x -> body (lambda) and x[i] <- v (store) only with spaces around them, so
    // ->url and <-chan stay symbols; a | match arm only at the start of a line
    var OPERATOR = {
      scope: 'operator',
      match: /:=|=>|\|>|\.>|(?<=\s)->(?=\s)|(?<=\s)<-(?=\s)|(?<=^[ \t]*)\|(?=\s)|not=|==|<=|>=|\+(?=\s)|\-(?=\s)|\*(?=\s)|\/(?=\s)/,
      relevance: 0
    };

    // Bare < and > only when surrounded by whitespace (not inside symbols like <!!)
    var COMP_OPERATOR = {
      scope: 'operator',
      match: /(?<=\s)<(?=\s)|(?<=\s)>(?=\s)/,
      relevance: 0
    };

    // --- Metadata: ^:keyword, ^Type, ^{...} ---
    var METADATA = {
      scope: 'meta',
      match: /\^:{1,2}[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*|\^[A-Z][a-zA-Z0-9_.]*/,
      relevance: 0
    };

    // --- Deref: @atom ---
    var DEREF = {
      scope: 'operator',
      match: /@/,
      relevance: 0
    };

    // --- Quote: 'symbol, '(...) ---
    var QUOTE = {
      scope: 'meta',
      match: /'/,
      relevance: 0
    };

    // --- Var ref: #'symbol ---
    var VAR_REF = {
      scope: 'meta',
      match: /#'[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/,
      relevance: 0
    };

    // --- Definition forms: defn name, a/defn name, deftm name, etc. ---
    // Only the defined name is a title; parameters and types stay plain.
    var DEFINITION = {
      match: [
        new RegExp(SYM_BEFORE + '(?:[A-Za-z0-9_.\\-]+\\/)?(?:defmacro|defn-?|defonce|def|defmulti|defmethod|' +
                   'defprotocol|defrecord|deftype|deftm|deftheorem|theorem|inductive)' + SYM_AFTER),
        /\s+/,
        new RegExp("[A-Za-z_\\-!?*+<>=&'][A-Za-z0-9_\\-!?*+<>=&'/.$%#]*")
      ],
      scope: { 1: 'keyword', 3: 'title.function' },
      relevance: 0
    };

    // --- ns form ---
    var NS_FORM = {
      begin: /\bns\s+/,
      beginScope: 'keyword',
      end: /(?=\s)/,
      contains: [{
        scope: 'title.class',
        match: /[a-zA-Z_][a-zA-Z0-9_.\-]*/,
        relevance: 0
      }]
    };

    // --- Build keyword pattern for block keywords ---
    var KW_PATTERN = wordsPattern(BLOCK_KEYWORDS);

    var KEYWORD = {
      match: new RegExp(SYM_BEFORE + '(' + KW_PATTERN + ')' + SYM_AFTER),
      scope: 'keyword',
      relevance: 0
    };

    // --- Any block head: the first word of a line that ends in ':' opens a
    // block, so library macros (par/map-void!, spin, a/theorem) highlight
    // without being listed. Not `x := …`, where x is a name, and not a line
    // that continues a bracket from the line above (`k :- Double] :- Void:`),
    // which closes a bracket before it opens one.
    var BLOCK_HEAD = {
      match: new RegExp("(?<=^[ \\t]*)[A-Za-z_*!?<>=&][A-Za-z0-9_\\-!?*+<>=&'/.$%#]*" +
                        '(?![^\\n]*:=)(?![^\\n\\[]*\\])(?![^\\n(]*\\))(?=[^\\n]*:[ \\t]*$)', 'm'),
      scope: 'keyword',
      relevance: 0
    };

    // --- Built-in function calls: map(...), filter(...), str(...) ---
    var BUILTIN_PATTERN = wordsPattern(CLOJURE_BUILTINS);

    var BUILTIN_CALL = {
      match: new RegExp(SYM_BEFORE + '(' + BUILTIN_PATTERN + ')(?=\\()'),
      scope: 'built_in',
      relevance: 0
    };

    // --- Built-in as value (not called): passed as argument, e.g. map(inc, xs) ---
    var BUILTIN_VALUE = {
      match: new RegExp(SYM_BEFORE + '(' + BUILTIN_PATTERN + ')' + SYM_AFTER),
      scope: 'built_in',
      relevance: 0
    };

    // --- Regular function calls: f(...) ---
    var FUNC_CALL = {
      match: new RegExp(
        SYM_BEFORE + '(?!(?:' + KW_PATTERN + '|' + BUILTIN_PATTERN + ')(?=\\())' +
        '[a-zA-Z_\\-!.?+*=<>&\'][a-zA-Z0-9_\\-!.?+*=<>&\'/;:$#]*(?=\\()'),
      scope: 'title.function',
      relevance: 0
    };

    // --- Java interop: .method(), .-field ---
    var METHOD_CALL = {
      match: /\.[a-zA-Z_][a-zA-Z0-9_]*(?=\()/,
      scope: 'title.function',
      relevance: 0
    };

    var FIELD_ACCESS = {
      match: /\.-[a-zA-Z_][a-zA-Z0-9_]*/,
      scope: 'title.function',
      relevance: 0
    };

    // --- Java class in new/static: new HashMap, Integer/parseInt ---
    var CONSTRUCTOR = {
      match: /(?<=\bnew\s+)[A-Z][a-zA-Z0-9_.]*/,
      scope: 'type',
      relevance: 0
    };

    var STATIC_CALL = {
      match: /[A-Z][a-zA-Z0-9_.]*\/[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/,
      scope: 'title.function',
      relevance: 0
    };

    var COMMA = { scope: 'punctuation', match: /,/, relevance: 0 };

    return {
      name: 'Superficie',
      aliases: ['sup'],
      contains: [
        COMMENT,
        STRING,
        REGEX,
        CHARACTER,
        DEFINITION,
        NS_FORM,
        BLOCK_HEAD,
        METADATA,
        VAR_REF,
        CONSTRUCTOR,
        STATIC_CALL,
        OPERATOR,
        COMP_OPERATOR,
        DEREF,
        QUOTE,
        METHOD_CALL,
        FIELD_ACCESS,
        KEYWORD,
        BUILTIN_CALL,
        FUNC_CALL,
        BUILTIN_VALUE,
        KEY,
        NUMBER,
        LITERAL,
        COMMA,
      ]
    };
  }

  // Register with highlight.js
  if (typeof hljs !== 'undefined') {
    hljs.registerLanguage('superficie', superficie);
  }
  // Support CommonJS/ES modules
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = superficie;
  }
})();
