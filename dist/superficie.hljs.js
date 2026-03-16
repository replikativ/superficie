/**
 * Superficie language definition for highlight.js
 *
 * Designed to be close to Clojure's highlighting while adding
 * superficie-specific syntax (|>, .>, :=, =>, end blocks).
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
      'if if-not if-let if-some when when-not when-let when-some when-first ' +
      'let binding with-open with-redefs loop cond condp case ' +
      'for doseq dotimes do try catch finally while ' +
      'end else new not throw recur and or mod rem in';

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
    var OPERATOR = {
      scope: 'operator',
      match: /:=|=>|\|>|\.>|not=|==|<=|>=|\+(?=\s)|\-(?=\s)|\*(?=\s)|\/(?=\s)/,
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

    // --- Definition forms: defn name, def name ---
    var DEFINITION = {
      begin: /\b(defmacro|defn-?|defonce|def)\s+/,
      beginScope: 'keyword',
      end: /(?=\(|:=|\s*$)/,
      contains: [{
        scope: 'title.function',
        match: /[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/,
        relevance: 0
      }]
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
    var KW_PATTERN = BLOCK_KEYWORDS.replace(/[- ]/g, function (c) {
      return c === ' ' ? '|' : '\\-';
    });

    var KEYWORD = {
      match: new RegExp('\\b(' + KW_PATTERN + ')\\b'),
      scope: 'keyword',
      relevance: 0
    };

    // --- Built-in function calls: map(...), filter(...), str(...) ---
    var BUILTIN_PATTERN = CLOJURE_BUILTINS.replace(/[- ]/g, function (c) {
      return c === ' ' ? '|' : '\\-';
    });

    var BUILTIN_CALL = {
      match: new RegExp('\\b(' + BUILTIN_PATTERN + ')(?=\\()'),
      scope: 'built_in',
      relevance: 0
    };

    // --- Built-in as value (not called): passed as argument, e.g. map(inc, xs) ---
    var BUILTIN_VALUE = {
      match: new RegExp('\\b(' + BUILTIN_PATTERN + ')\\b'),
      scope: 'built_in',
      relevance: 0
    };

    // --- Regular function calls: f(...) ---
    var FUNC_CALL = {
      match: new RegExp(
        '(?!\\b(?:' + KW_PATTERN + '|' + BUILTIN_PATTERN + ')\\b)' +
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
