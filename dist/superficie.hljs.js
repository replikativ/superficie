/**
 * Superficie language definition for highlight.js
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
    var BLOCK_KEYWORDS = 'defmacro defn defn- fn fn- def defonce ns ' +
      'if if-not if-let if-some when when-not when-let when-some when-first ' +
      'let binding with-open with-redefs loop cond condp case ' +
      'for doseq dotimes do try catch finally ' +
      'end else new not throw recur and or mod rem in';

    var NUMBER = {
      scope: 'number',
      relevance: 0,
      variants: [
        { match: /[-+]?0[xX][0-9a-fA-F]+N?/ },
        { match: /[-+]?[0-9]+\/[0-9]+N?/ },
        { match: /[-+]?[0-9]+((\.[0-9]*([eE][+-]?[0-9]+)?M?)|([eE][+-]?[0-9]+M?|M))/ },
        { match: /[-+]?([1-9][0-9]*|0)N?/ },
      ]
    };

    var CHARACTER = {
      scope: 'character',
      variants: [
        { match: /\\u[0-9a-fA-F]{4}/ },
        { match: /\\(newline|space|tab|formfeed|backspace|return)/ },
        { match: /\\\S/, relevance: 0 }
      ]
    };

    var REGEX = { scope: 'regex', begin: /#"/, end: /"/, contains: [hljs.BACKSLASH_ESCAPE] };
    var STRING = hljs.inherit(hljs.QUOTE_STRING_MODE, { illegal: null });
    var COMMENT = hljs.COMMENT(';', '$', { relevance: 0 });
    var LITERAL = { className: 'literal', begin: /\b(true|false|nil)\b/ };
    var COMMA = { scope: 'punctuation', match: /,/, relevance: 0 };
    var KEY = { className: 'symbol', begin: /:{1,2}[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/ };

    var OPERATOR = {
      className: 'operator',
      match: /:=|=>|\|>|\.>|not=|==|<=|>=|<|>/,
      relevance: 0
    };

    var DEFINITION = {
      begin: /\b(defmacro|defn-?|defonce|def)\s+/,
      beginScope: 'keyword',
      end: /(?=\(|:=|\s*$)/,
      contains: [{
        className: 'title.function',
        begin: /[a-zA-Z_\-!.?+*=<>&'][a-zA-Z0-9_\-!.?+*=<>&'/;:$#]*/,
        relevance: 0
      }]
    };

    var NS_FORM = {
      begin: /\bns\s+/,
      beginScope: 'keyword',
      end: /(?=\s)/,
      contains: [{
        className: 'title.class',
        begin: /[a-zA-Z_][a-zA-Z0-9_.\-]*/,
        relevance: 0
      }]
    };

    var KW_PATTERN = BLOCK_KEYWORDS.replace(/[- ]/g, function(c) { return c === ' ' ? '|' : '\\-'; });

    var KEYWORD = {
      match: new RegExp('\\b(' + KW_PATTERN + ')\\b'),
      className: 'keyword',
      relevance: 0
    };

    var FUNC_CALL = {
      match: new RegExp('(?!\\b(?:' + KW_PATTERN + ')\\b)[a-zA-Z_\\-!.?+*=<>&\'][a-zA-Z0-9_\\-!.?+*=<>&\'/;:$#]*(?=\\()'),
      className: 'title.function',
      relevance: 0
    };

    var METHOD_CALL = {
      match: /\.[a-zA-Z_][a-zA-Z0-9_]*(?=\()/,
      className: 'title.function',
      relevance: 0
    };

    var FIELD_ACCESS = {
      match: /\.-[a-zA-Z_][a-zA-Z0-9_]*/,
      className: 'title.function',
      relevance: 0
    };

    var COLLECTION = {
      begin: /[\[\{#]/,
      end: /[\]\}]/,
      relevance: 0
    };

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
        OPERATOR,
        METHOD_CALL,
        FIELD_ACCESS,
        KEYWORD,
        FUNC_CALL,
        KEY,
        NUMBER,
        LITERAL,
        COMMA,
        COLLECTION,
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
