# Superficie Grammar

This document explains the formal grammar of superficie, the surface syntax for Clojure. The grammar is defined in [EBNF](../resources/superficie.ebnf) and parsed with [instaparse](https://github.com/Engelberg/instaparse).

## Overview

Superficie maps Clojure S-expressions to a syntax with:
- **Infix operators** with standard precedence (`a + b * c`)
- **Block structure** using `:` and `end` delimiters instead of parentheses
- **Comma-separated arguments** instead of whitespace-separated
- **Pipe operators** (`|>`, `.>`) instead of threading macros

Everything that superficie can't express falls through to **S-expression passthrough** — raw Clojure in parentheses is always valid superficie.

## Program Structure

A superficie program is a sequence of top-level forms separated by blank lines:

```
program = top-form (blank-line top-form)*
```

Top-level forms are: `ns-form`, `def-form`, `defn-form`, `sexp` (raw Clojure), or any `expr`.

### Whitespace

Superficie has three whitespace modes:

| Token | Matches | Used for |
|-------|---------|----------|
| `_`   | spaces, tabs, newlines, commas, comments | general separator |
| `sp`  | spaces and tabs only | around infix operators |
| `eol` | at least one newline | between body expressions |

The distinction between `sp` and `_` is critical: infix operators require spaces/tabs (not newlines) on each side. This is how `my-func` (one symbol with a dash) is distinguished from `a - b` (subtraction with spaces).

### Body Structure

Block bodies contain one expression per line:

```
body = (eol expr)+
```

Multi-line expressions work via pipe operators which consume the leading newline:

```clojure
users
  |> filter(:active)
  |> map(:name)
```

## Expression Precedence

Expressions follow standard mathematical precedence, from lowest to highest binding:

| Level | Rule | Operators | Clojure equivalent |
|-------|------|-----------|--------------------|
| 1 | `pipe-expr` | `\|>` `.>` | `->>` `->` |
| 2 | `or-expr` | `or` | `or` |
| 3 | `and-expr` | `and` | `and` |
| 4 | `comp-expr` | `=` `<` `>` `<=` `>=` `not=` | `=` `<` `>` etc. |
| 5 | `add-expr` | `+` `-` | `+` `-` |
| 6 | `mul-expr` | `*` `/` `mod` `rem` | `*` `/` `mod` `rem` |
| 7 | unary | `not` `-` `@` `throw` `recur` | `not` `-` `deref` `throw` `recur` |
| 8 | `postfix-expr` | `.method()` `.-field` | `(.method obj)` `(.-field obj)` |
| 9 | `primary` | literals, calls, blocks | everything else |

Note: comparisons are **binary only** — `a < b < c` is a parse error. Use `a < b and b < c` instead. This is a deliberate simplification; Clojure's variadic `(< a b c)` can be expressed as a function call: `<(a, b, c)`.

## Definitions

### `def` / `defonce`

```
def-form = def-kw meta-symbol ':=' expr
def-kw   = 'def' | 'defonce'
```

Examples: `def pi := 3.14159`, `defonce config := load-config()`

### `defn` / `defn-` / `defmacro`

```
defn-form = defn-kw meta-symbol params ':' body 'end'       ;; single arity
          | defn-kw meta-symbol multi-arity+ 'end'           ;; multi arity
defn-kw   = 'defn' | 'defn-' | 'defmacro'
```

Single arity:
```clojure
defn greet(name):
  str("Hello, ", name)
end
```

Multi arity:
```clojure
defn greet
  ():
    greet("World")
  (name):
    str("Hello, ", name)
end
```

### Anonymous functions

```
anon-fn = 'fn' fn-name? params ':' body 'end'
        | 'fn' fn-name? multi-arity+ 'end'
```

`fn` is used for anonymous functions (Clojure's `fn`), while `defn` is used for named definitions (Clojure's `defn`).

## Block Expressions

All blocks follow the pattern `keyword ... ':' body 'end'`:

| Superficie | Clojure | Notes |
|------------|---------|-------|
| `if test: ... else: ... end` | `(if test ... ...)` | `else` is optional |
| `if-let x := expr: ... end` | `(if-let [x expr] ...)` | also `if-some` |
| `when test: ... end` | `(when test ...)` | also `when-not` |
| `when-let x := expr: ... end` | `(when-let [x expr] ...)` | also `when-some`, `when-first` |
| `let x := 1, y := 2: ... end` | `(let [x 1 y 2] ...)` | also `binding`, `with-open`, `with-redefs` |
| `loop x := 0: ... end` | `(loop [x 0] ...)` | |
| `cond: test => expr ... end` | `(cond test expr ...)` | |
| `condp pred expr: test => val ... end` | `(condp pred expr test val ...)` | `else =>` for default |
| `case expr: val => result ... end` | `(case expr val result ...)` | `else =>` for default |
| `for x in xs, when p: ... end` | `(for [x xs :when p] ...)` | also `doseq`, `dotimes` |
| `do: ... end` | `(do ...)` | |
| `try: ... catch Type e: ... end` | `(try ... (catch Type e ...))` | `finally:` optional |

### Bindings

Bindings use `:=` and are comma-separated:

```
bindings = target ':=' expr (',' target ':=' expr)*
```

Binding targets can be symbols, vectors (for sequential destructuring), or maps (for associative destructuring).

### For Comprehension Modifiers

Inside `for`/`doseq`/`dotimes`, modifiers follow bindings:

```clojure
for x in xs, y in ys, when x not= y, let z := x + y:
  [x, y, z]
end
```

The `when`, `while`, and `let` keywords here are **for-modifiers**, not block keywords. They are unambiguous because they only appear after a comma inside for-bindings.

## Function Calls

```
call-expr = callable '(' args ')'
callable  = keyword | symbol | set | map | vector | '(' expr ')'
```

Any callable followed by parenthesized, comma-separated arguments is a function call:

```clojure
map(inc, [1, 2, 3])        ;; (map inc [1 2 3])
:name(user)                 ;; (:name user)
#{:admin}(role)             ;; (#{:admin} role)
```

## Postfix: Java Interop

```clojure
obj.method(args)   ;; (.method obj args)
obj.-field         ;; (.-field obj)
new HashMap(16)    ;; (HashMap. 16)
```

Method calls and field access are postfix on the object. Constructors use the `new` keyword.

## Pipe Operators

```
pipe-expr = expr ('|>' step)+    ;; thread-last (->>)
          | expr ('.>' step)+    ;; thread-first (->)
```

Pipe steps can be function calls or `.method()` calls:

```clojure
data
  |> filter(:active)
  |> map(:name)
  .> .trim()
```

## S-Expression Passthrough

Any parenthesized Clojure expression is valid superficie:

```
sexp = '(' sexp-inner* ')'
```

This is the escape hatch for anything the surface syntax doesn't cover: `comment` forms, syntax-quoted macro bodies, reader conditionals, etc. The renderer automatically falls back to passthrough when a form can't roundtrip through surface syntax.

## Collections

```
vector = '[' (expr (',' expr)*)? ']'
map    = '{' (expr expr (',' expr expr)*)? '}'
set    = '#{' (expr (',' expr)*)? '}'
```

Commas are optional (as in Clojure) but conventional in superficie.

## Atoms

| Type | Examples | Notes |
|------|----------|-------|
| Numbers | `42`, `3.14`, `0xFF`, `1/3`, `42N`, `3.14M` | hex, ratio, bigint, bigdec |
| Strings | `"hello"` | standard escaping |
| Keywords | `:foo`, `:foo/bar` | namespaced keywords supported |
| Regex | `#"pattern"` | |
| Characters | `\a`, `\newline`, `\u0041` | |
| Nil/Bool | `nil`, `true`, `false` | |

## Symbols

Symbols follow Clojure conventions with a few special cases:

```
symbol = escaped-symbol          ;; `end`, `if` — backtick-escaped reserved words
       | dotted-ns-symbol        ;; java.util.UUID/randomUUID
       | dotted-symbol           ;; java.util.HashMap
       | identifier              ;; my-func, even?, ->vector
       | operator-symbol         ;; +, *, ->, >>=
```

Operator symbols (`+`, `*`, `/`, `->`, `->>`, etc.) can be used as values — e.g. `reduce(+, xs)`.

## Reserved Words

The following identifiers are reserved and cannot be used as symbol names without backtick escaping:

```clojure
defn defn- fn fn- defmacro defonce def
if if-not if-let if-some
when when-not when-let when-some when-first
with-open with-redefs let binding
loop cond condp case
do try catch finally
doseq dotimes for while
end else new not throw recur
ns nil true false
and or mod rem in
```

Use backtick escaping for reserved words as identifiers: `` `end` ``, `` `if` ``.

## Metadata

```
metadata = '^' value primary
```

Where value is a keyword (`^:private`), symbol (`^String`), string (`^"tag"`), or map (`^{:doc "..."}`).

## Reader Macros

```
quote = "'" primary      ;; 'x → (quote x)
var   = "#'" symbol      ;; #'x → (var x)
deref = '@' expr         ;; @x → (deref x)
```

Syntax-quote (`` ` ``), unquote (`~`), and unquote-splicing (`~@`) are handled via S-expression passthrough since they appear primarily in macro bodies.
