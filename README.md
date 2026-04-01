# Superficie

[![CircleCI](https://circleci.com/gh/replikativ/superficie.svg?style=shield)](https://circleci.com/gh/replikativ/superficie)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/superficie.svg)](https://clojars.org/org.replikativ/superficie)

**Surface syntax for Clojure** — a bidirectional renderer that translates Clojure S-expressions into familiar, readable syntax and back.

## Why?

During my PhD in machine learning, I worked in Clojure while everyone around me used Python. I couldn't show my code to colleagues, supervisors, or domain experts without first explaining parentheses. In presentations, papers, and code reviews, the syntax was a wall — not because S-expressions are bad, but because you can't expect someone to parse them on the fly when they've never seen them before.

That initial unfamiliarity typically takes a few days to overcome. But a few days is infinity when you're in a meeting, reading a blog post, or reviewing code with someone outside your team.

Superficie exists to remove that barrier. You write Clojure as normal. When you need to *show* it to someone, you render it to a syntax they can already read.

**[Try the playground](https://replikativ.github.io/superficie/examples/playground.html)** — paste any Clojure code and see it rendered live, or use the built-in SCI REPL to evaluate superficie syntax directly.

## Quick Look

<table>
<tr><th>Clojure</th><th>Superficie</th></tr>
<tr>
<td>

```clojure
(defn greet [name]
  (str "Hello, " name "!"))
```

</td>
<td>

```
defn greet [name]:
  str("Hello, " name "!")
end
```

</td>
</tr>
<tr>
<td>

```clojure
(defn quadratic [a b c x]
  (+ (* a x x) (* b x) c))
```

</td>
<td>

```
defn quadratic [a b c x]:
  a * x * x + b * x + c
end
```

</td>
</tr>
<tr>
<td>

```clojure
(defn process-users [users]
  (->> users
       (filter :active)
       (map :name)
       (sort)
       (take 10)))
```

</td>
<td>

```
defn process-users [users]:
  users |> filter(:active)
        |> map(:name)
        |> sort()
        |> take(10)
end
```

</td>
</tr>
<tr>
<td>

```clojure
(defprotocol Shape
  (area [this])
  (perimeter [this]))

(defrecord Circle [r]
  Shape
  (area [this] (* Math/PI r r))
  (perimeter [this] (* 2 Math/PI r)))
```

</td>
<td>

```
defprotocol Shape:
  area [this]
  perimeter [this]
end

defrecord Circle [r]:
  Shape
  area [this]:
    Math/PI * r * r
  end
  perimeter [this]:
    2 * Math/PI * r
  end
end
```

</td>
</tr>
</table>

## What It Is (and Isn't)

**Superficie is a communication tool.** It renders Clojure into syntax that Python/Julia/TypeScript developers can read immediately — for presentations, documentation, blog posts, and conversations with domain experts.

**It is also a usable language.** Superficie syntax can be parsed back to Clojure forms and evaluated directly — via the JVM, Babashka, or a browser SCI REPL. You can write `.sup` files, run a REPL, and interoperate with any Clojure ecosystem library.

**It is not a separate ecosystem.** There is no superficie runtime, no lock-in. Printing any Clojure source as superficie always works. Reading back has a small set of design constraints: superficie reserves certain keywords (`if`, `when`, `let`, `match`, …) as block syntax, and Clojure code that uses these as variable names or relies on read-time namespace resolution (`::alias/key`) cannot round-trip cleanly. Code written *in* superficie — respecting its block syntax — roundtrips completely.

## Syntax at a Glance

### Definitions

```
def pi: 3.14159
defonce conn: connect("localhost:5432")

defn factorial [n]:
  loop [i n acc 1]:
    if i <= 1 :
      acc
    else:
      recur(dec(i) acc * i)
    end
  end
end
```

### Control Flow

```
if x > 0 :
  :positive
else:
  :non-positive
end

cond:
  neg?(x)  => :negative
  zero?(x) => :zero
  :else    => :positive
end

case method :
  :get  => fetch(path)
  :post => create(path body)
  =>      not-found()
end
```

### Bindings

```
let [x 1 y 2]:
  x + y
end

for [x xs y ys :when x not= y]:
  [x y]
end
```

### Threading (Pipes)

```
;; ->> becomes |>
users |> filter(:active) |> map(:name) |> sort() |> take(10)

;; -> becomes .>
config .> assoc(:port 8080) .> merge(defaults)
```

### Java Interop

```
s.toUpperCase()
point.-x
new StringBuilder("hello")
Integer/parseInt(s)
Math/PI
```

### Protocols and Records

```
defprotocol Shape:
  area [this]
  perimeter [this]
end

defrecord Circle [r]:
  Shape
  area [this]:
    Math/PI * r * r
  end
  perimeter [this]:
    2 * Math/PI * r
  end
end

defmulti area: :shape
defmethod area :circle [s]:
  Math/PI * :r(s) * :r(s)
end
```

### Namespaces

```
ns myapp.core:
  require:
    [clojure.string :as str]
    [myapp.db :refer [query insert!]]
  import:
    [java.time Instant Duration]
end
```

### Error Handling and Interop

```
try:
  Integer/parseInt(s)
catch [NumberFormatException e]:
  println(e.getMessage())
  nil
end
```

### Macros and Syntax-Quote

Macros are first-class in superficie. The backtick syntax-quote applies to a block form, and `~`/`~@` unquote/unquote-splicing work inside it:

```
defmacro unless [pred & body]:
  `if not(~pred):
    do(~@body)
  end
end
```

This round-trips correctly with Clojure. The `clj->sup` converter preserves syntax-quote structure from existing macros rather than expanding it, so real-world macros render readably:

```clojure
;; Clojure
(defmacro -> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       `(~(first form) ~x ~@(next form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))
```

```
;; Superficie
defmacro -> [x & forms]:
  loop [x x, forms forms]:
    if forms :
      let [form first(forms), threaded if seq?(form):
        `~first(form)(~x ~@next(form))
      else:
        list(form x)
      end]:
        recur(threaded next(forms))
      end
    else:
      x
    end
  end
end
```

User-defined macros are called with function syntax (`unless(pred body)`). Block syntax (`unless pred: body end`) is reserved for macros registered in the block registry — either built-in forms or library macros that explicitly declare their surface block kind via `:superficie/role` metadata.

### Function Call Fallback

The renderer never fails. Any Clojure form that doesn't match a known block pattern or operator is rendered using function call syntax — `f(a b c)` — which is always valid superficie and always round-trips cleanly:

```clojure
;; Clojure
(defmacro my-macro [x]
  (list 'if x :yes :no))
```

```
;; Superficie — list call renders as a regular function call
defmacro my-macro [x]:
  list('if x :yes :no)
end
```

Note: `(...)` in superficie is **grouping for infix**, not a raw S-expression form — `(a + b) * c`. To write a literal quoted list in superficie source, use `'(...)`: `'(if x :yes :no)` reads back as `(quote (if x :yes :no))`.

## How It Works

### Pipeline

Superficie uses a hand-written **LL parser** with four stages inspired by [Racket's shrubbery notation](https://docs.racket-lang.org/shrubbery/):

```
source text
  → tokenizer  (characters → flat token vector; throws on unterminated strings)
  → grouper    (tokens → shrubbery tree; NEVER throws — bracket errors become
                ShrubError nodes embedded in the valid surrounding tree)
  → enforest   (shrubbery → healed token stream; re-wraps partial bracket
                children with synthetic delimiters; drops stray closers)
  → reader     (token stream → Clojure forms; LL(1) recursive-descent with
                block dispatch by keyword and Pratt infix climbing)
```

The key design choice is the **two-phase bracket / semantic split**:

- The **grouper** resolves bracket structure and reports *all* structural
  errors without aborting. `f(x]` produces `(f x)` with an attached
  `ShrubError`; the surrounding code is still parsed correctly.
- The **reader** handles semantics: block keywords, operator precedence,
  and namespace resolution. Semantic errors throw `ex-info` with
  structured `:line`, `:col`, `:source-context`, and `:hint` data.

This is the same separation Rust's compiler and Racket's Rhombus use: construct the bracket/token tree first (where recovery is mechanical), then parse semantics against a structurally valid input.

Error messages are formatted with source context and underlines:

```
Error: Expected 'end' to close defn block (line 2, col 7)
2 |   x + 1
  |       ^

Error: Unterminated string — missing closing " (line 2, col 7)
2 |   str("hello x)
  |       ^

Error: Maximum nesting depth (150) exceeded (line 1, col 301)
1 | f(f(f(f(f(f(f( ...
  |                ^
```

The `errors/format-error` function turns any superficie `ex-info` into this format, and is also available to users building tooling on top of the parser.

Each `block-dispatch` entry maps a surface keyword (`"defn"`, `"if"`, `"for"`, …) to a parse function that consumes the rest of the line and the indented body. Forms that don't match any block keyword are parsed as infix expressions or function calls via Pratt climbing.

### Block Registration

On the JVM, superficie maintains a **block registry**: when you evaluate a form, the printer records which Clojure vars have surface block representations. This lets the renderer correctly handle project-specific `def`-like macros — if your project defines `defcomponent`, the renderer can be told it uses the `defn` block pattern.

The registry is populated by `superficie.runtime/register-ns!` and updated incrementally via the REPL.

### Interleaving with Clojure Evaluation

The pipeline follows Clojure's incremental evaluation model: each top-level form is fully parsed and evaluated before the next form is read. This means:

- Macros defined in form N are available when parsing form N+1
- `ns` declarations take effect immediately, so subsequent forms resolve in the new namespace
- The REPL and file runner share the same model — no distinction between interactive and batch evaluation

```
;; In the REPL — each form is live immediately:
defmacro unless [pred & body]:
  `if not(~pred):
    do(~@body)
  end
end

;; Next input — unless is available immediately:
unless((= 1 2) println("1 != 2"))
```

## Usage

### JVM CLI

```bash
# Start a REPL
clj -M:repl

# Render Clojure to superficie
echo '(defn f [x] (+ x 1))' | clj -M -m superficie.main render

# Render a file
clj -M -m superficie.main render src/myapp/core.clj

# Parse superficie back to Clojure
clj -M -m superficie.main parse core.sup
```

### Babashka REPL

With [Babashka](https://babashka.org/) installed, no JVM startup time:

```bash
bb sup
```

This starts a full superficie REPL backed by Babashka's built-in SCI evaluator. All of Clojure's core functions are available, and defs persist across REPL entries.

```
superficie REPL — Ctrl-D to exit
user=> defn square [x]:
   ..   x * x
   .. end
user=> square(7)
49
```

### Library (JVM)

```clojure
{:deps {org.replikativ/superficie {:mvn/version "RELEASE"}}}
```

```clojure
(require '[superficie.core :as sup])

;; Clojure source string → superficie string
(sup/clj->sup "(defn f [x] (+ x 1))")
;; => "defn f [x]:\n  x + 1\nend"

;; Superficie string → Clojure source string
(sup/sup->clj "defn f [x]:\n  x + 1\nend")
;; => "(defn f [x] (+ x 1))"

;; Work with forms directly
(sup/sup->forms "def x: 42\nprintln(x)")
;; => [(def x 42) (println x)]

(sup/forms->sup ['(def x 42) '(println x)])
;; => "def x: 42\n\nprintln(x)"

;; Pretty-print with width-aware layout
(sup/pprint-sup (sup/clj->forms my-source) {:width 80})
```

### JavaScript / npm

```bash
npm install superficie
```

```javascript
const { renderString, parseString, toSup, toClj, supToForms } = require('superficie');

// Clojure → Superficie
renderString('(defn f [x] (+ x 1))');
// => 'defn f [x]:\n  x + 1\nend'

// Superficie → Clojure
parseString('defn f [x]:\n  x + 1\nend');
// => '(defn f [x] (+ x 1))'
```

Works with static site generators (Astro, Next.js, etc.) to automatically render Clojure code blocks as superficie at build time. See [datahike.io](https://github.com/replikativ/datahike.io) for a working example with a remark plugin.

### Browser Bundle

A browser bundle is provided at `dist/browser/superficie.js`. It exposes a global `superficie` object:

```html
<script src="https://unpkg.com/superficie/dist/browser/superficie.js"></script>
<script>
  // Clojure → Superficie (for display/documentation)
  console.log(superficie.renderString('(defn f [x] (+ x 1))'));
  // => 'defn f [x]:\n  x + 1\nend'

  // Superficie → Clojure
  console.log(superficie.parseString('defn f [x]:\n  x + 1\nend'));
</script>
```

### Browser SCI REPL

A separate REPL bundle at `dist/browser-repl/superficie-repl.js` includes a full [SCI](https://github.com/babashka/sci) evaluator. It exposes `superficieRepl`:

```html
<script src="dist/browser-repl/superficie-repl.js"></script>
<script>
  // Evaluate superficie source — returns {result, output, error}
  var r = superficieRepl.evalSup('def x: 42\nprintln(x)');
  console.log(r.output);  // "42"
  console.log(r.result);  // "nil"

  // Clear all definitions
  superficieRepl.reset();
</script>
```

The [playground](https://replikativ.github.io/superficie/examples/playground.html) includes a live REPL panel using this bundle.

### Syntax Highlighting

#### highlight.js (web)

A highlight.js plugin is provided at `dist/superficie.hljs.js`:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
<script src="superficie.hljs.js"></script>
<script>hljs.highlightAll();</script>

<pre><code class="language-superficie">
defn greet [name]:
  str("Hello" name "!")
end
</code></pre>
```

#### VS Code / TextMate

A TextMate grammar is provided in [`editors/vscode/`](editors/vscode/):

```bash
ln -s /path/to/superficie/editors/vscode ~/.vscode/extensions/superficie
```

## Status

Superficie roundtrips **596 / 699 files (85%)** across 14 real-world Clojure projects including [core.async](https://github.com/clojure/core.async), [Datahike](https://github.com/replikativ/datahike), [Onyx](https://github.com/onyx-platform/onyx), [Clara Rules](https://github.com/oracle-samples/clara-rules), [Malli](https://github.com/metosin/malli), and [others](#tested-projects).

Printing Clojure as superficie always succeeds. The ~15% that don't fully roundtrip fall into a few categories, all of which are design constraints rather than bugs:

- **Auto-resolved keywords** (`::alias/key`, `#::alias{…}`) — these require namespace context at read time
- **Block keyword names used as variables** — `match`, `when`, `let`, etc. are reserved as block syntax in superficie; Clojure code that uses them as plain variable names can't round-trip
- **Operators in unusual positions** — operator symbols in `:exclude` lists, pattern literals, etc.

Code written *in* superficie — which naturally avoids these patterns — roundtrips cleanly.

<details>
<summary id="tested-projects">Full test results</summary>

| Project | Files | Pass | Notes |
|---------|-------|------|-------|
| Proximum | 23 | 23 | |
| Datahike | 69 | 65 | |
| Stratum | 28 | 26 | |
| core.async | 45 | 41 | `::alias/key` |
| Malli | 32 | 12 | `::alias/key` |
| Datascript | 11 | 10 | |
| Clara Rules | 74 | 65 | `::alias/key` |
| SCI | 43 | 24 | `::alias/key` |
| Konserve | 16 | 15 | |
| rewrite-clj | 52 | 49 | |
| Babashka | 65 | 55 | |
| Electric | 52 | 35 | |
| Onyx | 134 | 133 | |
| Datalevin | 55 | 43 | |

</details>

## License

Copyright 2026 Christian Weilbach.

Apache Licence 2.0.
