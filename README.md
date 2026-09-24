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
      recur(dec(i), acc * i)
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
  :post => create(path, body)
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
config .> assoc(:port, 8080) .> merge(defaults)
```

### Anonymous Functions

```
;; a one-line fn passed as an argument is an arrow
map(x -> x * x, xs)
reduce((acc, x) -> acc + x, 0, xs)

;; #() keeps its positional parameters
map(#(inc(%)), xs)

;; anything else is a fn block
def square: fn [x]: x * x end
```

The arrow is printed only for a call argument whose parameters are plain names and whose body fits on one line. It needs spaces on both sides: `->(x, f())` is still thread-first.

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
        `~first(form)(~x, ~@next(form))
      else:
        list(form, x)
      end]:
        recur(threaded next(forms))
      end
    else:
      x
    end
  end
end
```

User-defined macros are called with function syntax (`unless(pred, body)`). Block syntax (`unless pred: body end`) is reserved for macros registered in the block registry — either built-in forms or library macros that declare a [shape](#shapes-for-library-macros) (or, for the built-in block kinds, a `:superficie/role`).

### Function Call Fallback

The renderer never fails. Any Clojure form that doesn't match a known block pattern or operator is rendered using comma-separated function call syntax — `f(a, b, c)` — which is always valid superficie and always round-trips cleanly:

```clojure
;; Clojure
(defmacro my-macro [x]
  (list 'if x :yes :no))
```

```
;; Superficie — list call renders as a regular function call
defmacro my-macro [x]:
  list('if, x, :yes, :no)
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
  `ShrubError`; the surrounding code is still parsed correctly. Reading
  (`sup->forms`) reports the first structural error at its location;
  `pipeline/run-resilient` returns the healed forms together with the errors.
- The **reader** handles semantics: block keywords, operator precedence,
  and namespace resolution. Semantic errors throw `ex-info` with
  structured `:line`, `:col`, `:source-context`, and `:hint` data.

This is the same separation Rust's compiler and Racket's Rhombus use: construct the bracket/token tree first (where recovery is mechanical), then parse semantics against a structurally valid input.

Error messages are formatted with source context and underlines. Block errors carry a hint from indentation or from a header whose `:` is missing, and evaluation adds hints for common slips such as `W-1` (read as one name; write `W - 1`), `True`, `None`, `elif` and `return`:

```
Error: Unexpected 'end' — no block is open here (line 3, col 1)
3 | end
  | ^
1 | defn f [x]
  | ^ header without ':'
Hint: `defn` at line 1 was read as a plain symbol, not a block: its header needs ':' at the end of the line
```

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

### Shapes for library macros

A **shape** tells superficie how a library macro's arguments split into a block header and a body. Superficie ships shapes for raster and ansatz:

```
;; (deftm norm [x :- Double, y :- Double] :- Double (sqrt (+ (* x x) (* y y))))
deftm norm [x :- Double y :- Double] :- Double:
  sqrt(x * x + y * y)
end

;; (a/theorem add-zero [n :- Nat] (= Nat (+ n 0) n) (simp Nat.add_zero))
a/theorem add-zero [n :- Nat] =(Nat, n + 0, n):
  simp(Nat.add_zero)
end
```

A shape is a vector of slots, keyed by the macro's fully-qualified symbol:

```clojure
'{raster.core/deftm [:name :doc? [:wrap? All 1] :params [:kw? :-] :body]
  ansatz.core/theorem [:name :params :form :body]}
```

| Slot | Meaning |
|------|---------|
| `:name`, `:form` | one required form |
| `:params` | one required vector |
| `:doc?`, `:attr?` | an optional string / map |
| `:form?` | an optional form, taken only when a body form remains |
| `[:kw? K]` | an optional `K form` pair, e.g. `:- Ret` |
| `[:wrap? S n]` | lift a trailing `(S a1..an …)` into the header, e.g. raster's `(All [T] …)` |
| `:body` | the remaining forms (last slot) |

A shape can also carry options, which apply only where the form is written as a block (the reader and printer both know that from the head):

| Option | Effect inside the block | Set for |
|--------|-------------------------|---------|
| `:dotted-calls true` | `A.b(x)` is the plain call `(A.b x)`, not the Java method call `(.b A x)`, so Lean-style names read naturally: `Nat.succ(n)`, `RBTree.node(Nat, …)`. A Java call prints in the explicit form `.toUpperCase(s)`. | ansatz's `a/defn`, `a/theorem`, `a/inductive` |
| `:match-arms true` | a `match` with `[pattern body]` clauses prints as `\| pattern => body` arms | the same ansatz forms |
| `:index {:get f :set g}` | in the body, `(f x i j)` prints as `x[i, j]`, and `x[i]` (no space before `[`) reads as `(f x i)`; `(g x i v)` prints as `x[i] <- v`. A bare symbol `f` is `{:get f}`. | raster's `deftm`, `ftm`, `par/map-void!` with `aget` and `aset` |

```
deftm laplacian [U :- Array(double) i :- Long W :- Long] :- Double:
  U[i - 1] + U[i + 1] + U[i - W] + U[i + W] - 4.0 * U[i]
end

deftm scale! [U :- Array(double) n :- Long k :- Double] :- Void:
  par/map-void! i n:
    U[i] <- k * U[i]
  end
end

a/defn len [xs :- List(Nat)] Nat:
  match xs:
    | nil => 0
    | cons(h, t) => 1 + len(t)
  end
end
```

`:index` names functions; it does not fix what indexing means. raster's `aget` and `aset` dispatch on the array type like Julia's `getindex`/`setindex!`, so `U[i]` and `U[i] <- v` work for every element type raster knows. A store is written `<-` (as OCaml writes `a.(i) <- v`) because `=` is equality and `:=` binds a name. It goes bare as a body statement or call argument and in parentheses elsewhere: `1 + (U[0] <- 1)`.

Register a shape, with its options, from any of these sources (the first that has a shape wins, and its options come with it):

- `(superficie.shapes/register-shape! qsym shape opts)`
- `:superficie/shape` and `:superficie/shape-options` metadata on the macro var
- a `superficie/shapes.edn` resource a library ships on its classpath (JVM), mapping each symbol to a shape or to `{:shape [...] :options {...}}`:
  ```clojure
  {my.lib/defkernel {:shape [:name :params :body] :options {:index at}}}
  ```
- from JavaScript, `registerShape("my.lib/defkernel", "[:name :params :body]", "{:index at}")`

Shapes are safe by construction. The reader needs no shape to parse a block: the header is everything between the head and `:`, the body everything up to `end`. The printer uses a shape only after checking that reading the block back gives the original form, and otherwise falls back to call syntax. Heads resolve through the file's `ns` form (`a/defn` with `[ansatz.core :as a]`, a referred `deftm`) the same way in the reader and the printer, and a head is always written back exactly as it appeared. A block header must stay on one line, except inside brackets, so a header whose `:` is missing never borrows the `:` of a later block.

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
unless((= 1 2), println("1 != 2"))
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

// A snippet without its ns form: name the requires it assumes,
// so library macros render as blocks
toSup('(deftm sq [x :- Double] :- Double (* x x))',
      {context: "(require '[raster.core :refer [deftm]])"});
// => 'deftm sq [x :- Double] :- Double:\n  x * x\nend'
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
  str("Hello", name, "!")
end
</code></pre>
```

#### VS Code / TextMate

A TextMate grammar is provided in [`editors/vscode/`](editors/vscode/):

```bash
ln -s /path/to/superficie/editors/vscode ~/.vscode/extensions/superficie
```

## Status

Superficie roundtrips **643 / 743 files (87%)** across 13 real-world Clojure projects including [core.async](https://github.com/clojure/core.async), [Datahike](https://github.com/replikativ/datahike), [Onyx](https://github.com/onyx-platform/onyx), [Clara Rules](https://github.com/oracle-samples/clara-rules), [Malli](https://github.com/metosin/malli), and [others](#tested-projects), and **408 / 435 files (94%)** of [raster](https://github.com/replikativ/raster) and [ansatz](https://github.com/replikativ/ansatz) (sources and examples). The check is exact: operator symbols must come back unchanged, so `clojure.core/*` and a referred `raster.numeric/*` stay distinct.

Printing Clojure as superficie always succeeds. What does not roundtrip falls into a few categories:

- **Auto-resolved keywords** (`::alias/key`, `#::alias{…}`) — these require namespace context at read time
- **Nested arithmetic of one operator** — `(* (* a b) c)` prints as `a * b * c`, which reads back as `(* a b c)`: the same value, a different form
- **Block keyword names used as variables** in some positions — e.g. a local named `match` or `when` directly before a block

Code written *in* superficie — which naturally avoids these patterns — roundtrips cleanly.

<details>
<summary id="tested-projects">Full test results</summary>

| Project | Files | Pass | Notes |
|---------|-------|------|-------|
| Proximum | 26 | 26 | |
| Datahike | 83 | 77 | |
| Stratum | 45 | 40 | |
| core.async | 37 | 35 | `::alias/key` |
| Malli | 32 | 15 | `::alias/key` |
| Datascript | 18 | 12 | |
| Clara Rules | 74 | 65 | `::alias/key` |
| SCI | 47 | 25 | `::alias/key` |
| Konserve | 23 | 21 | |
| rewrite-clj | 52 | 49 | |
| Babashka | 65 | 58 | |
| Onyx | 134 | 133 | |
| Datalevin | 107 | 87 | |
| raster | 352 | 328 | sources and examples |
| ansatz | 83 | 80 | sources and examples |

</details>

## License

Copyright 2026 Christian Weilbach.

Apache Licence 2.0.
