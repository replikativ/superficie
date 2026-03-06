# Superficie

[![Tests](https://github.com/replikativ/superficie/actions/workflows/test.yml/badge.svg)](https://github.com/replikativ/superficie/actions/workflows/test.yml)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/superficie.svg)](https://clojars.org/org.replikativ/superficie)
[![License](https://img.shields.io/badge/license-EPL%202.0-blue.svg)](https://www.eclipse.org/legal/epl-2.0/)

**Surface syntax for Clojure** — a bidirectional renderer that translates Clojure S-expressions into familiar, readable syntax.

## Why?

During my PhD in machine learning, I worked in Clojure while everyone around me used Python. I couldn't show my code to colleagues, supervisors, or domain experts without first explaining parentheses. In presentations, papers, and code reviews, the syntax was a wall — not because S-expressions are bad, but because you can't expect someone to parse them on the fly when they've never seen them before.

That initial unfamiliarity typically takes a few days to overcome. But a few days is infinity when you're in a meeting, reading a blog post, or reviewing code with someone outside your team.

Superficie exists to remove that barrier. You write Clojure as normal. When you need to *show* it to someone, you render it to a syntax they can already read.

## Quick Look

<table>
<tr>
<th>Clojure</th>
<th>Superficie</th>
</tr>
<tr>
<td>

```clojure
(defn greet [name]
  (str "Hello, " name "!"))
```

</td>
<td>

```
defn greet(name):
  str("Hello, ", name, "!")
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
defn quadratic(a, b, c, x):
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
defn process-users(users):
  users
    |> filter(:active)
    |> map(:name)
    |> sort()
    |> take(10)
end
```

</td>
</tr>
</table>

See [`examples/side-by-side.html`](examples/side-by-side.html) for a full interactive comparison with syntax highlighting.

## What It Is (and Isn't)

**Superficie is a communication tool.** It renders Clojure into syntax that Python/Julia/TypeScript developers can read immediately — for presentations, documentation, blog posts, and conversations with domain experts.

**It is not a new language.** There is no superficie runtime, no separate ecosystem, no reason to stop writing Clojure. The renderer is automatic and bidirectional: `.clj` to `.sup` and back, with perfect roundtripping.

Superficie can also serve as a gentle on-ramp for contexts where people encounter Clojure for the first time — embedded scripting, configuration, plugins. Shown side-by-side with its Clojure equivalent, it helps newcomers build fluency with S-expressions naturally rather than being blocked by them.

## How It Works

Every Clojure form has a superficie rendering:

- `(f x y)` becomes `f(x, y)` — function calls
- `(+ a (* b c))` becomes `a + b * c` — infix with standard precedence
- `(->> x (map f) (filter g))` becomes `x |> map(f) |> filter(g)` — pipes
- `(if test body else)` becomes `if test: body else: ... end` — blocks
- `(let [x 1] ...)` becomes `let x := 1: ... end` — bindings

Forms that can't be expressed in surface syntax (syntax-quoted macro bodies, uncommon reader macros) fall through to **S-expression passthrough** — raw Clojure in parentheses is always valid superficie. This means the renderer never fails: it just falls back gracefully.

## Design

- **Bidirectional**: `.clj` → `.sup` → `.clj` roundtrips cleanly
- **Whitespace-sensitive operators**: `a - b` is subtraction, `my-func` is a symbol
- **Familiar syntax**: `if`/`else`/`end` blocks, `for x in xs`, infix math
- **Full Clojure**: destructuring, metadata, macros, Java interop — nothing is lost
- **S-expression escape hatch**: any `(raw s-expr)` passes through for what surface syntax doesn't cover

See [`doc/grammar.md`](doc/grammar.md) for the full grammar reference.

## Syntax at a Glance

### Definitions and Functions

```
def pi := 3.14159

defn factorial(n):
  loop i := n, acc := 1:
    if i <= 1:
      acc
    else:
      recur(dec(i), acc * i)
    end
  end
end

;; Multi-arity
defn greet
  ():
    greet("World")
  (name):
    str("Hello, ", name, "!")
end
```

### Control Flow

```
if x > 0:
  :positive
else:
  :non-positive
end

cond:
  x < 0 => :negative
  x = 0 => :zero
  :else => :positive
end

case method:
  :get => fetch(path)
  :post => create(path, body)
  else => not-found()
end
```

### Bindings and Loops

```
let x := 1, y := 2:
  x + y
end

for x in xs, y in ys, when x not= y:
  [x, y]
end

loop lo := 0, hi := n:
  if hi - lo <= 1:
    lo
  else:
    let mid := (lo + hi) / 2:
      if aget(arr, mid) <= target:
        recur(mid, hi)
      else:
        recur(lo, mid)
      end
    end
  end
end
```

### Threading (Pipes)

```
;; ->> becomes |>
users
  |> filter(:active)
  |> map(:name)
  |> sort()

;; -> becomes .>
config
  .> assoc(:port, 8080)
  .> merge(defaults)
```

### Java Interop

```
obj.method(arg1, arg2)
obj.-field
new HashMap(16)
Integer/parseInt("42")
```

### Namespaces and Macros

```
ns myapp.core
  (:require [clojure.string :as str]
            [myapp.db :refer [query insert!]])
  (:import [java.time Instant Duration])
end

defmacro unless(pred, & body):
  `(if (not ~pred)
     (do ~@body))
end
```

## Usage

### CLI

```bash
# Render Clojure to superficie
echo '(defn f [x] (+ x 1))' | clj -M -m superficie.main render

# Render a file
clj -M -m superficie.main render src/myapp/core.clj

# Output to file
clj -M -m superficie.main render src/myapp/core.clj -o core.sup

# Parse superficie back to Clojure
clj -M -m superficie.main parse core.sup
```

### Library

```clojure
(require '[superficie.render :as render])
(require '[superficie.parse :as parse])

;; Clojure -> Superficie
(render/render-string (slurp "src/myapp/core.clj"))

;; Superficie -> Clojure
(parse/parse-string (slurp "src/myapp/core.sup"))
```

### Syntax Highlighting

A highlight.js language plugin is provided at [`dist/superficie.hljs.js`](dist/superficie.hljs.js):

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
<script src="superficie.hljs.js"></script>
```

## Status

Superficie roundtrips 523/533 files (98%) across 17 real-world Clojure projects including [core.async](https://github.com/clojure/core.async), [Datahike](https://github.com/replikativ/datahike), [Onyx](https://github.com/onyx-platform/onyx), [Clara Rules](https://github.com/oracle-samples/clara-rules), [Malli](https://github.com/metosin/malli), and [others](#tested-projects). The remaining failures are edge cases in macro internals (syntax-quote expansion, auto-resolved keywords), not surface-level code.

<details>
<summary id="tested-projects">Full test results</summary>

| Project | Files | Pass |
|---------|-------|------|
| Proximum | 22 | 22 |
| Datahike | 17 | 17 |
| Stratum | 27 | 27 |
| Methodic | 138 | 138 |
| core.async | 27 | 27 |
| Malli | 3 | 3 |
| Datascript | 2 | 2 |
| Clara Rules | 20 | 20 |
| core.logic | 18 | 13 |
| superv.async | 1 | 1 |
| Konserve | 2 | 2 |
| rewrite-clj | 5 | 5 |
| SCI | 2 | 2 |
| Babashka | 65 | 64 |
| Electric | 18 | 15 |
| Onyx | 116 | 116 |
| Datalevin | 50 | 49 |

</details>

## License

Copyright 2026 Christian Weilbach.

Eclipse Public License 2.0.
