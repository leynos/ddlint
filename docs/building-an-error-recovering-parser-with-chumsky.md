## DON’T PANIC: A Hitchhiker’s Guide to Building an Error-Recovering Parser with **Chumsky**

> *A completely remarkable book. Probably the most remarkable, certainly the most successful book ever to come out of the great publishing corporations of Ursa Minor.*

### 1 Know Where Your Towel (and Grammar) Is

Before you even think about summoning Chumsky’s combinators, write your grammar down — preferably in EBNF, biro on a napkin, or etched into the side of a Vogon constructor fleet. Chumsky mirrors whatever you hand it; change the napkin later and you’ll be spelunking inside recursive lambdas at 2 a.m.

**Checklist**

- A complete token list
- Precedence rules in plain English (“multiply before add, semicolons end statements, tea before Arthur, etc.”)
- Examples of legal *and* illegal programmes

### 2 Feed It Tokens, Not Breadcrumbs

Chumsky is much happier when it’s nibbling on a neat `Vec<TokenSpan>` than on raw characters. Use the `logos` crate (or your favourite lexical life-form) to slice the source first. You’ll get:

- Cleaner error messages (“unexpected `KwIf`”)
- Simple span maths: every token already knows its start & end byte
- The freedom to invent helpful token kinds (e.g. “Indent”, “Dedent”, “Unified Field Theory Symbol”)

### 3 Dealing with Left-Recursion, Infinite Loops and Other Things That Ate Betelgeuse

Left-recursive rules make top-down parsers seize up like Marvin’s shoulder joints. Rewrite them with repetition combinators (`many()`, `foldl()`), or use a precedence-climbing expression parser.

Forward references? Wrap them in `recursive(|expr| { … })` so Chumsky can see round corners.

Ambiguity? Break overlapping prefixes into separate branches *first* and only then hand the survivors to `choice()`.

### 4 Panic? No. Recovery? Yes.

Error recovery is what turns your parser from Vogon poetry into a Babel fish.

1. **Anchors:** Tell Chumsky that `;`, `}`, `]` and other setters of cosmic balance are “hard delimiters”. Use `recover_with(skip_until([]))`.
2. **Labels:** Tag sub-parsers with `.labelled("expression")` so the diagnostics mention something friendlier than “expected `Unknown(42)`”.
3. **Tri-state nodes:** Return `Option<AstNode>`; missing bits propagate but the parse soldier on.

In practice you’ll compose the built-ins (`NestedDelimiters`, `SkipUntil`) with a couple of bespoke closures and quickly look like you own the place.

### 5 Getting Codex to Behave (or: How to Babysit a 2-Metre Tall Neural Net)

Codex is a marvellous companion so long as you:

- **Constrain its universe.** Include the token enums, AST structs, and the precise combinators in the prompt.
- **Ask for one production at a time.** Whole-grammar requests invite hallucinations of alternate dimensions.
- **Round-trip ruthlessly.** Generate random AST → pretty-print → re-parse → assert equality. Failures mean Codex (or you) has mis-remembered the Restaurant at the End of the File.

### 6 Linting: The First Sip of the Differential Logic Engine

Treat the linter as the pre-solver phase of your differential logic engine:

1. Build symbol tables and scope graphs.
2. Run the cheap local checks (duplicates, arity, type holes).
3. Emit a constraint set and immediately feed it to the solver; conflicts become diagnostics.

Because differential logic supports incremental re-checking, you can deliver IDE feedback faster than a hyperspace bypass.

### 7 Keeping the Whole Show Flying

- **CI Pipeline:** `cargo insta test`, `cargo clippy --deny warnings`, and your round-trip parser tests on every push.
- **Editor integration:** Convert Chumsky’s `Rich` errors into LSP diagnostics; line/column already sorted.
- **Performance guardrails:** Benchmark on a late-game save. If a commit slows parsing or solving by &gt; 20 %, trigger the Heart of Gold and revert reality.

---

### TL;DR (because life is short and full of Thursdays)

Write the grammar first, lex separately, tame left-recursion, anchor recovery on hard delimiters, keep Codex on a tight leash, and let your linter double as the logic engine’s warm-up act.

And always keep your towel handy. It’s the most massively useful thing an interstellar parser hacker can carry.