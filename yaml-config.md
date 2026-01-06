## YAML Config / Pack Format

The Rust optimizer loads its pattern definitions from a single YAML file (by default
`configs/packs.yaml`). The file describes:

- `regexes`: named regular expressions (compiled once at startup)
- `conditions`: reusable predicates used by pattern steps
- `patterns`: named multi-step matchers
- `packs`: named lists of patterns to run

This document describes the supported schema and the “building blocks” you can
compose.

## Top-Level Structure

A config file is a mapping with these optional keys:

- `regexes`: `{ NAME: "regex" }`
- `conditions`: `{ name: <condition> }`
- `patterns`: `{ id: <pattern> }`
- `packs`: `{ pack_name: <pack> }`

Only the requested pack is loaded at runtime, but all referenced regexes,
conditions, and pattern IDs must exist.

Minimal example:

```yaml
regexes:
  NO_OP_LD: '^ld ([abcdehl]), \1$'

patterns:
  no_op_ld:
    name: No-op ld
    steps:
      - when: { regex: NO_OP_LD }

packs:
  core:
    patterns:
      - id: no_op_ld
```

## Packs

`packs` selects which patterns run.

```yaml
packs:
  rgbds:
    patterns:
      - id: py_no_op_ld
      - id: pointless_jumps
        enabled_by_default: false
```

Supported fields:

- `packs.<pack_name>.patterns`: list of patterns
- `packs.<pack_name>.patterns[].id` (required): key into the top-level `patterns` map
- `packs.<pack_name>.patterns[].enabled_by_default` (optional, default `true`)

Notes:

- Pattern *names* (the `patterns.<id>.name` field) must be unique within a pack.
  If two different IDs have the same `name`, YAML loading fails.
- Packs cannot be empty.

## Patterns

`patterns` defines the step-by-step match logic.

```yaml
patterns:
  pointless_jumps:
    name: Pointless jumps
    description: Detect unconditional jumps to the next label.
    steps:
      - when: { regex: POINTLESS_JUMPS_STEP1 }
      - when: { builtin: jump_target_label, jump_idx: 0 }
```

Supported fields:

- `patterns.<id>.name` (required): display name printed in output and used for suppression
- `patterns.<id>.description` (optional): shown in help / docs (if wired)
- `patterns.<id>.steps` (required): list of steps

Each step:

- `when` (required): a condition (see below)
- `rewind` (optional): integer; if the current step fails, the engine rewinds that
  many steps and retries.

### Matching Semantics (important)

A pattern matches **consecutive preprocessed lines**, one step per line.

- A step matches if its `when` condition matches the current line.
- On success, the engine advances to the next step and the current line becomes
  available as `prev.idx: 0` for the next step.
- On failure:
  - If `rewind` is present for the *current* step, the engine backs up by that
    many steps and re-evaluates.
  - Otherwise the pattern state resets and retries from the next line.

Suppression:

- A step is skipped if the line’s comment starts with
  `no-optimize <pattern-name>` (lowercased).
  - Example: `; no-optimize pointless jumps`

## Conditions

A `when:` condition is one of the following forms.

### Regex Conditions

- `{ regex: NAME }`: match `NAME` (from `regexes`) against the normalized `code`
  field.
- `{ text_regex: NAME }`: match `NAME` (from `regexes`) against the original
  `text` field.

The regex engine is `fancy_regex`, so features like look-ahead are supported.

### Direct Code/String Predicates

All of these operate on the normalized `code` field:

- `{ code_eq: "..." }`
- `{ code_ne: "..." }`
- `{ code_starts_with: "..." }`
- `{ code_ends_with: "..." }`
- `{ code_contains: "..." }`

### Named Conditions (reuse)

- `{ cond: name }`: reference `conditions.name`.

Named conditions are compiled with cycle detection.

### Boolean Combinators

- `any:` list of conditions (true if any child is true)
- `all:` list of conditions (true if all children are true)
- `not:` single condition

Example:

```yaml
conditions:
  not_control_flow:
    not: { regex: CONTROL_FLOW_PREFIX }

  label_definition_line:
    all:
      - { text_regex: LABEL_DEFINITION_TEXT_START }
      - not: { code_contains: ' ' }

patterns:
  example:
    name: Example
    steps:
      - when:
          all:
            - { cond: not_control_flow }
            - { cond: label_definition_line }
```

### Instruction Conditions

You can match on parsed mnemonics and operands:

```yaml
- when:
    instruction:
      mnemonic: ld
      operands:
        - { eq: a }
        - any:
            - { canon_eq: '[hli]' }
            - { canon_eq: '[hld]' }
```

Supported fields:

- `instruction.mnemonic` (optional): compared case-insensitively
- `instruction.operands` (optional): list of operand matchers
  - If provided, the operand count must match exactly.

Operand matchers:

- `{ eq: "..." }`: case-insensitive string match
- `{ canon_eq: "..." }`: compares after RGBDS canonicalization (e.g. `[hl+]` → `[hli]`)
- `{ is_zero_literal: true }`: matches numeric zero in common RGBDS spellings
- `{ any: [ ... ] }`: any-of for operand matchers

### Relative-to-Previous-Step Conditions

- `{ inc_dec_same_target_as_prev_ld: N }`:
  matches an `inc`/`dec` whose target equals (case-insensitively) the destination
  operand of the `ld` matched `N` steps earlier.

This is intentionally narrow and exists for parity with `optimize.py`.

### Builtins (legacy / internal)

The loader supports:

- `{ builtin: name }`
- `{ builtin: { name: name, jump_idx: 0 } }`

These are Rust-implemented predicates used for parity with `optimize.py`.
New patterns should generally prefer expressing logic via `conditions` +
`str_eq` + `instruction` unless you truly need a builtin.

## `str_eq`: String Expressions

`str_eq` compares two derived strings:

```yaml
- when:
    str_eq:
      left:  { current: code, after_prefix: 'ld ', before_comma: true, trim: true }
      right: { prev: { idx: 0, field: code }, after_prefix: 'add a, ', byte_at: 0 }
```

Form:

```yaml
str_eq:
  left:  <string-expr>
  right: <string-expr>
```

If either side cannot be evaluated (for example, `prev.idx` is out of range), the
condition is treated as **not matched**.

### String Expression Base

A string expression must specify **exactly one** base:

- `current: <field>`
- `prev: { idx: <n>, field: <field> }`
- `const: <string>`

Supported fields:

- `code`: normalized code for matching
- `text`: original line text (including indentation)
- `context`: current label/context
- `comment`: comment text (trimmed)
- `comment_lower`: lowercase comment text

### String Expression Transforms

After selecting a base string, transforms run in this order (when present):

1. `strip_prefix: "..."` — keep original if prefix not present
2. `after_prefix: "..."` — empty string if prefix not present
3. `before_comma: true` — left side of first comma (trimmed)
4. `after_comma: true` — right side of first comma (trimmed)
5. `after_comma_raw: true` — right side of first comma (not trimmed)
6. `pair_reg: true` — register-pair mapping (`h↔l`, `b↔c`, etc); non-matches become `""`
7. `byte_at: <n>` — ASCII byte at index `n` (out of range becomes `""`)
8. `last_char: true` — last ASCII byte (empty becomes `""`)
9. `last_token: true` — last whitespace-separated token
10. `strip_trailing_colon: true` — removes trailing `:`
11. `symbol_like: true` — takes a leading “symbol-like” run (`[A-Za-z0-9_.%$&]+`)
12. `trim: true` — trims surrounding whitespace

Notes:

- `byte_at` and `last_char` operate on bytes; stick to ASCII when using them.
- Transform order matters. If you need a specific extraction order, build the
  expression explicitly with `after_prefix` / `before_comma` / `trim` rather than
  assuming transforms will commute.

## Preprocessing Model (what conditions see)

The engine preprocesses each input line into multiple views:

- `text`: original line (no trailing newline)
- `code`: left-trimmed and whitespace-normalized code (comment removed)
- `comment`: comment text after `;`, trimmed
- `comment_lower`: lowercase comment
- `context`: label-like context

`regex` and `code_*` predicates operate on `code`.
`text_regex` operates on `text`.

## Practical Tips

- Prefer referencing `regexes` by name instead of repeating regex strings.
- Keep patterns conservative: extra matches can change output ordering and break
  parity with `optimize.py`.
- If a condition is reused in multiple patterns/steps, define it once under
  `conditions` and reference it via `{ cond: name }`.
