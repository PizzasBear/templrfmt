# templrfmt

Formatter for the [templr](https://github.com/PizzasBear/templr) templates library.

## Install

```sh
cargo install templrfmt
```

## Usage

```
Usage: templrfmt [OPTIONS] [INPUT_PATTERNS]...

Arguments:
  [INPUT_PATTERNS]...  A space separated list of file, directory or glob

Options:
  -s, --stdin    Format stdin and write to stdout
  -r, --rustfmt  Format with rustfmt
  -q, --quiet
  -h, --help     Print help
  -V, --version  Print version
```

## Using with Rust Analyzer

You can set the `rust-analyzer.rustfmt.overrideCommand` setting.

```json
  "rust-analyzer.rustfmt.overrideCommand": ["templrfmt", "--stdin", "--rustfmt"]
```

## Pretty-printer algorithm

The pretty-printer is based on Philip Karlton’s Mesa pretty-printer, as described in the appendix to
[Derek C. Oppen, “Pretty Printing” (1979), Stanford Computer Science Department STAN-CS-79-770](http://i.stanford.edu/pub/cstr/reports/cs/tr/79/770/CS-TR-79-770.pdf).
This algorithm's implementation is taken from `prettyplease` which is adapted from `rustc_ast_pretty`.

The algorithm takes from an input stream of length `n` and an output device with margin width `m`,
the algorithm requires time `O(n)` and space `O(m)`.
The algorithm is described in terms of two parallel processes;
the first scans the input stream to determine the space required to print logical blocks of tokens;
the second uses this information to decide where to break lines of text;
the two processes communicate by means of a buffer of size `o(m)`.
The algorithm does not wait for the entire stream to be input,
but begins printing as soon as it has received a linefull of input.
