# Shen 43.1 fork integration

## Scope and working agreement

Adopt upstream `4844072dde0208c3e4b99dcbfea58e4fdfd812e8` (port 3.2.0,
community kernel 43.1) on the pyrex41 fork. The previous fork tip, `d59bf13`,
is an ancestor: 34 commits behind, none unique. Work locally and on our forks.
Ask the user before any upstream comments, PRs, issues, replies, or other writes.

## Execution plan

1. Build in an isolated worktree, using the pinned Nix toolchain and a verified
   upstream release bootstrap. Fetch the pinned kernel, precompile, build,
   self-host, rebuild, and run `make test-all`.
2. Run the Bifrost corpus against this exact executable. Check optional SHA-256
   loading and distinguish unsupported cases from failures.
3. Run Yggdrasil host parity and Lisp target checks with this runtime. Update
   staging and initialization for the dictionary backend and the public 43.1
   character-stream primitives in an isolated Yggdrasil worktree.
4. Record exact commits, toolchain, commands and results here. Reconcile the
   remaining fork backlog without editing GitHub issues: #24's kernel upgrade
   is shipped upstream; its integration checks need fresh evidence. #20's native
   TLA visited-state map is a separate enhancement, with historical performance
   claims and a nonstandard old bootstrap that need revalidation before work.
5. Advance the local fork baseline once the build and integration evidence are
   established. Keep unrelated WIP and parallel Shen-C integration work intact.

## Evidence

Local macOS arm64 qualification passed on Nix SBCL 2.6.7. The verified
v3.1.0 source release was built as the bootstrap, then the 43.1 kernel and
compiler were precompiled, built, self-hosted and rebuilt. `make test-all`
passed: 154 kernel cases, compiler goldens, 333 runtime assertions, 20 CLI
checks, 123 extension checks, 47 accessor checks and 12 SHA-256 checks.

Bifrost's 43.1 adapter/version update passed 72 applicable cases, with two
42.2-specific cases explicitly skipped. A stale 42.2 launcher fails both new
43.1 checks. The Yggdrasil Lisp target passed its 10-fixture parity gate after
staging the new dictionary runtime. The user subsequently expanded scope to
Yggdrasil's shared 43.1 kernel and every pyrex41 port; see
[the portfolio](PORTS-43.1.md) for those separate gates.

[Receipts and sanitized logs](evidence/shen-cl-43.1/receipt.json) retain source,
bootstrap and executable identities. Upstream CI remains historical context.
No upstream interaction is authorized without checking with the user.

## Fork backlog assessment

- #24: the 43.1 runtime upgrade is shipped by upstream 3.2.0 and passes locally.
  Integration checks are being refreshed as part of the portfolio.
- #20: `shen-cl.eval-lisp` returns 3 for `(+ 1 2)` in the normal build. The
  documented `lisp.` and `lisp.sin` syntax reports an undefined-function error
  in both the 42.2 source-release bootstrap and 43.1 build. This is a pre-existing
  interop gap, separate from the kernel upgrade. A native TLA map remains a
  separate enhancement; the issue's old timings are not revalidated here.
- The published v3.1.0 macOS binary depends on
  `/opt/homebrew/opt/zstd/lib/libzstd.1.dylib` and cannot bootstrap this Nix-only
  installation directly. Its checksum-verified source release works.

