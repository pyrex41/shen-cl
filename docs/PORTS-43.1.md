# pyrex41 Shen 43.1 upgrade portfolio

## Scope and coordination

Upgrade every Shen language port under the pyrex41 GitHub account to 43.1.
The user explicitly authorized per-port agents on 2026-10-18. Shen-C is being
updated by the user and is excluded from agent changes. Libraries and tools
are classified separately; Bifrost and Yggdrasil are included integrations.

All work uses isolated worktrees, preserves existing WIP and remotes, and
retains the kernel/suite source identity. No GitHub comments, PRs, issues,
replies, tags or releases are part of this work. Upstream interaction requires
a fresh user check. Agents may push tested branches to pyrex41 forks; the
coordinator verifies the returned commit, diff and tests before advancing
fork default branches. Build, test, CI and publication status remain separate.

## Source and validation gates

Use community ShenOSKernel 43.1 (`shen-43.1` at
`715824a52b55e9c81a9501df8548ff3edd8411f5`), archive SHA-256
`900eedef44f440af78f9395ed06e689ce1604546fc309f5bc7c8bc84cc651920`.
Shen-C's user-owned source lineage is tracked separately. Adopt the complete
release, implement the public character-stream primitives and callable
metadata, build, run the canonical release suite, then focused port checks.
Do not infer qualification from a version string, an old suite or CI alone.
The official archive contains 23 KL files. Shen/CL adds its own compiler.kl
backend; an earlier audit incorrectly counted that as release file 24.
The corrected audit reads the checksum-verified archive directly.
Run the Bifrost matrix and Yggdrasil target gates against exact produced
launchers. Unsupported/blocked cases remain explicit.

## Port roster

All 17 agent-owned port repositories have published 43.1 upgrades. Twelve
ports pass the 154-assertion canonical suite. Joy, Odin, OCaml, Inets and
Forth retain the explicit runtime limits below; source identity alone does
not qualify those runtimes.

| Port | Published revision | Verified status |
|---|---|---|
| [shen-c](https://github.com/pyrex41/shen-c) | User-owned | Excluded from agent writes |
| [shen-cl](https://github.com/pyrex41/shen-cl) | master `caaf317` | Published; canonical 154/154, full native suite; CI success |
| [shen-go](https://github.com/pyrex41/shen-go) | master `54c68db` | Published; canonical 154/154, KL certification and port checks |
| [shen-erl](https://github.com/pyrex41/shen-erl) | master `dbe7bdf` | Published; canonical 154/154, Common Test 47/47 (Docker) |
| [shen-rust](https://github.com/pyrex41/shen-rust) | main `ec950a0` | Published; canonical 154/154, workspace 242 pass, Clippy/format pass; CI success |
| [shen-lua](https://github.com/pyrex41/shen-lua) | main `e3dec8b` | Published; canonical 154/154, 1,210 port assertions |
| [shen-scheme](https://github.com/pyrex41/shen-scheme) | master `4dc9cc2` | Published; canonical 154/154, native build CI passes; Pages deploy permission fails |
| [shen-joy](https://github.com/pyrex41/shen-joy) | main `fd73c2f` | Published; bounded subset, five Nix checks pass against 43.1 reference |
| [shen-swift](https://github.com/pyrex41/shen-swift) | main `c0efa5c` | Published; canonical 154/154, focused native CLI checks; XCTest unavailable |
| [shen-julia](https://github.com/pyrex41/shen-julia) | main `99c717b` | Published; fresh canonical 154/154, numeric dictionary fixes; combined runner has two baseline failures |
| [shen-truffle](https://github.com/pyrex41/shen-truffle) | master `29517d3` | Published; canonical 154/154, Maven 20/20; native-image untested |
| [ShenScript](https://github.com/pyrex41/ShenScript) | master `a4619e2` | Published; canonical 154/154, backend 468, async reader regression |
| [shen-llvm](https://github.com/pyrex41/shen-llvm) | master `031cdb8` | Published; canonical 154/154; RC and Boehm each 181/181; selfhost fixpoints and cross-checks pass; 39 required AOT reports pass; CI success |
| [shen-koka](https://github.com/pyrex41/shen-koka) | master `274432b` | Published; interpreter and native AOT each canonical 154/154; 387-step AOT build and seven runner checks pass |
| [shen-odin](https://github.com/pyrex41/shen-odin) | main `6660379` | Published fixture; 23 release KL files byte-identical, no evaluator: runtime UNKNOWN |
| [shen-ocaml](https://github.com/pyrex41/shen-ocaml) | main `786e5d8` | Published; native tests and AOT generation pass; canonical reader stalls on old and new baselines; legacy mixed-number equality gap |
| [shen-inets](https://github.com/pyrex41/shen-inets) | main `ff13498` | Published experimental compiler: 23 release files, 1,116/1,116 defuns compile; Python subset 21 pass; HVM unqualified |
| [shen-forth](https://github.com/pyrex41/shen-forth) | main `8fa11ff` | Published experimental interpreter; 112 focused assertions and launcher pass; canonical UNKNOWN at 1536 MiB RSS limit |

[Bifrost](https://github.com/pyrex41/bifrost): all maintained adapters now target 43.1 in implementation `95da9f3` and published head `c978111`.
The eight-port matrix (CL, Go, Rust, Lua, Scheme, JS, Swift, Truffle) executes
72 cases with no failures, with two known integer-precision divergences and
two unmatched historical 42.2 version rows skipped. The CL/Erlang matrix
executes all 72 with no failures or divergences. The stale 42.2 launcher control fails as
expected. Published and installed locally. Julia now executes 72 cases: 71 pass,
one known integer-overflow divergence, no failures. A post-cleanup run with
CL, freshly rebuilt Swift and Truffle has the same 71/1/0 result. Both runs
skip two historical kernel rows; Julia, Swift and Truffle additionally skip
the namespace capability cell. See the [final matrix receipts](evidence/ports-43.1/post-cleanup-matrix.json).

[Yggdrasil](https://github.com/pyrex41/yggdrasil): published shared 43.1 kernel in `8705799`, final integration `0aa8947`; Lisp with Lua, Go
and JS each passes all ten fixtures. Lisp with Scheme, Swift, Truffle, Rust and Julia each
passes four fixtures including dictionaries and stdin. Julia uses its
artifact builder's lib mode, omitting the registry's sysimage flag; per-program
sysimage generation remains unqualified. The final revision's fresh Go 1.27
suite passes 129 top-level tests in 227.7 seconds, with three top-level skips
and one subtest skip recorded in the [receipt](evidence/ports-43.1/yggdrasil-final-go.json).
The two substantive environment skips subsequently pass with Node and the
Shen-Go source checkout supplied, giving 131 top-level passes across these
runs; the helper-process and unsupported KL stdin subtest skips remain.
The CI Go host pin is `acba88a`. The final metadata fixture agrees between
Lisp and Go, and the dynamic-load group passes all three subtests. The merge
preserves the dynamic-load work already merged on the fork in PR #35.
Real Souffle, the Python reference evaluator and Shen agree for all 26
analysis fixtures, including metadata introspection, in
[CI](https://github.com/pyrex41/yggdrasil/actions/runs/39170315014).
Final hosted Go CI passes 131 top-level tests in 592.7 seconds with only the
helper subprocess skipped. Native-C CI passes on Linux and macOS.
Erlang independently passes four
fixture goldens in Docker; Joy passes its bounded sum artifact.

The local Bifrost/Yggdrasil commands now point at the qualified native builds.
Their former Nix links and gcroots remain intact, with an installation receipt
under the local opt directory. No upstream comments, PRs, issues or replies
have been made. Scheme's source build CI passes; its separate Pages deployment
fails for lack of bot write permission, with a fix prepared only on the upgrade
branch. Rust's format/version CI regression is fixed; its implementation run passes.
Go's generated receipt is refreshed and CI passes; Rust, Lua, ShenScript,
Truffle and Forth also pass at their latest checked revisions.
Bifrost passes all four workflows, including Linux/macOS native C adapter
checks using Clang with LTO disabled. This checks adapter behavior and does
not qualify C performance. LLVM's final Linux LLVM 20 CI passes both
181-test RC sanitizer and Boehm suites, both stage-2 fixpoints, cross-checks,
tracked-source cleanliness and canonical 154/154. Final Linux CI and fresh
Darwin AOT qualification against final Yggdrasil both pass all 39 required
reports and 98/99 retained expressions;
the remaining report has a historical quoted-string compile limitation.
These retained AOT expressions are separate from the 154 canonical assertions.
See the [AOT receipt](evidence/ports-43.1/llvm-aot-43.1.json).
The complete Linux pipeline passes at
[`031cdb8`](https://github.com/pyrex41/shen-llvm/actions/runs/39170316619).
All final LLVM, Yggdrasil and Bifrost workflow results are recorded in the
[CI receipt](evidence/ports-43.1/final-ci.json). Live default refs and CI
state are recorded in the [publication snapshot](evidence/ports-43.1/publication-snapshot.json)
beside the [source audit](evidence/ports-43.1/source-audit.json).

LLVM also passes fresh native self-hosting on Darwin ARM64 with LLVM 20:
stage 0, stage 1 and stage 2 LLVM IR are byte-identical. This check uses the
plain-object path without LTO; it does not qualify the full Darwin runtime
suite or LTO. See the [selfhost receipt](evidence/ports-43.1/llvm-darwin-selfhost.json).

Statuses are local evidence unless publication or CI is explicitly stated.
Canonical runtime passes do not imply Bifrost agreement or Yggdrasil support.
