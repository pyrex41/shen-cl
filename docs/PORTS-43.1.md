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
Run the Bifrost matrix and Yggdrasil target gates against exact produced
launchers. Unsupported/blocked cases remain explicit.

## Port roster

| Port | Fork default / candidate | Verified status |
|---|---|---|
| shen-c | User-owned | Excluded from agent writes |
| shen-cl | master `d9494df` | Published; canonical 154/154, full native suite; CI success |
| shen-go | master `acba88a` | Published; canonical 154/154, KL certification and port checks |
| shen-erl | master `dbe7bdf` | Published; canonical 154/154, Common Test 47/47 (Docker) |
| shen-rust | main `d893c8e` | Published; canonical 154/154, native/AOT checks |
| shen-lua | main `176e103` | Published; canonical 154/154, 1,210 port assertions |
| shen-scheme | master `4dc9cc2` | Published; canonical 154/154, native build CI passes; Pages deploy permission fails |
| shen-joy | main `fd73c2f` | Published; bounded subset, five Nix checks pass against 43.1 reference |
| shen-swift | main `5620446` | Published; canonical 154/154, focused native CLI checks; XCTest unavailable |
| shen-julia | candidate `0bc8aa6` | Canonical 154/154; broader compatibility failures under investigation |
| shen-truffle | master `ac8d2d8` | Published; canonical 154/154, Maven 20/20; native-image untested |
| ShenScript | master `f21f30e` | Published; canonical 154/154, backend 468, async reader regression |
| shen-llvm | candidate `303e7d0` | LLVM integration 18/18; full conformance and native shake repairs underway |
| shen-koka | candidate `d035880` | Canonical 154/154; stream contract and provenance review underway |
| shen-odin | main `2d84598` | Published fixture; 24 KL files byte-identical, no evaluator: runtime UNKNOWN |
| shen-ocaml | candidate `2d80373` | Complete source upgrade; native build/test underway |
| shen-inets | candidate `3659531` | Experimental compiler: 1,116/1,116 defuns compile; runtime unqualified |
| shen-forth | Isolated repair underway | Experimental scaffold; 43.1 boot repair in progress |

Bifrost: all maintained adapters now target 43.1 in integration commit `95da9f3`.
The eight-port matrix (CL, Go, Rust, Lua, Scheme, JS, Swift, Truffle) executes
72 cases with no failures, with two known integer-precision divergences and
two unmatched historical 42.2 version rows skipped. The CL/Erlang matrix
executes all 72 with no failures or divergences. The stale 42.2 launcher control fails as
expected. Installation refs and documentation are being updated before publish.

Yggdrasil: shared kernel upgraded wholesale to 43.1; Lisp with Lua, Go and JS each passes all
ten fixtures, including standalone structural dictionaries and stdin readers.
The shared-kernel oracle/footprint/init-order checks pass. A broad hosted test
run exceeded an overly short 280-second budget after 40 top-level tests passed;
the remaining tests are running with the suite's documented longer budget.
The CI host pin is updated to the reviewed Go 43.1 commit. Lisp with Scheme,
Swift and Truffle each passes four representative fixtures including dictionaries
and stdin reads. Rust fork CI failures are under investigation; other CI states
are tracked separately from the local suite results.

Statuses are local evidence unless publication or CI is explicitly stated.
Canonical runtime passes do not imply Bifrost agreement or Yggdrasil support.
