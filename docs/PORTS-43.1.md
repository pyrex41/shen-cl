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

| Port | Assignment | Status |
|---|---|---|
| shen-c | User | Excluded from agent writes |
| shen-cl | Coordinator | Upstream 3.2.0 integrated in isolated branch; full native suite PASS |
| shen-go | shen_go_431 | Active |
| shen-erl | shen_erl_431 | Active |
| shen-rust | shen_rust_431 | Active |
| shen-lua | shen_lua_431 | Active |
| shen-scheme | shen_scheme_431 | Active |
| shen-joy | shen_joy_431 | Active; bounded-VM scope must remain explicit |
| shen-swift | shen_swift_431 | Active |
| shen-julia | shen_julia_431 | Active |
| shen-truffle | shen_truffle_431 | Active |
| ShenScript | shenscript_431 | Active |
| shen-llvm | Next wave | Inventory pending |
| shen-koka | Next wave | Inventory pending |
| shen-odin | Next wave | Inventory pending |
| shen-ocaml | Next wave | Inventory pending |
| shen-inets | Next wave | Inventory pending |
| shen-forth | Next wave | Inventory pending |

Bifrost: local 43.1 adapter/version cases pass 72 applicable corpus checks;
two 42.2 cases are skipped; a 42.2 stale-launcher control fails as expected.
Yggdrasil: shared kernel upgraded wholesale to 43.1 in its isolated branch;
Lisp gate passes ten fixtures. Shared-kernel oracle and other-target checks
are underway. All statuses above are local evidence unless stated otherwise.
