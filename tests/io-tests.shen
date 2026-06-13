\* Copyright (c) 2026 shen-cl port authors.                          *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

\\ Port-authored IO regression suite. Mirrors shen-go's kl/io_coverage_test.go:
\\ open (in/out), close, a write-byte -> close -> read-byte round trip,
\\ read-byte at EOF returning -1, read-file-as-string and read-file-as-bytelist.
\\
\\ Files are written under a repo-relative path so the run is self-contained;
\\ *home-directory* is "" in the built image, so the path passes through open
\\ unchanged. Loaded after tests/test-harness.shen by run-port-tests.shen.
\\
\\ *hush* / pr-to-file (Shen issue #2, FIXED): with *hush* = true,
\\ (pr STR FileStream) is now WRITTEN on every build -- *hush* must not silence
\\ pr to a file stream. (Previously the CLISP and ECL builds ran the kernel KL pr,
\\ which returns early under *hush* and so produced a zero-byte file; SBCL/CCL
\\ already bypassed it via a native override. The fix gives ECL the same override
\\ and gives CLISP a byte-stream override that likewise ignores *hush*.)
\\ (write-byte ... FileStream) was always written regardless of *hush*.
\\ The port test runner loads under *hush* = true, so the round trips below clear
\\ *hush* first only to keep them independent of the runner's quiet flag.

(set *hush* false)

\\ --- write-byte -> close -> read-byte round trip + EOF (-1) ---
\\ Mirrors shen-go's TestStreamRoundTrip: write "Hi" (72 105), read back the
\\ two bytes then -1 at EOF.
(define io-roundtrip
  Path -> (let Out (open Path out)
               _ (write-byte 72 Out)
               _ (write-byte 105 Out)
               _ (close Out)
               In (open Path in)
               B1 (read-byte In)
               B2 (read-byte In)
               Eof (read-byte In)
               _ (close In)
            (@p B1 (@p B2 Eof))))

(assert= "stream round trip + EOF"
         (@p 72 (@p 105 -1))
         (io-roundtrip "tests/io-roundtrip.tmp"))

\\ --- read-file-as-string + read-file-as-bytelist against a real file ---
\\ Mirrors shen-go's TestFileReadPrimitives ("AB" -> "AB" and (65 66)).
(define io-write-text
  Path Text -> (let Out (open Path out)
                    _ (pr Text Out)
                 (close Out)))

(assert= "read-file-as-string"
         "AB"
         (do (io-write-text "tests/io-readfile.tmp" "AB")
             (read-file-as-string "tests/io-readfile.tmp")))

(assert= "read-file-as-bytelist"
         [65 66]
         (read-file-as-bytelist "tests/io-readfile.tmp"))

\\ --- close on a fresh out-stream then reading it back is consistent ---
\\ (open out truncates; an immediately-closed empty stream reads EOF first).
(assert= "empty stream reads EOF"
         -1
         (let Out (open "tests/io-empty.tmp" out)
              _ (close Out)
              In (open "tests/io-empty.tmp" in)
              B (read-byte In)
              _ (close In)
           B))

\\ Regression for Shen issue #2: under *hush* = true, (pr STR FileStream) must
\\ still WRITE -- the file must read back the payload, not be empty -- on every
\\ build (SBCL/CLISP/ECL). This is the assertion the fix corrects.
(assert= "pr to a file stream ignores *hush* (issue #2)"
         "payload"
         (let _ (set *hush* true)
              Out (open "tests/io-hush-pr.tmp" out)
              _ (pr "payload" Out)
              _ (close Out)
              _ (set *hush* false)
           (read-file-as-string "tests/io-hush-pr.tmp")))

\\ write-byte was never silenced by *hush*; assert it still holds across impls.
(assert= "write-byte ignores *hush*"
         [88]
         (let _ (set *hush* true)
              Out (open "tests/io-hush.tmp" out)
              _ (write-byte 88 Out)
              _ (close Out)
              _ (set *hush* false)
           (read-file-as-bytelist "tests/io-hush.tmp")))

\\ Restore the quiet flag the runner relies on for the remaining suites.
(set *hush* true)
