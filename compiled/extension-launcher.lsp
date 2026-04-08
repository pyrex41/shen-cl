;; Copyright (c) 2019 Bruno Deferrari.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(in-package :shen)

(DEFUN |shen.x.launcher.quiet-load| (V7078) (LET ((W7079 (|read-file| V7078))) (|map| (|lambda| Z7080 (|eval| Z7080)) W7079)))

(DEFUN |shen.x.launcher.version-string| () (|shen.app| (|version|) (|cn| " " (|shen.app| (|cons| '|port| (|cons| (|cons| (|language|) (|cons| (|port|) '())) (|cons| '|implementation| (|cons| (|cons| (|implementation|) (|cons| (|release|) '())) '())))) "
" '|shen.r|)) '|shen.a|))

(DEFUN |shen.x.launcher.help-text| (V7081) (|cn| "Usage: " (|shen.app| V7081 " [--version] [--help] <COMMAND> [<ARGS>]

commands:
    repl
        Launches the interactive REPL.
        Default action if no command is supplied.

    script <FILE> [<ARGS>]
        Runs the script in FILE. *argv* is set to [FILE | ARGS].

    eval <ARGS>
        Evaluates expressions and files. ARGS are evaluated from
        left to right and can be a combination of:
            -e, --eval <EXPR>
                Evaluates EXPR and prints result.
            -l, --load <FILE>
                Reads and evaluates FILE.
            -q, --quiet
                Silences interactive output.
            -s, --set <KEY> <VALUE>
                Evaluates KEY, VALUE and sets as global.
            -r, --repl
                Launches the interactive REPL after evaluating
                all the previous expresions." '|shen.a|)))

(DEFUN |shen.x.launcher.execute-all| (V7082) (BLOCK () (IF (NULL V7082) (RETURN (|cons| '|success| '())) (IF (CONSP V7082) (RETURN (PROGN (|thaw| (|hd| V7082)) (|shen.x.launcher.execute-all| (|tl| V7082)))) (RETURN (|shen.f-error| '|shen.x.launcher.execute-all|))))))

(DEFUN |shen.x.launcher.eval-string| (V7083) (|eval| (|head| (|read-from-string| V7083))))

(DEFUN |shen.x.launcher.eval-flag-map| (V7086) (BLOCK () (IF (EQUAL "-e" V7086) (RETURN "--eval") (IF (EQUAL "-l" V7086) (RETURN "--load") (IF (EQUAL "-q" V7086) (RETURN "--quiet") (IF (EQUAL "-s" V7086) (RETURN "--set") (IF (EQUAL "-r" V7086) (RETURN "--repl") (RETURN '|false|))))))))

(DEFUN |shen.x.launcher.eval-command-h| (V7091 V7092) (BLOCK () (IF (NULL V7091) (RETURN (|shen.x.launcher.execute-all| (|reverse| V7092))) (TAGBODY (IF (CONSP V7091) (|LET*| ((|V7091/hd| (|hd| V7091)) (|V7091/tl| (|tl| V7091))) (IF (AND (EQUAL "--eval" |V7091/hd|) (CONSP |V7091/tl|)) (RETURN (|shen.x.launcher.eval-command-h| (|tl| |V7091/tl|) (|cons| (|freeze| (|pr| (|shen.app| (|shen.x.launcher.eval-string| (|hd| |V7091/tl|)) "
" '|shen.a|) (|stoutput|))) V7092))) (IF (AND (EQUAL "--load" |V7091/hd|) (CONSP |V7091/tl|)) (RETURN (|shen.x.launcher.eval-command-h| (|tl| |V7091/tl|) (|cons| (|freeze| (|load| (|hd| |V7091/tl|))) V7092))) (IF (EQUAL "--quiet" |V7091/hd|) (RETURN (|shen.x.launcher.eval-command-h| |V7091/tl| (|cons| (|freeze| (|set| '|*hush*| '|true|)) V7092))) (TAGBODY (IF (AND (EQUAL "--set" |V7091/hd|) (CONSP |V7091/tl|)) (LET ((|V7091/tl/tl| (|tl| |V7091/tl|))) (IF (CONSP |V7091/tl/tl|) (RETURN (|shen.x.launcher.eval-command-h| (|tl| |V7091/tl/tl|) (|cons| (|freeze| (|set| (|shen.x.launcher.eval-string| (|hd| |V7091/tl|)) (|shen.x.launcher.eval-string| (|hd| |V7091/tl/tl|)))) V7092))) (GO |%%label511|))) (GO |%%label511|)) |%%label511| (IF (EQUAL "--repl" |V7091/hd|) (RETURN (PROGN (|shen.x.launcher.eval-command-h| '() V7092) (|cons| '|launch-repl| |V7091/tl|))) (GO |%%label510|))))))) (GO |%%label510|)) |%%label510| (RETURN (LET ((|Freeze7095| (|freeze| (COND ((CONSP V7091) (|cons| '|error| (|cons| (|cn| "Invalid eval argument: " (|shen.app| (|hd| V7091) "" '|shen.a|)) '()))) (T (|shen.f-error| '|shen.x.launcher.eval-command-h|)))))) (IF (CONSP V7091) (LET ((|Result7094| (LET ((W7093 (|shen.x.launcher.eval-flag-map| (|hd| V7091)))) (IF (EQ W7093 '|false|) (|fail|) (|shen.x.launcher.eval-command-h| (|cons| W7093 (|tl| V7091)) V7092))))) (IF (EQ |Result7094| (|fail|)) (|thaw| |Freeze7095|) |Result7094|)) (|thaw| |Freeze7095|))))))))

(DEFUN |shen.x.launcher.eval-command| (V7096) (|shen.x.launcher.eval-command-h| V7096 '()))

(DEFUN |shen.x.launcher.script-command| (V7097 V7098) (PROGN (|set| '|*argv*| (|cons| V7097 V7098)) (|shen.x.launcher.quiet-load| V7097) (|cons| '|success| '())))

(DEFUN |shen.x.launcher.launch-shen| (V7099) (BLOCK () (TAGBODY (IF (CONSP V7099) (LET ((|V7099/tl| (|tl| V7099))) (IF (NULL |V7099/tl|) (RETURN (|cons| '|launch-repl| '())) (IF (CONSP |V7099/tl|) (|LET*| ((|V7099/tl/hd| (|hd| |V7099/tl|)) (|V7099/tl/tl| (|tl| |V7099/tl|))) (IF (EQUAL "--help" |V7099/tl/hd|) (RETURN (|cons| '|show-help| (|cons| (|shen.x.launcher.help-text| (|hd| V7099)) '()))) (IF (EQUAL "--version" |V7099/tl/hd|) (RETURN (|cons| '|success| (|cons| (|shen.x.launcher.version-string|) '()))) (IF (EQUAL "repl" |V7099/tl/hd|) (RETURN (|cons| '|launch-repl| |V7099/tl/tl|)) (IF (AND (EQUAL "script" |V7099/tl/hd|) (CONSP |V7099/tl/tl|)) (RETURN (|shen.x.launcher.script-command| (|hd| |V7099/tl/tl|) (|tl| |V7099/tl/tl|))) (IF (EQUAL "eval" |V7099/tl/hd|) (RETURN (|shen.x.launcher.eval-command| |V7099/tl/tl|)) (RETURN (|cons| '|unknown-arguments| V7099)))))))) (GO |%%label516|)))) (GO |%%label516|)) |%%label516| (RETURN (|shen.f-error| '|shen.x.launcher.launch-shen|)))))

(DEFUN |shen.x.launcher.default-handle-result| (V7102) (BLOCK () (TAGBODY (IF (CONSP V7102) (|LET*| ((|V7102/hd| (|hd| V7102)) (|V7102/tl| (|tl| V7102))) (TAGBODY (IF (EQ |V7102/hd| '|success|) (IF (NULL |V7102/tl|) (RETURN '|shen.x.launcher.done|) (IF (AND (CONSP |V7102/tl|) (NULL (|tl| |V7102/tl|))) (RETURN (|pr| (|shen.app| (|hd| |V7102/tl|) "
" '|shen.a|) (|stoutput|))) (GO |%%label527|))) (GO |%%label527|)) |%%label527| (IF (AND (EQ |V7102/hd| '|error|) (AND (CONSP |V7102/tl|) (NULL (|tl| |V7102/tl|)))) (RETURN (|pr| (|cn| "ERROR: " (|shen.app| (|hd| |V7102/tl|) "
" '|shen.a|)) (|stoutput|))) (IF (EQ |V7102/hd| '|launch-repl|) (RETURN (|shen.repl|)) (IF (AND (EQ |V7102/hd| '|show-help|) (AND (CONSP |V7102/tl|) (NULL (|tl| |V7102/tl|)))) (RETURN (|pr| (|shen.app| (|hd| |V7102/tl|) "
" '|shen.a|) (|stoutput|))) (IF (AND (EQ |V7102/hd| '|unknown-arguments|) (CONSP |V7102/tl|)) (LET ((|V7102/tl/tl| (|tl| |V7102/tl|))) (IF (CONSP |V7102/tl/tl|) (RETURN (|pr| (|cn| "ERROR: Invalid argument: " (|shen.app| (|hd| |V7102/tl/tl|) (|cn| "
Try `" (|shen.app| (|hd| |V7102/tl|) " --help' for more information.
" '|shen.a|)) '|shen.a|)) (|stoutput|))) (GO |%%label523|))) (GO |%%label523|))))))) (GO |%%label523|)) |%%label523| (RETURN (|shen.f-error| '|shen.x.launcher.default-handle-result|)))))

(DEFUN |shen.x.launcher.main| (V7103) (|shen.x.launcher.default-handle-result| (|shen.x.launcher.launch-shen| V7103)))

