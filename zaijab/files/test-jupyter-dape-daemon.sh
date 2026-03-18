#!/bin/bash
# Test jupyter-dape.el using an isolated emacs -q daemon.
# All assertions run within a single dape session.
# Session restart is a known bug tracked separately.

DAEMON=jdape-test
EC="emacsclient -s $DAEMON"
JUPYTER_DAPE=/home/zjabbar/code/guix-channel/zaijab/files/jupyter-dape.el
TEST_PY=/home/zjabbar/code/guix-channel/zaijab/files/test.py

PASS=0
FAIL=0

emacsclient -s $DAEMON -e '(kill-emacs)' 2>/dev/null || true
sleep 0.5

echo "Starting emacs -q daemon..."
emacs -q --daemon=$DAEMON 2>&1
echo "Daemon started."

trap "$EC -e '(kill-emacs)' 2>/dev/null || true" EXIT

e() {
    echo "  => $1"
    $EC -e "$1" 2>&1
}

eload() {
    local tmp=$(mktemp /tmp/jdape-el-XXXXXX.el)
    cat > "$tmp"
    $EC -e "(load-file \"$tmp\")" 2>&1
    rm -f "$tmp"
}

wait_for() {
    local pred="$1" timeout="${2:-8}" desc="${3:-condition}"
    echo "  Waiting for: $desc (timeout=${timeout}s)"
    local i=0
    while [ $i -lt $timeout ]; do
        result=$($EC -e "$pred" 2>/dev/null)
        if [ -n "$result" ] && [ "$result" != "nil" ]; then
            echo "  OK: $result"
            return 0
        fi
        sleep 1
        i=$((i+1))
    done
    echo "  TIMEOUT: $desc"
    return 1
}

check() {
    local desc="$1" pred="$2"
    result=$($EC -e "$pred" 2>/dev/null)
    if [ -n "$result" ] && [ "$result" != "nil" ]; then
        echo "  PASS: $desc"
        PASS=$((PASS+1))
    else
        echo "  FAIL: $desc (got: $result)"
        FAIL=$((FAIL+1))
    fi
}

# ── Bootstrap ─────────────────────────────────────────────────────────────────
echo ""
echo "=== Bootstrap ==="
e "(progn (require 'package) (package-initialize))"
e "(require 'dape)"
e "(require 'jupyter)"
e "(load-file \"$JUPYTER_DAPE\")"

# ── Setup ─────────────────────────────────────────────────────────────────────
echo ""
echo "=== Setup: kernel + buffer association ==="
e "(jupyter-dape-mode 1)"
e "(find-file \"$TEST_PY\")"
e "(jupyter-run-repl \"python3\")"
wait_for "(cl-find-if (lambda (b) (string-match-p \"jupyter-repl\" (buffer-name b))) (buffer-list))" \
         10 "jupyter-repl buffer"
sleep 3
eload <<'EOF'
(let* ((rb (cl-find-if (lambda (b) (string-match-p "jupyter-repl" (buffer-name b)))
                        (buffer-list)))
       (c (when rb (with-current-buffer rb jupyter-current-client))))
  (if c (progn (with-current-buffer "test.py" (jupyter-repl-associate-buffer c)) t)
    (error "No jupyter client found")))
EOF

eload <<'EOF'
(with-current-buffer "test.py"
  (goto-char (point-min))
  (forward-line 9)
  (dape-breakpoint-toggle)
  t)
EOF

# ── Start single dape session ─────────────────────────────────────────────────
echo ""
echo "=== Starting dape session ==="
eload <<'EOF'
(with-current-buffer "test.py"
  (let ((cfg (assoc 'jupyter-dape dape-configs)))
    (if cfg (dape (dape--config-eval 'jupyter-dape (cdr cfg)))
      (error "jupyter-dape not in dape-configs"))))
EOF
wait_for "(not (null (dape--live-connections)))" 8 "dape connection"

# ── Eval test.py to hit breakpoint ────────────────────────────────────────────
eload <<'EOF'
(with-current-buffer "test.py"
  (jupyter-eval-string
   (buffer-substring-no-properties (point-min) (point-max))))
EOF
wait_for "(dape--stopped-threads (car (dape--live-connections)))" 8 "breakpoint hit"

# ── Test 1: Breakpoint ────────────────────────────────────────────────────────
echo ""
echo "=== Test 1: Breakpoint ==="
check "stopped at breakpoint" "(dape--stopped-threads (car (dape--live-connections)))"
check "stopped at line 10" \
      "(equal (plist-get (dape--current-stack-frame (car (dape--live-connections))) :line) 10)"

# ── Test 2: Evaluate expression at breakpoint (known-good frame) ──────────────
echo ""
echo "=== Test 2: Evaluate expression ==="
eload <<'EOF'
(setq jt/eval-result "pending")
(dape--evaluate-expression
 (car (dape--live-connections))
 (plist-get (dape--current-stack-frame (car (dape--live-connections))) :id)
 "i" "hover"
 (lambda (body error)
   (setq jt/eval-result (or error (plist-get body :result) "no-result"))))
EOF
wait_for "(not (equal jt/eval-result \"pending\"))" 8 "eval result"
check "evaluated 'i'" "(stringp jt/eval-result)"
echo "  Value of i: $($EC -e 'jt/eval-result' 2>/dev/null)"

# ── Test 3: Step into pr() ───────────────────────────────────────────────────
echo ""
echo "=== Test 3: Step into pr() ==="
e "(dape-step-in (car (dape--live-connections)))"
wait_for "(let* ((conn (car (dape--live-connections))) (th (car (dape--stopped-threads conn))) (frames (plist-get th :stackFrames))) (and frames (equal (plist-get (car frames) :name) \"pr\")))" \
         8 "stepped into pr() frame"
e "(plist-get (car (plist-get (car (dape--stopped-threads (car (dape--live-connections)))) :stackFrames)) :name)"
check "stepped into pr() frame" \
      "(equal (plist-get (car (plist-get (car (dape--stopped-threads (car (dape--live-connections)))) :stackFrames)) :name) \"pr\")"

# ── Test 4: Step out of pr() ─────────────────────────────────────────────────
echo ""
echo "=== Test 4: Step out of pr() ==="
e "(dape-step-out (car (dape--live-connections)))"
wait_for "(let* ((conn (car (dape--live-connections))) (th (car (dape--stopped-threads conn))) (frames (plist-get th :stackFrames))) (and frames (not (equal (plist-get (car frames) :name) \"pr\"))))" \
         8 "stepped back out of pr()"
e "(plist-get (car (plist-get (car (dape--stopped-threads (car (dape--live-connections)))) :stackFrames)) :name)"
check "stepped back out — frame is not pr" \
      "(not (equal (plist-get (car (plist-get (car (dape--stopped-threads (car (dape--live-connections)))) :stackFrames)) :name) \"pr\"))"

# ── Test 5: Step next ─────────────────────────────────────────────────────────
echo ""
echo "=== Test 5: Step next ==="
e "(dape-next (car (dape--live-connections)))"
wait_for "(dape--stopped-threads (car (dape--live-connections)))" 8 "stopped after next"
check "still stopped after next" "(dape--stopped-threads (car (dape--live-connections)))"

# ── Test 6: Continue re-hits breakpoint ───────────────────────────────────────
echo ""
echo "=== Test 6: Continue re-hits breakpoint ==="
e "(dape-continue (car (dape--live-connections)))"
wait_for "(dape--stopped-threads (car (dape--live-connections)))" 8 "breakpoint re-hit"

# ── Test 7: Regression — normal eval without dape active ─────────────────────
echo ""
echo "=== Test 7: Regression — eval without dape ==="
e "(dape-quit (car (dape--live-connections)))"
wait_for "(null (dape--live-connections))" 8 "dape session quit"
eload <<'EOF'
(setq jt/regression-result "pending")
(with-current-buffer "test.py"
  (jupyter-eval-string "1 + 1"))
(run-at-time 2 nil (lambda () (setq jt/regression-result "done")))
EOF
wait_for "(not (equal jt/regression-result \"pending\"))" 8 "regression eval"
check "normal eval works without dape" "(equal jt/regression-result \"done\")"

# ── Test 8: Session restart (main bug) ────────────────────────────────────────
# After dape-quit, a second dape session must connect without
# "Kernel debugger failed to start" error.
echo ""
echo "=== Test 8: Session restart ==="
eload <<'EOF'
(with-current-buffer "test.py"
  (let ((cfg (assoc 'jupyter-dape dape-configs)))
    (if cfg (dape (dape--config-eval 'jupyter-dape (cdr cfg)))
      (error "jupyter-dape not in dape-configs"))))
EOF
wait_for "(not (null (dape--live-connections)))" 12 "session 2 connection"
check "session 2 connects without error" "(not (null (dape--live-connections)))"

echo "  [diag] session 2 bridge-active: $($EC -e 'jupyter-dape--bridge-active' 2>/dev/null)"
echo "  [diag] session 2 init-caps cached: $($EC -e '(not (null jupyter-dape--init-capabilities))' 2>/dev/null)"
echo "  [diag] session 2 breakpoints: $($EC -e '(length dape--breakpoints)' 2>/dev/null)"
echo "  [diag] session 2 conn state: $($EC -e '(dape--state (car (dape--live-connections)))' 2>/dev/null)"

eload <<'EOF'
(with-current-buffer "test.py"
  (jupyter-eval-string
   (buffer-substring-no-properties (point-min) (point-max))))
EOF
wait_for "(dape--stopped-threads (car (dape--live-connections)))" 15 "session 2 breakpoint hit"
echo "  [diag] after wait — conn state: $($EC -e '(dape--state (car (dape--live-connections)))' 2>/dev/null)"
echo "  [diag] after wait — threads: $($EC -e '(length (dape--threads (car (dape--live-connections))))' 2>/dev/null)"
$EC -e "(with-temp-file \"/tmp/jdape-session2-messages.log\" (insert-buffer-substring \"*Messages*\"))" 2>/dev/null
echo "  [diag] *Messages* saved to /tmp/jdape-session2-messages.log"
check "session 2 stopped at breakpoint" "(dape--stopped-threads (car (dape--live-connections)))"
check "session 2 stopped at line 10" \
      "(equal (plist-get (dape--current-stack-frame (car (dape--live-connections))) :line) 10)"

e "(dape-quit (car (dape--live-connections)))"
wait_for "(null (dape--live-connections))" 8 "session 2 quit"

# ── Summary ───────────────────────────────────────────────────────────────────
echo ""
echo "=== Summary ==="
echo "  PASS: $PASS"
echo "  FAIL: $FAIL"
