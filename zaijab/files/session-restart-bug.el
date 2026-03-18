;; SESSION RESTART BUG
;; ====================
;;
;; SYMPTOM
;; -------
;; After calling `dape-quit` and starting a new dape session (calling `dape`
;; again in the same Emacs session with the same Jupyter kernel), the second
;; session fails with:
;;
;;   "Kernel debugger failed to start; ensure ipykernel >= 6"
;;
;; The first session works fine.  Every subsequent session fails.
;;
;;
;; ROOT CAUSE
;; ----------
;; When `dape-quit` tears down the session it sends a DAP `disconnect` request
;; through the TCP bridge to the Jupyter kernel.  ipykernel forwards this to
;; debugpy, which terminates its debug session.  After that, ipykernel cannot
;; restart debugpy in response to a second `initialize` request — `debugInfo`
;; keeps returning `isStarted = false` even after a fresh `initialize`, causing
;; `jupyter-dape--configure` to signal the error above.
;;
;; Confirmed by diagnostic:
;;   Session 1: connects, 1 live connection.
;;   dape-quit:  connections drop to 0.
;;   debugInfo:  isStarted = false  (debugpy was killed by disconnect).
;;   initialize: sent, appears to succeed.
;;   debugInfo:  isStarted = false  (debugpy still not running).
;;   → error "Kernel debugger failed to start"
;;
;;
;; ATTEMPTED FIX (incomplete, do not rely on)
;; -------------------------------------------
;; An attempt was made to intercept the `disconnect` command in
;; `jupyter-dape--forward-to-kernel`, send a synthetic success response back
;; to DAPE so it does not hang, and set `bridge-active = nil` — without
;; forwarding the disconnect to the kernel.  The idea was to keep debugpy alive
;; so the second session could reuse it.
;;
;; The intercept code is currently in `jupyter-dape.el` at the top of
;; `jupyter-dape--forward-to-kernel`.  It runs, but `dape-quit` does NOT
;; appear to send the `disconnect` request through the TCP bridge before
;; closing the connection — it may call `jsonrpc-shutdown` first, which closes
;; the socket before our filter sees the disconnect message.  As a result the
;; intercept is never reached and the kernel receives the disconnect anyway.
;;
;;
;; WHAT TO INVESTIGATE NEXT
;; ------------------------
;; 1. Confirm whether `dape-quit` sends `disconnect` through jsonrpc BEFORE
;;    or AFTER calling `jsonrpc-shutdown`.  Check `dape-kill` in dape.el and
;;    look for the `with-disconnect` macro / call sequence.
;;
;; 2. If disconnect is sent before shutdown: the synthetic-response intercept
;;    approach is correct but the response may not flush before the process is
;;    deleted.  Try delaying `jsonrpc-shutdown` or use `run-at-time` to
;;    schedule bridge teardown after the filter returns.
;;
;; 3. If disconnect is sent after shutdown (or not at all through our bridge):
;;    hook into dape's connection teardown — e.g. `dape-quit-hook` or advice
;;    on `dape-kill` — to prevent the disconnect from reaching the kernel by
;;    closing the bridge BEFORE dape tries to send through it.
;;
;; 4. Alternative approach: instead of intercepting disconnect, investigate
;;    whether ipykernel can restart debugpy after a disconnect if given enough
;;    time or a different command sequence.  Check ipykernel source for how it
;;    handles a second `initialize` after `disconnect`.
;;
;; 5. The `jupyter-dape--init-capabilities` cache persists across sessions
;;    which is correct.  No issue there.
;;
;;
;; RELEVANT CODE LOCATIONS
;; -----------------------
;; jupyter-dape.el:
;;   - `jupyter-dape--forward-to-kernel`  — where intercept attempt lives
;;   - `jupyter-dape--configure`          — sends debugInfo + initialize
;;   - `jupyter-dape--stop-bridge`        — tears down TCP bridge
;;
;; dape.el (in Guix profile):
;;   - `dape-quit` / `dape-kill`          — session teardown entry point
;;   - `with-disconnect` macro            — wraps disconnect request
;;   - `jsonrpc-shutdown`                 — closes the TCP connection
