#!/bin/bash
# Launch emacs --batch to test jupyter-dape.el
# Logs go to /tmp/jdape-test*.log

rm -f /tmp/jdape-test*.log

echo "Starting emacs --batch test..."
emacs --batch \
  -l ~/.config/emacs/init.el \
  -l /home/zjabbar/code/guix-channel/zaijab/files/test-jupyter-dape.el \
  2>&1 | tee /tmp/jdape-test-stderr.log

echo ""
echo "=== Test Log ==="
cat /tmp/jdape-test.log 2>/dev/null || echo "(no test log produced)"
echo ""
echo "=== Additional logs ==="
ls -1 /tmp/jdape-test-*.log 2>/dev/null
