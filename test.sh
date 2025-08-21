git update-ref refs/heads/main b11a4eb8bbda6b593dc3c0552abaea5a19458c7f
echo "ref: refs/heads/main" > .git/HEAD
mv .git/index .git/index.bad
git reset
git add -A
git commit -m "Recover work after crash"
