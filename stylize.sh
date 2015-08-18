find . -name "*.hs" -exec sh -c '{ rm "$0" && /app/sandbox/bin/stylish-haskell > "$0"; } < "$0"' {} \;
