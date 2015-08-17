find . -name "*.hs" -exec sh -c '{ rm "$0" && stylish-haskell > "$0"; } < "$0"' {} \;
