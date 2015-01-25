set -e

head_dir() {
  echo "$1" | sed 's|^./_root.*$|./_root|'
}
tail_dir() {
  echo "$1" | sed 's|^./_root||'
}
target_home() {
  echo "$1" | sed "s|^./_|$HOME/.|"
}


for src in $(find . -wholename './_*' -type f); do
  if [ "$(head_dir $src)" = "./_root" ]; then
    target="$(tail_dir $src)"
    needsudo=1
  else
    target="$(target_home $src)"
  fi
  case "$1" in
    push)
      if [ "needsudo" ]; then
        (sudo "$src" "$target")
      else
        cp "$src" "$target"
      fi
    ;;
    pull)
      cp "$target" "$src"
    ;;
    diff)
      diff -u "$target" "$src"
    ;;
  esac
done
