exit_trap() {
  local REAL_EXIT=$?

  if [ "$REAL_EXIT" -eq 64 ]; then
    echo "Process exited with status code 64, which we reserve to indicate" >&2
    echo "allowed-to-fail in CI. Replacing with exit code 65!" >&2
    exit 65
  fi
  exit $REAL_EXIT
}
