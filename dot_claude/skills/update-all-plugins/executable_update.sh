#!/usr/bin/env bash
# Refresh every marketplace, then update each installed plugin at its own scope.
# Prints one status line per plugin and a summary; exits non-zero if any update failed.
set -uo pipefail

echo "== Refreshing marketplaces"
claude plugin marketplace update || echo "!! marketplace refresh failed; continuing with cached catalogs"

plugins=$(claude plugin list --json | jq -r '.[] | "\(.id) \(.scope) \(.version)"') || {
  echo "!! could not list installed plugins"; exit 1; }

updated=() unchanged=() failed=() skipped=()
while read -r id scope before; do
  [ -z "$id" ] && continue
  # synced plugins come from the claude.ai account; `plugin update` rejects that scope
  if [ "$scope" = "synced" ]; then skipped+=("$id"); continue; fi
  # </dev/null: no TTY, so a marketplace-declared command change is refused, not auto-accepted
  if out=$(claude plugin update "$id" --scope "$scope" </dev/null 2>&1); then
    after=$(claude plugin list --json | jq -r --arg id "$id" --arg s "$scope" \
      '.[] | select(.id==$id and .scope==$s) | .version')
    if [ "$before" != "$after" ]; then
      updated+=("$id ($scope): $before -> $after")
    else
      unchanged+=("$id")
    fi
  else
    failed+=("$id ($scope): $(echo "$out" | tail -1)")
  fi
done <<< "$plugins"

echo
echo "== Updated (${#updated[@]})";     printf '  %s\n' "${updated[@]+"${updated[@]}"}"
echo "== Already current (${#unchanged[@]})"
echo "== Skipped, synced from account (${#skipped[@]})"
echo "== Failed (${#failed[@]})";       printf '  %s\n' "${failed[@]+"${failed[@]}"}"

[ ${#failed[@]} -eq 0 ]
