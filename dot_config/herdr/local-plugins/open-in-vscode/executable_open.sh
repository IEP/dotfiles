#!/bin/bash
# Opens the invoking workspace's folder in VS Code.
# Folder priority: git worktree checkout > focused pane cwd > any pane cwd.
set -euo pipefail
export PATH="/opt/homebrew/bin:/usr/local/bin:$PATH"

herdr="${HERDR_BIN_PATH:-herdr}"
ctx="${HERDR_PLUGIN_CONTEXT_JSON:-{\}}"
ws="${HERDR_WORKSPACE_ID:-$(jq -r '.workspace_id // .workspace.workspace_id // empty' <<<"$ctx")}"

if [[ -z "$ws" ]]; then
  echo "open-in-vscode: no workspace in context: $ctx" >&2
  exit 1
fi

dir=$("$herdr" workspace list | jq -r --arg ws "$ws" \
  '.result.workspaces[] | select(.workspace_id == $ws) | .worktree.checkout_path // empty')

if [[ -z "$dir" ]]; then
  dir=$("$herdr" pane list --workspace "$ws" | jq -r \
    '.result.panes | (map(select(.focused)) + .)[0] | .foreground_cwd // .cwd // empty')
fi

if [[ -z "$dir" || ! -d "$dir" ]]; then
  echo "open-in-vscode: could not resolve a folder for workspace $ws" >&2
  exit 1
fi

if command -v code >/dev/null; then
  code "$dir"
else
  open -a "Visual Studio Code" "$dir"
fi
