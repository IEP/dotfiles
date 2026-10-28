#!/bin/bash
# Opens the invoking workspace's folder in Finder (macOS) or the default file manager (Linux).
# Folder priority: git worktree checkout > focused pane cwd > any pane cwd.
set -euo pipefail
# herdr may launch us without the login shell PATH (Homebrew, snap, ~/.local).
export PATH="/opt/homebrew/bin:/usr/local/bin:/snap/bin:$HOME/.local/bin:$PATH"

herdr="${HERDR_BIN_PATH:-herdr}"
ctx="${HERDR_PLUGIN_CONTEXT_JSON:-{\}}"
ws="${HERDR_WORKSPACE_ID:-$(jq -r '.workspace_id // .workspace.workspace_id // empty' <<<"$ctx")}"

if [[ -z "$ws" ]]; then
  echo "open-in-file-manager: no workspace in context: $ctx" >&2
  exit 1
fi

dir=$("$herdr" workspace list | jq -r --arg ws "$ws" \
  '.result.workspaces[] | select(.workspace_id == $ws) | .worktree.checkout_path // empty')

if [[ -z "$dir" ]]; then
  dir=$("$herdr" pane list --workspace "$ws" | jq -r \
    '.result.panes | (map(select(.focused)) + .)[0] | .foreground_cwd // .cwd // empty')
fi

if [[ -z "$dir" || ! -d "$dir" ]]; then
  echo "open-in-file-manager: could not resolve a folder for workspace $ws" >&2
  exit 1
fi

if [[ "$(uname)" == Darwin ]]; then
  open "$dir"
  exit 0
fi

# Linux: xdg-open dispatches to the desktop's default handler for inode/directory
# (Nautilus, Dolphin, Thunar, ...). Detach so the file manager outlives this script.
launch() { setsid -f "$@" >/dev/null 2>&1 </dev/null; }

if command -v xdg-open >/dev/null; then
  launch xdg-open "$dir"
elif command -v gio >/dev/null; then
  launch gio open "$dir"
else
  for fm in nautilus dolphin nemo thunar caja pcmanfm-qt pcmanfm; do
    if command -v "$fm" >/dev/null; then
      launch "$fm" "$dir"
      exit 0
    fi
  done
  echo "open-in-file-manager: no file manager found (install xdg-utils)" >&2
  exit 1
fi
