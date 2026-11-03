// Opens the current space's folder in VS Code, then closes itself. Bound in keys.json
// (mod.open:open-in-vscode). Works on macOS and Linux: branches on window().platform.
// Folder priority: git worktree checkout > focused pane cwd > any pane cwd > space folder.
import { View, div } from "gpui-kit";
import { closeOverlay, exec, focused, notify, panes, props, spaces, window as appWindow } from "herdr";

const LABEL = "Open in VS Code";

async function worktreeRoot(dir) {
  const r = await exec("git", ["rev-parse", "--show-toplevel", "--git-dir", "--git-common-dir"], { cwd: dir });
  if (r.code !== 0) return null;
  const [root, gitDir, commonDir] = r.stdout.trim().split("\n");
  return gitDir !== commonDir ? root : null;
}

async function spaceFolder(space) {
  const tabs = new Set(space.tabs.map((t) => t.id));
  const dir = focused()?.cwd
    ?? panes().find((p) => tabs.has(p.tab) && p.cwd)?.cwd
    ?? space.folder;
  if (!dir) return null;
  return (await worktreeRoot(dir)) ?? dir;
}

// Linux: detach with setsid so the program outlives exec's 30 s limit and this popup.
async function launch(program, args) {
  const r = appWindow().platform === "macos"
    ? await exec(program, args)
    : await exec("setsid", ["-f", program, ...args]);
  if (r.code !== 0) throw new Error(r.stderr.trim() || `${program} exited ${r.code}`);
}

async function open(dir) {
  try {
    return await launch("code", [dir]);
  } catch (e) {
    // `code` missing from PATH on macOS: fall back to the app bundle.
    if (appWindow().platform !== "macos") throw e;
  }
  await launch("open", ["-a", "Visual Studio Code", dir]);
}

export default class OpenInVSCode extends View {
  init(_props, cx) {
    cx.spawn(async () => {
      try {
        const space = spaces().find((s) => s.id === focused()?.space);
        if (!space) throw new Error("No space is focused.");
        if (space.machine) throw new Error(`This space is on ${space.machine}.`);
        const dir = await spaceFolder(space);
        if (!dir) throw new Error("Could not find a folder for this space.");
        await open(dir);
      } catch (e) {
        notify({ title: `${LABEL} failed`, body: String(e?.message ?? e), level: "error" });
      } finally {
        closeOverlay(props().overlay);
      }
    });
  }

  render() {
    return div();
  }
}
