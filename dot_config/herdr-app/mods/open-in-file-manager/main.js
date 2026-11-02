// Port of herdr's local plugin "open-in-file-manager": a key (keys.json: mod.open:open-in-file-manager)
// opens this popup, which opens the focused folder and closes itself.
import { View, div } from "gpui-kit";
import { exec, focused, spaces, props, closeOverlay, notify, theme } from "herdr";

const LABEL = "Open in file manager";
const PROGRAM = "xdg-open";

export default class OpenIn extends View {
  init(_props, cx) {
    cx.spawn(async () => {
      const f = focused();
      const space = spaces().find((s) => s.id === f?.space) ?? spaces().find((s) => s.active);
      const dir = f?.cwd || space?.folder;
      try {
        if (!dir) throw new Error("no folder for the focused pane or space");
        if (space?.machine) throw new Error(`space is on remote machine ${space.machine}`);
        await exec("setsid", ["-f", PROGRAM, dir]);
      } catch (e) {
        notify({ title: `${LABEL} failed`, body: String(e?.message ?? e), level: "error" });
      }
      closeOverlay(props().overlay);
    });
  }

  render() {
    const t = theme();
    return div().size_full().flex().items_center().justify_center().bg(t.popover)
      .text_size(13).text_color(t.muted).child(`${LABEL}…`);
  }
}
