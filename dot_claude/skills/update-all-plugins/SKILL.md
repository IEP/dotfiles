---
name: update-all-plugins
description: Refresh all plugin marketplaces and update every installed Claude Code plugin to its latest version.
disable-model-invocation: true
allowed-tools: Bash(bash ~/.claude/skills/update-all-plugins/update.sh)
---

# Update all plugins

Run the bundled script — do not reimplement its steps:

```bash
bash ~/.claude/skills/update-all-plugins/update.sh
```

It may take a minute or two (one `claude plugin update` per plugin).

Then report to the user:

1. The **Updated** list with old → new versions, or say nothing changed.
2. Any **Failed** entries with their error line. If a failure says a
   marketplace-declared command changed and needs confirmation, do NOT re-run
   with `-y` yourself — show the user the plugin name and tell them to run
   `claude plugin update <id>` in their own terminal to review and accept it.
3. If anything was updated: remind them to restart Claude Code, since updated
   plugins only load in new sessions.
