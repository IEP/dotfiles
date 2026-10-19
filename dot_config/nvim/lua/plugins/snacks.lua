-- Show dotfiles and git-ignored files in the explorer by default.
-- `H` / `I` inside the explorer still toggle them for the current session.
return {
  "folke/snacks.nvim",
  opts = {
    picker = {
      sources = {
        explorer = { hidden = true, ignored = true },
      },
    },
  },
}
