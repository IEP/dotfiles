-- Make completion draw words from *every* open buffer, not just visible ones.

--- Every listed, loaded, real-file buffer.
--- blink's default `get_bufnrs` maps over `nvim_list_wins()`, so out of the box
--- it only sees buffers currently displayed in a window (i.e. the current one).
--- @return integer[]
local function all_bufnrs()
  return vim.tbl_filter(function(buf)
    return vim.bo[buf].buflisted and vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == ""
  end, vim.api.nvim_list_bufs())
end

return {
  "saghen/blink.cmp",
  opts = {
    sources = {
      providers = {
        -- blink wires `buffer` as a *fallback* of lsp/path, so it is only
        -- queried when those return zero items. In an LSP-backed file that
        -- means buffer words never show up at all. Clearing the fallbacks
        -- promotes `buffer` to a top-level source that runs on every keystroke
        -- alongside the LSP.
        lsp = { fallbacks = {} },
        path = { fallbacks = {} },

        buffer = {
          -- Keep plain buffer words ranked below real LSP/snippet items.
          -- Raise toward 0 to make them compete more; lower to bury them.
          score_offset = -3,
          opts = {
            get_bufnrs = all_bufnrs,
            -- `/` and `?` search completion defaults to the current buffer only.
            get_search_bufnrs = all_bufnrs,
            -- Indexing limits. Buffers past max_async are skipped entirely, and
            -- once the total is exceeded blink silently drops buffers by
            -- `retention_order`. Defaults (200KB / 500KB) are easy to hit once
            -- every buffer is in scope.
            max_async_buffer_size = 1024 * 1024, -- 1MB per buffer
            max_total_buffer_size = 8 * 1024 * 1024, -- 8MB across all buffers
          },
        },
      },
    },
  },
}
