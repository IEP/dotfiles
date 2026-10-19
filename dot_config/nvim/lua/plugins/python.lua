-- Point pyright at the project's own virtualenv, resolved from the LSP root_dir
-- rather than nvim's cwd. Without this, opening nvim in a parent directory
-- (e.g. `~/code` instead of `~/code/myproject`) makes pyright fall back to the
-- `python` on $PATH, so third-party imports show as unresolved.

--- Tools that keep venvs outside the project. Each is only queried when its
--- marker file exists in the root, so unrelated projects pay no subprocess cost.
--- `cmd` must print a venv prefix (the dir containing `bin/python`).
local venv_tools = {
  { marker = "poetry.lock", cmd = { "poetry", "env", "info", "-p" } },
  { marker = "Pipfile", cmd = { "pipenv", "--venv" } },
  { marker = ".python-version", cmd = { "pyenv", "prefix" } },
}

--- Run `cmd` in `root` and return its trimmed stdout, or nil on any failure.
--- @param cmd string[]
--- @param root string
--- @return string|nil
local function query(cmd, root)
  if vim.fn.executable(cmd[1]) == 0 then
    return nil
  end
  local ok, res = pcall(function()
    return vim.system(cmd, { cwd = root, text = true }):wait(3000)
  end)
  if ok and res.code == 0 and res.stdout then
    local out = vim.trim(res.stdout)
    return out ~= "" and out or nil
  end
end

--- Find a virtualenv interpreter for the project at `root`.
--- @param root string
--- @return string|nil
local function find_venv_python(root)
  for _, name in ipairs({ ".venv", "venv", "env", ".env" }) do
    local python = vim.fs.joinpath(root, name, "bin", "python")
    if vim.uv.fs_stat(python) then
      return python
    end
  end

  for _, tool in ipairs(venv_tools) do
    if vim.uv.fs_stat(vim.fs.joinpath(root, tool.marker)) then
      local prefix = query(tool.cmd, root)
      local python = prefix and vim.fs.joinpath(prefix, "bin", "python")
      if python and vim.uv.fs_stat(python) then
        return python
      end
    end
  end
end

--- @param _ lsp.InitializeParams
--- Whether the project's pyproject.toml enables django-stubs' mypy plugin.
--- Such projects treat mypy as the source of truth: pyright can't run mypy
--- plugins, so it misreports Django-synthesized attributes like `<fk>_id`.
--- @type table<string, boolean>
local django_mypy_cache = {}
--- @param root string
--- @return boolean
local function uses_django_mypy(root)
  if django_mypy_cache[root] == nil then
    local f = io.open(vim.fs.joinpath(root, "pyproject.toml"))
    local text = f and f:read("*a") or ""
    if f then
      f:close()
    end
    django_mypy_cache[root] = text:find("mypy_django_plugin", 1, true) ~= nil
  end
  return django_mypy_cache[root]
end

--- @param _ lsp.InitializeParams
--- @param config vim.lsp.ClientConfig
local function use_project_venv(_, config)
  local root = config.root_dir
  if not root then
    return
  end
  -- Mutate in place: the client already holds a reference to this table,
  -- so reassigning `config.settings` would never reach the server.
  config.settings = config.settings or {}
  local s = config.settings

  local python = find_venv_python(root)
  if python then
    s.python = s.python or {}
    s.python.pythonPath = python
  end

  if uses_django_mypy(root) then
    -- Keep hover/completion/navigation; leave type errors to mypy below.
    -- pyright reads python.analysis, basedpyright reads basedpyright.analysis.
    for _, ns in ipairs({ "python", "basedpyright" }) do
      s[ns] = s[ns] or {}
      s[ns].analysis = s[ns].analysis or {}
      s[ns].analysis.typeCheckingMode = "off"
    end
  end
end

-- mypy with the project's own venv + config, only for django-mypy projects.
-- nvim-lint's `cwd` is static, so `condition` (run synchronously right before
-- each lint) points this table at the current buffer's project.
local function build_mypy_django()
  local mypy_django = vim.tbl_extend("force", require("lint.linters.mypy"), {})
  mypy_django.condition = function(ctx)
    local root = vim.fs.root(ctx.filename, "pyproject.toml")
    if not (root and uses_django_mypy(root)) then
      return false
    end
    local python = find_venv_python(root)
    local mypy = python and vim.fs.joinpath(vim.fs.dirname(python), "mypy")
    if not (mypy and vim.uv.fs_stat(mypy)) then
      return false
    end
    -- Run from the root so mypy finds [tool.mypy] and the plugin can import
    -- DJANGO_SETTINGS_MODULE, exactly as CI does.
    mypy_django.cmd = mypy
    mypy_django.cwd = root
    return true
  end
  return mypy_django
end

return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        pyright = { before_init = use_project_venv },
        basedpyright = { before_init = use_project_venv },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = function(_, opts)
      opts.linters = opts.linters or {}
      opts.linters.mypy_django = build_mypy_django()
      opts.linters_by_ft = opts.linters_by_ft or {}
      opts.linters_by_ft.python = vim.list_extend(opts.linters_by_ft.python or {}, { "mypy_django" })
    end,
  },
}
