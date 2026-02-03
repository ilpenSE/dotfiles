-- init.lua

-- lazy.nvim bootstrap
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugin listesi
require("lazy").setup({
  -- Tema
  { "tomasiser/vim-code-dark" },
  { "folke/tokyonight.nvim" },
  { "Mofiqul/dracula.nvim" },
  { "scottmckendry/cyberdream.nvim" },

  -- Auto bracket
  { "windwp/nvim-autopairs" },

  -- Keymap helper
  { "folke/which-key.nvim" },
})

-- ==================
-- Genel Ayarlar
-- ==================

-- Tema
vim.cmd("colorscheme dracula")

-- Satır numaraları
vim.opt.number = true
vim.opt.relativenumber = false

-- Tabspace = 2
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

-- ==================
-- Keymap (Ctrl+C, V, Z, A)
-- ==================
vim.g.mapleader = " "
vim.opt.clipboard = "unnamedplus"

-- Select all
vim.keymap.set("n", "<C-a>", "ggVG", { noremap = true, silent = true })
vim.keymap.set("i", "<C-a>", "<Esc>ggVG", { noremap = true, silent = true })
vim.keymap.set("v", "<C-a>", "ggVG", { noremap = true, silent = true })

-- Copy / paste to system clipboard
vim.keymap.set("v", "<C-c>", '"+y<Esc>', { noremap = true, silent = true })  -- visual: copy selection
vim.keymap.set("n", "<C-c>", '<Cmd>%y+<CR>', { noremap = true, silent = true }) -- normal: copy whole buffer
vim.keymap.set("n", "<C-v>", '"+p', { noremap = true, silent = true })
vim.keymap.set("i", "<C-v>", '<Esc>"+pa', { noremap = true, silent = true })

vim.keymap.set("n", "<C-z>", ":undo<CR>", { noremap = true, silent = true })
-- Normal mode mappings
vim.api.nvim_set_keymap('n', '<C-Right>', 'w', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-Left>', 'b', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-Up>', '0', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-Down>', '$', { noremap = true, silent = true })

-- Shift+Arrow selection
vim.api.nvim_set_keymap('n', '<S-Right>', 'v>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<S-Left>', 'v<', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<S-Up>', 'vk', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<S-Down>', 'vj', { noremap = true, silent = true })

vim.api.nvim_set_keymap('v', '<S-Right>', '>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<S-Left>', '<', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<S-Up>', 'k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<S-Down>', 'j', { noremap = true, silent = true })

-- Normal + Visual mode için Ctrl+Shift+Arrow ile kelime selection
-- Ctrl+Shift+Right: bir sonraki kelimeyi seç
vim.api.nvim_set_keymap('n', '<C-S-Right>', 'vawl', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-S-Right>', 'oawl', { noremap = true, silent = true })

-- Ctrl+Shift+Left: bir önceki kelimeyi seç
vim.api.nvim_set_keymap('n', '<C-S-Left>', 'vawh', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-S-Left>', 'oawh', { noremap = true, silent = true })


-- ==================
-- Auto pairs
-- ==================
require("nvim-autopairs").setup {}
