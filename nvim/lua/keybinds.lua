-- Leaders
vim.keymap.set('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Bindings
local builtin = require('telescope.builtin')
require('which-key').add({
    { '<leader><leader>', "<Plug>(leap)", desc = 'Leap' },
    { '<leader><Tab>', "<C-w>w", desc = 'Swap active window' },

    { '<leader>b', group = 'Buffer' },
    { '<leader>bb', builtin.buffers, desc = 'List buffers' },
    { '<leader>bd', "<cmd>bdelete<cr>", desc = 'Kill current buffer' },
    { '<leader>bD', "<cmd>%bdelete<cr>", desc = 'Kill all buffers' },

    { '<leader>e', group = 'Error' },
    { '<leader>ep', function() vim.diagnostic.jump({ count = -1 }) end, desc = 'Previous error' },
    { '<leader>en', function() vim.diagnostic.jump({ count = 1 }) end, desc = 'Next error' },
    { '<leader>el', vim.diagnostic.setloclist, desc = 'List local errors' },
    { '<leader>ee', vim.diagnostic.open_float, desc = 'Show error' },

    { '<leader>h', group = 'Help' },
    { '<leader>hh', builtin.help_tags, desc = 'Search help tags' },
    { '<leader>hk', builtin.keymaps, desc = 'Search normal mode keymaps' },

    { '<leader>j', group = 'Jump' },
    { '<leader>jw', "<Plug>(leap)", desc = 'Leap to word' },
    { '<leader>jd', "<C-i>", desc = 'Go to' },
    { '<leader>jb', "<C-o>", desc = 'Go back' },

    { '<leader>l', group = 'LSP' },
    { '<leader>l?', "<cmd>checkhealth vim.lsp<cr>", desc = 'Check LSP' },
    { '<leader>le', vim.lsp.buf.code_action, desc = 'Execute code action' },
    { '<leader>lfd', vim.lsp.buf.definition, desc = 'Find definition' },
    { '<leader>lfr', vim.lsp.buf.references, desc = 'Find references' },
    { '<leader>lF', function() vim.lsp.buf.format({ async = true }) end, desc = 'Format' },
    { '<leader>ld', vim.lsp.buf.hover, desc = 'Describe' },
    { '<leader>lr', vim.lsp.buf.rename, desc = 'Rename symbol' },
    { '<leader>lh', vim.lsp.buf.document_highlight, desc = 'Highlight symbol' },

    { '<leader>p', group = 'Project' },
    { '<leader>pf', builtin.find_files, desc = 'Find file' },

    { '<leader>s', group = 'Search' },
    { '<leader>sr', builtin.live_grep, desc = 'Search with ripgrep' },

    { '<leader>w', group = 'Window' },
    { '<leader>ws', "<C-w>w", desc = 'Swap active window' },
    { '<leader>w%', "<C-w>v", desc = 'Split vertically' },
    { '<leader>w\"', "<C-w>s", desc = 'Split horizontally' },
    { '<leader>wd', "<C-w>q", desc = 'Kill current window' },
    { '<leader>wD', "<C-w>o", desc = 'Kill other windows' },
})

