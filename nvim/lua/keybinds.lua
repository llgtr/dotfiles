-- Leaders
vim.keymap.set('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Bindings
require('which-key').add({
    { '<leader><leader>', "<Plug>(leap)", desc = 'Leap' },
    { '<leader><Tab>', "<C-w>w", desc = 'Swap active window' },

    { '<leader>b', group = 'Buffer' },
    { '<leader>bb', "<cmd>lua require('telescope.builtin').buffers()<cr>", desc = 'List buffers' },
    { '<leader>bd', "<cmd>bdelete<cr>", desc = 'Kill current buffer' },
    { '<leader>bD', "<cmd>%bdelete<cr>", desc = 'Kill all buffers' },

    { '<leader>e', group = 'Error' },
    { '<leader>ep', function() vim.diagnostic.jump({ count = -1 }) end, desc = 'Previous error' },
    { '<leader>en', function() vim.diagnostic.jump({ count = 1 }) end, desc = 'Next error' },
    { '<leader>el', vim.diagnostic.setloclist, desc = 'List local errors' },
    { '<leader>ee', vim.diagnostic.open_float, desc = 'Show error' },

    { '<leader>h', group = 'Help' },
    { '<leader>hh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", desc = 'Search help tags' },
    { '<leader>hk', "<cmd>lua require('telescope.builtin').keymaps()<cr>", desc = 'Search normal mode keymaps' },

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
    { '<leader>pf', "<cmd>lua require('telescope.builtin').find_files()<cr>", desc = 'Find file' },

    { '<leader>s', group = 'Search' },
    { '<leader>sr', "<cmd>lua require('telescope.builtin').live_grep()<cr>", desc = 'Search with ripgrep' },

    { '<leader>w', group = 'Window' },
    { '<leader>ws', "<C-w>w", desc = 'Swap active window' },
    { '<leader>w%', "<C-w>v", desc = 'Split vertically' },
    { '<leader>w\"', "<C-w>s", desc = 'Split horizontally' },
    { '<leader>wd', "<C-w>q", desc = 'Kill current window' },
    { '<leader>wD', "<C-w>o", desc = 'Kill other windows' },
})

