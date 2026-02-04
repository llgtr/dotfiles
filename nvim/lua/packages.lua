vim.pack.add({
    { src = "https://github.com/nvim-lua/plenary.nvim.git", version = "master" },
    { src = "https://github.com/nvim-telescope/telescope.nvim.git", version = "v0.2.1" },
    { src = "https://github.com/folke/which-key.nvim.git", version = "v3.17.0" },
    { src = "https://codeberg.org/andyg/leap.nvim.git" },
    { src = "https://github.com/saghen/blink.cmp.git", version = "v1.9.0" },
})

require('which-key').setup({
    preset = 'modern',
    icons = {
        mappings = false,
    },
    win = {
        title = true,
        title_pos = "left",
    },
    replace = {
        key = {
            function(key)
                if key:find("<") then
                    return string.lower(key)
                else
                    return key
                end
            end
        },
    },
})

require('blink.cmp').setup({
    keymap = { preset = 'default' },
    appearance = { nerd_font_variant = 'mono' },
    completion = {
        documentation = { auto_show = false },
        menu = {
            draw = {
                columns = {
                    { "label", "label_description", gap = 1 },
                    { "kind" } 
                },
            },
        },
    },
    sources = { default = { 'lsp', 'path' } },
    fuzzy = { implementation = "prefer_rust_with_warning" }
})

