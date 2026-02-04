local configs = {
    'ts_ls',
    'pyright',
    'rust_analyzer',
    'zls',
}

for _, config in ipairs(configs) do
    vim.lsp.enable(config)
end
