-- Setup the diagnostic framework with desired visuals
vim.diagnostic.config({
    virtual_text = true,
    signs = false,
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = '',
            [vim.diagnostic.severity.WARN] = '',
            [vim.diagnostic.severity.INFO] = '',
            [vim.diagnostic.severity.HINT] = '',
        },
        numhl = {
            [vim.diagnostic.severity.ERROR] = 'DiagnosticError',
            [vim.diagnostic.severity.WARN] = 'DiagnosticWarn',
            [vim.diagnostic.severity.INFO] = 'DiagnosticInfo',
            [vim.diagnostic.severity.HINT] = 'DiagnosticHint',
        },
    },
    float = { border = 'rounded', source = 'if_many' },
    jump = { float = true },
})

-- Setup "annex" to statusline that shows diagnostics and lsp state
local sl_bg = vim.api.nvim_get_hl(0, { name = 'StatusLineNC' }).bg

for _, level in ipairs({ 'Error', 'Warn', 'Info', 'Hint' }) do
    local diag_fg = vim.api.nvim_get_hl(0, { name = 'Diagnostic' .. level }).fg
    vim.api.nvim_set_hl(0, 'StatusLineDiagnostic' .. level, { fg = diag_fg, bg = sl_bg })
end

function lsp_info()
    local buf_clients = vim.lsp.get_clients()
    if #buf_clients == 0 then
        return '✘'
    end

    -- NOTE: `count(0)` gets count for current buffer only
    local counts = vim.diagnostic.count(0)
    local errors = counts[vim.diagnostic.severity.ERROR] or 0
    local warnings = counts[vim.diagnostic.severity.WARN] or 0
    local infos = counts[vim.diagnostic.severity.INFO] or 0
    local hints = counts[vim.diagnostic.severity.HINT] or 0
    local total = errors + warnings + infos + hints

    if total == 0 then
        return '✔'
    end

    local parts = {}
    table.insert(parts, '%#StatusLineDiagnosticError#' .. errors .. '%#StatusLineNC#')
    table.insert(parts, '%#StatusLineDiagnosticWarn#' .. warnings .. '%#StatusLineNC#')
    table.insert(parts, '%#StatusLineDiagnosticInfo#' .. infos .. '%#StatusLineNC#')
    table.insert(parts, '%#StatusLineDiagnosticHint#' .. hints .. '%#StatusLineNC#')

    return table.concat(parts, ', ')
end

local current = vim.api.nvim_get_option('statusline')
vim.opt.statusline = current .. '%#StatusLineNC# %{%v:lua.lsp_info()%} '

-- Setup autocommand for clearing LSP highlights on cursor move
vim.api.nvim_create_autocmd({ "CursorMoved", "InsertEnter" }, {
    group = vim.api.nvim_create_augroup("LspRefHighlight", { clear = true }),
    callback = function()
        vim.lsp.buf.clear_references()
    end,
})
