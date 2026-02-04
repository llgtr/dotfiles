set runtimepath^=~/.vim
source ~/.vimrc

" function! LspStatus()
"     let clients = luaeval('vim.lsp.get_clients({bufnr=' . bufnr('%') . '})')
"     if len(clients) > 0
"         return 'LSP'
"     else
"         return 'NO LSP'
"     endif
" endfunction
" 
" let current = &statusline
" let &statusline = current . '%#StatusLineNC# %{LspStatus()} '

lua require("setup")
lua require("packages")
lua require("keybinds")
lua require("lsp")
