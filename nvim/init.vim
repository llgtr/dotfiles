set runtimepath^=~/.vim
source ~/.vimrc

lua require("setup")
lua require("packages")
lua require("keybinds")
lua require("lsp")
