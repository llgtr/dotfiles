# Dotfiles.

Run `install` to get started.

### Git subtrees

Subtrees are used to include dependencies for this repo. This ensures that the configuration is easy to clone and will be independent from upstream changes. Use the following commands to add and update:

* Add remote (optional)
  * `git remote add <name> <git_url>`

* Add subtree
  * `git subtree add --prefix <path_from_root> <remote_name / git_url> master --squash`

* Update subtree
  * `git subtree pull --prefix <path_from_root> <remote_name / git_url> master --squash`

### Credits/Thanks

* [Mathias Bynens' .macos script](https://github.com/mathiasbynens/dotfiles/blob/master/.macos)

* [nobootsound](https://github.com/teored90/nobootsound)

* [Holman's bootstrap](https://github.com/holman/dotfiles/blob/master/script/bootstrap)

* [base16](https://github.com/chriskempson/base16)
