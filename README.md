# VAmacs

![VAmacs](logo.png)

Personal Emacs config.

It is compatible **ONLY with GNU Emacs 25.1 and above**.

## Features

- Out of box.
- Clean and Fast.
- Quick fuzzy search.
- Better Org support.
- Support multiple programming languages
  - C/C++/Java
  - Python/PHP/Shell/Powershell
  - Javascript
  - HTML/CSS/XML
  - Markdown
  - ...
- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git/SVN integration.
- Projectile integration.
- Workspace integration.

## Prerequiste

### OS

- GNU Linux
- macOS
- Windows (Cygwin)

### GNU Emacs

Please refer to [Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

## Quick Start

### Install

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
git clone --depth 1 https://gitlab.com/vikasadiwal/emacs.d.git ~/.emacs.d
```

Start Emacs and wait for it to download and compile all the packages on the
initial startup.
Enjoy!

### Update

``` emacs-lisp
# Update VAmacs (then restart), including configurations and packages
M-x va-update
M-x va-update-and-restart

# Update Emacs configurations only
M-x va-update-config

# Update packages only (then restart)
M-x va-update-packages
M-x va-update-packages-and-restart

# Update all including configurations, packages and dotfiles
M-x va-update-all
```

## Customization

### Customize-group

`M-x customize-group` and select `va`. Set and save the configurations,
then restart Emacs.

### Manual

Copy `custom-template.el` to `custom.el` and change the configurations, then
restart Emacs.

For Example:

``` emacs-lisp
(setq va-logo nil)                        ; Logo file or nil (official logo)
(setq va-full-name "user name")           ; User full name
(setq va-mail-address "user@email.com")   ; Email address
(setq va-proxy "127.0.0.1:1080")          ; Network proxy
(setq va-theme classic)                   ; Color theme: default, classic, dark, light or daylight
(setq va-cnfonts t)                       ; Use cnfonts or not: t or nil
(setq va-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq va-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
(setq va-ivy-icon nil)                    ; Display icons in ivy or not: t or nil
(setq va-pretty-magit nil)                ; Prettify magit or not: t or nil
(setq va-company-enable-yas t)            ; Enable yasnippet for company or not: t or nil
(setq va-benchmark t)                     ; Enable initialization benchmark or not: t or nil
```

The default pacakge archives is `melpa`. You can change it in `custom.el`, or
switch manually via `M-x switch-package-archives` anytime.

For the personal configurations, you could put to `~/.emacs.d/custom-post.el`.

## Screenshots

![Dasboard](/uploads/02d6eb934f834ed03e5b184a05663fce/Screenshot_from_2019-01-08_14-40-57.png)


## FAQ

1. Why is the modline messy?

    Powerline fonts or all-the-icons are missing on your system. Please install
    [powerline-fonts](https://github.com/powerline/fonts) for `telephone-line` or
    run `M-x all-the-icons-install-fonts` for `doom-modeline`.

1. How to use the VAmacs Dashboard?

    Set `(setq va-dashboard t)` in `~/.emacs.d/custom.el`. Dashboard will
    be opened at startup. After startup, you could use `F2` to reopen it anytime.
    In the dashboard, you could easily jump to Homepage(`H`), Restore
    Session(`R`), Edit Config (`E`), Update(`U`), Recent Files (`r`),
    Bookmarks(`m`) and Projects(`p`).

1. Does VAmacs support Language Server Protocol (LSP)?

    LSP is supported and enabled by default in Centuar Emacs now. `eglot` is the
    default client, and `lsp-mode` is another choice. Before use it you should
    install language servers as below. Use `(setq va-lsp nil)` to disable
    `LSP` if you don't like it.
    - Golang: `go get -u github.com/sourcegraph/go-langserver`
    - Python: `pip install python-language-server`
    - Ruby:  `gem install solargraph`
    - Javascript/Typescript: `npm i -g javascript-typescript-langserver`
    - CSS: `npm i -g vscode-css-languageserver-bin`
    - HTML: `npm i -g vscode-html-languageserver-bin`
    - Bash/Shell: `npm i -g bash-language-server`. Require Python2.5+, use
      `--python` to specify.
    - C/C++/Objective-C : `brew install cquery` or dwonload binary from
      [here](https://github.com/cquery-project/cquery/releases).
    - Rust: `rustup component add rls-preview rust-analysis rust-src`
    - Java:
      ``` shell
      wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
      tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
      ```
    - PHP: refer to the [installation
      guide](https://github.com/felixfbecker/php-language-server#installation).
      ``` shell
      composer require felixfbecker/language-server
      composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs
      ```

1. How to enable `plantuml` in `org-mode`?

    Put `(setq org-plantuml-jar-path "<path of plantumx.x.x.jar>")` in `custom.el`.
