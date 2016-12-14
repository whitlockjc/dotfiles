# dotfiles

The purpose of this repository is to store my dotfiles and OS agnostic tooling configurations.  As you well know, the
majority of how you configure your tools is the same across different systems other than some minor environmental
differences.  Where this is the case, like with emacs and my zsh, I have extracted the
_environment-specific configuration_ into a convention-based approach where a file is sourced to gather this
information.  Where these files are necessary are documented below.

## emacs

### Installation

```
cd ~
ln -sf $SRC_ROOT/.emacs .
ln -sf $SRC_ROOT/.emacs.d .
```

### Configuration

There are various environment variables that Emacs uses and while Emacs can successfully pull these from your shell
environment in most cases, if you launch Emacs from outside your shell _(like when using `/Applications/Emacs.app` on
OS X)_ you need to tell Emacs these things explicitly.  To handle this, I have stored all environment-specific
configuration in `~/.emacs.d/env-config.el` and `~/.emacs` loads this file to gather the information.  Here is an
example:

``` emacs-lisp
;;; env-config.el --- Environment-specific Emacs Configuration -*- lexical-binding: t; -*-
;;;
;;; This file contains various configuration variables used in the Emacs configuration files:
;;;
;;; * env-config-gopath: This should be set to the same value your GOPATH environment variable is set to in your shell
;;; * env-config-goroot: This should be set to the same value your GOROOT environment variable is set to in your shell
;;; * env-config-path:   This should be set to the same value your PATH environment variable is set to in your shell
;;; * env-config-shell:  This should be set to the same value your SHELL environment variable is set to in your shell
;;;
;;; The reason you have to duplicate your shell environment variables in your Emacs configuration is that Emacs cannot
;;; gather your shell-provided environment variables when launching Emacs from the UI.


;;; Local variables
(setq env-config-home "/Users/not-you")
(setq env-config-brew-home (format "/usr/local" env-config-home))

;;; Variables used elsewhere
(setq env-config-gopath (format "%s/development/go" env-config-home))
(setq env-config-goroot (format "%s/opt/go/libexec" env-config-brew-home))
(setq env-config-path (format "%s/bin:%s/libexec/bin:%s/bin" env-config-goroot env-config-goroot env-config-brew-home))
(setq env-config-shell (format "%s/bin/zsh" env-config-brew-home))
```

Also, various Emacs packages have external dependencies that need to be installed for the package to work.  These are
listed below.

#### Go

* Install [gocode](https://github.com/nsf/gocode) _(`go get -u github.com/nsf/gocode`)_
* Install [godef](https://github.com/rogpeppe/godef) _(`go get -u github.com/rogpeppe/godef`)_
* Install [golint](https://github.com/golang/lint) _(`go get -u github.com/golang/lint/golint`)_
* Install [gorename](https://godoc.org/golang.org/x/tools/cmd/gorename) _(`go get -u golang.org/x/tools/cmd/gorename`)_
* Install [guru](https://docs.google.com/document/d/1_Y9xCEMj5S-7rv2ooHpZNH15JgRT5iM742gJkw5LtmQ/edit) _(`go get -u golang.org/x/tools/cmd/guru`)_

#### JavaScript

* Install Node.js for your JavaScript interpreter
* Install tern.js for code assist and completion _(`npm install tern`)_

## tmux

The current tmux configuration is cross-platform and does not require any extra configuration.

### Install

```
cd ~
ln -sf $SRC_ROOT/.tmux.conf .
```

## zsh (and oh-my-zsh)

### Installation

```
cd ~
ln -sf $SRC_ROOT/.zshrc
mkdir -p ~/.oh-my-zsh/custom/themes
ln -sf $SRC_ROOT/.oh-my-zsh/custom/themes/jeremy.zsh-theme ~/.oh-my-zsh/custom/themes
```

### Configuration

Your shell's environment variables, and configuration, will change based on what OS you're on.  To handle this, I have
stored all environment-specific configuration in `~/.zshrc-env-config` and `~/.zshrc` sources this file to gather the
information.  Here is an example:

``` shell
# Local variables
BREW_HOME=/usr/local
NVM_HOME=$BREW_HOME/opt/nvm

# Exported variables
export EDITOR=emacs
export GOPATH=$HOME/development/go
export GOROOT=$HOME/tools/brew/opt/go/libexec
export LD_LIBRARY_PATH=$HOME/tools/brew/lib:$LD_LIBRARY_PATH
export PATH=$HOME/tools/brew/bin:$GOPATH/bin:$GOROOT/bin:$PATH
```
