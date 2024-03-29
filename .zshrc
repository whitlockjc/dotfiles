# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="jeremy"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#
# Interesting: emacs golang tmux
plugins=(colorize docker git golang)

# User configuration (Done here to allow for overriding oh-my-zsh configuration)

if [ -e $HOME/.zshrc-env-config ];
then
   source $HOME/.zshrc-env-config
fi

source $ZSH/oh-my-zsh.sh

# Aliases
alias emacs="emacsclient -t"
alias emacsw="emacsclient -c -a emacs"
alias ls="ls --color=auto"

# NVM
if [ $NVM_HOME ];
then
    export NVM_DIR="$HOME/.nvm"

    . "$NVM_HOME/nvm.sh"

    export NODE_PATH=$NODE_PATH:`npm root -g`
fi

# iTerm2
export ITERM2_SQUELCH_MARK=1

if [ -f "${HOME}/.iterm2_shell_integration.zsh" ]; then
    source "${HOME}/.iterm2_shell_integration.zsh"
fi

iterm2_print_user_vars() {
    UPTIME=$(uptime | sed 's/^.*up//' | sed 's/,//g' | tr -s ' ' | sed 's/ [0-9][0-9]* users .*//' | awk '{split($0,a,":"); if (length(a) == 2) {print a[1]" hours "a[2]" mins"} else {print $0}}')
    iterm2_set_user_var uptime ${UPTIME}
}

# Terminal information
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

if [ $INSIDE_EMACS ];
then
    unset zle_bracketed_paste
fi

export TERM=xterm-256color

# Perl support
PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;

# Google Cloud SDK Support
# The next line updates PATH for the Google Cloud SDK.
if [ -f "${GCLOUD_SDK}/path.zsh.inc" ]; then
  source "${GCLOUD_SDK}/path.zsh.inc"
fi

# The next line enables shell command completion for gcloud.
if [ -f "${GCLOUD_SDK}/completion.zsh.inc" ]; then
  source "${GCLOUD_SDK}/completion.zsh.inc"
fi

