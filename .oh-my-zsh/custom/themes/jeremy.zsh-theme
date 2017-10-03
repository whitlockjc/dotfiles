# Personal Theme for Jeremy
#
# @author Jeremy Whitlock <jcwhitlock@google.com>

# Custom colors
eval my_blue='$FG[075]'
eval my_gray='$FG[245]'
eval my_green='$FG[010]'
eval my_orange='$FG[214]'
eval my_purple='$FG[105]'
eval my_yellow='$FG[011]'

function location_info() {
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ];
    then
        echo "$my_gray($USER@%M)%{$reset_color%} "
    fi
}

PROMPT='$FX[bold]$my_orange➜  $(location_info)$FX[bold]$my_blue%5c%{$reset_color%} %(1j.$my_gray(${my_purple}jobs${my_gray}:$my_orange%j$my_gray)%{$reset_color%} .)$(git_prompt_info)% %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_CLEAN="$my_gray)%{$reset_color%} $FX[bold]$my_green✔ "
ZSH_THEME_GIT_PROMPT_DIRTY="$my_gray)%{$reset_color%} $FX[bold]$my_yellow✗ "
ZSH_THEME_GIT_PROMPT_PREFIX="$my_gray(${my_purple}git${my_gray}:$my_orange"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
