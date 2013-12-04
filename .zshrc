# .zshrc
# (c) 2011 by Augusto Hack <hack dot augusto at gmail dot com>
#

# Force sourcing of /etc/profile (correctly set XDG variables for su and sudo)
. /etc/profile

# load extras if installed on the system
f='/usr/share/zsh/site-contrib/powerline.zsh'
[[ -f $f ]] && . $f
f='/usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh'
[[ -f $f ]] && . $f

#---[ Aliases ]---
function python_fallback(){

    if  [[ $# == 0 ]]; then
        /usr/bin/env python;
        return;
    fi;

    error=$(/usr/bin/env python $@ 2&>1)

    if [[ $? != 0 ]]; then;
        /usr/bin/env python2 $@;

        if [[ $? != 0 ]]; then;
            echo $error;
            return $?;
        fi;
    fi;

}

function wrapper(){

    prog=$1;
    shift;

    pyg_args=$1;
    shift;

    $prog $@ | pygmentize $pyg_args;
}

function smart_listing(){
    # This is a "smart" ls
    # the only reason for this is because the -B flag is not overwritten by the -a/-A flags
    # this is not gonna list vim backup files on normal ls, but is gonna list them when the all flag is set
    show_all=false
    i=1
    for arg in $@; do
        if [[ $arg == '-B' ]]; then
            argv[$i]=()
        elif [[ $arg == '-a' ]]; then 
            show_all=true
            argv[$i]=()
        elif [[ $arg == '-A' ]]; then
            show_all=true
            argv[$i]=()
        fi
        ((i=i+1))
    done

    if ($show_all); then
        show_all='-A'
    else
        show_all='-B'
    fi
    \ls --color=auto $show_all $argv
}

# https://nicholassterling.wordpress.com/2012/03/30/a-zsh-map-function/
function map_() {
  print -- "${(e)2}"
}
function map() {
  typeset f="$1"; shift
  typeset x
  typeset result=0
  for x; map_ "$x" "$f" || result=$?
  return $result
}
function mapa() {
  typeset f="\$[ $1 ]"; shift
  map "$f" "$@"
}

# urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
function urlencode() { php -r '$s = isset($argv[1]) ? $argv[1] : fgets(STDIN); echo urlencode($s) . "\n";' $@ }
function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
function urlencodestream() { mapa urlencode }
function format() { python2.7 -c "import sys; print sys.argv[1].format(*(sys.argv[2:] if len(sys.argv) > 2 else sys.stdin))" $@; }

eval `dircolors`
#alias chromium='chromium --ignore-gpu-blacklist'
#alias ls='ls --color=auto' # replaced by smart_listing
alias ls=smart_listing
alias l=ls
#alias python=python_fallback
#alias grep='grep --color=auto'

# colored pagination 
export ACK_PAGER_COLOR="less -x4SRFX"
if (( ! $+commands[ack] && $+commands[ack-grep] )); then
    alias ack='ack-grep';
fi
#alias grep='grep --color=auto'
#alias grep='ack --pager="less -R"'
#alias ack='ack --pager="less -R"'

if (( $+commands[wdiff] )); then
    # TODO: [- or -] may happen inside a regex, needs to change the start and end for deletion
    filter_changes='/\[-/,/-\]/; /{\+/,/\+}/'

    if (( $+commands[cwdiff] )); then
        function _diff(){ \wdiff $@ | awk $filter_changes | cwdiff -f }
    else
        function _diff(){ \wdiff $@ | awk $filter_changes }
    fi
elif (( $+commands[colordiff] && $+commands[diff] )); then
    function _diff(){ \diff $@ | colordiff }
fi

if whence _diff > /dev/null; then
    alias diff=_diff
fi

alias vi='vim'
alias emacs='emacs -nw'
alias em='emacs'
alias gcc='colorgcc'
#alias cat='wrapper cat -g'
alias ...='cd ../..'
alias ....='cd ../../..'
alias difftree='rsync -crv --dry-run '
alias info='info --vi-keys'

# `alias -s` suffix alias, specifies the program to open the determined suffix
alias -s tex=vim c=vim cpp=vim

#+++[ Aliases ]+++

#---[ Variables ]---

# Environment high level assembly variables
#export hlalib="/mnt/files/files/code/assembly/hla/hlalib"
#export hlainc="/mnt/files/files/code/assembly/hla/include"

# including ~/.bin in the PATH environment variable
#PATH=~/.bin:$PATH:~/.android_sdk/tools:~/.android_sdk/platform-tools
PATH=~/.bin:$PATH
CLASSPATH=~/projects/java:$CLASSPATH
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

#RPHP_IR_PATH=/mnt/ext/var/abs/local/rphp/src/rphp-buil/lib
#RPHP_RUNTIME_PATH=~/.bin
#LD_LIBRARY_PATH=~/.lib:$LD_LIBRARY_PATH
#export RPHP_IR_PATH RPHP_RUNTIME_PATH LD_LIBRARY_PATH

#+++[ Variables ]+++

#---[ System settings ]---
limit	-s coredumpsize 0
umask	0027

#---[ ZSH Options ]---
#
#---Directory---
#
# AUTO_CD		peform cd if a directory name is given
# AUTO_PUSHD		cd push the old directory onto the directory stack
# CD_ABLE_VARS		expand text (if not a command nor a directory in PWD) to ~/text
# PUSH_IGNORE_DUPS	prohibit duplicate directories in the directory stack
# PUSHD_SILENT		do not print directory stack after pushd popd
# PUSHD_TO_HOME		'pushd' acts like 'pushd $HOME'

setopt	AUTO_CD AUTO_PUSHD CD_ABLE_VARS PUSHD_IGNORE_DUPS PUSHD_SILENT PUSHD_TO_HOME

#+++Directory+++

#---Completion---
#
# ALWAYS_TO_END		moves the cursor to the end of the command
# AUTO_LIST		automatcally list choices on ambiguity
# AUTO_PARAM_SLASH
# NO_LIST_BEEP	no beeping

setopt	ALWAYS_TO_END AUTO_LIST	NO_LIST_BEEP

#+++Completion+++

#---Expasion---
#
# EXTENDED_BLOG		treat '#' '~' '^' as part of the expression for filename generation
# GLOB_DOTS		do not require a initial '.' to match
# NOMATCH		if the globbing is unsucessful, leave the string alone
# KSH_GLOB		*, +, and ? have the same meaning as with regular expressions, BUT it needs to be right BEFORE a group, ex.: ?(example)
# PROMPT_SUBST allows expansion and substitution in the prompt

# NO_NOMATCH    set so that `git log HEAD^` does not try to glob and give the error "zsh: no matches found: HEAD^"
setopt	EXTENDED_GLOB GLOB_DOTS NOMATCH KSH_GLOB PROMPT_SUBST 

#+++Expasion+++

#---History---
#
# EXTENDED_HISTORY	save commands in history at this format:
# 			:begin of the command:time elapsed:command
# HIST_IGNORE_DUPS	insert the new command and remove all older duplications of this
# NO_HIST_BEEP		no beeping
# HIST_IGNORE_SPACE	if a command start with ' ' it is not inserted in the history file
# HIST_REDUCE_BLANKS	remove unnecessary blank spaces

setopt	EXTENDED_HISTORY NO_HIST_BEEP HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_REDUCE_BLANKS
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.history
DIRSTACKSIZE=7

#+++HISTORY+++

#---I/O---
# CORRECT_ALL		try to correct everythin on the line
# RM_STAR_WAIT		ignore keyboard input for 10 sec when 'rm *' or 'rm PATH *'

setopt	CORRECT_ALL RM_STAR_WAIT

#+++I/O+++

#---Job Control---
#
# LONG_LIST_JOBS	use long list as default
# NOTIFY		immediate report of the status of bg jobs
# NO_BG_NICE		run bg jobs as fg jobs
# NO_HUP		continue running bg jobs even of shell is closed

setopt	LONG_LIST_JOBS NOTIFY NO_BG_NICE NO_HUP

#+++Job Control+++

#---other---
setopt	NO_BEEP MULTIOS
#+++other+++

#---[ Modules ]---

fpath=($fpath $HOME/.zsh/func)
typeset -U fpath

source ~/.zsh/func/git-extras.plugin.zsh

zmodload zsh/complist
autoload -Uz compinit
autoload -U promptinit
autoload -U zgitinit add-zsh-hook
compinit
# incompatibily with unix command
#zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile

#+++[ Modules ]+++

#---[ Completition system ]---

# the widget complete-word must be used to do the completion instead of expand-complete,
# otherwise the _expand completer will not work
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
#zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

zstyle :compinstall filename '/home/hack/.zshrc'

# pip zsh completion start
function _pip_completion {
    local words cword
    read -Ac words
    read -cn cword
    reply=( $( COMP_WORDS="$words[*]" \
        COMP_CWORD=$(( cword-1 )) \
        PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip2

#+++[ Completition system ]+++

#---Git---
zgitinit
git_filestatus() {
    zgit_isgit      || return 1
    zgit_inworktree || return 1

    local output

    if zgit_hasunmerged; then
        output='%F{red}!%f'
    elif zgit_hasuntracked; then
        output='%F{blue}?%f'
    elif ! zgit_isworktreeclean; then
        output='%F{yellow}#%f'
    elif zgit_hasuncommited; then
        output='%F{white}#%f'
    else
        output='%F{green}#%f'
    fi

    echo -n $output

    return 0
}

git_branchstatus() {
    zgit_isgit || return 1

    local -a ahead behind 

    if zgit_tracking_merge &> /dev/null; then
        echo -n "  %F{green}[git:$(zgit_head)"

        ahead=($(git rev-list --reverse $(zgit_tracking_merge)..HEAD))
        behind=($(git rev-list --reverse HEAD..$(zgit_tracking_merge)))

        if [ $#ahead -gt 0 ]; then
            echo -n " +$#ahead"
        elif [ $#behind -gt 0 ]; then
            echo -n " -$#behind"
        fi
    else
        echo -n "  %F{yellow}[git:$(zgit_head)"
    fi

    echo "]%f"
}

#+++Git+++

#---Prompt---

myprompt() {
    git_filestatus || echo ' #'
}

export PS2="> "

if [[ $UID -eq '0' ]]; then;
    export PS1="%F{red}%n%f %1. %(?.%#.%F{red}%#%f) "
    export RPS1="%F{magenta}%T%f"
elif [[ $USER = 'dev' ]]; then;
    export PS1="%F{yellow}%n%f %1.%# "
    export RPS1="%F{magenta}%T%f"
else;
    export PS1="%F{cyan}%n%f %1.%\$(git_branchstatus) \$(myprompt) "
    export RPS1="%F{magenta}%T%f"
fi;

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    if [[ $UID -eq '0' ]]; then;
        export PS1="%M $PS1"
    elif [[ $USER = 'dev' ]]; then;
        export PS1="%M $PS1"
    else;
        export PS1="%M $PS1"
    fi;
fi; 
#add-zsh-hook precmd gitprompt


#+++Prompt+++


#+++[ ZSH OPTIONS ]+++

# Don't expand files matching:
fignore=(.o .old .pro .pyc \~)

#---[ Environment ]---

export EDITOR=vim

#---Manual--- 

# search order
export MANSECT=3:1:9:8:2:5:4:7:6:n

# highlight with most
# PAGER='most -s'
# BROWSER='most -s'

# highlight with less
PAGER='less'
BROWSER='less'

export LESS='-R'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# UTF-8 Sexiness!
#export LESSCHARSET="utf-8"
#export LOCALE="en_US.utf8"
#export LANG=en_US.utf8
#export LC="en_US.utf8"
#export LC_CTYPE="en_US.utf8"
#export LC_NUMERIC="en_US.utf8"
#export LC_TIME="en_US.utf8"
#export LC_COLLATE="en_US.utf8"
#export LC_MONETARY="en_US.utf8"
#export LC_MESSAGES="en_US.utf8"
#export LC_PAPER="en_US.utf8"
#export LC_NAME="en_US.utf8"
#export LC_ADDRESS="en_US.utf8"
#export LC_TELEPHONE="en_US.utf8"
#export LC_MEASUREMENT="en_US.utf8"
#export LC_IDENTIFICATION="en_US.utf8"
#export LC_ALL="en_US.utf8"

#+++Manual+++

#+++[ Environment ]+++

#---[ Key bindings ]---

# escape sequences 'instring' found as ^v followed by the keys on the terminal
# special commands found on zshzle
bindkey -v # explicitily set viins as main keymap

bindkey "^I" complete-word
if [[ $TERM = 'screen' ]]; then;
    bindkey '[1~'	beginning-of-line # Home
    bindkey '[2~'	overwrite-mode	  # Insert
    bindkey '[3~'	delete-char	      # Del
    bindkey '[4~'	end-of-line	      # End	
    bindkey 'OD'	backward-word	  # CTRL <-
    bindkey 'OC'	forward-word	  # CTRL ->
    bindkey '[Z'  reverse-menu-complete
    bindkey '[A'  history-beginning-search-backward # CTRL up
    bindkey '[B'  history-beginning-search-forward  # CTRL down
elif [[ $TERM = 'rxvt' ]]; then;
    bindkey '[7~'	beginning-of-line # Home
    bindkey '[2~'	overwrite-mode	  # Insert
    bindkey '[3~'	delete-char	  # Del
    bindkey '[8~'	end-of-line	  # End	
    bindkey 'Od'	backward-word	  # CTRL <-
    bindkey 'Oc'	forward-word	  # CTRL ->
elif [[ $TERM = 'linux' ]]; then;
    bindkey '[1~'	beginning-of-line # Home
    bindkey '[2~'	overwrite-mode	  # Insert
    bindkey '[3~'	delete-char	  # Del
    bindkey '[4~'	end-of-line	  # End	
    # bindkey '[D'	backward-word	  # CTRL <-
    # bindkey '[C'	forward-word	  # CTRL ->
elif [[ $TERM = 'xterm' ]]; then;
    bindkey 'OH'	beginning-of-line # Home
    bindkey '[2~'	overwrite-mode	  # Insert
    bindkey '[3~'	delete-char	  # Del
    bindkey 'OF'	end-of-line	  # End	
    bindkey '[1;5D'	backward-word	  # CTRL <-
    bindkey '[1;5C'	forward-word	  # CTRL ->
fi


#+++[ Key bidings ]+++

#---[ Startup ]---

# to run `xargs zsh -i -c "shell_function"` without showing fortune
if [[ -z $_FORTUNE ]]; then
    fortune
    export _FORTUNE=1
fi

#+++[ Startup ]+++

