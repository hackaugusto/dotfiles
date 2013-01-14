# .zshrc
# (c) 2011 by Augusto Hack <hack dot augusto at gmail dot com>
#

# Force sourcing of /etc/profile (correctly set XDG variables for su and sudo)
. /etc/profile

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

eval `dircolors`
#alias ls='ls --color=auto' # replaced by smart_listing
alias ls=smart_listing
alias python=python_fallback
#alias grep='grep --color=auto'

# colored pagination 
export ACK_PAGER_COLOR="less -x4SRFX"
if (( ! $+commands[ack] && $+commands[ack-grep] )); then;
    alias ack='ack-grep';
fi

alias grep='ack'
alias vi='vim'
alias gcc='colorgcc'
#alias cat='wrapper cat -g'
alias ...='cd ../..'
alias ....='cd ../../..'

# `alias -s` suffix alias, specifies the program to open the determined suffix
alias -s tex=vim c=vim cpp=vim

#+++[ Aliases ]+++

#---[ Variables ]---

# Environment high level assembly variables
#export hlalib="/mnt/files/files/code/assembly/hla/hlalib"
#export hlainc="/mnt/files/files/code/assembly/hla/include"

# including ~/.bin in the PATH environment variable
PATH=~/.bin:$PATH:~/.android_sdk/tools:~/.android_sdk/platform-tools
CLASSPATH=~/projects/java:$CLASSPATH

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

setopt	EXTENDED_GLOB GLOB_DOTS NOMATCH KSH_GLOB

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

#---Prompt---

export PS2="> "

if [[ $UID -eq '0' ]]; then;
    export PS1="%F{red}%n%f %1. %(?.%#.%F{red}%#%f) "
    export RPS1="%F{magenta}%T%f"
elif [[ $USER = 'dev' ]]; then;
    export PS1="%F{yellow}%n%f %1.%# "
    export RPS1="%F{magenta}%T%f"
else;
    export PS1="%F{cyan}%n%f %1.%# "
    export RPS1="%F{magenta}%T%f"
fi;


#+++Prompt+++


#+++[ ZSH OPTIONS ]+++

# Don't expand files matching:
fignore=(.o .c~ .old .pro)

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
if [[ $TERM = 'screen' ]]; then;
	bindkey '[1~'	beginning-of-line # Home
	bindkey '[2~'	overwrite-mode	  # Insert
	bindkey '[3~'	delete-char	  # Del
	bindkey '[4~'	end-of-line	  # End	
	bindkey 'OD'	backward-word	  # CTRL <-
	bindkey 'OC'	forward-word	  # CTRL ->
    bindkey '[Z'  reverse-menu-complete
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
	bindkey '[D'	backward-word	  # CTRL <-
	bindkey '[C'	forward-word	  # CTRL ->
elif [[ $TERM = 'xterm' ]]; then;
	bindkey 'OH'	beginning-of-line # Home
	bindkey '[2~'	overwrite-mode	  # Insert
	bindkey '[3~'	delete-char	  # Del
	bindkey 'OF'	end-of-line	  # End	
	bindkey '[1;5D'	backward-word	  # CTRL <-
	bindkey '[1;5C'	forward-word	  # CTRL ->
fi


#+++[ Key bidings ]+++

#---[ Completition system ]---

zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/hack/.zshrc'

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

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

#---[ Modules ]---

zmodload zsh/complist
autoload -Uz compinit
compinit
# incompatibily with unix command
#zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile

#+++[ Modules ]+++

#---[ Startup ]---

fortune

#+++[ Startup ]+++

