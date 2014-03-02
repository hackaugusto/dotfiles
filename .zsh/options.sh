limit -s coredumpsize 0
fignore=(.o .old .pro .pyc \~)
umask 0027

# AUTO_CD		        peform cd if a directory name is given
# AUTO_PUSHD		    cd push the old directory onto the directory stack
# CD_ABLE_VARS		    expand text (if not a command nor a directory in PWD) to ~/text
# PUSH_IGNORE_DUPS	    prohibit duplicate directories in the directory stack
# PUSHD_SILENT		    do not print directory stack after pushd popd
# PUSHD_TO_HOME		    'pushd' acts like 'pushd $HOME'
setopt AUTO_CD AUTO_PUSHD CD_ABLE_VARS PUSHD_IGNORE_DUPS PUSHD_SILENT PUSHD_TO_HOME

# ALWAYS_TO_END		    moves the cursor to the end of the command
# AUTO_LIST		        automatcally list choices on ambiguity
# AUTO_PARAM_SLASH
# NO_LIST_BEEP	        no beeping
# NO_BEEP	            beep! beep!
setopt ALWAYS_TO_END AUTO_LIST	NO_LIST_BEEP NO_BEEP

# EXTENDED_BLOG		    treat '#' '~' '^' as part of the expression for filename generation
# GLOB_DOTS		        do not require a initial '.' to match
# NOMATCH		        if the globbing is unsucessful, leave the string alone
# KSH_GLOB		        *, +, and ? have the same meaning as with regular expressions, BUT it needs to be right BEFORE a group, ex.: ?(example)
# PROMPT_SUBST          allows expansion and substitution in the prompt
# NO_NOMATCH            set so that `git log HEAD^` does not try to glob and give the error "zsh: no matches found: HEAD^"
setopt EXTENDED_GLOB GLOB_DOTS NOMATCH KSH_GLOB PROMPT_SUBST 

# EXTENDED_HISTORY	    save commands in history at this format ":begin of the command:time elapsed:command"
# HIST_IGNORE_DUPS	    insert the new command and remove all older duplications of this
# NO_HIST_BEEP		    no beeping
# HIST_IGNORE_SPACE	    if a command start with ' ' it is not inserted in the history file
# HIST_REDUCE_BLANKS	remove unnecessary blank spaces
setopt EXTENDED_HISTORY NO_HIST_BEEP HIST_IGNORE_ALL_DUPS HIST_IGNORE_SPACE HIST_REDUCE_BLANKS
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.history
DIRSTACKSIZE=7

# CORRECT_ALL		    try to correct everythin on the line
# RM_STAR_WAIT		    ignore keyboard input for 10 sec when 'rm *' or 'rm PATH *'
setopt CORRECT_ALL RM_STAR_WAIT

# LONG_LIST_JOBS	    use long list as default
# NOTIFY		        immediate report of the status of bg jobs
# NO_BG_NICE		    run bg jobs as fg jobs
# NO_HUP		        continue running bg jobs even of shell is closed
setopt LONG_LIST_JOBS NOTIFY NO_BG_NICE NO_HUP MULTIOS
