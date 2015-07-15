#!/usr/bin/sh

function escape_sequence() {
    echo "$(tput kich1)$(tput khome)$(tput kpp)$(tput kdch1)$(tput kend)$(tput knp)$(tput kcuu1)$(tput kcub1)$(tput kcud1)$(tput kcuf1)" |
        od -x | md5sum | cut -d' ' -f1;
}

# todo: this closes the subshell, not what I want
echo 'type the keys "insert, home, page up, delete, end, page down, arrow up, arrow left, arrow down, arro right, enter" in sequence followed by a single <ctrl> + d'
_sum=$(cat | od -x | md5sum | cut -d' ' -f1)
#cd /usr/share/terminfo;
#ls */* | while read l; do
#  TERM=${l#*/};
cd /usr/share/terminfo/s;
ls * | while read l; do
 TERM=$l;
  [[ $(escape_sequence) == $_sum ]] && echo $l;
done | grep '^screen' 2&> /dev/null
