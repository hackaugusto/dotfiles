
# Font

https://www.codingfont.com/

https://github.com/RedHatOfficial/RedHatFont


# MacOS

As of Monterey 12.6 the system ncurses is `5.7.20081102`:

```
ncurses5.4-config --version
```

And it does not have tmux in its database:

```sh
infocmp tmux
```

So using `set -g default-terminal tmux` does not work and breaks ZSH.
