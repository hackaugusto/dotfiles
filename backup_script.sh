#!/usr/bin/env zsh
# vim:set ts=4 sts=4 sw=4 et:

if [[ $ARGC != 2 ]]; then;
    echo "Usage: $@[0] config_file destination_folder"
    exit
fi

config=$@[1]
dest=$@[2]

cat $config | while read line; do;
    file=${(e)line};

    if [[ -n $file ]]; then;

        if [[ -a $file ]]; then;
            # cp -R $file $dest
        else
            echo "File '$file' does not exist."
        fi
    fi
done

cd $dest

git add *
git commit -m 'automatic update';
git push origin;

