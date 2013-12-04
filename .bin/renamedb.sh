#!/usr/bin/sh

usage=""" $0 <user> <password> <old_database_name> <new_database_name> """

user=$1
pass=$2
old=$3
new=$4

if [[ -z $old ]]; then
    echo $usage
    exit 2;
fi

mysql -u $user -p$pass -e "create database \`$new\`;";

for table in `mysql -u $user -p$pass -B -N -e "show tables;" $old`; {
    mysql -u $user -p$pass -e "rename table \`$old\`.\`$table\` to \`$new\`.\`$table\`"
}
