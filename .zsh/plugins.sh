if ! zgen saved; then
    echo "Creating a zgen save"

    zgen load StackExchange/blackbox
    zgen save
fi
