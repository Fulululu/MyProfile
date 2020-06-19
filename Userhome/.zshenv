typeset -U path
path=($HOME/bin /usr/local/bin /usr/local/sbin /usr/local/include $path[@])

# golang
export GOPATH=$HOME/go
path=($GOPATH/bin $path[@])

