# Makes colorizing output a bit easier.
# $1: string to be colorized
# $2: tput color code
# $3: flag for bold text
function echo_c {
    local setcolor="$(tput setaf "$2")"
    local setnormal="$(tput sgr0)"
    [[ ${3:-} == "-b" ]] && local setbold="$(tput bold)"

    echo "${setbold:-}${setcolor}$1${setnormal}"
}

# Timer for various simple timing needs.
# $1: time in seconds (mandatory)
function timer {
    if [[ $1 == "" ]]; then
        echo "$(echo_c "ERROR:" 1 -b) Missing parameter"
        return
    elif ! [[ $1 =~ '^[0-9]+$' ]]; then
        echo "$(echo_c "ERROR:" 1 -b) Not a number"
        return
    fi

    for i in {1..$1}; do sleep 1; echo -n -e $i\\r; done
}

# Find process using a port
# $1: port number
function port_user() {
    if [[ $# -eq 1 ]]; then
       lsof -iTCP -sTCP:LISTEN -n -P | grep -i --color $1
    else
       lsof -iTCP -sTCP:LISTEN -n -P
    fi
}

function fu() {
    if [[ -v FNM_DIR ]]; then
        fnm use
    else
        eval "$(fnm env)" && fnm use
    fi
}
