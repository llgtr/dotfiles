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

# Find process using a port
# $1: port number
function port_user() {
    if [[ $# -eq 1 ]]; then
       lsof -iTCP -sTCP:LISTEN -n -P | grep -i --color $1
    else
       lsof -iTCP -sTCP:LISTEN -n -P
    fi
}

# Wrapper for using fnm without having to load it for each shell
function fu() {
    if [[ -v FNM_DIR ]]; then
        fnm use
    else
        eval "$(fnm env)" && fnm use
    fi
}

# Find and extract files matching given extension from given directory
# $1: File extension
# $2: Source directory
function extract_files() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "$(echo_c "ERROR:" 1 -b) Invalid parameters" >&2
        return 1
    fi

    local FILE_EXTENSION="$1"
    local SOURCE_DIR="$2"
    local TARGET_DIR="${FILE_EXTENSION#.}"

    echo "This will extract files from $(realpath $SOURCE_DIR)"
    printf "%s" "$(echo_c "Are you sure [y/n]? " 3)"
    read -r -n 1
    mkdir -p "$TARGET_DIR"

    find "$SOURCE_DIR" -type f -name "*$FILE_EXTENSION" | while read -r file; do
        echo "Moving $file"
        mv "$file" "$TARGET_DIR"
    done
}
