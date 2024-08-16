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
# $1: Source directory
# $2: File extension
function extract_files() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "$(echo_c "ERROR:" 1 -b) Invalid parameters" >&2
        echo "Usage: extract_files $(tput smul)source_directory$(tput rmul) $(tput smul)file_extension$(tput rmul)"
        return 1
    fi

    local SOURCE_DIR="$1"
    local FILE_EXTENSION="$2"
    local TARGET_DIR="${FILE_EXTENSION#.}"

    read -q "REPLY?$(echo_c "Create directory '${TARGET_DIR}'? " 3)"
    if [[ "$REPLY" =~ [yY] ]]; then
        mkdir -p "$TARGET_DIR"
    fi

    echo "\n$(echo_c "This will move matching files from" 3) $(realpath $SOURCE_DIR) $(echo_c "to" 3) $(realpath $TARGET_DIR)"
    read -q "REPLY?$(echo_c "Continue? " 3)"
    if [[ "$REPLY" =~ [yY] ]]; then
        echo
        find "$SOURCE_DIR" -type f -name "*$FILE_EXTENSION" | while read -r file; do
            echo "$(echo_c "Moving" 6) $file $(echo_c "to" 6) $TARGET_DIR"
            mv "$file" "$TARGET_DIR"
        done
    fi
}
