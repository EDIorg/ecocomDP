#!/bin/bash

# CSV to JSON converter using BASH
# original script from https://gist.github.com/dsliberty/3de707bc656cf757a0cb
# Usage ./csv2json.sh input.csv > output.json
#       cat <input.csv> | csv2json > output.json

# downloaded July 2018, by mob
# this very wonderful version copied straight from this comment: 
# https://gist.github.com/dsliberty/3de707bc656cf757a0cb#gistcomment-2365887

#set -x
shopt -s extglob

input="${1:-/dev/stdin}"

SEP=","

[ -z "${input}" ] && echo "No CSV input file specified" && exit 1
[ ! -e "${input}" ] && echo "Unable to locate ${input}" && exit 1

csv_nextField()
{
    local line="$(echo "${1}" | sed 's/\r//g')"
    local start=0
    local stop=0

    if [[ -z "${line}" ]]; then
        return 0
    fi

    local offset=0
    local inQuotes=0
    while [[ -n "${line}" ]]; do
        local char="${line:0:1}"
        line="${line:1}"

        if [[ "${char}" == "${SEP}" && ${inQuotes} -eq 0 ]]; then
            inQuotes=0
            break
        elif [[ "${char}" == '"' ]]; then
            if [[ ${inQuotes} -eq 1 ]]; then
                inQuotes=0
            else
                inQuotes=1
            fi
        else
            echo -n "${char}"
        fi
        offset=$(( ${offset} + 1 ))
    done

    echo ""
    return $(( ${offset} + 1 ))
}

read -r first_line < "${input}"
a=0
headings=$(echo "${first_line}" | awk -F"${SEP}" {'print NF'})

if [ "${input}" = "/dev/stdin" ]; then
  while read -r line
  do
    lines_str+="$line"$'\n'
    c=1
  done < "${input}"
else
  lines_str="$(cat "${input}")"
  c=0
fi

lines_num=$(echo "${lines_str}" | wc -l)

while [[ ${a} -lt ${headings} ]]; do
    field="$(csv_nextField "${first_line}")"
    first_line="${first_line:${?}}"
    head_array[${a}]="${field}"
    a=$(( ${a} + 1 ))
done

#c=0
echo "["
while [ ${c} -lt ${lines_num} ]
do
    read -r each_line
    each_line="$(echo "${each_line}" | sed 's/\r//g')"

    if [[ ${c} -eq 0 ]]; then
        c=$(( ${c} + 1 ))
    else
        d=0
        echo "    {"
        while [[ ${d} -lt ${headings} ]]; do
            item="$(csv_nextField "${each_line}")"
            each_line="${each_line:${?}}"
            echo -n "        \"${head_array[${d}]}\": "
            case "${item}" in
                "")
                    echo -n "null"
                    ;;
                null|true|false|\"*\"|+([0123456789]))
                    echo -n ${item}
                    ;;
                *)
                    echo -n "\"${item}\""
                    ;;
            esac
            d=$(( ${d} + 1 ))
            [[ ${d} -lt ${headings} ]] && echo "," || echo ""
        done

        echo -n "    }"

        c=$(( ${c} + 1 ))
        [[ ${c} -lt ${lines_num} ]] && echo "," || echo ""
    fi

done <<< "${lines_str}"
echo "]"

