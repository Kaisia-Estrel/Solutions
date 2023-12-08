#!/usr/bin/env bash

function chars() {
    for i in $(seq "${#1}"); do
        echo "${1:$((i-1)):1} "
    done
}

function typeOfCard() {
    declare -a cards
    while read -r i; do
        cards+=("$i")
    done <<< "$(chars "$1")"

    declare -A cardCounts
    for i in "${cards[@]}"; do
        if [[ -z "${cardCounts[$i]}" ]]; then
            cardCounts["$i"]="1"
        else
            cardCounts["$i"]=$((cardCounts["$i"] + 1))
        fi
    done

    IFS=$'\n'
    mapfile -t sorted < <(sort <<< "${cardCounts[*]}")
    unset IFS

    for ((i = 0; i < ${#sorted[@]}; i++)); do
        sorted[i]=${sorted[i]##*( )}
        sorted[i]=${sorted[i]%%*( )}
    done

    case "${sorted[@]}" in
        "5") echo "7" ;;
        "1 4") echo "6" ;;
        "2 3") echo "5" ;;
        "1 1 3") echo "4" ;;
        "1 2 2") echo "3" ;;
        "1 1 1 2") echo "2" ;;
        "1 1 1 1 1") echo "1" ;;
        *) echo "?" ;;
    esac
}

function strengthOfCard() {
    declare -a cards
    while read -r i; do
        cards+=("$i")
    done <<< "$(chars "$1")"

    for ((i = 0; i < ${#cards[@]}; i++)); do
        sorted[i]=${cards[i]##*( )}
        sorted[i]=${cards[i]%%*( )}
    done

    local strength=0
    for ((i = 0; i < "${#cards[@]}"; i++)); do
        local multiplier="$((10 ** ((4-i)*2)))"
        case "${cards["$i"]}" in
            "2") strength="$((strength + 1 * multiplier))" ;;
            "3") strength="$((strength + 2 * multiplier))" ;;
            "4") strength="$((strength + 3 * multiplier))" ;;
            "5") strength="$((strength + 4 * multiplier))" ;;
            "6") strength="$((strength + 5 * multiplier))" ;;
            "7") strength="$((strength + 6 * multiplier))" ;;
            "8") strength="$((strength + 7 * multiplier))" ;;
            "9") strength="$((strength + 8 * multiplier))" ;;
            "T") strength="$((strength + 9 * multiplier))" ;;
            "J") strength="$((strength + 10 * multiplier))" ;;
            "Q") strength="$((strength + 11 * multiplier))" ;;
            "K") strength="$((strength + 12 * multiplier))" ;;
            "A") strength="$((strength + 13 * multiplier))" ;;
            *) echo "${cardCounts["$i"]}" ;;
        esac
    done
    echo "$strength"
}


function part1() {
    declare -A bids
    declare -A strengths
    while read -r -a p; do
        hand="${p[0]}"
        bet="${p[1]}"
        bids["$hand"]="$bet"
        strengths["$hand"]=$(( $(strengthOfCard "$hand") + "$(typeOfCard "$hand")" * 100000000000))
    done <input.txt

    declare -A reverseStrengths
    for i in "${!strengths[@]}"; do
        reverseStrengths["${strengths["$i"]}"]="$i"
    done

    IFS=$'\n'
    mapfile -t sortedStrengths < <(sort <<< "${strengths[*]}")
    unset IFS

    totalWinings=0
    rankI=1
    for i in "${sortedStrengths[@]}"; do
        totalWinings=$((totalWinings + "${bids["${reverseStrengths["$i"]}"]}" * rankI))
        rankI=$((rankI + 1))
    done

    echo "part1: $totalWinings"
}


function strengthOfCardPart2() {
    declare -a cards
    while read -r i; do
        cards+=("$i")
    done <<< "$(chars "$1")"

    for ((i = 0; i < ${#cards[@]}; i++)); do
        sorted[i]=${cards[i]##*( )}
        sorted[i]=${cards[i]%%*( )}
    done

    local strength=0
    for ((i = 0; i < "${#cards[@]}"; i++)); do
        local multiplier="$((10 ** ((4-i)*2)))"
        case "${cards["$i"]}" in
            "J") strength="$((strength + 1 * multiplier))" ;;
            "2") strength="$((strength + 2 * multiplier))" ;;
            "3") strength="$((strength + 3 * multiplier))" ;;
            "4") strength="$((strength + 4 * multiplier))" ;;
            "5") strength="$((strength + 5 * multiplier))" ;;
            "6") strength="$((strength + 6 * multiplier))" ;;
            "7") strength="$((strength + 7 * multiplier))" ;;
            "8") strength="$((strength + 8 * multiplier))" ;;
            "9") strength="$((strength + 9 * multiplier))" ;;
            "T") strength="$((strength + 10 * multiplier))" ;;
            "Q") strength="$((strength + 11 * multiplier))" ;;
            "K") strength="$((strength + 12 * multiplier))" ;;
            "A") strength="$((strength + 13 * multiplier))" ;;
            *) echo "${cardCounts["$i"]}" ;;
        esac
    done
    echo "$strength"
}

function expandOnJ() {
    withoutJ="${1//J/}"

    declare -a result
    result+=("$withoutJ")
    for i in $(seq $((5 - ${#withoutJ}))); do
        charsSoFar="${#result[@]}"
        for ((j = 0; j < "$charsSoFar"; j++)); do
            result+=("${result["$j"]}3")
            result+=("${result["$j"]}4")
            result+=("${result["$j"]}5")
            result+=("${result["$j"]}6")
            result+=("${result["$j"]}7")
            result+=("${result["$j"]}8")
            result+=("${result["$j"]}9")
            result+=("${result["$j"]}T")
            result+=("${result["$j"]}Q")
            result+=("${result["$j"]}K")
            result+=("${result["$j"]}A")
            result["$j"]="${result["$j"]}2"
        done
    done
    echo "${result[@]}"
}

function typeOfCardPart2() {
    if [[ "$1" == "JJJJJ" ]]; then
      echo "7"
      return 0
    fi
    local maxType=1
    for i in $(expandOnJ "$1"); do
        local newtype
        newtype="$(typeOfCard "$i")"
        if [[ "$newtype" -gt "$maxType" ]]; then
            maxType="$newtype"
        fi
    done
    echo "$maxType"
}

function part2() {
    declare -A bids
    declare -A strengths

    local line
    line=0
    while read -r -a p; do
        hand="${p[0]}"
        bet="${p[1]}"
        bids["$hand"]="$bet"
        echo "on ($line/1000): ${p[*]}"
        strengths["$hand"]=$(( $(strengthOfCardPart2 "$hand") + "$(typeOfCardPart2 "$hand")" * 100000000000))
        line=$((line+1))
    done < input.txt


    declare -A reverseStrengths
    for i in "${!strengths[@]}"; do
        reverseStrengths["${strengths["$i"]}"]="$i"
    done

    IFS=$'\n'
    mapfile -t sortedStrengths < <(sort <<< "${strengths[*]}")
    unset IFS

    totalWinings=0
    rankI=1
    for i in "${sortedStrengths[@]}"; do
        totalWinings=$((totalWinings + "${bids["${reverseStrengths["$i"]}"]}" * rankI))
        rankI=$((rankI + 1))
    done

    echo "part2: $totalWinings"
}

part1
part2
