#!/usr/bin/env bash
# nerd_snacks_tree.sh
# Uses the same tree icons as snacks.explorer/picker for showing project structure.

export LANG=C.UTF-8
export LC_ALL=C.UTF-8

PROJECT_ROOT="${1:-.}"

# Folders to exclude
EXCLUDE_DIRS=("node_modules" "venv" "__pycache__" "target" "build" ".git" ".hg" ".svn")

# Snacks‑style tree icons
ICON_VERTICAL="│ "
ICON_BRANCH="├╴"
ICON_LAST="└╴"
ICON_DIR="󰉋 "  # same as snacks dir icon
ICON_FILE="󰈔 " # same as snacks file icon

function should_exclude() {
  local NAME="$1"
  for EX in "${EXCLUDE_DIRS[@]}"; do
    [[ "$NAME" == "$EX" ]] && return 0
  done
  return 1
}

function print_tree() {
  local DIR="$1"
  local PREFIX="$2"
  local ITEMS=("$DIR"/*)
  local TOTAL=${#ITEMS[@]}
  local COUNT=0

  for ENTRY in "${ITEMS[@]}"; do
    [[ -e "$ENTRY" ]] || continue
    local NAME=$(basename "$ENTRY")
    should_exclude "$NAME" && continue
    COUNT=$((COUNT + 1))

    # Choose branch icon
    if [[ $COUNT -eq $TOTAL ]]; then
      BRANCH="$ICON_LAST"
      NEXT_PREFIX="$PREFIX    "
    else
      BRANCH="$ICON_BRANCH"
      NEXT_PREFIX="$PREFIX$ICON_VERTICAL"
    fi

    if [[ -d "$ENTRY" ]]; then
      printf "%s%s%s%s\n" "$PREFIX" "$BRANCH" "$ICON_DIR" "$NAME"
      print_tree "$ENTRY" "$NEXT_PREFIX"
    else
      printf "%s%s%s%s\n" "$PREFIX" "$BRANCH" "$ICON_FILE" "$NAME"
    fi
  done
}

echo "Project structure for: $PROJECT_ROOT"
print_tree "$PROJECT_ROOT" ""
