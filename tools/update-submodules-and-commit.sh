#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=0
PUSH=0
TOP_LEVEL_ONLY=0

usage() {
  cat <<'USAGE'
Usage: bash tools/update-submodules-and-commit.sh [--dry-run] [--push] [--top-level-only]

Maintainer-only helper for synchronizing GX_SIMULATOR submodule pointers.

Options:
  --dry-run   Update submodules, then report commits/pushes without creating them.
  --push      Push submodule commits first, then push the GX_SIMULATOR commit.
  --top-level-only
              Do not commit inside submodule repositories. Only record top-level
              GX_SIMULATOR submodule pointers whose target commits are already
              visible on remotes.
  -h, --help  Show this help.

Dry-run still runs:
  git submodule update --init --recursive --remote

With --top-level-only, dry-run instead runs:
  git submodule update --init --remote
  git submodule foreach 'git submodule update --init --recursive'

It does not commit or push.
USAGE
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --dry-run)
      DRY_RUN=1
      ;;
    --push)
      PUSH=1
      ;;
    --top-level-only)
      TOP_LEVEL_ONLY=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "ERROR: unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

run() {
  echo "+ $*"
  "$@"
}

repo_label() {
  local repo=$1
  if [ "$repo" = "." ]; then
    echo "GX_SIMULATOR"
  else
    echo "$repo"
  fi
}

current_branch() {
  git -C "$1" symbolic-ref --quiet --short HEAD
}

remote_contains_head() {
  local repo=$1
  git -C "$repo" branch -r --contains HEAD | grep -v ' -> ' | grep -q .
}

push_current_branch() {
  local repo=$1
  local branch
  branch=$(current_branch "$repo") || {
    echo "ERROR: $(repo_label "$repo") is on a detached HEAD; refusing to push." >&2
    exit 1
  }

  if git -C "$repo" rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
    run git -C "$repo" push
  else
    run git -C "$repo" push origin "HEAD:$branch"
  fi
}

submodule_paths_for_repo() {
  local repo=$1
  if [ -f "$repo/.gitmodules" ]; then
    git -C "$repo" config -f .gitmodules --get-regexp '^submodule\..*\.path$' |
      awk '{print $2}'
  fi
}

changed_pointer_paths() {
  local repo=$1
  local path
  local old_head
  local new_head
  shift

  for path in "$@"; do
    old_head=$(git -C "$repo" ls-tree HEAD -- "$path" | awk '{print $3}')
    new_head=$(git -C "$repo/$path" rev-parse HEAD)

    if [ -n "$old_head" ] && [ "$old_head" != "$new_head" ]; then
      echo "$path"
    fi
  done
}

ensure_changed_children_are_remote_or_pushable() {
  local repo=$1
  local child
  local child_path
  shift

  while IFS= read -r child; do
    [ -n "$child" ] || continue
    child_path="$repo/$child"

    if remote_contains_head "$child_path"; then
      continue
    fi

    if [ "$PUSH" -eq 1 ]; then
      echo "Submodule $(repo_label "$child_path") points to a commit not on any remote branch; pushing it first."
      push_current_branch "$child_path"
      continue
    fi

    cat >&2 <<EOF
ERROR: $(repo_label "$repo") would record submodule pointer "$child" at a commit
that is not visible on any remote branch.

Push that submodule commit first, or rerun this maintainer helper with --push.
This prevents creating a parent commit that future clones cannot reproduce.
EOF
    exit 1
  done < <(changed_pointer_paths "$repo" "$@")
}

commit_pointer_updates() {
  local repo=$1
  local label
  local branch
  local paths=()
  local changed_paths=()
  local path

  label=$(repo_label "$repo")
  echo
  echo "Checking $label for submodule pointer changes..."

  while IFS= read -r path; do
    [ -n "$path" ] && paths+=("$path")
  done < <(submodule_paths_for_repo "$repo")

  if [ "${#paths[@]}" -eq 0 ]; then
    echo "No nested submodules are configured in $label."
    return 0
  fi

  while IFS= read -r path; do
    [ -n "$path" ] && changed_paths+=("$path")
  done < <(changed_pointer_paths "$repo" "${paths[@]}")

  if [ "${#changed_paths[@]}" -eq 0 ]; then
    echo "No submodule pointer changes to commit in $label."
    return 0
  fi

  echo "Submodule pointer changes detected in $label:"
  git -C "$repo" diff --submodule=short -- "${changed_paths[@]}"

  branch=$(current_branch "$repo") || {
    cat >&2 <<EOF
ERROR: $label needs a commit but is on a detached HEAD.

Check out the intended branch in that repository, then rerun this helper.
EOF
    exit 1
  }

  ensure_changed_children_are_remote_or_pushable "$repo" "${changed_paths[@]}"

  if [ "$DRY_RUN" -eq 1 ]; then
    echo "DRY RUN: would commit submodule pointer updates in $label on branch $branch."
    return 0
  fi

  git -C "$repo" add -- "${changed_paths[@]}"
  run git -C "$repo" commit -m "Update submodule pointers"

  if [ "$PUSH" -eq 1 ]; then
    echo "Pushing $label before any parent repository records this commit."
    push_current_branch "$repo"
  else
    echo "Created a local commit in $label. Push it before committing any parent pointer to it."
  fi
}

echo "WARNING: this helper is intended only for GX_SIMULATOR maintainers/developers"
echo "with write access to GX_SIMULATOR and the relevant submodule repositories."
echo "Normal users should use git submodule update --init --recursive instead."
echo

if [ "$TOP_LEVEL_ONLY" -eq 1 ]; then
  echo "Top-level-only mode: submodule repositories will not receive commits."
fi

if [ "$DRY_RUN" -eq 1 ]; then
  echo "Dry-run mode: submodules will be updated, but commits and pushes will be reported only."
fi

if [ "$PUSH" -eq 1 ]; then
  echo "Push mode: submodule commits will be pushed before parent commits."
else
  echo "No-push mode: commits may be created locally, but nothing will be pushed."
fi

echo
if [ "$TOP_LEVEL_ONLY" -eq 1 ]; then
  echo "Updating direct GX_SIMULATOR submodules from their configured remote branches..."
  run git submodule update --init --remote
  echo
  echo "Restoring nested submodules to the commits recorded by their parent submodules..."
  run git submodule foreach 'git submodule update --init --recursive'
else
  echo "Updating all submodules recursively from their configured remote branches..."
  run git submodule update --init --recursive --remote
fi

SUBMODULES=()
while IFS= read -r repo; do
  [ -n "$repo" ] && SUBMODULES+=("$repo")
done < <(
  git submodule foreach --quiet --recursive 'printf "%s\n" "$displaypath"' |
    awk '{ print gsub("/", "/"), $0 }' |
    sort -rn |
    cut -d' ' -f2-
)

if [ "$TOP_LEVEL_ONLY" -eq 1 ]; then
  echo
  echo "Skipping commits inside submodule repositories because --top-level-only was provided."
else
  for repo in "${SUBMODULES[@]}"; do
    commit_pointer_updates "$repo"
  done
fi

commit_pointer_updates "."

if [ "$PUSH" -eq 1 ]; then
  echo
  echo "Pushing GX_SIMULATOR after submodule commits are available remotely..."
  push_current_branch "."
fi

echo
echo "Done."
