#!/bin/bash
set -e

echo "🔄 Fetching latest changes from origin..."
git fetch origin

echo "🧹 Resetting main repo to origin/master..."
git reset --hard origin/master
git clean -fd

echo "📦 Updating submodules recursively from remote..."
git submodule update --init --recursive --remote

echo "📌 Committing submodule updates if needed..."
git submodule foreach --recursive '
  echo "🔍 Checking submodule: $name"
  git add .
  if ! git diff --cached --quiet; then
    echo "📥 Committing changes in $name..."
    git commit -m "Update submodule $name to latest remote commit"
  else
    echo "✅ No changes to commit in $name"
  fi
'

echo "📥 Committing updated submodule references in main repo..."
git add .
if ! git diff --cached --quiet; then
  git commit -m "Track updated submodule references"
else
  echo "✅ No changes to commit in main repo"
fi

echo "🚀 Done. You can now push with: git push origin master"
