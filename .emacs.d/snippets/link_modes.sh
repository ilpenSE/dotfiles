#!/bin/bash
# SSPS = Shared Snippets

CXX_SSPS=("for" "stmacro")
for sp in "${CXX_SSPS[@]}"; do
  if [[ ! -f "./c++-mode/$sp" ]]; then
    ln -s ../simpc-mode/$sp ./c++-mode/$sp 2>&1 > /dev/null
  fi
done

C_SSPS=$(find ./simpc-mode -type f -exec basename {} \; | sed 's/\.[^.]*$//')
for sp in "${C_SSPS[@]}"; do
  if [[ ! -f "./c-mode/$sp" ]]; then
    ln -s ../simpc-mode/$sp ./c-mode/$sp 2>&1 > /dev/null
  fi
done
