#!/bin/bash

for file in *; do
  cp "$file" "test_$file"
done
