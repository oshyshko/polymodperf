#!/bin/sh

set -xue

# jump to project directory
cd "$(dirname $0)/../"

stack build
stack exec polymodperf-exe
