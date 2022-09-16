#!/bin/bash

set -x

ALRARGS="--non-interactive --no-tty --no-color"
ALR="alr1.3-dev"
echo "Updating index"
${ALR} index --update-all
echo "Getting package list"

TARGET="alire-$(date +%Y%m%d)"
mkdir -p "${TARGET}"
pushd "${TARGET}"

${ALR} ${ALRARGS} search --list | cut -d' ' -f1 | grep -v 'NAME' >index.txt

for package in $(cat index.txt); do
    echo "${pkg}"
    ${ALR} ${ALRARGS} get --only ${package}
done

popd
