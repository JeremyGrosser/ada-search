#!/bin/bash

ALRARGS="--non-interactive --no-tty --no-color"
ALR="alr1.2"
echo "Updating index"
${ALR} index --update-all
echo "Getting package list"
PACKAGES=$(${ALR} ${ALRARGS} search --list | cut -d' ' -f1 | grep -v 'NAME')

TARGET="alire-$(date +%Y%m%d)"
mkdir -p "${TARGET}"
pushd "${TARGET}"

for package in ${PACKAGES}; do
    echo "${pkg}"
    ${ALR} ${ALRARGS} get --only ${package}
done

popd
