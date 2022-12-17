#!/usr/bin/env bash
#
# Runs the CI tests
#

set -e

SCRIPTS_DIR=$( dirname `readlink -f -- "${0}"` )
ASDF_SOURCE_REGISTRY=~/.config/common-lisp/source-registry.conf.d

if [ -z "${WORKSPACE}" ]; then
    echo "Workspace is not set, exiting."
    exit 1
fi

# Install Quicklisp
/usr/local/bin/install-quicklisp

# Configure ASDF, so that it finds our systems
mkdir -p "${ASDF_SOURCE_REGISTRY}"
echo "(:tree \"${WORKSPACE}\")" > "${ASDF_SOURCE_REGISTRY}/workspace.conf"

${SCRIPTS_DIR}/run-tests.sh
