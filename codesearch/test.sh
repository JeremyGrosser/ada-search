#!/bin/bash

alr build
export PATH_INFO="/"
export QUERY_STRING=""
export REQUEST_METHOD="GET"

bin/codesearch
