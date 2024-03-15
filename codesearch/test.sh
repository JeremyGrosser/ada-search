#!/bin/bash

alr build
export PATH_INFO="/"
export QUERY_STRING="q=synack"
export REQUEST_METHOD="GET"

bin/codesearch
