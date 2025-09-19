#!/bin/bash

# Colang Test Suite Runner Script
# Simple wrapper for running the test suite

echo "=================================================="
echo "           Colang Test Suite Runner"
echo "=================================================="
echo

# Check if we're in the right directory
if [ ! -f "tests/run-tests.rkt" ]; then
    echo "Error: run-tests.rkt not found. Please run this script from the colang project root."
    exit 1
fi

# Run the main test runner
racket tests/run-tests.rkt

# Capture exit code
exit_code=$?

echo
if [ $exit_code -eq 0 ]; then
    echo "🎉 Test suite completed successfully!"
else
    echo "💥 Test suite completed with failures!"
fi

exit $exit_code