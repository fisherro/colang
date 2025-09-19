# Colang Test Suite

This is a comprehensive test suite for the Colang programming language.

## Overview

## Test Structure

### Test Suites

1. **Routine Definitions Suite** (`routine-definitions-suite.rkt`)
   - Tests all three routine definition forms: anonymous, explicit, and shorthand
   - Validates routine behavior with various parameter configurations
   - Tests lexical scoping and recursion
   - **Tests:** 13 tests covering routine definition syntax and behavior

2. **Yield and Coroutine Suite** (`yield-coroutine-suite.rkt`)
   - Tests yield expressions and coroutine lifecycle
   - Validates resumable state management
   - Tests multiple yields and coroutine communication
   - **Tests:** 23 tests covering yield behavior and coroutine state

3. **Construction and Activation Suite** (`construction-activation-suite.rkt`)
   - Tests `new` construction with various argument patterns
   - Validates quick activation vs instance activation
   - Tests partial argument application
   - **Tests:** 21 tests covering construction and activation patterns

4. **Predicates Suite** (`predicates-suite.rkt`)
   - Tests `routine?` and `resumable?` predicates
   - Validates predicate behavior with various data types
   - Tests predicate lifecycle through coroutine execution
   - **Tests:** 30 tests covering predicate behavior

5. **Integration Suite** (`integration-suite.rkt`)
   - Tests complex scenarios and design patterns
   - Validates producer-consumer patterns, state machines, pipelines
   - Tests advanced coroutine interactions
   - **Tests:** 15 tests covering complex integration scenarios

## Running Tests

### Quick Run

```bash
./run-tests.sh
```

### Individual Test Suites

```bash
racket tests/routine-definitions-suite.rkt
racket tests/yield-coroutine-suite.rkt
racket tests/construction-activation-suite.rkt
racket tests/predicates-suite.rkt
racket tests/integration-suite.rkt
```

### Detailed Test Runner

```bash
racket tests/run-tests.rkt
```

## Test Output

Each test suite provides:

- ✅ Clear pass/fail indicators for each test
- 📊 Summary statistics (total, passed, failed)
- 🎉 Overall success/failure status
- 📝 Descriptive test names explaining what is being tested

## Test Design

### Principles

- **Comprehensive Coverage**: Tests cover all features mentioned in spec.md
- **Clear Output**: Visual indicators make it easy to see what passed/failed
- **Self-Contained**: Each test suite can run independently
- **Specification-Driven**: Tests directly validate behavior described in the language spec

### Test Utilities

- `test-utils.rkt`: Basic Racket testing utilities
- `colang-test-helpers.rkt`: Colang-specific testing routines
- Test suites use a simple but effective testing pattern with clear assertions

### Validation Approach

- Tests validate both successful cases and edge cases
- Predicates are tested with various input types
- Coroutine lifecycle is thoroughly tested through multiple states
- Complex scenarios test real-world usage patterns

## Adding New Tests

To add new tests:

1. Choose the appropriate test suite file
2. Follow the existing test pattern using `test-check`
3. Update the test count tracking if needed
4. Run the individual test suite to verify
5. Run the full test suite to ensure no regressions

## Future Enhancements

Potential improvements to the test suite:

- Performance benchmarking tests
- Error handling and edge case tests
- Integration with continuous integration systems
- Property-based testing for generated test cases
- Memory usage and garbage collection tests
