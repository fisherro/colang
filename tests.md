# Colang Test Suite

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

### Test Utilities

- `test-utils.rkt`: Basic Racket testing utilities
- `colang-test-helpers.rkt`: Colang-specific testing routines

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
