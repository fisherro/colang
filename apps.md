# Applications

Some applications to build examples of with Colang.

## 1. **Producer-Consumer Pipeline** (Medium complexity)

A chain of coroutines where each one processes data and passes it to the next. For example:

- Data generator → Filter → Transformer → Consumer
- Great for showing coroutine composition and data flow

## 2. **Simple Text Adventure Game** (Medium-High complexity)

- Game state coroutine that manages rooms, inventory, etc.
- Player input handler coroutine
- Story/dialogue coroutines that can be suspended and resumed
- Shows stateful interaction patterns

## 3. **Cooperative Task Scheduler** (High complexity)

- Multiple "task" coroutines that yield control to each other
- A scheduler coroutine that manages execution order
- Demonstrates cooperative multitasking

## 4. **Stream Processing** (Medium complexity)

- Infinite data streams implemented as coroutines
- Operations like map, filter, take, zip implemented as coroutines
- Shows functional programming patterns with coroutines

## 5. **Simple Parser/Lexer** (Medium-High complexity)

- Lexer coroutine that yields tokens
- Parser coroutine that consumes tokens
- Shows how coroutines can model state machines

## 6. **Conway's Game of Life** (Medium complexity)

- Grid coroutine that yields generations
- Display coroutine that shows the grid
- Simple but visually interesting
