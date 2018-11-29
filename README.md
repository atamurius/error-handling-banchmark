# Error handling options benchmark

Sample implementations of different error handling
approaches:
- `ThrowParser`: exception throwing
- `TryParser`: exception packing into `Try[T]`
- `EitherParser`: error handling as `Either[String, T]`
- `ValidatedParser`: using `cats` `Validated` for
  collecting as much errors as possible (non-fail-fast)
  
## Problem

Problem here is data conversion and validation.
Each implementation is a `PersonParser` that
converts `Map[String, String]` into 
`Person(String, Int, Boolean)` or error.