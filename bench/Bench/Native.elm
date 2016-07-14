module Bench.Native exposing (..)

import Native.Benchmark


type Benchmark a
    = Benchmark


type BenchmarkSuite
    = BenchmarkSuite


bench : String -> (() -> a) -> Benchmark a
bench =
    Native.Benchmark.bench


suite : String -> List (Benchmark a) -> BenchmarkSuite
suite =
    Native.Benchmark.suite


run : List (BenchmarkSuite) -> b -> b
run =
    Native.Benchmark.run
