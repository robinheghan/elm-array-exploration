module Bench.Native exposing (..)

import Native.Benchmark


type Benchmark a
    = Benchmark


type BenchmarkSuite a
    = BenchmarkSuite


bench : String -> (() -> a) -> Benchmark a
bench =
    Native.Benchmark.bench


suite : String -> List (Benchmark a) -> BenchmarkSuite a
suite =
    Native.Benchmark.suite


run : List (BenchmarkSuite a) -> b -> b
run =
    Native.Benchmark.run
