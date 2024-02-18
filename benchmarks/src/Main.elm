module Main exposing (main)

import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Cbor.Decode.Benchmark


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "Cbor.Decode"
            [ Benchmark.describe "fold"
                [ Benchmark.benchmark
                    "sorted"
                    Cbor.Decode.Benchmark.largeRecordFoldSorted
                , Benchmark.benchmark
                    "shuffled"
                    Cbor.Decode.Benchmark.largeRecordFoldShuffled
                ]
            , Benchmark.describe "record+field"
                [ Benchmark.benchmark
                    "sorted"
                    Cbor.Decode.Benchmark.largeRecordSorted
                , Benchmark.benchmark
                    "shuffled"
                    Cbor.Decode.Benchmark.largeRecordShuffled
                ]
            ]
