module ComplexNumbersFieldTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Field"
        [ Test.fuzz2
            Fuzz.niceFloat
            Fuzz.niceFloat
            "tests ComplexNumbers add"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            (Real.Real real)
                            (Imaginary.Imaginary <| Real.Real imaginary)

                    expected =
                        ComplexNumbers.ComplexNumber
                            (Real.Real <| 2 * real)
                            (Imaginary.Imaginary <| Real.Real <| 2 * imaginary)
                in
                ComplexNumbers.add testValue testValue
                    |> Expect.equal expected
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests ComplexNumbers addition is commutative"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    testValueOne =
                        ComplexNumbers.add a b

                    testValueTwo =
                        ComplexNumbers.add b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests ComplexNumbers addition is associative"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary three)

                    testValueOne =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    testValueTwo =
                        ComplexNumbers.add a (ComplexNumbers.add b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests ComplexNumbers zero is identity"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)
                in
                ComplexNumbers.add testValue ComplexNumbers.zero
                    |> Expect.equal testValue
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests ComplexNumbers multiplication distributes over addition"
          <|
            \one two three ->
                let
                    a =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    b =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    c =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary three)

                    testValueOne =
                        ComplexNumbers.multiply a (ComplexNumbers.add b c)

                    testValueTwo =
                        ComplexNumbers.multiply a b
                            |> ComplexNumbers.add (ComplexNumbers.multiply a c)

                    result =
                        ComplexNumbers.equal.eq testValueOne testValueTwo
                in
                if result then
                    Expect.pass

                else
                    Expect.fail "equal"
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests ComplexNumbers subtract"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber real (Imaginary.Imaginary imaginary)

                    zero =
                        ComplexNumbers.zero

                    expected =
                        testValue
                in
                ComplexNumbers.subtract testValue zero
                    |> Expect.equal expected
        ]
