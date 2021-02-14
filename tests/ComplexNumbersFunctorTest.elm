module ComplexNumbersFunctorTest exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Internal.ComplexNumbers
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers Functor abstraction"
        [ Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers map identity"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    mapResult =
                        ComplexNumbers.map identity complexNumber
                in
                mapResult
                    |> Expect.equal complexNumber
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers map identity polar representation"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    mapResult =
                        Internal.ComplexNumbers.map identity complexNumber
                in
                mapResult
                    |> Expect.equal complexNumber
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers Functor composition"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary two)

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        ComplexNumbers.map fdotG complexNumber
                in
                mapResult
                    |> Expect.equal (ComplexNumbers.map f (ComplexNumbers.map g complexNumber))
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests ComplexNumbers Functor composition polar representation"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    f =
                        (*) 2

                    g =
                        (-) 1

                    fdotG =
                        f << g

                    mapResult =
                        Internal.ComplexNumbers.map fdotG complexNumber
                in
                mapResult
                    |> Expect.equal (Internal.ComplexNumbers.map f (Internal.ComplexNumbers.map g complexNumber))
        ]
