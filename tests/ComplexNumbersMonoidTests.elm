module ComplexNumbersMonoidTests exposing (suite)

import CommutativeMonoid
import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2
            (Fuzz.map Real.Real Fuzz.niceFloat)
            (Fuzz.map Real.Real Fuzz.niceFloat)
            "tests ComplexNumber empty or identity value for sum"
          <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumber real (Imaginary.Imaginary imaginary)

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.sumCommutativeMonoid
                in
                monoid.semigroup expected monoid.identity
                    |> Expect.equal expected
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests monoidally add"
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

                    expected =
                        ComplexNumbers.add (ComplexNumbers.add a b) c

                    listOfMonoids =
                        [ a, b, c ]

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.sumCommutativeMonoid
                in
                monoid.concat listOfMonoids
                    |> Expect.equal expected
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests ComplexNumber empty or identity value for product"
          <|
            \real imaginary ->
                let
                    expected =
                        ComplexNumbers.ComplexNumber real (Imaginary.Imaginary imaginary)

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.productCommutativeMonoid

                    result =
                        ComplexNumbers.equal.eq (monoid.semigroup expected monoid.identity) expected
                in
                if result then
                    Expect.pass

                else
                    Expect.fail "equal"
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests monoidally product"
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

                    expected =
                        ComplexNumbers.multiply (ComplexNumbers.multiply a b) c

                    listOfMonoids =
                        [ a, b, c ]

                    (CommutativeMonoid.CommutativeMonoid monoid) =
                        ComplexNumbers.productCommutativeMonoid

                    result =
                        ComplexNumbers.equal.eq (monoid.concat listOfMonoids) expected
                in
                if result then
                    Expect.pass

                else
                    Expect.fail "equal"
        ]
