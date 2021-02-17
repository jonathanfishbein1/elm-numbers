module ComplexNumbersGroupTests exposing (suite)

import ComplexNumbers
import Expect
import Fuzz
import Imaginary
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The Group abstraction"
        [ Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests Complex Number sum group has an inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary <| Real.Real two)

                    inversePlusA =
                        ComplexNumbers.sumGroup.monoid.semigroup
                            (ComplexNumbers.sumGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.sumGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.sumGroup.inverse complexNumber)
                in
                Expect.true "All equal identity"
                    (inversePlusA
                        == ComplexNumbers.sumGroup.monoid.identity
                        && aPlusInverse
                        == ComplexNumbers.sumGroup.monoid.identity
                    )
        , Test.fuzz2
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            "tests Complex Number has an multiplicative inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary <| Real.Real two)

                    inversePlusA =
                        ComplexNumbers.productGroup.monoid.semigroup
                            (ComplexNumbers.productGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.productGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.productGroup.inverse complexNumber)
                in
                Expect.true "All equal identity"
                    (ComplexNumbers.equal.eq inversePlusA ComplexNumbers.productGroup.monoid.identity
                        && ComplexNumbers.equal.eq aPlusInverse ComplexNumbers.productGroup.monoid.identity
                    )
        ]
