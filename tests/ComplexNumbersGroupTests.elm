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
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests Complex Number sum group has an inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    inversePlusA =
                        ComplexNumbers.sumGroup.monoid.semigroup
                            (ComplexNumbers.sumGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.sumGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.sumGroup.inverse complexNumber)
                in
                if
                    inversePlusA
                        == ComplexNumbers.sumGroup.monoid.identity
                        && aPlusInverse
                        == ComplexNumbers.sumGroup.monoid.identity
                then
                    Expect.pass

                else
                    Expect.fail "All equal identity"
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests Complex Number has an multiplicative inverse"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    inversePlusA =
                        ComplexNumbers.productGroup.monoid.semigroup
                            (ComplexNumbers.productGroup.inverse complexNumber)
                            complexNumber

                    aPlusInverse =
                        ComplexNumbers.productGroup.monoid.semigroup
                            complexNumber
                            (ComplexNumbers.productGroup.inverse complexNumber)
                in
                if
                    ComplexNumbers.equal.eq inversePlusA ComplexNumbers.productGroup.monoid.identity
                        && ComplexNumbers.equal.eq aPlusInverse ComplexNumbers.productGroup.monoid.identity
                then
                    Expect.pass

                else
                    Expect.fail "All equal identity"
        ]
