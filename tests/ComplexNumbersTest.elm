module ComplexNumbersTest exposing (suite)

import ComplexNumbers exposing (ComplexNumber, imaginary)
import Expect
import Fuzz
import Imaginary
import Internal.ComplexNumbers
import Real
import Test


suite : Test.Test
suite =
    Test.describe "The ComplexNumbers module"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers modulus"
          <|
            \one two ->
                let
                    number =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary <| Real.Real two)

                    (ComplexNumbers.ComplexNumber (Real.Real real) (Imaginary.Imaginary (Real.Real imaginary))) =
                        number

                    length =
                        real
                            ^ 2
                            + imaginary
                            ^ 2
                            |> sqrt
                            |> Real.Real
                in
                Expect.true "modules equals length " (Real.equal.eq (ComplexNumbers.modulus number) length)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests |c1||c2| = |c1c2|"
          <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    lengthOne =
                        ComplexNumbers.modulus numberOne

                    lengthTwo =
                        ComplexNumbers.modulus numberTwo

                    productLengthOneLengthTwo =
                        Real.multiply lengthOne lengthTwo

                    modulesOfProductOfNumberOneNumberTwo =
                        ComplexNumbers.multiply numberOne numberTwo
                            |> ComplexNumbers.modulus
                in
                Expect.true "modules equals length " (Real.equal.eq productLengthOneLengthTwo modulesOfProductOfNumberOneNumberTwo)
        , Test.fuzz3
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests |c1 + c2| <= |c1| + |c2| (triangle inequality rule)"
          <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    (Real.Real modulesOfSumOfNumberOneNumberTwo) =
                        ComplexNumbers.add numberOne numberTwo
                            |> ComplexNumbers.modulus

                    modulusOne =
                        ComplexNumbers.modulus numberOne

                    modulusTwo =
                        ComplexNumbers.modulus numberTwo

                    (Real.Real sumLengthOneLengthTwo) =
                        Real.add modulusOne modulusTwo
                in
                modulesOfSumOfNumberOneNumberTwo |> Expect.atMost sumLengthOneLengthTwo
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests ComplexNumbers conjugate"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    expected =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary (Real.negate imaginary))
                in
                ComplexNumbers.conjugate testValue
                    |> Expect.equal expected
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests ComplexNumbers conjugate of conjugate equals original CompleNumber"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    conjugate =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary (Real.negate imaginary))

                    conjugateConjugate =
                        ComplexNumbers.conjugate conjugate
                in
                Expect.equal conjugateConjugate testValue
        , Test.fuzz3
            Fuzz.int
            Fuzz.int
            Fuzz.int
            "tests conjugation respects addition"
          <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary <| Real.Real two)

                    numberTwo =
                        ComplexNumbers.ComplexNumber
                            (Real.Real two)
                            (Imaginary.Imaginary <| Real.Real three)

                    conjugateOne =
                        ComplexNumbers.conjugate numberOne

                    conjugateTwo =
                        ComplexNumbers.conjugate numberTwo

                    sumOfconjugateOneconjugateTwo =
                        ComplexNumbers.add conjugateOne conjugateTwo

                    sumOfNumberOneNumberTwo =
                        ComplexNumbers.add numberOne numberTwo

                    conjugateOfsumOfNumberOneNumberTwo =
                        ComplexNumbers.conjugate sumOfNumberOneNumberTwo
                in
                Expect.equal sumOfconjugateOneconjugateTwo conjugateOfsumOfNumberOneNumberTwo
        , Test.fuzz3
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            (Fuzz.map Real.Real Fuzz.int)
            "tests conjugation respects multiplication"
          <|
            \one two three ->
                let
                    numberOne =
                        ComplexNumbers.ComplexNumber
                            one
                            (Imaginary.Imaginary two)

                    numberTwo =
                        ComplexNumbers.ComplexNumber
                            two
                            (Imaginary.Imaginary three)

                    conjugateOne =
                        ComplexNumbers.conjugate numberOne

                    conjugateTwo =
                        ComplexNumbers.conjugate numberTwo

                    productOfconjugateOneconjugateTwo =
                        ComplexNumbers.add conjugateOne conjugateTwo

                    productOfNumberOneNumberTwo =
                        ComplexNumbers.add numberOne numberTwo

                    conjugateOfproductOfNumberOneNumberTwo =
                        ComplexNumbers.conjugate productOfNumberOneNumberTwo
                in
                Expect.equal productOfconjugateOneconjugateTwo conjugateOfproductOfNumberOneNumberTwo
        , Test.fuzz2
            (Fuzz.map Real.Real Fuzz.float)
            (Fuzz.map Real.Real Fuzz.float)
            "tests complex number multipled by conjugate equals modulus squared"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    conjugate =
                        ComplexNumbers.conjugate testValue

                    producttestValueconjugate =
                        ComplexNumbers.multiply testValue conjugate
                            |> ComplexNumbers.modulus

                    expected =
                        Real.multiply (ComplexNumbers.modulus testValue) (ComplexNumbers.modulus testValue)
                in
                Expect.equal producttestValueconjugate expected
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests reciprocal of complex number equals conjugate divided by modules squared"
          <|
            \real imaginary ->
                let
                    testValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    reciprocal =
                        ComplexNumbers.divide ComplexNumbers.one testValue

                    conjugate =
                        ComplexNumbers.conjugate testValue

                    modulusSquared =
                        Real.multiply (ComplexNumbers.modulus testValue) (ComplexNumbers.modulus testValue)

                    modulusSquaredComplexNumber =
                        ComplexNumbers.ComplexNumber
                            modulusSquared
                            Imaginary.zero

                    conjugateDividedByModulesSquared =
                        ComplexNumbers.divide conjugate modulusSquaredComplexNumber
                in
                Expect.true
                    "reciprecal and conjugate divided by modules squared equal"
                    (ComplexNumbers.equal.eq reciprocal conjugateDividedByModulesSquared)
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            "tests conjuage of z divided by w equals the conjugate of z divided by the conjuaget of w: with w not equal to zero"
          <|
            \real imaginary ->
                let
                    z =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    w =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    zDividedByW =
                        ComplexNumbers.divide z w

                    conjugateZDividedByW =
                        ComplexNumbers.conjugate zDividedByW

                    zConjugate =
                        ComplexNumbers.conjugate z

                    wConjugate =
                        ComplexNumbers.conjugate w

                    zConjugateDividedBywConjugate =
                        ComplexNumbers.divide zConjugate wConjugate
                in
                Expect.true
                    "conjugate of z divided by w equals the conjugate of z divided by the conjugate of w"
                    (ComplexNumbers.equal.eq conjugateZDividedByW zConjugateDividedBywConjugate)
        , Test.test
            "length of z is 0 if z real and imaginary parts are 0"
          <|
            \_ ->
                let
                    z =
                        ComplexNumbers.zero

                    zLength =
                        ComplexNumbers.modulus z
                in
                Expect.equal zLength Real.zero
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange 1 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 -1))
            "length of z is not 0 if z real and imaginary parts are not 0"
          <|
            \real imaginary ->
                let
                    z =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    zLength =
                        ComplexNumbers.modulus z
                in
                Expect.notEqual zLength Real.zero
        , Test.fuzz2
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            (Fuzz.map Real.Real (Fuzz.floatRange -10 10))
            "tests convertFromCartesianToPolar |> convertFromPolarToCartesian round trips"
          <|
            \real imaginary ->
                let
                    cartesianTestValue =
                        ComplexNumbers.ComplexNumber
                            real
                            (Imaginary.Imaginary imaginary)

                    polarTestValue =
                        ComplexNumbers.convertFromCartesianToPolar cartesianTestValue

                    conversionResult =
                        ComplexNumbers.convertFromPolarToCartesian polarTestValue

                    result =
                        ComplexNumbers.equal.eq cartesianTestValue conversionResult
                in
                Expect.true "Should be equal" result
        , Test.fuzz3
            Fuzz.float
            Fuzz.float
            Fuzz.float
            "tests ComplexNumbers polar multiplication is commutative"
          <|
            \one two three ->
                let
                    a =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    b =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus two)
                            (Internal.ComplexNumbers.Theta three)

                    testValueOne =
                        Internal.ComplexNumbers.multiply a b

                    testValueTwo =
                        Internal.ComplexNumbers.multiply b a
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.fuzz3
            (Fuzz.intRange -10 10)
            (Fuzz.intRange -10 10)
            (Fuzz.intRange -10 10)
            "tests ComplexNumbers polar multiplication is associative"
          <|
            \one two three ->
                let
                    a =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    b =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus two)
                            (Internal.ComplexNumbers.Theta three)

                    c =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta three)

                    testValueOne =
                        Internal.ComplexNumbers.multiply
                            (Internal.ComplexNumbers.multiply a b)
                            c

                    testValueTwo =
                        Internal.ComplexNumbers.multiply a (Internal.ComplexNumbers.multiply b c)
                in
                testValueOne
                    |> Expect.equal testValueTwo
        , Test.test
            "tests ComplexNumbers polar division"
          <|
            \_ ->
                let
                    complexNumberDividend =
                        ComplexNumbers.ComplexNumber (Real.Real 2) (Imaginary.Imaginary Real.zero)
                            |> ComplexNumbers.convertFromCartesianToPolar

                    complexNumberDivisor =
                        ComplexNumbers.ComplexNumber (Real.Real 1) (Imaginary.Imaginary Real.one)
                            |> ComplexNumbers.convertFromCartesianToPolar

                    quotient =
                        Internal.ComplexNumbers.divide complexNumberDividend complexNumberDivisor

                    quotientMod =
                        Internal.ComplexNumbers.modulus quotient

                    quotientPhase =
                        Internal.ComplexNumbers.theta quotient

                    quotientCartesian =
                        ComplexNumbers.convertFromPolarToCartesian quotient

                    expected =
                        ComplexNumbers.ComplexNumber
                            (Real.Real (quotientMod * Basics.cos quotientPhase))
                            (Imaginary.Imaginary <| Real.Real (quotientMod * Basics.sin quotientPhase))
                in
                Expect.equal quotientCartesian expected
        , Test.fuzz2
            Fuzz.int
            Fuzz.int
            "tests power"
          <|
            \one two ->
                let
                    complexNumber =
                        Internal.ComplexNumbers.ComplexNumber
                            (Internal.ComplexNumbers.Modulus one)
                            (Internal.ComplexNumbers.Theta two)

                    powerResult =
                        Internal.ComplexNumbers.power 2 complexNumber

                    productResult =
                        Internal.ComplexNumbers.multiply complexNumber complexNumber
                in
                powerResult
                    |> Expect.equal productResult
        , Test.fuzz2
            Fuzz.float
            Fuzz.float
            "print ComplexNumber"
          <|
            \one two ->
                let
                    complexNumber =
                        ComplexNumbers.ComplexNumber
                            (Real.Real one)
                            (Imaginary.Imaginary <| Real.Real two)

                    printedComplexNumber =
                        ComplexNumbers.print complexNumber

                    readComplexNumber =
                        ComplexNumbers.read printedComplexNumber
                in
                Expect.equal readComplexNumber (Ok complexNumber)
        , Test.test
            "test Euler identity"
          <|
            \_ ->
                let
                    complexNumberAtPi =
                        ComplexNumbers.euler (Real.Real Basics.pi)
                in
                Expect.true "e ^ (i * pi) + 1 = 0"
                    (ComplexNumbers.equal.eq
                        (ComplexNumbers.add complexNumberAtPi ComplexNumbers.one)
                        ComplexNumbers.zero
                    )
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "test length of e ^ (i * theta)"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.euler one
                in
                Expect.true "length of e ^ (i * theta) == 1" (Real.equal.eq (ComplexNumbers.modulus complexNumber) Real.one)
        , Test.fuzz
            (Fuzz.map Real.Real Fuzz.float)
            "conjugate of e ^ (i * theta) = e ^ -i * theta)"
          <|
            \one ->
                let
                    complexNumber =
                        ComplexNumbers.euler one

                    complexNumberConjugate =
                        ComplexNumbers.conjugate complexNumber

                    complexNumberNegativeTheta =
                        ComplexNumbers.euler (Real.negate one)
                in
                Expect.equal complexNumberConjugate complexNumberNegativeTheta
        , Test.test
            "tests ComplexNumbers root"
          <|
            \_ ->
                let
                    number =
                        ComplexNumbers.ComplexNumber
                            Real.one
                            Imaginary.i

                    cubeRoot =
                        ComplexNumbers.roots 3 number
                            |> List.map (ComplexNumbers.round 4)

                    expected =
                        [ ComplexNumbers.ComplexNumber (Real.Real -0.7937) (Imaginary.Imaginary (Real.Real 0.7937))
                        , ComplexNumbers.ComplexNumber (Real.Real -0.2905) (Imaginary.Imaginary (Real.Real -1.0842))
                        , ComplexNumbers.ComplexNumber (Real.Real 1.0842) (Imaginary.Imaginary (Real.Real 0.2905))
                        ]
                in
                Expect.equalLists cubeRoot expected
        , Test.test
            "test DeMoivre's equation"
          <|
            \_ ->
                let
                    testComplexNumber =
                        ComplexNumbers.deMoivre (Real.Real <| Basics.pi / 6) 2

                    expected =
                        ComplexNumbers.ComplexNumber (Real.Real (1 / 2)) (Imaginary.Imaginary (Real.Real (Basics.sqrt 3 / 2)))
                in
                Expect.true "(e^30*i)^2 == 0.5 + (sqrt 3/2)i"
                    (ComplexNumbers.equal.eq
                        testComplexNumber
                        expected
                    )
        ]
