module ComplexNumbers exposing
    ( ComplexNumber(..)
    , i
    , zero
    , one
    , real
    , imaginary
    , add
    , multiply
    , subtract
    , divide
    , modulus
    , conjugate
    , imaginaryAxisReflection
    , power
    , convertFromCartesianToPolar
    , convertFromPolarToCartesian
    , euler
    , sumSemigroup, productSemigroup, sumCommutativeSemigroup, productCommutativeSemigroup
    , sumMonoid, productMonoid, sumCommutativeMonoid, productCommutativeMonoid
    , sumGroup, productGroup, abelianGroup
    , ring, divisionRing, commutativeRing, commutativeDivisionRing
    , field
    , map
    , pure
    , andMap
    , andThen
    , equal
    , parseComplexNumber
    , read
    , print
    , printiNotation
    , printiNotationWithRounding
    )

{-| A module for complex numbers


# Types

@docs ComplexNumber


# Values

@docs i
@docs zero
@docs one


# Arithmetic operations on complex numbers

@docs real
@docs imaginary
@docs add
@docs multiply
@docs subtract
@docs divide
@docs modulus
@docs conjugate
@docs imaginaryAxisReflection
@docs power
@docs convertFromCartesianToPolar
@docs convertFromPolarToCartesian
@docs euler


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs sumSemigroup, productSemigroup, sumCommutativeSemigroup, productCommutativeSemigroup
@docs sumMonoid, productMonoid, sumCommutativeMonoid, productCommutativeMonoid
@docs sumGroup, productGroup, abelianGroup
@docs ring, divisionRing, commutativeRing, commutativeDivisionRing
@docs field
@docs map
@docs pure
@docs andMap
@docs andThen

#Equality

@docs equal


# Read and Print

@docs parseComplexNumber
@docs read
@docs print
@docs printiNotation
@docs printiNotationWithRounding

-}

import AbelianGroup
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeRing
import CommutativeSemigroup
import DivisionRing
import Field
import Group
import Imaginary
import Internal.ComplexNumbers
import Monoid
import Parser exposing ((|.), (|=))
import Real
import Ring
import Round
import Semigroup
import Typeclasses.Classes.Equality



-- Types


{-| Cartesian representation of a complex number
-}
type ComplexNumber a
    = ComplexNumber (Real.Real a) (Imaginary.Imaginary a)


{-| zero
-}
zero : ComplexNumber number
zero =
    ComplexNumber Real.zero Imaginary.zero


{-| one
-}
one : ComplexNumber number
one =
    ComplexNumber Real.one Imaginary.zero


{-| The number i
-}
i : ComplexNumber number
i =
    ComplexNumber Real.zero Imaginary.i


{-| Extracts the real part of a complex number
-}
real : ComplexNumber a -> Real.Real a
real (ComplexNumber rl _) =
    rl


{-| Extracts the imaginary part of a complex number
-}
imaginary : ComplexNumber a -> Imaginary.Imaginary a
imaginary (ComplexNumber _ imag) =
    imag


{-| Add two complex numbers together
-}
add :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
add complexOne complexTwo =
    map2 (+) complexOne complexTwo


sumEmpty : ComplexNumber number
sumEmpty =
    zero


{-| Multiply two complex numbers together
-}
multiply :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
multiply complexNumberOne complexNumberTwo =
    Internal.ComplexNumbers.multiply
        (convertFromCartesianToPolar complexNumberOne)
        (convertFromCartesianToPolar complexNumberTwo)
        |> convertFromPolarToCartesian


productEmpty : ComplexNumber number
productEmpty =
    one


{-| Subtract two complex numbers together
-}
subtract :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
subtract complexNumberOne complexNumberTwo =
    map2 (-) complexNumberOne complexNumberTwo


{-| Divide two complex numbers together
-}
divide :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
divide complexNumberDividend complexNumberCartesianDivisor =
    Internal.ComplexNumbers.divide
        (convertFromCartesianToPolar complexNumberDividend)
        (convertFromCartesianToPolar complexNumberCartesianDivisor)
        |> convertFromPolarToCartesian


{-| Calculate the modulus of a complex number
-}
modulus : ComplexNumber Float -> Float
modulus (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    (rl ^ 2 + imag ^ 2)
        |> sqrt


{-| Calculate the conjugate of a complex number
-}
conjugate : ComplexNumber number -> ComplexNumber number
conjugate (ComplexNumber rl img) =
    ComplexNumber rl (Imaginary.negate img)


{-| Calculate the imaginary axis reflection of a complex number
-}
imaginaryAxisReflection : ComplexNumber number -> ComplexNumber number
imaginaryAxisReflection (ComplexNumber rl img) =
    ComplexNumber (Real.negate rl) img


{-| Convert from the Cartesian representation of a complex number to the polar representation
-}
convertFromCartesianToPolar :
    ComplexNumber Float
    -> Internal.ComplexNumbers.ComplexNumber Float
convertFromCartesianToPolar (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    let
        polar =
            toPolar ( rl, imag )
    in
    Internal.ComplexNumbers.ComplexNumber
        (Internal.ComplexNumbers.Modulus <| Tuple.first polar)
        (Internal.ComplexNumbers.Theta <| Tuple.second polar)


{-| Convert from the polar representation of a complex number to the Cartesian representation
-}
convertFromPolarToCartesian :
    Internal.ComplexNumbers.ComplexNumber Float
    -> ComplexNumber Float
convertFromPolarToCartesian (Internal.ComplexNumbers.ComplexNumber (Internal.ComplexNumbers.Modulus ro) (Internal.ComplexNumbers.Theta theta)) =
    let
        cartesian =
            fromPolar ( ro, theta )
    in
    ComplexNumber (Real.Real <| Tuple.first cartesian) (Imaginary.Imaginary <| Tuple.second cartesian)


{-| Map over a complex number
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber rl img) =
    ComplexNumber (Real.map f rl) (Imaginary.map f img)


{-| Place a value in the minimal Complex Number Cartesian context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber (Real.pure a) (Imaginary.pure a)


{-| Apply for Complex Number Cartesian representaiton applicative
-}
andMap :
    ComplexNumber a
    -> ComplexNumber (a -> b)
    -> ComplexNumber b
andMap (ComplexNumber rl imag) (ComplexNumber fReal fImaginary) =
    ComplexNumber (Real.andMap rl fReal) (Imaginary.andMap imag fImaginary)


{-| Monadic bind for Complex Number Cartesian representaiton
-}
andThen :
    (a -> ComplexNumber b)
    -> ComplexNumber a
    -> ComplexNumber b
andThen f (ComplexNumber (Real.Real previousReal) (Imaginary.Imaginary previousImaginary)) =
    ComplexNumber
        (real <| f previousReal)
        (imaginary <| f previousImaginary)


{-| Lift a binary function to work with complex numbers
-}
map2 :
    (a -> b -> c)
    -> ComplexNumber a
    -> ComplexNumber b
    -> ComplexNumber c
map2 f a b =
    andMap b (map f a)


{-| Equality of Complex Numbers
-}
equalImplementation :
    ComplexNumber Float
    -> ComplexNumber Float
    -> Bool
equalImplementation (ComplexNumber realOne imaginaryOne) (ComplexNumber realTwo imaginaryTwo) =
    Real.equal.eq realOne realTwo
        && Imaginary.equal.eq imaginaryOne imaginaryTwo


{-| `Equal` type for `ComplexNumber`.
-}
equal : Typeclasses.Classes.Equality.Equality (ComplexNumber Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Calculate a complex number raised to a power
-}
power : Float -> ComplexNumber Float -> ComplexNumber Float
power n complexNumber =
    Internal.ComplexNumbers.power n (convertFromCartesianToPolar complexNumber)
        |> convertFromPolarToCartesian


{-| Print ComplexNumber
-}
print : ComplexNumber Float -> String
print (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    "ComplexNumber Real.Real "
        ++ String.fromFloat rl
        ++ " Imaginary.Imaginary "
        ++ String.fromFloat imag


{-| Print ComplexNumber i notation with rounding function
-}
printiNotationWithRounding : (Float -> String) -> ComplexNumber Float -> String
printiNotationWithRounding toString (ComplexNumber (Real.Real rl) (Imaginary.Imaginary imag)) =
    (if rl < 0 then
        "−"

     else
        "+"
    )
        ++ toString (Basics.abs rl)
        ++ (if imag < 0 then
                "−"

            else
                "+"
           )
        ++ toString (Basics.abs imag)
        ++ "i"


{-| Print ComplexNumber i notation with two decimal places
-}
printiNotation : ComplexNumber Float -> String
printiNotation =
    printiNotationWithRounding (Round.round 2)


{-| Read ComplexNumber
-}
read : String -> Result (List Parser.DeadEnd) (ComplexNumber Float)
read vectorString =
    Parser.run parseComplexNumber vectorString


{-| Parse ComplexNumber
-}
parseComplexNumber : Parser.Parser (ComplexNumber Float)
parseComplexNumber =
    Parser.succeed ComplexNumber
        |. Parser.keyword "ComplexNumber"
        |. Parser.spaces
        |= Real.parseReal
        |. Parser.spaces
        |= Imaginary.parseImaginary


{-| Semigroup for Complex Numbers with addition as the operation
-}
sumSemigroup : Semigroup.Semigroup (ComplexNumber number)
sumSemigroup =
    add


{-| Semigroup for Complex Numbers with addition as the operation
-}
productSemigroup : Semigroup.Semigroup (ComplexNumber Float)
productSemigroup =
    multiply


{-| Semigroup for Complex Numbers with addition as the operation
-}
sumCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ComplexNumber number)
sumCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup sumSemigroup


{-| Semigroup for Complex Numbers with multiplicatoin as the operation
-}
productCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (ComplexNumber Float)
productCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup productSemigroup


{-| Monoid for Complex Numbers with addition as the operation
-}
sumMonoid : Monoid.Monoid (ComplexNumber number)
sumMonoid =
    Monoid.semigroupAndIdentity sumSemigroup sumEmpty


{-| Monoid for Complex Numbers with multiplication as the operation
-}
productMonoid : Monoid.Monoid (ComplexNumber Float)
productMonoid =
    Monoid.semigroupAndIdentity productSemigroup productEmpty


{-| Monoid for Complex Numbers with addition as the operation
-}
sumCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ComplexNumber number)
sumCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid sumMonoid


{-| Monoid for Complex Numbers with multiplication as the operation
-}
productCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (ComplexNumber Float)
productCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid productMonoid


{-| Group for Complex Numbers with addition as the operation
-}
sumGroup : Group.Group (ComplexNumber number)
sumGroup =
    { monoid = sumMonoid, inverse = \(ComplexNumber rl imag) -> ComplexNumber (Real.negate rl) (Imaginary.negate imag) }


{-| Group for Complex Numbers with multiplication as the operation
-}
productGroup : Group.Group (ComplexNumber Float)
productGroup =
    { monoid = productMonoid, inverse = divide one }


{-| Group for Complex Numbers with addition as the operation
-}
abelianGroup : AbelianGroup.AbelianGroup (ComplexNumber number)
abelianGroup =
    AbelianGroup.AbelianGroup sumGroup


{-| Ring for Complex Numbers
-}
ring : Ring.Ring (ComplexNumber Float)
ring =
    { addition = abelianGroup, multiplication = productMonoid }


{-| Division Ring for Complex Numbers
-}
divisionRing : DivisionRing.DivisionRing (ComplexNumber Float)
divisionRing =
    { addition = abelianGroup, multiplication = productGroup }


{-| Commutative Ring for Complex Numbers
-}
commutativeRing : CommutativeRing.CommutativeRing (ComplexNumber Float)
commutativeRing =
    CommutativeRing.CommutativeRing ring


{-| Commutative Division Ring for Complex Numbers
-}
commutativeDivisionRing : CommutativeDivisionRing.CommutativeDivisionRing (ComplexNumber Float)
commutativeDivisionRing =
    CommutativeDivisionRing.CommutativeDivisionRing divisionRing


{-| Field for Complex Numbers
-}
field : Field.Field (ComplexNumber Float)
field =
    Field.Field commutativeDivisionRing


{-| Euler's equation
-}
euler : Float -> ComplexNumber Float
euler theta =
    ComplexNumber (Real.Real <| Basics.cos theta) (Imaginary.Imaginary <| Basics.sin theta)
