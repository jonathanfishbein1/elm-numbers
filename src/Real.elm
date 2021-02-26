module Real exposing
    ( Real(..)
    , zero
    , one
    , negativeOne
    , real
    , negate
    , round
    , add, multiply, divide, greaterThan, power
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
    , print
    , parseReal
    , printNotationWithRounding
    )

{-| A module for Real numbers


# Types

@docs Real


# Values

@docs zero
@docs one
@docs negativeOne


# Arithmetic operations on real numbers

@docs real
@docs negate
@docs round


# Binary operations

@docs add, multiply, divide, greaterThan, power


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

@docs print
@docs parseReal
@docs printNotationWithRounding

-}

import AbelianGroup
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeRing
import CommutativeSemigroup
import DivisionRing
import Field
import Float.Extra
import Group
import Monoid
import Parser exposing ((|.), (|=))
import Ring
import Round
import Semigroup
import Typeclasses.Classes.Equality


{-| Real portion
-}
type Real r
    = Real r


{-| zero
-}
zero : Real number
zero =
    Real 0


{-| one
-}
one : Real number
one =
    Real 1


{-| one
-}
negativeOne : Real number
negativeOne =
    Real -1


{-| Add two complex numbers together
-}
add :
    Real number
    -> Real number
    -> Real number
add (Real realOne) (Real realOneTwo) =
    realOne
        + realOneTwo
        |> Real


sumEmpty : Real number
sumEmpty =
    zero


{-| Multiply two complex numbers together
-}
multiply :
    Real number
    -> Real number
    -> Real number
multiply (Real realOne) (Real realTwo) =
    realOne
        * realTwo
        |> Real


{-| Multiply two complex numbers together
-}
power :
    Real Float
    -> Real Float
    -> Real Float
power (Real rl) (Real exp) =
    rl
        ^ exp
        |> Real


productEmpty : Real number
productEmpty =
    one


{-| Divide two complex numbers together
-}
divide :
    Real Float
    -> Real Float
    -> Real Float
divide (Real realDividend) (Real realDivisor) =
    realDividend
        / realDivisor
        |> Real


{-| Negate a Real number
-}
negate : Real number -> Real number
negate (Real rl) =
    Real -rl


{-| Extracts the value of a Real number
-}
real : Real a -> a
real (Real rl) =
    rl


{-| Map over a Real number
-}
map : (a -> b) -> Real a -> Real b
map f (Real r) =
    Real <| f r


{-| Place a value in the minimal Real Number context
-}
pure : a -> Real a
pure a =
    Real a


{-| Apply for Real Number representaiton applicative
-}
andMap :
    Real a
    -> Real (a -> b)
    -> Real b
andMap (Real rl) (Real fReal) =
    Real <| fReal rl


{-| Monadic bind for Real Number representaiton
-}
andThen :
    (a -> Real b)
    -> Real a
    -> Real b
andThen f (Real previousReal) =
    f previousReal


{-| Equality of Real Numbers
-}
equalImplementation :
    Real Float
    -> Real Float
    -> Bool
equalImplementation (Real realOne) (Real realTwo) =
    Float.Extra.equalWithin 0.000000001 realOne realTwo


{-| `Equal` type for `Real`.
-}
equal : Typeclasses.Classes.Equality.Equality (Real Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Greater than of Real Numbers
-}
greaterThan : Real number -> Real number -> Bool
greaterThan (Real realOne) (Real realTwo) =
    realOne <= realTwo


{-| Round Real Number
-}
round : Int -> Real Float -> Real Float
round numberOfDigits (Real num) =
    Round.roundNum numberOfDigits num
        |> Real


{-| Semigroup for Real Numbers with addition as the operation
-}
sumSemigroup : Semigroup.Semigroup (Real number)
sumSemigroup =
    add


{-| Semigroup for Real Numbers with addition as the operation
-}
productSemigroup : Semigroup.Semigroup (Real number)
productSemigroup =
    multiply


{-| Semigroup for Real Numbers with addition as the operation
-}
sumCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real number)
sumCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup sumSemigroup


{-| Semigroup for Real Numbers with multiplicatoin as the operation
-}
productCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real number)
productCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup productSemigroup


{-| Monoid for Real Numbers with addition as the operation
-}
sumMonoid : Monoid.Monoid (Real number)
sumMonoid =
    Monoid.semigroupAndIdentity sumSemigroup sumEmpty


{-| Monoid for Real Numbers with multiplication as the operation
-}
productMonoid : Monoid.Monoid (Real number)
productMonoid =
    Monoid.semigroupAndIdentity productSemigroup productEmpty


{-| Monoid for Real Numbers with addition as the operation
-}
sumCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real number)
sumCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid sumMonoid


{-| Monoid for Real Numbers with multiplication as the operation
-}
productCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real number)
productCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid productMonoid


{-| Group for Real Numbers with addition as the operation
-}
sumGroup : Group.Group (Real number)
sumGroup =
    { monoid = sumMonoid, inverse = negate }


{-| Group for Real Numbers with multiplication as the operation
-}
productGroup : Group.Group (Real Float)
productGroup =
    { monoid = productMonoid, inverse = divide one }


{-| Group for Real Numbers with addition as the operation
-}
abelianGroup : AbelianGroup.AbelianGroup (Real number)
abelianGroup =
    AbelianGroup.AbelianGroup sumGroup


{-| Ring for Real Numbers
-}
ring : Ring.Ring (Real Float)
ring =
    { addition = abelianGroup, multiplication = productMonoid }


{-| Division Ring for Real Numbers
-}
divisionRing : DivisionRing.DivisionRing (Real Float)
divisionRing =
    { addition = abelianGroup, multiplication = productGroup }


{-| Commutative Ring for Real Numbers
-}
commutativeRing : CommutativeRing.CommutativeRing (Real Float)
commutativeRing =
    CommutativeRing.CommutativeRing ring


{-| Commutative Division Ring for Real Numbers
-}
commutativeDivisionRing : CommutativeDivisionRing.CommutativeDivisionRing (Real Float)
commutativeDivisionRing =
    CommutativeDivisionRing.CommutativeDivisionRing divisionRing


{-| Field for Real Numbers
-}
field : Field.Field (Real Float)
field =
    Field.Field commutativeDivisionRing


float : Parser.Parser Float
float =
    Parser.number
        { int = Just toFloat
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just identity
        }


positiveOrNegativeFloat : Parser.Parser Float
positiveOrNegativeFloat =
    Parser.oneOf
        [ Parser.succeed Basics.negate
            |. Parser.symbol "-"
            |= float
        , float
        ]


{-| Parse Real Number
-}
parseReal : Parser.Parser (Real Float)
parseReal =
    Parser.succeed Real
        |. Parser.keyword "Real.Real"
        |. Parser.spaces
        |= positiveOrNegativeFloat


{-| Print Real Number
-}
print : Real Float -> String
print (Real rl) =
    "Real.Real "
        ++ String.fromFloat rl


{-| Print Real i notation with rounding function
-}
printNotationWithRounding : (Float -> String) -> Real Float -> String
printNotationWithRounding toString (Real rl) =
    (if rl < 0 then
        "−"

     else
        "+"
    )
        ++ toString (Basics.abs rl)
