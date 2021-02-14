module Real exposing
    ( Real(..)
    , zero
    , one
    , real
    , negate
    , map
    , pure
    , andMap
    , andThen
    , equal
    )

{-| A module for Real numbers


# Types

@docs Real


# Values

@docs zero
@docs one


# Arithmetic operations on complex numbers

@docs real
@docs negate


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs map
@docs pure
@docs andMap
@docs andThen

#Equality

@docs equal

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
import Ring
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
    Real Float
    -> Real Float
    -> Real Float
multiply (Real realOne) (Real realTwo) =
    realOne
        * realTwo
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
negate rl =
    Real -(real rl)


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
    Real <| real <| f previousReal


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


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexSumSemigroup : Semigroup.Semigroup (Real number)
complexSumSemigroup =
    add


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexProductSemigroup : Semigroup.Semigroup (Real Float)
complexProductSemigroup =
    multiply


{-| Semigroup for Complex Numbers with addition as the operation
-}
complexSumCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real number)
complexSumCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSumSemigroup


{-| Semigroup for Complex Numbers with multiplicatoin as the operation
-}
complexProductCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real Float)
complexProductCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexProductSemigroup


{-| Monoid for Complex Numbers with addition as the operation
-}
complexSumMonoid : Monoid.Monoid (Real number)
complexSumMonoid =
    Monoid.semigroupAndIdentity complexSumSemigroup sumEmpty


{-| Monoid for Complex Numbers with multiplication as the operation
-}
complexProductMonoid : Monoid.Monoid (Real Float)
complexProductMonoid =
    Monoid.semigroupAndIdentity complexProductSemigroup productEmpty


{-| Monoid for Complex Numbers with addition as the operation
-}
complexSumCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real number)
complexSumCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexSumMonoid


{-| Monoid for Complex Numbers with multiplication as the operation
-}
complexProductCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real Float)
complexProductCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexProductMonoid


{-| Group for Complex Numbers with addition as the operation
-}
complexSumGroup : Group.Group (Real number)
complexSumGroup =
    { monoid = complexSumMonoid, inverse = negate }


{-| Group for Complex Numbers with multiplication as the operation
-}
complexProductGroup : Group.Group (Real Float)
complexProductGroup =
    { monoid = complexProductMonoid, inverse = divide one }


{-| Group for Complex Numbers with addition as the operation
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (Real number)
complexAbelianGroup =
    AbelianGroup.AbelianGroup complexSumGroup


{-| Ring for Complex Numbers
-}
complexRing : Ring.Ring (Real Float)
complexRing =
    { addition = complexAbelianGroup, multiplication = complexProductMonoid }


{-| Division Ring for Complex Numbers
-}
complexDivisionRing : DivisionRing.DivisionRing (Real Float)
complexDivisionRing =
    { addition = complexAbelianGroup, multiplication = complexProductGroup }


{-| Commutative Ring for Complex Numbers
-}
complexCommutativeRing : CommutativeRing.CommutativeRing (Real Float)
complexCommutativeRing =
    CommutativeRing.CommutativeRing complexRing


{-| Commutative Division Ring for Complex Numbers
-}
complexCommutativeDivisionRing : CommutativeDivisionRing.CommutativeDivisionRing (Real Float)
complexCommutativeDivisionRing =
    CommutativeDivisionRing.CommutativeDivisionRing complexDivisionRing


{-| Field for Complex Numbers
-}
complexField : Field.Field (Real Float)
complexField =
    Field.Field complexCommutativeDivisionRing
