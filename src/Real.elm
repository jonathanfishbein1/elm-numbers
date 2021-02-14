module Real exposing
    ( Real(..)
    , zero
    , one
    , real
    , negate
    , sumCommutativeSemigroup, productCommutativeSemigroup
    , sumCommutativeMonoid, productCommutativeMonoid
    , commutativeRing
    , field
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

@docs sumSemigroup, productSemigroup, sumCommutativeSemigroup, productCommutativeSemigroup
@docs sumMonoid, productMonoid, sumCommutativeMonoid, productCommutativeMonoid
@docs sumGroup, productGroup, complexAbelianGroup
@docs complexRing, complexDivisionRing, commutativeRing, commutativeDivisionRing
@docs field
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
sumSemigroup : Semigroup.Semigroup (Real number)
sumSemigroup =
    add


{-| Semigroup for Complex Numbers with addition as the operation
-}
productSemigroup : Semigroup.Semigroup (Real Float)
productSemigroup =
    multiply


{-| Semigroup for Complex Numbers with addition as the operation
-}
sumCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real number)
sumCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup sumSemigroup


{-| Semigroup for Complex Numbers with multiplicatoin as the operation
-}
productCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Real Float)
productCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup productSemigroup


{-| Monoid for Complex Numbers with addition as the operation
-}
sumMonoid : Monoid.Monoid (Real number)
sumMonoid =
    Monoid.semigroupAndIdentity sumSemigroup sumEmpty


{-| Monoid for Complex Numbers with multiplication as the operation
-}
productMonoid : Monoid.Monoid (Real Float)
productMonoid =
    Monoid.semigroupAndIdentity productSemigroup productEmpty


{-| Monoid for Complex Numbers with addition as the operation
-}
sumCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real number)
sumCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid sumMonoid


{-| Monoid for Complex Numbers with multiplication as the operation
-}
productCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Real Float)
productCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid productMonoid


{-| Group for Complex Numbers with addition as the operation
-}
sumGroup : Group.Group (Real number)
sumGroup =
    { monoid = sumMonoid, inverse = negate }


{-| Group for Complex Numbers with multiplication as the operation
-}
productGroup : Group.Group (Real Float)
productGroup =
    { monoid = productMonoid, inverse = divide one }


{-| Group for Complex Numbers with addition as the operation
-}
abelianGroup : AbelianGroup.AbelianGroup (Real number)
abelianGroup =
    AbelianGroup.AbelianGroup sumGroup


{-| Ring for Complex Numbers
-}
complexRing : Ring.Ring (Real Float)
complexRing =
    { addition = abelianGroup, multiplication = productMonoid }


{-| Division Ring for Complex Numbers
-}
complexDivisionRing : DivisionRing.DivisionRing (Real Float)
complexDivisionRing =
    { addition = abelianGroup, multiplication = productGroup }


{-| Commutative Ring for Complex Numbers
-}
commutativeRing : CommutativeRing.CommutativeRing (Real Float)
commutativeRing =
    CommutativeRing.CommutativeRing complexRing


{-| Commutative Division Ring for Complex Numbers
-}
commutativeDivisionRing : CommutativeDivisionRing.CommutativeDivisionRing (Real Float)
commutativeDivisionRing =
    CommutativeDivisionRing.CommutativeDivisionRing complexDivisionRing


{-| Field for Complex Numbers
-}
field : Field.Field (Real Float)
field =
    Field.Field commutativeDivisionRing
