module Imaginary exposing
    ( Imaginary(..)
    , i
    , zero
    , imaginary
    , negate
    , map
    , pure
    , andMap
    , equal
    , andThen, parseImaginary, print
    )

{-| A module for Imaginary numbers


# Types

@docs Imaginary


# Values

@docs i
@docs zero


# Arithmetic operations on complex numbers

@docs imaginary
@docs negate


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs map
@docs pure
@docs andMap

#Equality

@docs equal

-}

import Parser exposing ((|.), (|=))
import Real
import Typeclasses.Classes.Equality


{-| Imaginary number
-}
type Imaginary i
    = Imaginary (Real.Real i)


{-| i
-}
i : Imaginary number
i =
    Imaginary Real.one


{-| zero
-}
zero : Imaginary number
zero =
    Imaginary Real.zero


{-| Negate an Imaginary number
-}
negate : Imaginary number -> Imaginary number
negate imag =
    Imaginary (Real.negate (imaginary imag))


{-| Extracts the value of an Imaginary number
-}
imaginary : Imaginary a -> Real.Real a
imaginary (Imaginary imag) =
    imag


{-| Map over anImaginary Imaginary number
-}
map : (a -> b) -> Imaginary a -> Imaginary b
map f (Imaginary r) =
    Real.map f r
        |> Imaginary


{-| Place a value in the minimal Imaginary context
-}
pure : a -> Imaginary a
pure a =
    Real.Real a
        |> Imaginary


{-| Apply for Imaginary representaiton applicative
-}
andMap :
    Imaginary a
    -> Imaginary (a -> b)
    -> Imaginary b
andMap (Imaginary imag) (Imaginary fImag) =
    Real.andMap imag fImag
        |> Imaginary


{-| Monadic bind for Imaginary Number representaiton
-}
andThen :
    (a -> Imaginary b)
    -> Imaginary a
    -> Imaginary b
andThen f (Imaginary (Real.Real previousReal)) =
    f previousReal


{-| Equality of Imaginary Numbers
-}
equalImplementation :
    Imaginary Float
    -> Imaginary Float
    -> Bool
equalImplementation (Imaginary realOne) (Imaginary realTwo) =
    Real.equal.eq realOne realTwo


{-| `Equal` type for `Imaginary`.
-}
equal : Typeclasses.Classes.Equality.Equality (Imaginary Float)
equal =
    Typeclasses.Classes.Equality.eq equalImplementation


{-| Parse Imaginary
-}
parseImaginary : Parser.Parser (Imaginary Float)
parseImaginary =
    Parser.succeed Imaginary
        |. Parser.keyword "Imaginary.Imaginary"
        |. Parser.spaces
        |= Real.parseReal


{-| Print Imaginary Number
-}
print : Imaginary Float -> String
print (Imaginary rl) =
    "Imaginary.Imaginary "
        ++ Real.print rl
