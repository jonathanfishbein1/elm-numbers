module Imaginary exposing
    ( Imaginary(..)
    , i
    , zero
    , negativeI
    , imaginary
    , negate
    , multiply
    , map
    , pure
    , andMap
    , andThen
    , equal
    , parseImaginary
    , print
    , printiNotationWithRounding, round
    )

{-| A module for Imaginary numbers


# Types

@docs Imaginary


# Values

@docs i
@docs zero
@docs negativeI


# Arithmetic operations on imaginary numbers

@docs imaginary
@docs negate


# Binary operations

@docs multiply


# Semigroup, Monoid, Group, Ring, Field, Functor, Applicative Functor, and Monad

@docs map
@docs pure
@docs andMap
@docs andThen

#Equality

@docs equal


# Read and Print

@docs parseImaginary
@docs print

-}

import Parser exposing ((|.), (|=))
import Real
import Round
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


{-| i
-}
negativeI : Imaginary number
negativeI =
    Imaginary Real.negativeOne


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


{-| Multiply two complex numbers together
-}
multiply :
    Imaginary number
    -> Imaginary number
    -> Imaginary number
multiply (Imaginary imaginaryOne) (Imaginary imaginaryTwo) =
    Real.multiply imaginaryOne imaginaryTwo
        |> Imaginary


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


round : Int -> Imaginary Float -> Imaginary Float
round numberOfDigits (Imaginary num) =
    Real.round numberOfDigits num
        |> Imaginary


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


{-| Print Real i notation with rounding function
-}
printiNotationWithRounding : (Float -> String) -> Imaginary Float -> String
printiNotationWithRounding toString (Imaginary img) =
    Real.printiNotationWithRounding toString img ++ "i"
