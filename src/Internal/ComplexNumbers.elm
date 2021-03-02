module Internal.ComplexNumbers exposing
    ( ComplexNumber(..)
    , Modulus(..)
    , Theta(..)
    , andMap
    , andThen
    , divide
    , map
    , map2
    , modulus
    , multiply
    , power
    , product
    , productEmpty
    , pure
    , roots
    , theta
    )

{-| Modulus or magnitude portion
-}

import Monoid
import Real


type Modulus m
    = Modulus (Real.Real m)


{-| Angle in real-complex plane of modulus
-}
type Theta t
    = Theta (Real.Real t)


{-| Polar representation of a complex number
-}
type ComplexNumber a
    = ComplexNumber (Modulus a) (Theta a)


{-| Extracts the modulus part of a complex number
-}
modulus : ComplexNumber a -> Real.Real a
modulus (ComplexNumber (Modulus ro) _) =
    ro


{-| Extracts the imaginary part of a complex number
-}
theta : ComplexNumber a -> Real.Real a
theta (ComplexNumber _ (Theta thta)) =
    thta


{-| Multiply two complex numbers in polar representations together
-}
multiply :
    ComplexNumber number
    -> ComplexNumber number
    -> ComplexNumber number
multiply (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber
        (Real.multiply roOne
            roTwo
            |> Modulus
        )
        (Real.add thetaOne
            thetaTwo
            |> Theta
        )


{-| Divide two complex numbers in polar representations together
-}
divide :
    ComplexNumber Float
    -> ComplexNumber Float
    -> ComplexNumber Float
divide (ComplexNumber (Modulus roOne) (Theta thetaOne)) (ComplexNumber (Modulus roTwo) (Theta thetaTwo)) =
    ComplexNumber
        (Real.divide roOne
            roTwo
            |> Modulus
        )
        (Real.subtract thetaOne
            thetaTwo
            |> Theta
        )


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumber number -> ComplexNumber number
power n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    ComplexNumber
        (Real.power roOne
            (n
                |> Real.Real
            )
            |> Modulus
        )
        (Real.multiply (Real.Real n)
            thetaOne
            |> Theta
        )


{-| Calculate the roots of a complex number
-}
roots : Int -> ComplexNumber Float -> List (ComplexNumber Float)
roots n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    List.map
        (\k ->
            ComplexNumber
                (Real.power roOne
                    ((1 / Basics.toFloat n)
                        |> Real.Real
                    )
                    |> Modulus
                )
                (Real.multiply (Real.Real (1 / Basics.toFloat n))
                    (Real.add thetaOne (Real.Real (Basics.toFloat k * 2 * Basics.pi)))
                    |> Theta
                )
        )
        (List.range 1 n)


{-| Map over a complex number in polar representation
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber (Modulus ro) (Theta thta)) =
    ComplexNumber
        (Real.map f ro
            |> Modulus
        )
        (Real.map f thta
            |> Theta
        )


{-| Place a value in the minimal Complex Number polar context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber
        (a
            |> Real.pure
            |> Modulus
        )
        (a
            |> Real.pure
            |> Theta
        )


{-| Apply for Complex Number polar representaiton applicative
-}
andMap :
    ComplexNumber a
    -> ComplexNumber (a -> b)
    -> ComplexNumber b
andMap (ComplexNumber (Modulus ro) (Theta thta)) (ComplexNumber (Modulus fRo) (Theta fTheta)) =
    ComplexNumber
        (Real.andMap ro fRo
            |> Modulus
        )
        (Real.andMap thta fTheta
            |> Theta
        )


{-| Monadic andThen for Complex Number polar representaiton
-}
andThen :
    (a -> ComplexNumber b)
    -> ComplexNumber a
    -> ComplexNumber b
andThen f (ComplexNumber (Modulus previousModulus) (Theta previousTheta)) =
    ComplexNumber
        (Real.andThen (f >> modulus) previousModulus
            |> Modulus
        )
        (Real.andThen (f >> theta) previousTheta
            |> Theta
        )


map2 :
    (a -> b -> c)
    -> ComplexNumber a
    -> ComplexNumber b
    -> ComplexNumber c
map2 f a b =
    andMap b (map f a)


{-| one
-}
one : ComplexNumber number
one =
    ComplexNumber (Modulus Real.one) (Theta Real.zero)


productEmpty : ComplexNumber number
productEmpty =
    one


{-| Monoidally multiply two complex numbers together
-}
product : Monoid.Monoid (ComplexNumber Float)
product =
    Monoid.semigroupAndIdentity
        multiply
        productEmpty
