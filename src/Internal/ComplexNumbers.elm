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


type Modulus m
    = Modulus m


{-| Angle in real-complex plane of modulus
-}
type Theta t
    = Theta t


{-| Polar representation of a complex number
-}
type ComplexNumber a
    = ComplexNumber (Modulus a) (Theta a)


{-| Extracts the modulus part of a complex number
-}
modulus : ComplexNumber a -> a
modulus (ComplexNumber (Modulus ro) _) =
    ro


{-| Extracts the imaginary part of a complex number
-}
theta : ComplexNumber a -> a
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
        (roOne
            * roTwo
            |> Modulus
        )
        (thetaOne
            + thetaTwo
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
        (roOne
            / roTwo
            |> Modulus
        )
        (thetaOne
            - thetaTwo
            |> Theta
        )


{-| Calculate a complex number raised to a power
-}
power : number -> ComplexNumber number -> ComplexNumber number
power n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    ComplexNumber
        (roOne
            ^ n
            |> Modulus
        )
        (n
            * thetaOne
            |> Theta
        )


{-| Calculate the roots of a complex number
-}
roots : Int -> ComplexNumber Float -> List (ComplexNumber Float)
roots n (ComplexNumber (Modulus roOne) (Theta thetaOne)) =
    List.map
        (\k ->
            ComplexNumber
                (roOne
                    ^ (1 / Basics.toFloat n)
                    |> Modulus
                )
                ((1 / Basics.toFloat n)
                    * (thetaOne + (Basics.toFloat k * 2 * Basics.pi))
                    |> Theta
                )
        )
        (List.range 1 n)


{-| Map over a complex number in polar representation
-}
map : (a -> b) -> ComplexNumber a -> ComplexNumber b
map f (ComplexNumber (Modulus ro) (Theta thta)) =
    ComplexNumber
        (f ro
            |> Modulus
        )
        (f thta
            |> Theta
        )


{-| Place a value in the minimal Complex Number polar context
-}
pure : a -> ComplexNumber a
pure a =
    ComplexNumber (Modulus a) (Theta a)


{-| Apply for Complex Number polar representaiton applicative
-}
andMap :
    ComplexNumber a
    -> ComplexNumber (a -> b)
    -> ComplexNumber b
andMap (ComplexNumber (Modulus ro) (Theta thta)) (ComplexNumber (Modulus fRo) (Theta fTheta)) =
    ComplexNumber
        (fRo ro
            |> Modulus
        )
        (fTheta thta
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
        (f previousModulus
            |> modulus
            |> Modulus
        )
        (f previousTheta
            |> theta
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
    ComplexNumber (Modulus 1) (Theta 0)


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
