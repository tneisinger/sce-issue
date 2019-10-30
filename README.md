This repo demonstrates how to use the servant-quickcheck and
servant-checked-exceptions libraries together.  In order to use both libraries,
an instance of `HasGenRequest (Throws err :> rest)` needs to be defined.  Here is
simple way to define that instance:

```
instance (HasGenRequest rest) => HasGenRequest (Throws err :> rest) where
  genRequest _ = genRequest (Proxy :: Proxy rest)
```

Check out [test/Spec.hs](test/Spec.hs) for more detail, or clone this repo and
run `stack test` to verify that this works.
