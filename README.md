This repo demonstrates an issue in using the servant-quickcheck and
servant-checked-exceptions libraries together.  To reproduce the issue, clone
this repo and run `stack test`.

The issue seems to be that there is not a
Servant.QuickCheck.Internal.HasGenRequest.HasGenRequest instance defined for the
Servant.Checked.Exceptions.Throws type constructor.
