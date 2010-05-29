
This library povides FFI functions like 'alloca', except polymorphic over Monad. It depends on
the MonadCatchIO library.

MonadCactchIO comes in two flavors: one for mtl and one for transformers. Both can be found on Hackage:

* http://hackage.haskell.org/package/MonadCatchIO-mtl
* http://hackage.haskell.org/package/MonadCatchIO-transformers

This library is consequently offered in two flavors, one for mtl and one for transformers. They share source (under the src directory) but have differing package descriptions.