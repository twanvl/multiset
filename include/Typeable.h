{- --------------------------------------------------------------------------
// Macros to help make Typeable instances.
//
// INSTANCE_TYPEABLEn(tc,tcname,"tc") defines
//
//      instance Typeable/n/ tc
//      instance Typeable a => Typeable/n-1/ (tc a)
//      instance (Typeable a, Typeable b) => Typeable/n-2/ (tc a b)
//      ...
//      instance (Typeable a1, ..., Typeable an) => Typeable (tc a1 ... an)
// --------------------------------------------------------------------------
-}

#ifndef TYPEABLE_H
#define TYPEABLE_H

#if __GLASGOW_HASKELL__ >= 707
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable tycon
#elif defined(__GLASGOW_HASKELL__)
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable1 tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable2 tycon
#define INSTANCE_TYPEABLE3(tycon,tcname,str) deriving instance Typeable3 tycon
#else
#define INSTANCE_TYPEABLE0(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }
#define INSTANCE_TYPEABLE1(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }
#define INSTANCE_TYPEABLE2(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable1 (tycon a) where { typeOf1 = typeOf1Default }; \
  instance (Typeable a, Typeable b) => Typeable (tycon a b) where { typeOf = typeOfDefault }
#endif

#endif
