{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Util.Tuples where

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{-
    Overloaded tuple extraction
-}
class Pair p a b | p -> a b where
    first      :: p -> a
    second     :: p -> b
    set_first  :: p -> a -> p
    set_second :: p -> b -> p
	
class Pair p a b => Triple p a b c | p -> c where
    third     :: p -> c
    set_third :: p -> c -> p
	
class Triple p a b c => Quad p a b c d | p -> d where
    fourth     :: p -> d
    set_fourth :: p -> d -> p
    
class Quad p a b c d => Quint p a b c d e | p -> e where
    fifth     :: p -> e
    set_fifth :: p -> e -> p
    
class Quint p a b c d e => Sext p a b c d e f | p -> f where
    sixth     :: p -> f
    set_sixth :: p -> f -> p
    
instance Pair (a,b) a b where
    first      (x, y)   = x
    second     (x, y)   = y
    set_first  (_, y) x = (x, y)
    set_second (x, _) y = (x, y)

instance Pair (a,b,c) a b where
    first      (x,_,_)     = x
    second     (_,y,_)     = y
    set_first  (_, y, z) x = (x, y, z)
    set_second (x, _, z) y = (x, y, z)

instance Pair (a,b,c,d) a b where
    first  (x,_,_,_) = x
    second (_,x,_,_) = x
    set_first  (_, y, z, w) x = (x, y, z, w)
    set_second (x, _, z, w) y = (x, y, z, w)

instance Pair (a,b,c,d,e) a b where
    first  (x,_,_,_,_) = x
    second (_,x,_,_,_) = x
    set_first  (_, y, z, w, v) x = (x, y, z, w, v)
    set_second (x, _, z, w, v) y = (x, y, z, w, v)

instance Pair (a,b,c,d,e,f) a b where
    first  (x,_,_,_,_,_) = x
    second (_,x,_,_,_,_) = x
    set_first  (_, y, z, w, v, u) x = (x, y, z, w, v, u)
    set_second (x, _, z, w, v, u) y = (x, y, z, w, v, u)
	
instance Pair [a] a a where
    first  (x:_)   = x
    second (_:x:_) = x
    set_first  (_:xs)   x  = x:xs
    set_second (x:_:xs) x' = x:(x':xs)
	
instance Triple (a,b,c) a b c where
    third (_,_,x) = x
    set_third (x,y,_) z = (x,y,z)
	
instance Triple (a,b,c,d) a b c where
    third (_,_,x,_) = x
    set_third (x,y,_,w) z = (x,y,z,w)
	
instance Triple (a,b,c,d,e) a b c where
    third (_,_,x,_,_) = x
    set_third (x,y,_,w,v) z = (x,y,z,w,v)
	
instance Triple (a,b,c,d,e,f) a b c where
    third (_,_,x,_,_,_) = x
    set_third (x,y,_,w,v,u) z = (x,y,z,w,v,u)
	
instance Triple [a] a a a where
    third (_:_:x:_) = x
    set_third (x:y:_:xs) z = x:y:z:xs
    
instance Quad (a,b,c,d) a b c d where
    fourth (_,_,_,x) = x
    set_fourth (x,y,z,_) w = (x,y,z,w)
    
instance Quad (a,b,c,d,e) a b c d where
    fourth (_,_,_,x,_) = x
    set_fourth (x,y,z,_,v) w = (x,y,z,w,v)
    
instance Quad (a,b,c,d,e,f) a b c d where
    fourth (_,_,_,x,_,_) = x
    set_fourth (x,y,z,_,v,u) w = (x,y,z,w,v,u)
    
instance Quad [a] a a a a where
    fourth (_:_:_:x:_) = x
    set_fourth (x:y:z:_:xs) w = x:y:z:w:xs

instance Quint (a,b,c,d,e) a b c d e where
    fifth (_,_,_,_,x) = x
    set_fifth (x,y,z,w,_) v = (x,y,z,w,v)
    
instance Quint (a,b,c,d,e,f) a b c d e where
    fifth (_,_,_,_,x,_) = x
    set_fifth (x,y,z,w,_,u) v = (x,y,z,w,v,u)
    
instance Quint [a] a a a a a where
    fifth (_:_:_:_:x:_) = x
    set_fifth (x:y:z:w:_:xs) v = x:y:z:w:v:xs
    
instance Sext (a,b,c,d,e,f) a b c d e f where
    sixth (_,_,_,_,_,x) = x
    set_sixth (x,y,z,w,v,_) u = (x,y,z,w,v,u)
    
instance Sext [a] a a a a a a where
    sixth (_:_:_:_:_:x:_) = x
    set_sixth (x:y:z:w:v:_:xs) u = x:y:z:w:v:u:xs

{-
    Overloaded sum extraction
-}
class Sum2 s a b | s -> a b where
    sum2 :: s -> Either a b
    seither2 :: (a -> c) -> (b -> c) -> s -> c
    seither2 f g s = either f g (sum2 s)
    
instance Sum2 (Either a b) a b where
    sum2 = id
    
instance Sum2 (Maybe a) a () where
    sum2 (Just x) = Left x
    sum2 Nothing  = Right ()
    