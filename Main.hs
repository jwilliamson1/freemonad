{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where


main = putStrLn $ showProgram program

data Toy b next =
    Output b next
    | Bell next
    | Done

-- output 'A'
-- done
t1 = Output 'A' Done
t2 = Bell (Output 'A' Done)

newtype Cheat f = Cheat (f (Cheat f))

--ewtype Fix f = Fix (f (Fix f))

f1 = Fix (Output 'A' (Fix Done))

f2 = Fix (Bell (Fix (Output 'A' (Fix Done))))

data FixE f e = Fix (f (FixE f e)) | Throw e

catch (Fix x) f = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

data IncompleteException = IncompleteException

--subroutine = Fix (Output 'A' (Throw IncompleteException))

--program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))
 
data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
p1 = Pure 'x'

liftF command = Free (fmap Pure command)
output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"