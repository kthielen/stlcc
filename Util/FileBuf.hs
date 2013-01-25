
module Util.FileBuf where
import Util.Num
import Util.String
import Util.Sequence

import Data.Bits
import Data.Char
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import System.IO
import Directory
import GHC.IOBase
import GHC.Base
import GHC.Ptr
import GHC.Word
import Data.Time.Clock.POSIX

{-
    Utilities for binary file I/O
-}
type FileBuf = (Handle, Ptr Word8, Int)

pushCurLoc :: FileBuf -> Integer -> (Integer -> IO a) -> IO a
pushCurLoc b destloc f = do
    cloc <- tellBuf b;
    jumpBuf b destloc;
    r <- f cloc;
    jumpBuf b cloc;
    return r

atLoc :: FileBuf -> Integer -> IO a -> IO a
atLoc b destloc act = pushCurLoc b destloc (\_ -> act)
    
rewriteCurLoc :: FileBuf -> Integer -> IO ()
rewriteCurLoc b patchloc = pushCurLoc b patchloc (\loc -> writeBuf b (cw32 loc))
    
openBuf :: String -> IOMode -> IO FileBuf
openBuf s m = do
    let blen =  255;
    h        <- openBinaryFile s m;
    b        <- mallocArray blen :: IO (Ptr Word8);
    return (h, b, blen)
    
closeBuf :: FileBuf -> IO ()
closeBuf (h, _, _) = hClose h

tellBuf :: FileBuf -> IO Integer
tellBuf (h, _, _) = hTell h

eofBuf :: FileBuf -> IO Bool
eofBuf (h, _, _) = hIsEOF h

jumpBuf :: FileBuf -> Integer -> IO ()
jumpBuf (h, _, _) n = hSeek h AbsoluteSeek n

readBuf :: FileBuf -> Int -> IO [Word8]
readBuf (h, p, len) n = read n [] where
    read 0 ws           = return ws
    read n ws | n > len = do { ws' <- read len ws; read (n - len) ws' }
    read n ws           = do { n' <- hGetBuf h p n; ws' <- peekArray n p; return $ ws ++ (pad ws' n 0) }

readStr :: FileBuf -> IO String
readStr b = readToByte b 0

readLine :: FileBuf -> IO String
readLine b = readToByte b 0x0A

readToByte :: FileBuf -> Word8 -> IO String
readToByte b c = do
    [c'] <- readBuf b 1;
    rb c'
    where
        rb c' | c' == c = return ""
        rb c'           = do { cs' <- readToByte b c; return ((chr (castn c')):cs') }
        
readChBuf :: FileBuf -> Int -> IO String
readChBuf b n = do
    c <- readBuf b n;
    return (rchbuf c n)
    
readW8 :: FileBuf -> IO Word8
readW8 b = do
    c <- readBuf b 1;
    return (r8 c)
    
readW16 :: FileBuf -> IO Word16
readW16 b = do
    c <- readBuf b 2;
    return (r16 c)
    
readW32 :: FileBuf -> IO Word32
readW32 b = do
    c <- readBuf b 4;
    return (r32 c)
    
writeBufs :: FileBuf -> [[Word8]] -> IO ()
writeBufs b w = writeBuf b (concat w)

writeBufsWith :: FileBuf -> (a -> [Word8]) -> [a] -> IO ()
writeBufsWith b f xs = writeBuf b (concat (map f xs))
    
writeBuf :: FileBuf -> [Word8] -> IO ()
writeBuf (h, p, len) ws = write 0 ws where
    write 0 []            = return ()
    write n []            = hPutBuf h p n
    write n ws | n == len = do { hPutBuf h p len; write 0 ws }
    write n (w:ws)        = do { pokeElemOff p n w; write (n+1) ws }

writeW8 :: FileBuf -> Word8 -> IO ()
writeW8 b c = writeBuf b [c]

writeW16 :: FileBuf -> Word16 -> IO ()
writeW16 b c = writeBuf b (w16 c)

writeW32 :: FileBuf -> Word32 -> IO ()
writeW32 b c = writeBuf b (w32 c)
    
ctime :: IO Word32
ctime = do
    d <- getPOSIXTime;
    return (floor d)

rstr :: [Word8] -> String
rstr cs = map (chr . castn) cs
    
rchbuf :: [Word8] -> Int -> String
rchbuf cs n = map (chr . castn) $ takeWhile (/= 0) $ take n cs
    
r8 :: [Word8] -> Word8
r8 (x:_) = x

r16 :: [Word8] -> Word16
r16 (l:h:_) = ((castn h :: Word16) `shiftL` 8) + (castn l :: Word16)

r32 :: [Word8] -> Word32
r32 (ll:hl:lh:hh:_) = ((castn $ r16 [lh,hh]) `shiftL` 16) + (castn $ r16 [ll,hl]) :: Word32

chbuf :: String -> Int -> [Word8]
chbuf s len = s' ++ pad where
    s'  = str $ take len s
    pad = take (len - length s') $ repeat 0
    
str :: String -> [Word8]
str []     = w8 0
str (c:cs) = (castn (ord c) :: Word8):(str cs)

w8 :: Word8 -> [Word8]
w8 x = [x]

w16 :: Word16 -> [Word8]
w16 x = [lb, hb] where
    lb = castn (x.&.0xff) :: Word8
    hb = castn (shiftR x 8) :: Word8

w32 :: Word32 -> [Word8]
w32 x = w16 lw ++ w16 hw where
    lw = castn (x.&.0xffff) :: Word16
    hw = castn (shiftR x 16) :: Word16

cw8 :: Integral a => a -> [Word8]
cw8 x = w8 (castn x :: Word8)

cw16 :: Integral a => a -> [Word8]
cw16 x = w16 (castn x :: Word16)

cw32 :: Integral a => a -> [Word8]
cw32 x = w32 (castn x :: Word32)

updW32 :: [Word8] -> Int -> Word32 -> [Word8]
updW32 d n w = pfx ++ w32 w ++ drop 4 sfx where
    (pfx, sfx) = splitTake d n

class Serializable a where
    serialize :: a -> [Word8]

instance Serializable Word8 where
    serialize x = w8 x
    
instance Serializable Word16 where
    serialize x = w16 x
    
instance Serializable Word32 where
    serialize x = w32 x
