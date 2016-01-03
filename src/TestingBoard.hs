module TestingBoard where 

import Data.Attoparsec.ByteString.Char8

import FEN 
import Board hiding ()
import qualified Board as B

unsafeParseFEN = either (error "unsafeParseFEN failed") fromFEN . parseOnly parseFEN  

tests = fmap unsafeParseFEN [
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
        "8/8/8/Pp6/8/3N4/8/8 w - b6 0 3"]
