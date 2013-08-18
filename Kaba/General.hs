module General
    ( (-->)
    , Arguments
    , Command
    , Handler
    ) where


(-->) :: a -> b -> (a, b)
(-->) = (,)


type Arguments = [String]
type Command   = String
type Handler   = Arguments -> FilePath -> IO ()
