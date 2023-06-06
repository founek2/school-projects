module WebDataTypes exposing (..)

import Http


type WebData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success a


{-| This holds also ID for loading, so UI can for example disable button for particular item
-}
type UpdateData a
    = UpdateNotAsked
    | UpdateLoading Int
    | UpdateFailure Http.Error
    | UpdateSucess a
